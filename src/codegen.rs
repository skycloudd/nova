use crate::{
    ast::{BinaryOp, UnaryOp},
    low_ir::{
        BasicBlock, BasicBlockId, Expression, Function, Instruction, Terminator, TopLevel,
        TypedExpression,
    },
    mir::{FuncId, VarId},
    mir_no_span::Type,
    IdGen,
};
use cranelift::{
    codegen::{ir::Type as IrType, Context},
    prelude::*,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};
use log::debug;
use rustc_hash::FxHashMap;

pub fn codegen(low_ir: Vec<TopLevel>) -> ObjectProduct {
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {msg}");
    });

    let settings = settings::builder();

    let isa = isa_builder.finish(settings::Flags::new(settings)).unwrap();

    let builder =
        ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names()).unwrap();

    let mut module = ObjectModule::new(builder);

    Codegen::new(&mut module).codegen(low_ir);

    module.finish()
}

struct Codegen<'a> {
    builder_ctx: FunctionBuilderContext,
    ctx: Context,
    module: &'a mut dyn Module,
}

impl<'a> Codegen<'a> {
    fn new(module: &'a mut dyn Module) -> Self {
        Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
        }
    }

    fn codegen(&mut self, low_ir: Vec<TopLevel>) {
        let mut functions = FxHashMap::default();

        for top_level in &low_ir {
            match top_level {
                TopLevel::Function(function) => {
                    let mut sig = self.module.make_signature();

                    for param in &function.params {
                        sig.params.push(param.1.into());
                    }

                    sig.returns.push(function.return_ty.into());

                    let id = self
                        .module
                        .declare_function(
                            function.name,
                            if function.name == "main" {
                                Linkage::Export
                            } else {
                                Linkage::Local
                            },
                            &sig,
                        )
                        .unwrap();

                    functions.insert(function.id, id);
                }
            }
        }

        for top_level in low_ir {
            match top_level {
                TopLevel::Function(function) => {
                    self.codegen_function(function, &functions);
                }
            }
        }
    }

    fn codegen_function(
        &mut self,
        function: Function,
        functions: &FxHashMap<FuncId, cranelift_module::FuncId>,
    ) {
        for param in &function.params {
            self.ctx.func.signature.params.push(param.1.into());
        }

        self.ctx
            .func
            .signature
            .returns
            .push(function.return_ty.into());

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);

        builder.switch_to_block(entry_block);

        builder.seal_block(entry_block);

        let mut translator = FunctionTranslator::new(builder, self.module, functions);

        let function_id = function.id;

        translator.translate(function, entry_block);

        translator.builder.finalize();

        let id = *functions.get(&function_id).unwrap();

        self.module.define_function(id, &mut self.ctx).unwrap();

        self.module.clear_context(&mut self.ctx);
    }
}

struct FunctionTranslator<'a, 'b> {
    builder: FunctionBuilder<'a>,
    module: &'a mut dyn Module,
    functions: &'b FxHashMap<FuncId, cranelift_module::FuncId>,
    vars: FxHashMap<VarId, Variable>,
    var_gen: VarGen,
}

impl<'a, 'b> FunctionTranslator<'a, 'b> {
    fn new(
        builder: FunctionBuilder<'a>,
        module: &'a mut dyn Module,
        functions: &'b FxHashMap<FuncId, cranelift_module::FuncId>,
    ) -> Self {
        Self {
            builder,
            module,
            functions,
            vars: FxHashMap::default(),
            var_gen: VarGen::new(),
        }
    }

    fn translate(&mut self, function: Function, entry_block: Block) {
        debug!("translate function: {}", function.id.0);

        for (idx, param) in function.params.iter().enumerate() {
            let var = self.variable();

            self.builder.declare_var(var, param.1.into());

            self.builder
                .def_var(var, self.builder.block_params(entry_block)[idx]);

            self.vars.insert(param.0, var);
        }

        let mut blocks = FxHashMap::default();

        for block in &function.body {
            blocks.insert(block.id, self.builder.create_block());
        }

        self.builder
            .ins()
            .jump(*blocks.get(&function.body[0].id).unwrap(), &[]);

        for block in function.body {
            self.translate_block(block, &blocks);
        }

        self.builder.seal_all_blocks();
    }

    fn translate_block(&mut self, block: BasicBlock, blocks: &FxHashMap<BasicBlockId, Block>) {
        debug!("translate block: {:?}", block.id);

        self.builder
            .switch_to_block(*blocks.get(&block.id).unwrap());

        for instr in block.instructions {
            self.translate_instruction(instr);
        }

        self.translate_terminator(block.terminator, blocks);
    }

    fn translate_terminator(
        &mut self,
        terminator: Terminator,
        blocks: &FxHashMap<BasicBlockId, Block>,
    ) {
        debug!("translate terminator: {:?}", terminator);

        match terminator {
            Terminator::Goto(block) => {
                self.builder.ins().jump(*blocks.get(&block).unwrap(), &[]);
            }
            Terminator::Return(expr) => {
                let expr = self.translate_expr(expr);

                self.builder.ins().return_(&[expr]);
            }
            Terminator::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.translate_expr(condition);

                let then_block = *blocks.get(&then_block).unwrap();
                let else_block = *blocks.get(&else_block).unwrap();

                self.builder
                    .ins()
                    .brif(condition, then_block, &[], else_block, &[]);
            }
            Terminator::Unreachable => {
                self.builder.ins().trap(TrapCode::UnreachableCodeReached);
            }
        }
    }

    fn translate_instruction(&mut self, instr: Instruction) {
        debug!("translate instr: {:?}", instr);

        match instr {
            Instruction::Expr(expr) => {
                self.translate_expr(expr);
            }
            Instruction::Let { name, value } => {
                let var = self.variable();

                self.builder.declare_var(var, value.ty.into());

                let value = self.translate_expr(value);

                self.builder.def_var(var, value);

                self.vars.insert(name, var);
            }
            Instruction::Assign { name, value } => {
                let var = *self.vars.get(&name).unwrap();

                let value = self.translate_expr(value);

                self.builder.def_var(var, value);
            }
        }
    }

    fn translate_expr(&mut self, expr: TypedExpression) -> Value {
        debug!("translate expr: {:?}", expr);

        match expr.expr {
            Expression::Variable(var) => {
                let var = self.vars.get(&var).unwrap();

                self.builder.use_var(*var)
            }
            Expression::Boolean(bool) => self.builder.ins().iconst(BOOL_TYPE, i64::from(bool)),
            Expression::Integer(int) => self.builder.ins().iconst(INT_TYPE, i64::from(int)),
            Expression::Float(float) => self.builder.ins().f32const(float),
            Expression::String(_string) => unimplemented!(
                "strings will be implemented in stdlib when pointers are implemented"
            ),
            Expression::Unary { op, value } => {
                let value_ty = value.ty;
                let value = self.translate_expr(*value);

                match (op, value_ty) {
                    (UnaryOp::Negate, Type::Integer) => self.builder.ins().ineg(value),
                    (UnaryOp::Negate, Type::Float) => self.builder.ins().fneg(value),
                    (UnaryOp::Not, Type::Boolean) => self.builder.ins().bnot(value),
                    _ => unreachable!(),
                }
            }
            Expression::Binary { lhs, op, rhs } => {
                let lhs_ty = lhs.ty;
                let lhs = self.translate_expr(*lhs);

                let rhs_ty = rhs.ty;
                let rhs = self.translate_expr(*rhs);

                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => match op {
                        BinaryOp::Equals => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        BinaryOp::NotEquals => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        BinaryOp::Plus => self.builder.ins().iadd(lhs, rhs),
                        BinaryOp::Minus => self.builder.ins().isub(lhs, rhs),
                        BinaryOp::Multiply => self.builder.ins().imul(lhs, rhs),
                        BinaryOp::Divide => self.builder.ins().sdiv(lhs, rhs),
                        BinaryOp::GreaterThanEquals => {
                            self.builder
                                .ins()
                                .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                        }
                        BinaryOp::LessThanEquals => {
                            self.builder
                                .ins()
                                .icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                        }
                        BinaryOp::GreaterThan => {
                            self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                        }
                        BinaryOp::LessThan => {
                            self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
                        }
                    },
                    (Type::Float, Type::Float) => match op {
                        BinaryOp::Equals => self.builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                        BinaryOp::NotEquals => self.builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
                        BinaryOp::Plus => self.builder.ins().fadd(lhs, rhs),
                        BinaryOp::Minus => self.builder.ins().fsub(lhs, rhs),
                        BinaryOp::Multiply => self.builder.ins().fmul(lhs, rhs),
                        BinaryOp::Divide => self.builder.ins().fdiv(lhs, rhs),
                        BinaryOp::GreaterThanEquals => {
                            self.builder
                                .ins()
                                .fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
                        }
                        BinaryOp::LessThanEquals => {
                            self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
                        }
                        BinaryOp::GreaterThan => {
                            self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
                        }
                        BinaryOp::LessThan => self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                    },
                    (Type::Boolean, Type::Boolean) => match op {
                        BinaryOp::Equals => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        BinaryOp::NotEquals => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            Expression::Convert { ty, expr } => {
                let expr_ty = expr.ty;
                let expr = self.translate_expr(*expr);

                match (expr_ty, ty) {
                    (from, to) if from == to => expr,
                    _ => unreachable!(),
                }
            }
            Expression::Call { func, args } => {
                let callee = self.functions.get(&func).unwrap();

                let local_callee = self.module.declare_func_in_func(*callee, self.builder.func);

                let args = args
                    .into_iter()
                    .map(|arg| self.translate_expr(arg))
                    .collect::<Vec<_>>();

                let call = self.builder.ins().call(local_callee, &args);

                self.builder.inst_results(call)[0]
            }
        }
    }

    fn variable(&mut self) -> Variable {
        self.var_gen.next().unwrap()
    }
}

const INT_TYPE: IrType = types::I32;
const FLOAT_TYPE: IrType = types::F32;
const BOOL_TYPE: IrType = types::I8;

impl From<Type> for IrType {
    fn from(value: Type) -> Self {
        match value {
            Type::Integer => INT_TYPE,
            Type::Float => FLOAT_TYPE,
            Type::Boolean => BOOL_TYPE,
            Type::String => unimplemented!(
                "strings will be implemented in stdlib when pointers are implemented"
            ),
        }
    }
}

impl From<Type> for AbiParam {
    fn from(value: Type) -> Self {
        Self::new(value.into())
    }
}

#[derive(Debug)]
struct VarGen {
    id_gen: IdGen,
}

impl VarGen {
    const fn new() -> Self {
        Self {
            id_gen: IdGen::new(),
        }
    }

    fn next(&mut self) -> Option<Variable> {
        Some(Variable::new(self.id_gen.next()?))
    }
}
