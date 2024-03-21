use crate::{
    ast::{BinaryOp, UnaryOp},
    mir::{FuncId, Type, VarId},
    mir_no_span, FloatTy, IntTy,
};

#[derive(Debug)]
pub enum TopLevel {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: FuncId,
    pub params: Vec<(VarId, Type)>,
    pub return_ty: Type,
    pub body: Vec<BasicBlock>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Goto(Goto),
    If {
        condition: TypedExpression,
        then_block: Goto,
        else_block: Goto,
    },
}

#[derive(Clone, Debug)]
pub enum Goto {
    Block(BasicBlockId),
    Return(TypedExpression),
}

#[derive(Clone, Copy, Debug, Default)]
pub struct BasicBlockId(usize);

#[derive(Clone, Debug)]
pub enum Instruction {
    Expr(TypedExpression),
    Let { name: VarId, value: TypedExpression },
    Assign { name: VarId, value: TypedExpression },
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(String),
    Unary {
        op: UnaryOp,
        value: Box<TypedExpression>,
    },
    Binary {
        lhs: Box<TypedExpression>,
        op: BinaryOp,
        rhs: Box<TypedExpression>,
    },
    Convert {
        ty: Type,
        expr: Box<TypedExpression>,
    },
    Call {
        func: FuncId,
        args: Vec<TypedExpression>,
    },
}

pub fn lower(ast: Vec<mir_no_span::TopLevel>) -> Vec<TopLevel> {
    LoweringContext::default().lower(ast)
}

#[derive(Debug, Default)]
struct LoweringContext {
    blocks: Vec<UnfinishedBasicBlock>,
    current_block: Option<BasicBlockId>,
    loop_stack: Vec<LoopInfo>,
}

impl LoweringContext {
    fn finish_block(&mut self, terminator: Terminator) {
        self.current_block_mut().finish(terminator);
    }

    fn current_block_mut(&mut self) -> &mut UnfinishedBasicBlock {
        self.current_block
            .map(|id| self.blocks.get_mut(id.0).unwrap())
            .unwrap()
    }

    fn new_block(&mut self) -> BasicBlockId {
        let id = self.blocks.len();
        self.blocks.push(UnfinishedBasicBlock::new(id));
        BasicBlockId(id)
    }

    fn switch_to_block(&mut self, main_block: BasicBlockId) {
        self.current_block = Some(main_block);
    }
}

impl LoweringContext {
    fn lower(&mut self, ast: Vec<mir_no_span::TopLevel>) -> Vec<TopLevel> {
        ast.into_iter().map(|tl| self.lower_top_level(tl)).collect()
    }

    fn lower_top_level(&mut self, top_level: mir_no_span::TopLevel) -> TopLevel {
        match top_level {
            mir_no_span::TopLevel::Function(f) => TopLevel::Function(self.lower_function(f)),
        }
    }

    fn lower_function(&mut self, function: mir_no_span::Function) -> Function {
        let main_block = self.new_block();
        self.switch_to_block(main_block);

        for stmt in function.body {
            if self.lower_statement(stmt) {
                break;
            }
        }

        let body = self
            .blocks
            .clone()
            .into_iter()
            .map(|block| block.try_into().unwrap())
            .collect();

        Function {
            name: function.name,
            params: function.args,
            return_ty: function.return_ty,
            body,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn lower_statement(&mut self, stmt: mir_no_span::Statement) -> bool {
        match stmt {
            mir_no_span::Statement::Expr(expr) => {
                let expr = Self::lower_expression(expr);

                self.current_block_mut()
                    .instructions
                    .push(Instruction::Expr(expr));

                false
            }
            mir_no_span::Statement::Block(statements) => {
                for stmt in statements {
                    if self.lower_statement(stmt) {
                        return true;
                    }
                }

                false
            }
            mir_no_span::Statement::Loop(statements) => {
                let loop_block = self.new_block();
                let exit_block = self.new_block();

                self.finish_block(Terminator::Goto(Goto::Block(loop_block)));

                self.switch_to_block(loop_block);

                let loop_stack_len_before = self.loop_stack.len();

                self.loop_stack.push(LoopInfo {
                    start_block: loop_block,
                    exit_block,
                });

                let mut finished_block = false;

                for stmt in statements {
                    if self.lower_statement(stmt) {
                        finished_block = true;
                        break;
                    }
                }

                self.loop_stack.pop();

                assert_eq!(self.loop_stack.len(), loop_stack_len_before);

                if !finished_block {
                    self.finish_block(Terminator::Goto(Goto::Block(loop_block)));
                }

                self.switch_to_block(exit_block);

                false
            }
            mir_no_span::Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let then_block = self.new_block();
                let else_block = self.new_block();
                let exit_block = self.new_block();

                let condition = Self::lower_expression(condition);

                self.finish_block(Terminator::If {
                    condition,
                    then_block: Goto::Block(then_block),
                    else_block: Goto::Block(else_block),
                });

                self.switch_to_block(then_block);

                let mut finished_block = false;

                for stmt in then_branch {
                    if self.lower_statement(stmt) {
                        finished_block = true;
                        break;
                    }
                }

                if !finished_block {
                    self.finish_block(Terminator::Goto(Goto::Block(exit_block)));
                }

                self.switch_to_block(else_block);

                let mut finished_block = false;

                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        if self.lower_statement(stmt) {
                            finished_block = true;
                            break;
                        }
                    }
                }

                if !finished_block {
                    self.finish_block(Terminator::Goto(Goto::Block(exit_block)));
                }

                self.switch_to_block(exit_block);

                false
            }
            mir_no_span::Statement::Let { name, value } => {
                let value = Self::lower_expression(value);

                self.current_block_mut()
                    .instructions
                    .push(Instruction::Let { name, value });

                false
            }
            mir_no_span::Statement::Assign { name, value } => {
                let value = Self::lower_expression(value);

                self.current_block_mut()
                    .instructions
                    .push(Instruction::Assign { name, value });

                false
            }
            mir_no_span::Statement::Break => {
                let loop_info = self.loop_stack.last().unwrap();

                self.finish_block(Terminator::Goto(Goto::Block(loop_info.exit_block)));

                true
            }
            mir_no_span::Statement::Continue => {
                let loop_info = self.loop_stack.last().unwrap();

                self.finish_block(Terminator::Goto(Goto::Block(loop_info.start_block)));

                true
            }
            mir_no_span::Statement::Return(expr) => {
                let expr = Self::lower_expression(expr);

                let terminator = Terminator::Goto(Goto::Return(expr));

                self.finish_block(terminator);

                true
            }
        }
    }

    fn lower_expression(expr: mir_no_span::TypedExpression) -> TypedExpression {
        TypedExpression {
            expr: match expr.expr {
                mir_no_span::Expression::Variable(var) => Expression::Variable(var),
                mir_no_span::Expression::Boolean(bool) => Expression::Boolean(bool),
                mir_no_span::Expression::Integer(int) => Expression::Integer(int),
                mir_no_span::Expression::Float(float) => Expression::Float(float),
                mir_no_span::Expression::String(string) => Expression::String(string),
                mir_no_span::Expression::Unary { op, rhs } => Expression::Unary {
                    op,
                    value: Box::new(Self::lower_expression(*rhs)),
                },
                mir_no_span::Expression::Binary { lhs, op, rhs } => Expression::Binary {
                    lhs: Box::new(Self::lower_expression(*lhs)),
                    op,
                    rhs: Box::new(Self::lower_expression(*rhs)),
                },
                mir_no_span::Expression::Convert { ty, expr } => Expression::Convert {
                    ty,
                    expr: Box::new(Self::lower_expression(*expr)),
                },
                mir_no_span::Expression::Call { func, args } => Expression::Call {
                    func,
                    args: args.into_iter().map(Self::lower_expression).collect(),
                },
            },
            ty: expr.ty,
        }
    }
}

#[derive(Debug)]
struct LoopInfo {
    start_block: BasicBlockId,
    exit_block: BasicBlockId,
}

#[derive(Clone, Debug)]
struct UnfinishedBasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
}

impl UnfinishedBasicBlock {
    const fn new(id: usize) -> Self {
        Self {
            id: BasicBlockId(id),
            instructions: Vec::new(),
            terminator: None,
        }
    }

    fn finish(&mut self, terminator: Terminator) {
        self.terminator = Some(terminator);
    }
}

impl TryInto<BasicBlock> for UnfinishedBasicBlock {
    type Error = ();

    fn try_into(self) -> Result<BasicBlock, Self::Error> {
        if let Some(terminator) = self.terminator {
            Ok(BasicBlock {
                id: self.id,
                instructions: self.instructions,
                terminator,
            })
        } else {
            Err(())
        }
    }
}
