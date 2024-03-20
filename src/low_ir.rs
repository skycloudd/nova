use crate::{
    ast::{BinaryOp, UnaryOp},
    mir::{self, FuncId, Type, VarId},
    FloatTy, IntTy,
};

#[derive(Debug)]
pub enum TopLevel<'ast> {
    Function(Function<'ast>),
}

#[derive(Debug)]
pub struct Function<'ast> {
    pub name: FuncId,
    pub params: Vec<(VarId, Type)>,
    pub return_ty: Type,
    pub body: Vec<BasicBlock<'ast>>,
}

#[derive(Debug)]
pub struct BasicBlock<'ast> {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction<'ast>>,
    pub terminator: Terminator<'ast>,
}

#[derive(Clone, Debug)]
pub enum Terminator<'ast> {
    Goto(Goto<'ast>),
    If {
        condition: TypedExpression<'ast>,
        then_block: Goto<'ast>,
        else_block: Goto<'ast>,
    },
}

#[derive(Clone, Debug)]
pub enum Goto<'ast> {
    Block(BasicBlockId),
    Return(TypedExpression<'ast>),
}

#[derive(Clone, Copy, Debug, Default)]
pub struct BasicBlockId(usize);

#[derive(Clone, Debug)]
pub enum Instruction<'ast> {
    Expr(TypedExpression<'ast>),
    Let {
        name: VarId,
        value: TypedExpression<'ast>,
    },
    Assign {
        name: VarId,
        value: TypedExpression<'ast>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedExpression<'ast> {
    pub expr: Expression<'ast>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Expression<'ast> {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(&'ast str),
    Unary {
        op: UnaryOp,
        value: Box<TypedExpression<'ast>>,
    },
    Binary {
        lhs: Box<TypedExpression<'ast>>,
        op: BinaryOp,
        rhs: Box<TypedExpression<'ast>>,
    },
    Convert {
        ty: Type,
        expr: Box<TypedExpression<'ast>>,
    },
    Call {
        func: FuncId,
        args: Vec<TypedExpression<'ast>>,
    },
}

pub fn lower(ast: Vec<mir::TopLevel>) -> Vec<TopLevel> {
    LoweringContext::default().lower(ast)
}

#[derive(Debug, Default)]
struct LoweringContext<'ast> {
    blocks: Vec<UnfinishedBasicBlock<'ast>>,
    current_block: Option<BasicBlockId>,
    loop_stack: Vec<LoopInfo>,
}

impl<'ast> LoweringContext<'ast> {
    fn finish_block(&mut self, terminator: Terminator<'ast>) {
        self.current_block_mut().finish(terminator);
    }

    fn current_block_mut(&mut self) -> &mut UnfinishedBasicBlock<'ast> {
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

impl<'ast> LoweringContext<'ast> {
    fn lower(&mut self, ast: Vec<mir::TopLevel<'ast>>) -> Vec<TopLevel<'ast>> {
        ast.into_iter().map(|tl| self.lower_top_level(tl)).collect()
    }

    fn lower_top_level(&mut self, top_level: mir::TopLevel<'ast>) -> TopLevel<'ast> {
        match top_level {
            mir::TopLevel::Function(f) => TopLevel::Function(self.lower_function(f)),
        }
    }

    fn lower_function(&mut self, function: mir::Function<'ast>) -> Function<'ast> {
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

    fn lower_statement(&mut self, stmt: mir::Statement<'ast>) -> bool {
        match stmt {
            mir::Statement::Expr(expr) => {
                let expr = Self::lower_expression(expr);

                self.current_block_mut()
                    .instructions
                    .push(Instruction::Expr(expr));

                false
            }
            mir::Statement::Block(statements) => {
                for stmt in statements {
                    if self.lower_statement(stmt) {
                        return true;
                    }
                }

                false
            }
            mir::Statement::Loop(statements) => {
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
            mir::Statement::If {
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
            mir::Statement::Let { name, value } => {
                let value = Self::lower_expression(value);

                self.current_block_mut()
                    .instructions
                    .push(Instruction::Let { name, value });

                false
            }
            mir::Statement::Assign { name, value } => {
                let value = Self::lower_expression(value);

                self.current_block_mut()
                    .instructions
                    .push(Instruction::Assign { name, value });

                false
            }
            mir::Statement::Break => {
                let loop_info = self.loop_stack.last().unwrap();

                self.finish_block(Terminator::Goto(Goto::Block(loop_info.exit_block)));

                true
            }
            mir::Statement::Continue => {
                let loop_info = self.loop_stack.last().unwrap();

                self.finish_block(Terminator::Goto(Goto::Block(loop_info.start_block)));

                true
            }
            mir::Statement::Return(expr) => {
                let expr = Self::lower_expression(expr);

                let terminator = Terminator::Goto(Goto::Return(expr));

                self.finish_block(terminator);

                true
            }
        }
    }

    fn lower_expression(expr: mir::TypedExpression<'ast>) -> TypedExpression<'ast> {
        TypedExpression {
            expr: match expr.expr {
                mir::Expression::Variable(var) => Expression::Variable(var),
                mir::Expression::Boolean(bool) => Expression::Boolean(bool),
                mir::Expression::Integer(int) => Expression::Integer(int),
                mir::Expression::Float(float) => Expression::Float(float),
                mir::Expression::String(string) => Expression::String(string),
                mir::Expression::Unary { op, rhs } => Expression::Unary {
                    op,
                    value: Box::new(Self::lower_expression(*rhs)),
                },
                mir::Expression::Binary { lhs, op, rhs } => Expression::Binary {
                    lhs: Box::new(Self::lower_expression(*lhs)),
                    op,
                    rhs: Box::new(Self::lower_expression(*rhs)),
                },
                mir::Expression::Convert { ty, expr } => Expression::Convert {
                    ty,
                    expr: Box::new(Self::lower_expression(*expr)),
                },
                mir::Expression::Call { func, args } => Expression::Call {
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
struct UnfinishedBasicBlock<'ast> {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction<'ast>>,
    pub terminator: Option<Terminator<'ast>>,
}

impl<'ast> UnfinishedBasicBlock<'ast> {
    const fn new(id: usize) -> Self {
        Self {
            id: BasicBlockId(id),
            instructions: Vec::new(),
            terminator: None,
        }
    }

    fn finish(&mut self, terminator: Terminator<'ast>) {
        self.terminator = Some(terminator);
    }
}

impl<'ast> TryInto<BasicBlock<'ast>> for UnfinishedBasicBlock<'ast> {
    type Error = ();

    fn try_into(self) -> Result<BasicBlock<'ast>, Self::Error> {
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
