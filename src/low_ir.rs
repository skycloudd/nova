use crate::mir_no_span as mir;

#[derive(Debug)]
pub struct BasicBlock {
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

#[derive(Debug)]
pub enum Terminator {
    Goto(BasicBlockId),
    If {
        condition: TypedExpression,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
    },
    Finish,
}

pub type BasicBlockId = usize;

#[derive(Debug)]
pub enum Instruction {
    Expr(TypedExpression),
    BuiltinPrint(TypedExpression),
    Let { name: VarId, value: TypedExpression },
    Assign { name: VarId, value: TypedExpression },
}

#[derive(Debug)]
pub struct TypedExpression {
    expr: Expression,
    ty: Type,
}

#[derive(Debug)]
pub enum Expression {
    Variable(VarId),
    Boolean(bool),
    Integer(i32),
    Float(f32),
    Colour {
        r: u8,
        g: u8,
        b: u8,
    },
    Vector {
        x: Box<TypedExpression>,
        y: Box<TypedExpression>,
    },
    Operation(Box<Operation>),
}

pub type VarId = usize;

#[derive(Debug)]
pub enum Operation {
    IntegerEquals(TypedExpression, TypedExpression),
    IntegerNotEquals(TypedExpression, TypedExpression),
    IntegerPlus(TypedExpression, TypedExpression),
    IntegerMinus(TypedExpression, TypedExpression),
    IntegerMultiply(TypedExpression, TypedExpression),
    IntegerDivide(TypedExpression, TypedExpression),
    IntegerGreaterThanEquals(TypedExpression, TypedExpression),
    IntegerLessThanEquals(TypedExpression, TypedExpression),
    IntegerGreaterThan(TypedExpression, TypedExpression),
    IntegerLessThan(TypedExpression, TypedExpression),

    FloatEquals(TypedExpression, TypedExpression),
    FloatNotEquals(TypedExpression, TypedExpression),
    FloatPlus(TypedExpression, TypedExpression),
    FloatMinus(TypedExpression, TypedExpression),
    FloatMultiply(TypedExpression, TypedExpression),
    FloatDivide(TypedExpression, TypedExpression),
    FloatGreaterThanEquals(TypedExpression, TypedExpression),
    FloatLessThanEquals(TypedExpression, TypedExpression),
    FloatGreaterThan(TypedExpression, TypedExpression),
    FloatLessThan(TypedExpression, TypedExpression),

    BooleanEquals(TypedExpression, TypedExpression),
    BooleanNotEquals(TypedExpression, TypedExpression),

    IntegerNegate(TypedExpression),
    FloatNegate(TypedExpression),
    BooleanNot(TypedExpression),
}

#[derive(Debug)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
}

impl From<mir::Type> for Type {
    fn from(ty: mir::Type) -> Self {
        match ty {
            mir::Type::Boolean => Type::Boolean,
            mir::Type::Integer => Type::Integer,
            mir::Type::Float => Type::Float,
            mir::Type::Colour => Type::Colour,
            mir::Type::Vector => Type::Vector,
        }
    }
}

pub fn lower(ast: Vec<mir::TypedStatement>) -> Vec<BasicBlock> {
    LoweringContext {
        blocks: Vec::new(),
        current_block: 0,
        loop_stack: Vec::new(),
    }
    .lower(ast)
}

struct LoweringContext {
    blocks: Vec<BasicBlock>,
    current_block: BasicBlockId,
    loop_stack: Vec<(BasicBlockId, BasicBlockId)>,
}

impl LoweringContext {
    fn new_block(&mut self) -> BasicBlockId {
        let id = self.blocks.len();

        self.blocks.push(BasicBlock {
            instructions: vec![],
            terminator: None,
        });

        id
    }

    fn switch_to(&mut self, block: BasicBlockId) {
        self.current_block = block;
    }

    fn current(&self) -> &BasicBlock {
        self.blocks.get(self.current_block).unwrap()
    }

    fn current_mut(&mut self) -> &mut BasicBlock {
        self.blocks.get_mut(self.current_block).unwrap()
    }

    fn finish(&mut self, terminator: Terminator) {
        if self.current().terminator.is_some() {
            panic!("block {} already finished", self.current_block);
        }

        self.current_mut().terminator = Some(terminator);
    }

    fn finish_checked(&mut self, terminator: Terminator) {
        if self.current().terminator.is_none() {
            self.current_mut().terminator = Some(terminator);
        }
    }

    fn loop_stack_push(&mut self, loop_: BasicBlockId, merge: BasicBlockId) {
        self.loop_stack.push((loop_, merge));
    }

    fn loop_stack_pop(&mut self) -> (BasicBlockId, BasicBlockId) {
        self.loop_stack.pop().unwrap()
    }

    fn loop_stack_top(&self) -> (BasicBlockId, BasicBlockId) {
        *self.loop_stack.last().unwrap()
    }

    fn lower(mut self, ast: Vec<mir::TypedStatement>) -> Vec<BasicBlock> {
        self.blocks = vec![];

        let start = self.new_block();
        self.switch_to(start);

        for statement in ast {
            self.lower_statement(statement);
        }

        self.finish(Terminator::Finish);

        self.blocks
    }

    fn lower_statement(&mut self, statement: mir::TypedStatement) -> bool {
        match statement {
            mir::TypedStatement::Expr(expr) => {
                let expr = self.lower_expression(expr);

                self.current_mut()
                    .instructions
                    .push(Instruction::Expr(expr));

                false
            }
            mir::TypedStatement::BuiltinPrint(expr) => {
                let expr = self.lower_expression(expr);

                self.current_mut()
                    .instructions
                    .push(Instruction::BuiltinPrint(expr));

                false
            }
            mir::TypedStatement::Loop(statements) => {
                let loop_start = self.new_block();

                let merge_block = self.new_block();

                self.finish(Terminator::Goto(loop_start));

                self.switch_to(loop_start);

                self.loop_stack_push(loop_start, merge_block);

                for statement in statements {
                    if self.lower_statement(statement) {
                        break;
                    }
                }

                self.loop_stack_pop();

                self.finish_checked(Terminator::Goto(loop_start));

                self.switch_to(merge_block);

                false
            }
            mir::TypedStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.lower_expression(condition);

                let then_block = self.new_block();

                let else_block = self.new_block();

                let merge_block = self.new_block();

                self.finish(Terminator::If {
                    condition,
                    then_block,
                    else_block,
                });

                self.switch_to(then_block);

                for statement in then_branch {
                    if self.lower_statement(statement) {
                        break;
                    }
                }

                self.finish_checked(Terminator::Goto(merge_block));

                self.switch_to(else_block);

                if let Some(else_branch) = else_branch {
                    for statement in else_branch {
                        if self.lower_statement(statement) {
                            break;
                        }
                    }
                }

                self.finish_checked(Terminator::Goto(merge_block));

                self.switch_to(merge_block);

                false
            }
            mir::TypedStatement::Let { name, value } => {
                let value = self.lower_expression(value);

                self.current_mut()
                    .instructions
                    .push(Instruction::Let { name, value });

                false
            }
            mir::TypedStatement::Const { name, value } => {
                let value = self.lower_expression(value);

                self.current_mut()
                    .instructions
                    .push(Instruction::Let { name, value });

                false
            }
            mir::TypedStatement::Assign { name, value } => {
                let value = self.lower_expression(value);

                self.current_mut()
                    .instructions
                    .push(Instruction::Assign { name, value });

                false
            }
            mir::TypedStatement::Break => {
                let merge_block = self.loop_stack_top().1;

                self.finish(Terminator::Goto(merge_block));

                true
            }
            mir::TypedStatement::Continue => {
                let loop_block = self.loop_stack_top().0;

                self.finish(Terminator::Goto(loop_block));

                true
            }
        }
    }

    fn lower_expression(&mut self, expression: mir::TypedExpression) -> TypedExpression {
        TypedExpression {
            expr: match expression.expr {
                mir::Expression::Variable(name) => Expression::Variable(name),
                mir::Expression::Boolean(value) => Expression::Boolean(value),
                mir::Expression::Integer(value) => Expression::Integer(value),
                mir::Expression::Float(value) => Expression::Float(value),
                mir::Expression::Colour { r, g, b } => Expression::Colour { r, g, b },
                mir::Expression::Vector { x, y } => Expression::Vector {
                    x: Box::new(self.lower_expression(*x)),
                    y: Box::new(self.lower_expression(*y)),
                },
                mir::Expression::Operation(operation) => {
                    Expression::Operation(Box::new(match *operation {
                        mir::Operation::IntegerEquals(lhs, rhs) => Operation::IntegerEquals(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::IntegerNotEquals(lhs, rhs) => Operation::IntegerNotEquals(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::IntegerPlus(lhs, rhs) => Operation::IntegerPlus(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::IntegerMinus(lhs, rhs) => Operation::IntegerMinus(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::IntegerMultiply(lhs, rhs) => Operation::IntegerMultiply(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::IntegerDivide(lhs, rhs) => Operation::IntegerDivide(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                            Operation::IntegerGreaterThanEquals(
                                self.lower_expression(lhs),
                                self.lower_expression(rhs),
                            )
                        }
                        mir::Operation::IntegerLessThanEquals(lhs, rhs) => {
                            Operation::IntegerLessThanEquals(
                                self.lower_expression(lhs),
                                self.lower_expression(rhs),
                            )
                        }
                        mir::Operation::IntegerGreaterThan(lhs, rhs) => {
                            Operation::IntegerGreaterThan(
                                self.lower_expression(lhs),
                                self.lower_expression(rhs),
                            )
                        }
                        mir::Operation::IntegerLessThan(lhs, rhs) => Operation::IntegerLessThan(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatEquals(lhs, rhs) => Operation::FloatEquals(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatNotEquals(lhs, rhs) => Operation::FloatNotEquals(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatPlus(lhs, rhs) => Operation::FloatPlus(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatMinus(lhs, rhs) => Operation::FloatMinus(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatMultiply(lhs, rhs) => Operation::FloatMultiply(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatDivide(lhs, rhs) => Operation::FloatDivide(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatGreaterThanEquals(lhs, rhs) => {
                            Operation::FloatGreaterThanEquals(
                                self.lower_expression(lhs),
                                self.lower_expression(rhs),
                            )
                        }
                        mir::Operation::FloatLessThanEquals(lhs, rhs) => {
                            Operation::FloatLessThanEquals(
                                self.lower_expression(lhs),
                                self.lower_expression(rhs),
                            )
                        }
                        mir::Operation::FloatGreaterThan(lhs, rhs) => Operation::FloatGreaterThan(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::FloatLessThan(lhs, rhs) => Operation::FloatLessThan(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::BooleanEquals(lhs, rhs) => Operation::BooleanEquals(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::BooleanNotEquals(lhs, rhs) => Operation::BooleanNotEquals(
                            self.lower_expression(lhs),
                            self.lower_expression(rhs),
                        ),
                        mir::Operation::IntegerNegate(value) => {
                            Operation::IntegerNegate(self.lower_expression(value))
                        }
                        mir::Operation::FloatNegate(value) => {
                            Operation::FloatNegate(self.lower_expression(value))
                        }
                        mir::Operation::BooleanNot(value) => {
                            Operation::BooleanNot(self.lower_expression(value))
                        }
                    }))
                }
            },
            ty: expression.ty.into(),
        }
    }
}
