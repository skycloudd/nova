use crate::{
    mir_no_span::{self as mir, Action},
    FloatTy, IntTy,
};

#[derive(Debug)]
pub struct UnfinishedBasicBlock {
    id: BasicBlockId,
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock {
    id: BasicBlockId,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

impl BasicBlock {
    pub const fn id(&self) -> BasicBlockId {
        self.id
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub const fn terminator(&self) -> &Terminator {
        &self.terminator
    }

    pub fn instructions_mut(&mut self) -> &mut Vec<Instruction> {
        &mut self.instructions
    }

    pub fn terminator_mut(&mut self) -> &mut Terminator {
        &mut self.terminator
    }
}

impl TryFrom<UnfinishedBasicBlock> for BasicBlock {
    type Error = ();

    fn try_from(block: UnfinishedBasicBlock) -> Result<Self, Self::Error> {
        Ok(Self {
            id: block.id,
            instructions: block.instructions,
            terminator: block.terminator.ok_or(())?,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Terminator {
    Goto(Goto),
    If {
        condition: TypedExpression,
        then_block: Goto,
        else_block: Goto,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Goto {
    Block(BasicBlockId),
    Finish,
}

pub type BasicBlockId = usize;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Expr(TypedExpression),
    Let {
        name: VarId,
        value: TypedExpression,
    },
    Assign {
        name: VarId,
        value: TypedExpression,
    },
    Action {
        name: Action,
        args: Vec<TypedExpression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    Colour {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    },
    Vector {
        x: Box<TypedExpression>,
        y: Box<TypedExpression>,
    },
    Object(Object),
    Operation(Box<Operation>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Player,
}

pub type VarId = usize;

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
    Object,
    ObjectSet,
}

impl From<mir::Type> for Type {
    fn from(ty: mir::Type) -> Self {
        match ty {
            mir::Type::Boolean => Self::Boolean,
            mir::Type::Integer => Self::Integer,
            mir::Type::Float => Self::Float,
            mir::Type::Colour => Self::Colour,
            mir::Type::Vector => Self::Vector,
            mir::Type::Object => Self::Object,
            mir::Type::ObjectSet => Self::ObjectSet,
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
    .into_iter()
    .map(TryInto::try_into)
    .collect::<Result<Vec<_>, _>>()
    .unwrap()
}

struct LoweringContext {
    blocks: Vec<UnfinishedBasicBlock>,
    current_block: BasicBlockId,
    loop_stack: Vec<(BasicBlockId, BasicBlockId)>,
}

impl LoweringContext {
    fn new_block(&mut self) -> BasicBlockId {
        let id = self.blocks.len();

        self.blocks.push(UnfinishedBasicBlock {
            id,
            instructions: vec![],
            terminator: None,
        });

        id
    }

    fn switch_to(&mut self, block: BasicBlockId) {
        self.current_block = block;
    }

    fn current(&self) -> &UnfinishedBasicBlock {
        self.blocks.get(self.current_block).unwrap()
    }

    fn current_mut(&mut self) -> &mut UnfinishedBasicBlock {
        self.blocks.get_mut(self.current_block).unwrap()
    }

    fn finish(&mut self, terminator: Terminator) {
        assert!(
            self.current().terminator.is_none(),
            "block {} already finished",
            self.current_block
        );

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

    fn lower(mut self, ast: Vec<mir::TypedStatement>) -> Vec<UnfinishedBasicBlock> {
        self.blocks = vec![];

        let start = self.new_block();
        self.switch_to(start);

        for statement in ast {
            self.lower_statement(statement);
        }

        self.finish(Terminator::Goto(Goto::Finish));

        self.blocks
    }

    fn lower_statement(&mut self, statement: mir::TypedStatement) -> bool {
        match statement {
            mir::TypedStatement::Expr(expr) => {
                let expr = Self::lower_expression(expr);

                self.current_mut()
                    .instructions
                    .push(Instruction::Expr(expr));
            }
            mir::TypedStatement::Block(statements) => {
                for statement in statements {
                    if self.lower_statement(statement) {
                        break;
                    }
                }
            }
            mir::TypedStatement::Loop(statements) => {
                let loop_start = self.new_block();

                let merge_block = self.new_block();

                self.finish(Terminator::Goto(Goto::Block(loop_start)));

                self.switch_to(loop_start);

                self.loop_stack_push(loop_start, merge_block);

                for statement in statements {
                    if self.lower_statement(statement) {
                        break;
                    }
                }

                self.loop_stack_pop();

                self.finish_checked(Terminator::Goto(Goto::Block(loop_start)));

                self.switch_to(merge_block);
            }
            mir::TypedStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = Self::lower_expression(condition);

                let then_block = self.new_block();

                let else_block = else_branch.as_ref().map(|_| self.new_block());

                let merge_block = self.new_block();

                self.finish(Terminator::If {
                    condition,
                    then_block: Goto::Block(then_block),
                    else_block: Goto::Block(else_block.unwrap_or(merge_block)),
                });

                self.switch_to(then_block);

                for statement in then_branch {
                    if self.lower_statement(statement) {
                        break;
                    }
                }

                self.finish_checked(Terminator::Goto(Goto::Block(merge_block)));

                if let Some(else_block) = else_block {
                    self.switch_to(else_block);

                    if let Some(else_branch) = else_branch {
                        for statement in else_branch {
                            if self.lower_statement(statement) {
                                break;
                            }
                        }
                    }

                    self.finish_checked(Terminator::Goto(Goto::Block(merge_block)));
                }

                self.switch_to(merge_block);
            }
            mir::TypedStatement::Let { name, value }
            | mir::TypedStatement::Const { name, value } => {
                let value = Self::lower_expression(value);

                self.current_mut()
                    .instructions
                    .push(Instruction::Let { name, value });
            }
            mir::TypedStatement::Assign { name, value } => {
                let value = Self::lower_expression(value);

                self.current_mut()
                    .instructions
                    .push(Instruction::Assign { name, value });
            }
            mir::TypedStatement::Break => {
                let merge_block = self.loop_stack_top().1;

                self.finish(Terminator::Goto(Goto::Block(merge_block)));

                return true;
            }
            mir::TypedStatement::Continue => {
                let loop_block = self.loop_stack_top().0;

                self.finish(Terminator::Goto(Goto::Block(loop_block)));

                return true;
            }
            mir::TypedStatement::Action { name, args } => {
                let args = args
                    .into_iter()
                    .map(Self::lower_expression)
                    .collect::<Vec<_>>();

                self.current_mut()
                    .instructions
                    .push(Instruction::Action { name, args });
            }
        }

        false
    }

    fn lower_expression(expression: mir::TypedExpression) -> TypedExpression {
        TypedExpression {
            expr: match expression.expr {
                mir::Expression::Variable(name) => Expression::Variable(name),
                mir::Expression::Boolean(value) => Expression::Boolean(value),
                mir::Expression::Integer(value) => Expression::Integer(value),
                mir::Expression::Float(value) => Expression::Float(value),
                mir::Expression::Colour { r, g, b, a } => Expression::Colour { r, g, b, a },
                mir::Expression::Vector { x, y } => Expression::Vector {
                    x: Box::new(Self::lower_expression(*x)),
                    y: Box::new(Self::lower_expression(*y)),
                },
                mir::Expression::Object(object) => Expression::Object(match object {
                    mir::Object::Player => Object::Player,
                }),
                mir::Expression::Operation(operation) => {
                    Expression::Operation(Box::new(match *operation {
                        mir::Operation::IntegerEquals(lhs, rhs) => Operation::IntegerEquals(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::IntegerNotEquals(lhs, rhs) => Operation::IntegerNotEquals(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::IntegerPlus(lhs, rhs) => Operation::IntegerPlus(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::IntegerMinus(lhs, rhs) => Operation::IntegerMinus(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::IntegerMultiply(lhs, rhs) => Operation::IntegerMultiply(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::IntegerDivide(lhs, rhs) => Operation::IntegerDivide(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                            Operation::IntegerGreaterThanEquals(
                                Self::lower_expression(lhs),
                                Self::lower_expression(rhs),
                            )
                        }
                        mir::Operation::IntegerLessThanEquals(lhs, rhs) => {
                            Operation::IntegerLessThanEquals(
                                Self::lower_expression(lhs),
                                Self::lower_expression(rhs),
                            )
                        }
                        mir::Operation::IntegerGreaterThan(lhs, rhs) => {
                            Operation::IntegerGreaterThan(
                                Self::lower_expression(lhs),
                                Self::lower_expression(rhs),
                            )
                        }
                        mir::Operation::IntegerLessThan(lhs, rhs) => Operation::IntegerLessThan(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatEquals(lhs, rhs) => Operation::FloatEquals(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatNotEquals(lhs, rhs) => Operation::FloatNotEquals(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatPlus(lhs, rhs) => Operation::FloatPlus(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatMinus(lhs, rhs) => Operation::FloatMinus(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatMultiply(lhs, rhs) => Operation::FloatMultiply(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatDivide(lhs, rhs) => Operation::FloatDivide(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatGreaterThanEquals(lhs, rhs) => {
                            Operation::FloatGreaterThanEquals(
                                Self::lower_expression(lhs),
                                Self::lower_expression(rhs),
                            )
                        }
                        mir::Operation::FloatLessThanEquals(lhs, rhs) => {
                            Operation::FloatLessThanEquals(
                                Self::lower_expression(lhs),
                                Self::lower_expression(rhs),
                            )
                        }
                        mir::Operation::FloatGreaterThan(lhs, rhs) => Operation::FloatGreaterThan(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::FloatLessThan(lhs, rhs) => Operation::FloatLessThan(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::BooleanEquals(lhs, rhs) => Operation::BooleanEquals(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::BooleanNotEquals(lhs, rhs) => Operation::BooleanNotEquals(
                            Self::lower_expression(lhs),
                            Self::lower_expression(rhs),
                        ),
                        mir::Operation::IntegerNegate(value) => {
                            Operation::IntegerNegate(Self::lower_expression(value))
                        }
                        mir::Operation::FloatNegate(value) => {
                            Operation::FloatNegate(Self::lower_expression(value))
                        }
                        mir::Operation::BooleanNot(value) => {
                            Operation::BooleanNot(Self::lower_expression(value))
                        }
                    }))
                }
            },
            ty: expression.ty.into(),
        }
    }
}

mod print {
    use super::{BasicBlock, Expression, Goto, Instruction, Object, Operation, Terminator};

    impl std::fmt::Display for BasicBlock {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            print_basic_block(f, self)
        }
    }

    impl std::fmt::Display for Object {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Player => write!(f, "player"),
            }
        }
    }

    impl std::fmt::Display for Goto {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Block(block) => write!(f, "bb{}", block),
                Self::Finish => write!(f, "finish"),
            }
        }
    }

    fn print_basic_block(f: &mut std::fmt::Formatter<'_>, block: &BasicBlock) -> std::fmt::Result {
        writeln!(f, "bb{}:", block.id)?;

        for instruction in &block.instructions {
            write!(f, "    ")?;

            print_instruction(f, instruction)?;

            writeln!(f)?;
        }

        write!(f, "    ")?;
        print_terminator(f, &block.terminator)?;

        Ok(())
    }

    fn print_instruction(
        f: &mut std::fmt::Formatter<'_>,
        instruction: &Instruction,
    ) -> std::fmt::Result {
        match instruction {
            Instruction::Expr(expr) => {
                print_expression(f, &expr.expr)?;

                write!(f, ";")
            }
            Instruction::Let { name, value } => {
                write!(f, "let var_{name} = ")?;

                print_expression(f, &value.expr)?;

                write!(f, ";")
            }
            Instruction::Assign { name, value } => {
                write!(f, "var_{name} = ")?;

                print_expression(f, &value.expr)?;

                write!(f, ";")
            }
            Instruction::Action { name, args } => {
                write!(f, "action {name}: ")?;

                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    print_expression(f, &arg.expr)?;
                }

                write!(f, ";")
            }
        }
    }

    fn print_terminator(
        f: &mut std::fmt::Formatter<'_>,
        terminator: &Terminator,
    ) -> std::fmt::Result {
        match terminator {
            Terminator::Goto(goto) => write!(f, "goto {goto};"),
            Terminator::If {
                condition,
                then_block,
                else_block,
            } => {
                write!(f, "if ")?;

                print_expression(f, &condition.expr)?;

                write!(f, " then {then_block} else {else_block};")
            }
        }
    }

    fn print_expression(
        f: &mut std::fmt::Formatter<'_>,
        expression: &Expression,
    ) -> std::fmt::Result {
        write!(f, "(")?;

        match expression {
            Expression::Variable(name) => write!(f, "var_{name}")?,
            Expression::Boolean(value) => write!(f, "{value}")?,
            Expression::Integer(value) => write!(f, "{value}")?,
            Expression::Float(value) => write!(f, "{value}")?,

            Expression::Colour { r, g, b, a } => write!(f, "#{r:02x}{g:02x}{b:02x}{a:02x}")?,
            Expression::Vector { x, y } => {
                write!(f, "{{ ")?;

                print_expression(f, &x.expr)?;

                write!(f, ", ")?;

                print_expression(f, &y.expr)?;

                write!(f, " }}")?;
            }
            Expression::Object(object) => write!(f, "{object}")?,
            Expression::Operation(operation) => print_operation(f, operation)?,
        }

        write!(f, ")")
    }

    fn print_operation(f: &mut std::fmt::Formatter<'_>, operation: &Operation) -> std::fmt::Result {
        match operation {
            Operation::IntegerEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " == ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerNotEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " != ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerPlus(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " + ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerMinus(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " - ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerMultiply(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " * ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerDivide(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " / ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " >= ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerLessThanEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " <= ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerGreaterThan(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " > ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerLessThan(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " < ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " == ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatNotEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " != ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatPlus(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " + ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatMinus(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " - ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatMultiply(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " * ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatDivide(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " / ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatGreaterThanEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " >= ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatLessThanEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " <= ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatGreaterThan(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " > ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::FloatLessThan(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " < ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::BooleanEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " == ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::BooleanNotEquals(lhs, rhs) => {
                print_expression(f, &lhs.expr)?;

                write!(f, " != ")?;

                print_expression(f, &rhs.expr)
            }
            Operation::IntegerNegate(value) => {
                write!(f, "-")?;

                print_expression(f, &value.expr)
            }
            Operation::FloatNegate(value) => {
                write!(f, "-")?;

                print_expression(f, &value.expr)
            }
            Operation::BooleanNot(value) => {
                write!(f, "!")?;

                print_expression(f, &value.expr)
            }
        }
    }
}
