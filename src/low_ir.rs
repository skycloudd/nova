use crate::{mir_no_span as mir, FloatTy, IntTy};

#[derive(Debug)]
pub struct UnfinishedBasicBlock {
    id: BasicBlockId,
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

#[derive(Debug)]
pub struct BasicBlock {
    id: BasicBlockId,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

impl BasicBlock {
    pub const fn id(&self) -> BasicBlockId {
        self.id
    }

    pub const fn terminator(&self) -> &Terminator {
        &self.terminator
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
    #[allow(dead_code)]
    ty: Type,
}

#[derive(Debug)]
pub enum Expression {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
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
            mir::Type::Boolean => Self::Boolean,
            mir::Type::Integer => Self::Integer,
            mir::Type::Float => Self::Float,
            mir::Type::Colour => Self::Colour,
            mir::Type::Vector => Self::Vector,
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

        self.finish(Terminator::Finish);

        self.blocks
    }

    fn lower_statement(&mut self, statement: mir::TypedStatement) -> bool {
        match statement {
            mir::TypedStatement::Expr(expr) => {
                let expr = Self::lower_expression(expr);

                self.current_mut()
                    .instructions
                    .push(Instruction::Expr(expr));

                false
            }
            mir::TypedStatement::BuiltinPrint(expr) => {
                let expr = Self::lower_expression(expr);

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
                let condition = Self::lower_expression(condition);

                let then_block = self.new_block();

                let else_block = else_branch.as_ref().map(|_| self.new_block());

                let merge_block = self.new_block();

                self.finish(Terminator::If {
                    condition,
                    then_block,
                    else_block: else_block.unwrap_or(merge_block),
                });

                self.switch_to(then_block);

                for statement in then_branch {
                    if self.lower_statement(statement) {
                        break;
                    }
                }

                self.finish_checked(Terminator::Goto(merge_block));

                if let Some(else_block) = else_block {
                    self.switch_to(else_block);

                    if let Some(else_branch) = else_branch {
                        for statement in else_branch {
                            if self.lower_statement(statement) {
                                break;
                            }
                        }
                    }

                    self.finish_checked(Terminator::Goto(merge_block));
                }

                self.switch_to(merge_block);

                false
            }
            mir::TypedStatement::Let { name, value }
            | mir::TypedStatement::Const { name, value } => {
                let value = Self::lower_expression(value);

                self.current_mut()
                    .instructions
                    .push(Instruction::Let { name, value });

                false
            }
            mir::TypedStatement::Assign { name, value } => {
                let value = Self::lower_expression(value);

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

    fn lower_expression(expression: mir::TypedExpression) -> TypedExpression {
        TypedExpression {
            expr: match expression.expr {
                mir::Expression::Variable(name) => Expression::Variable(name),
                mir::Expression::Boolean(value) => Expression::Boolean(value),
                mir::Expression::Integer(value) => Expression::Integer(value),
                mir::Expression::Float(value) => Expression::Float(value),
                mir::Expression::Colour { r, g, b } => Expression::Colour { r, g, b },
                mir::Expression::Vector { x, y } => Expression::Vector {
                    x: Box::new(Self::lower_expression(*x)),
                    y: Box::new(Self::lower_expression(*y)),
                },
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
    use super::{BasicBlock, Expression, Instruction, Operation, Terminator};

    impl std::fmt::Display for BasicBlock {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            print_basic_block(f, self)
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
            Instruction::BuiltinPrint(expr) => {
                write!(f, "print ")?;

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
        }
    }

    fn print_terminator(
        f: &mut std::fmt::Formatter<'_>,
        terminator: &Terminator,
    ) -> std::fmt::Result {
        match terminator {
            Terminator::Goto(block) => write!(f, "goto bb{block};"),
            Terminator::If {
                condition,
                then_block,
                else_block,
            } => {
                write!(f, "if ")?;

                print_expression(f, &condition.expr)?;

                write!(f, " then bb{then_block} else bb{else_block};")
            }
            Terminator::Finish => write!(f, "finish;"),
        }
    }

    fn print_expression(
        f: &mut std::fmt::Formatter<'_>,
        expression: &Expression,
    ) -> std::fmt::Result {
        match expression {
            Expression::Variable(name) => write!(f, "var_{name}"),
            Expression::Boolean(value) => write!(f, "{value}"),
            Expression::Integer(value) => write!(f, "{value}"),
            Expression::Float(value) => write!(f, "{value}"),

            Expression::Colour { r, g, b } => write!(f, "#{r:2x}{g:2x}{b:2x}"),
            Expression::Vector { x, y } => {
                write!(f, "{{ ")?;

                print_expression(f, &x.expr)?;

                write!(f, ", ")?;

                print_expression(f, &y.expr)?;

                write!(f, " }}")
            }
            Expression::Operation(operation) => {
                write!(f, "(")?;

                print_operation(f, operation)?;

                write!(f, ")")
            }
        }
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

pub mod eval {
    use crate::{FloatTy, IntTy};

    use super::{BasicBlock, BasicBlockId, Expression, Instruction, Operation, Terminator, VarId};
    use rustc_hash::FxHashMap;

    pub fn evaluate(blocks: &[Option<BasicBlock>]) {
        let mut state = State::new();

        state.evaluate_basic_block(blocks, 0);
    }

    #[derive(Clone)]
    enum Value {
        Boolean(bool),
        Integer(IntTy),
        Float(FloatTy),
        Colour { r: u8, g: u8, b: u8 },
        Vector { x: Box<Value>, y: Box<Value> },
    }

    impl std::fmt::Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Boolean(value) => write!(f, "{value}"),
                Self::Integer(value) => write!(f, "{value}"),
                Self::Float(value) => write!(f, "{value}"),
                Self::Colour { r, g, b } => write!(f, "#{r:02x}{g:02x}{b:02x}"),
                Self::Vector { x, y } => write!(f, "{{ {x}, {y} }}"),
            }
        }
    }

    enum ControlFlow {
        Goto(BasicBlockId),
        Finish,
    }

    struct State {
        variables: FxHashMap<VarId, Value>,
    }

    impl State {
        fn new() -> Self {
            Self {
                variables: FxHashMap::default(),
            }
        }

        fn evaluate_basic_block(&mut self, blocks: &[Option<BasicBlock>], block: BasicBlockId) {
            let mut next_block = block;

            loop {
                let block = blocks[next_block].as_ref().unwrap();

                for instruction in &block.instructions {
                    self.evaluate_instruction(instruction);
                }

                match self.evaluate_terminator(&block.terminator) {
                    ControlFlow::Goto(block) => next_block = block,
                    ControlFlow::Finish => break,
                }
            }
        }

        fn evaluate_instruction(&mut self, instruction: &Instruction) {
            match instruction {
                Instruction::Expr(expr) => {
                    self.evaluate_expression(&expr.expr);
                }
                Instruction::BuiltinPrint(expr) => {
                    let value = self.evaluate_expression(&expr.expr);

                    println!("{value}");
                }
                Instruction::Let { name, value } | Instruction::Assign { name, value } => {
                    let value = self.evaluate_expression(&value.expr);

                    self.variables.insert(*name, value);
                }
            }
        }

        fn evaluate_terminator(&mut self, terminator: &Terminator) -> ControlFlow {
            match terminator {
                Terminator::Goto(block) => ControlFlow::Goto(*block),
                Terminator::If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let condition = self.evaluate_expression(&condition.expr);

                    match condition {
                        Value::Boolean(true) => ControlFlow::Goto(*then_block),
                        Value::Boolean(false) => ControlFlow::Goto(*else_block),
                        _ => unreachable!(),
                    }
                }
                Terminator::Finish => ControlFlow::Finish,
            }
        }

        fn evaluate_expression(&self, expr: &Expression) -> Value {
            match expr {
                Expression::Variable(name) => self.variables[name].clone(),
                Expression::Boolean(value) => Value::Boolean(*value),
                Expression::Integer(value) => Value::Integer(*value),
                Expression::Float(value) => Value::Float(*value),
                Expression::Colour { r, g, b } => Value::Colour {
                    r: *r,
                    g: *g,
                    b: *b,
                },
                Expression::Vector { x, y } => Value::Vector {
                    x: Box::new(self.evaluate_expression(&x.expr)),
                    y: Box::new(self.evaluate_expression(&y.expr)),
                },
                Expression::Operation(operation) => self.evaluate_operation(operation),
            }
        }

        fn evaluate_operation(&self, operation: &Operation) -> Value {
            match operation {
                Operation::IntegerEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Boolean(lhs == rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerNotEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Boolean(lhs != rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerPlus(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerMinus(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerMultiply(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerDivide(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs / rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Boolean(lhs >= rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerLessThanEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Boolean(lhs <= rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerGreaterThan(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Boolean(lhs > rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerLessThan(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Boolean(lhs < rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Boolean(lhs == rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatNotEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Boolean(lhs != rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatPlus(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatMinus(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatMultiply(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatDivide(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatGreaterThanEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Boolean(lhs >= rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatLessThanEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Boolean(lhs <= rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatGreaterThan(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Boolean(lhs > rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatLessThan(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Boolean(lhs < rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::BooleanEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Boolean(lhs), Value::Boolean(rhs)) => Value::Boolean(lhs == rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::BooleanNotEquals(lhs, rhs) => {
                    let lhs = self.evaluate_expression(&lhs.expr);
                    let rhs = self.evaluate_expression(&rhs.expr);

                    match (lhs, rhs) {
                        (Value::Boolean(lhs), Value::Boolean(rhs)) => Value::Boolean(lhs != rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerNegate(value) => {
                    let value = self.evaluate_expression(&value.expr);

                    match value {
                        Value::Integer(value) => Value::Integer(-value),
                        _ => unreachable!(),
                    }
                }
                Operation::FloatNegate(value) => {
                    let value = self.evaluate_expression(&value.expr);

                    match value {
                        Value::Float(value) => Value::Float(-value),
                        _ => unreachable!(),
                    }
                }
                Operation::BooleanNot(value) => {
                    let value = self.evaluate_expression(&value.expr);

                    match value {
                        Value::Boolean(value) => Value::Boolean(!value),
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}
