use rustc_hash::FxHashSet;

use crate::low_ir::{BasicBlock, Expression, Goto, Instruction, Operation, Terminator, VarId};

pub fn optimise(blocks: &mut Vec<BasicBlock>) {
    loop {
        let before = blocks.clone();

        simplify_terminators(blocks);

        if &before == blocks {
            break;
        }
    }

    dead_variable_elimination(blocks);

    dead_code_elimination(blocks);
}

fn simplify_terminators(blocks: &mut [BasicBlock]) {
    let blocks_cloned = blocks.to_vec();

    for block in blocks.iter_mut() {
        let terminator = block.terminator_mut();

        match terminator {
            Terminator::Goto(goto) => match goto {
                Goto::Block(refers) => {
                    let refers_block = &blocks_cloned[*refers];

                    if refers_block.instructions().is_empty() {
                        *terminator = refers_block.terminator().clone();
                    }
                }
                Goto::Finish => {}
            },
            Terminator::If {
                condition,
                then_block: then_goto,
                else_block: else_goto,
            } => {
                if condition.expr == Expression::Boolean(true) {
                    *terminator = Terminator::Goto(then_goto.clone());
                } else if condition.expr == Expression::Boolean(false) {
                    *terminator = Terminator::Goto(else_goto.clone());
                } else {
                    if let Goto::Block(then) = then_goto {
                        let then_block = &blocks_cloned[*then];

                        if then_block.instructions().is_empty() {
                            if let Terminator::Goto(goto) = then_block.terminator() {
                                *then_goto = goto.clone();
                            }
                        }
                    }

                    if let Goto::Block(else_) = else_goto {
                        let else_block = &blocks_cloned[*else_];

                        if else_block.instructions().is_empty() {
                            if let Terminator::Goto(goto) = else_block.terminator() {
                                *else_goto = goto.clone();
                            }
                        }
                    }
                }
            }
        }
    }
}

fn dead_code_elimination(blocks: &mut Vec<BasicBlock>) {
    let references = blocks
        .iter()
        .flat_map(|block| match block.terminator() {
            Terminator::Goto(goto) => match goto {
                Goto::Block(id) => vec![*id],
                Goto::Finish => vec![],
            },
            Terminator::If {
                condition: _,
                then_block,
                else_block,
            } => match (then_block, else_block) {
                (Goto::Block(then), Goto::Block(else_)) => vec![*then, *else_],
                (Goto::Block(then), Goto::Finish) => vec![*then],
                (Goto::Finish, Goto::Block(else_)) => vec![*else_],
                (Goto::Finish, Goto::Finish) => vec![],
            },
        })
        .collect::<Vec<_>>();

    blocks.retain(|block| references.contains(&block.id()) || block.id() == 0);
}

fn dead_variable_elimination(blocks: &mut [BasicBlock]) {
    let mut used_variables = vec![];

    for block in blocks.iter() {
        for instruction in block.instructions() {
            used_variables.extend(get_used_variables(instruction));
        }

        used_variables.extend(get_used_variables_terminator(block.terminator()));
    }

    let used_variables = FxHashSet::from_iter(used_variables);

    for block in blocks {
        block
            .instructions_mut()
            .retain(|instruction| match instruction {
                Instruction::Expr(_) => false, // no side effects
                Instruction::Action { .. } => true,
                Instruction::Let { name, value: _ } | Instruction::Assign { name, value: _ } => {
                    used_variables.contains(name)
                }
            });
    }
}

fn get_used_variables(instruction: &Instruction) -> Vec<VarId> {
    match instruction {
        Instruction::Expr(expr) => get_used_variables_expr(&expr.expr),
        Instruction::Let { name: _, value } | Instruction::Assign { name: _, value } => {
            get_used_variables_expr(&value.expr)
        }
        Instruction::Action { name: _, args } => {
            let mut used = vec![];

            for arg in args {
                used.extend(get_used_variables_expr(&arg.expr));
            }

            used
        }
    }
}

fn get_used_variables_expr(expr: &Expression) -> Vec<VarId> {
    match expr {
        Expression::Variable(name) => vec![*name],
        Expression::Boolean(_)
        | Expression::Integer(_)
        | Expression::Float(_)
        | Expression::Colour {
            r: _,
            g: _,
            b: _,
            a: _,
        }
        | Expression::Object(_) => vec![],
        Expression::Vector { x, y } => {
            let mut used = get_used_variables_expr(&x.expr);

            used.extend(get_used_variables_expr(&y.expr));

            used
        }
        Expression::Operation(operation) => match *operation.clone() {
            Operation::IntegerEquals(lhs, rhs)
            | Operation::IntegerNotEquals(lhs, rhs)
            | Operation::IntegerPlus(lhs, rhs)
            | Operation::IntegerMinus(lhs, rhs)
            | Operation::IntegerMultiply(lhs, rhs)
            | Operation::IntegerDivide(lhs, rhs)
            | Operation::IntegerGreaterThanEquals(lhs, rhs)
            | Operation::IntegerLessThanEquals(lhs, rhs)
            | Operation::IntegerGreaterThan(lhs, rhs)
            | Operation::IntegerLessThan(lhs, rhs)
            | Operation::FloatEquals(lhs, rhs)
            | Operation::FloatNotEquals(lhs, rhs)
            | Operation::FloatPlus(lhs, rhs)
            | Operation::FloatMinus(lhs, rhs)
            | Operation::FloatMultiply(lhs, rhs)
            | Operation::FloatDivide(lhs, rhs)
            | Operation::FloatGreaterThanEquals(lhs, rhs)
            | Operation::FloatLessThanEquals(lhs, rhs)
            | Operation::FloatGreaterThan(lhs, rhs)
            | Operation::FloatLessThan(lhs, rhs)
            | Operation::BooleanEquals(lhs, rhs)
            | Operation::BooleanNotEquals(lhs, rhs) => {
                let mut used = get_used_variables_expr(&lhs.expr);

                used.extend(get_used_variables_expr(&rhs.expr));

                used
            }

            crate::low_ir::Operation::IntegerNegate(rhs)
            | Operation::FloatNegate(rhs)
            | Operation::BooleanNot(rhs) => get_used_variables_expr(&rhs.expr),
        },
    }
}

fn get_used_variables_terminator(terminator: &Terminator) -> Vec<VarId> {
    match terminator {
        Terminator::If {
            condition,
            then_block: _,
            else_block: _,
        } => get_used_variables_expr(&condition.expr),
        Terminator::Goto(_) => vec![],
    }
}
