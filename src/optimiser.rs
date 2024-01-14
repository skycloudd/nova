use rustc_hash::FxHashSet;

use crate::low_ir::{BasicBlock, Expression, Instruction, Operation, Terminator, VarId};

pub fn optimise(blocks: &[BasicBlock]) -> Vec<Option<BasicBlock>> {
    let mut blocks = simplify_terminators(blocks);

    loop {
        let new_blocks = simplify_terminators(&blocks);

        if blocks == new_blocks {
            break;
        }

        blocks = new_blocks;
    }

    dead_variable_elimination(&mut blocks);

    dead_code_elimination(blocks)
}

fn simplify_terminators(blocks: &[BasicBlock]) -> Vec<BasicBlock> {
    blocks
        .iter()
        .map(|block| {
            let mut new_block = block.clone();

            match block.terminator() {
                Terminator::Goto(refers) => {
                    let refers_block = &blocks[*refers];

                    if refers_block.instructions().is_empty() {
                        new_block.set_terminator(refers_block.terminator().clone());
                    }
                }
                Terminator::If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let then_block_ = &blocks[*then_block];
                    let else_block_ = &blocks[*else_block];

                    let mut new_then_block_id = *then_block;

                    if then_block_.instructions().is_empty() {
                        if let Terminator::Goto(refers) = then_block_.terminator() {
                            new_then_block_id = *refers;
                        }
                    }

                    if let Expression::Boolean(value) = condition.expr {
                        if value {
                            new_block.set_terminator(Terminator::Goto(new_then_block_id));
                        } else {
                            new_block.set_terminator(Terminator::Goto(*else_block));
                        }

                        return new_block;
                    }

                    let mut new_else_block_id = *else_block;

                    if else_block_.instructions().is_empty() {
                        if let Terminator::Goto(refers) = else_block_.terminator() {
                            new_else_block_id = *refers;
                        }
                    }

                    new_block.set_terminator(Terminator::If {
                        condition: condition.clone(),
                        then_block: new_then_block_id,
                        else_block: new_else_block_id,
                    });
                }
                Terminator::Finish => {}
            }

            new_block
        })
        .collect()
}

fn dead_code_elimination(blocks: Vec<BasicBlock>) -> Vec<Option<BasicBlock>> {
    let references = blocks
        .iter()
        .flat_map(|block| match block.terminator() {
            Terminator::Goto(block) => vec![*block],
            Terminator::If {
                condition: _,
                then_block,
                else_block,
            } => vec![*then_block, *else_block],
            Terminator::Finish => vec![],
        })
        .collect::<Vec<_>>();

    blocks
        .into_iter()
        .map(|block| {
            if references.contains(&block.id()) || block.id() == 0 {
                Some(block)
            } else {
                None
            }
        })
        .collect()
}

fn dead_variable_elimination(blocks: &mut [BasicBlock]) {
    let mut used_variables = vec![];

    for block in blocks.as_ref() {
        for instruction in block.instructions() {
            let used = get_used_variables(instruction);

            used_variables.extend(used);
        }

        used_variables.extend(get_used_variables_terminator(block.terminator()));
    }

    let used_variables = FxHashSet::from_iter(used_variables);

    for block in blocks {
        block
            .instructions_mut()
            .retain(|instruction| match instruction {
                Instruction::Expr(_) => false,
                Instruction::BuiltinPrint(_) => true,
                Instruction::Let { name, value: _ } => used_variables.contains(name),
                Instruction::Assign { name, value: _ } => used_variables.contains(name),
            });
    }
}

fn get_used_variables(instruction: &Instruction) -> Vec<VarId> {
    match instruction {
        Instruction::Expr(expr) => get_used_variables_expr(&expr.expr),
        Instruction::BuiltinPrint(expr) => get_used_variables_expr(&expr.expr),
        Instruction::Let { name: _, value } => get_used_variables_expr(&value.expr),
        Instruction::Assign { name: _, value } => get_used_variables_expr(&value.expr),
    }
}

fn get_used_variables_expr(expr: &Expression) -> Vec<VarId> {
    match expr {
        Expression::Variable(name) => vec![*name],
        Expression::Boolean(_) => vec![],
        Expression::Integer(_) => vec![],
        Expression::Float(_) => vec![],
        Expression::Colour { r: _, g: _, b: _ } => vec![],
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
        Terminator::Goto(_) => vec![],
        Terminator::If {
            condition,
            then_block: _,
            else_block: _,
        } => get_used_variables_expr(&condition.expr),
        Terminator::Finish => vec![],
    }
}
