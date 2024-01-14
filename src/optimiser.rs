use crate::low_ir::{BasicBlock, Terminator};

pub fn optimise(blocks: &[BasicBlock]) -> Vec<Option<BasicBlock>> {
    let blocks = simplify_terminators(blocks);

    dead_code_elimination(blocks)
}

fn simplify_terminators(blocks: &[BasicBlock]) -> Vec<BasicBlock> {
    blocks
        .iter()
        .map(|block| {
            let mut new_block = block.clone();

            if let Terminator::Goto(refers) = block.terminator() {
                let refers_block = &blocks[*refers];

                if refers_block.instructions().is_empty() {
                    new_block.set_terminator(refers_block.terminator().clone());
                }
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
