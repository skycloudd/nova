use crate::low_ir::{BasicBlock, Terminator};

pub fn dead_code_elimination(blocks: Vec<BasicBlock>) -> Vec<Option<BasicBlock>> {
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
