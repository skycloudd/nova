use crate::low_ir::TopLevel;

pub fn codegen(low_ir: &[TopLevel]) {
    Codegen::new().codegen(low_ir);
}

struct Codegen {}

impl Codegen {
    const fn new() -> Self {
        Self {}
    }

    fn codegen(&mut self, low_ir: &[TopLevel]) {
        todo!("{:#?}", low_ir)
    }
}
