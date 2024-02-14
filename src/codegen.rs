use crate::{
    ast::{BinaryOp, Type, UnaryOp},
    low_ir::{
        BasicBlock, Expression, Goto, Instruction, Procedure, Terminator, TopLevel, TypedExpression,
    },
    mir::{self, ProcId, VarId},
    IdGen,
};
use levelfile::{
    Action, ActionType, CallParameter, Colour, DynamicType, Exolvl, FunctionCall, NovaScript,
    NovaValue, Parameter, StaticType, Variable, Vec2,
};
use rustc_hash::FxHashMap;

pub fn codegen(low_ir: &[TopLevel], exolvl: &mut Exolvl) {
    Codegen::new(exolvl).codegen(low_ir);
}

#[derive(Clone, Debug)]
struct ProcSignature {
    call_block_id: i32,
    param_ids: Vec<i32>,
    blocks: Vec<i32>,
}

struct Codegen<'a> {
    exolvl: &'a mut Exolvl,
    id_gen: IdGen,
    proc_map: FxHashMap<ProcId, ProcSignature>,
    var_map: FxHashMap<VarId, i32>,
    current_proc: Option<ProcId>,
}

impl Codegen<'_> {
    fn new(exolvl: &mut Exolvl) -> Codegen {
        Codegen {
            exolvl,
            id_gen: IdGen::default(),
            proc_map: FxHashMap::default(),
            var_map: FxHashMap::default(),
            current_proc: None,
        }
    }

    fn codegen(&mut self, low_ir: &[TopLevel]) {
        low_ir
            .iter()
            .map(|tl| match tl {
                TopLevel::Procedure(proc) => proc,
            })
            .for_each(|proc| {
                let call_block_id = self.id_gen.next_i32();

                let param_ids = (0..proc.args.len())
                    .map(|_| self.id_gen.next_i32())
                    .collect();

                for arg in &proc.args {
                    let variable_id = self.id_gen.next_i32();

                    self.var_map.insert(arg.0, variable_id);

                    self.exolvl.level_data.global_variables.push(Variable {
                        variable_id,
                        name: format!("var_{}", arg.0 .0),
                        static_type: match arg.1 {
                            Type::Boolean => StaticType::Bool,
                            Type::Integer => StaticType::Int,
                            Type::Float => StaticType::Float,
                            Type::String => StaticType::String,
                            Type::Colour => StaticType::Color,
                            Type::Vector => StaticType::Vector,
                        },
                        initial_value: default_novavalue(match arg.1 {
                            Type::Boolean => DynamicType::BoolConstant,
                            Type::Integer => DynamicType::IntConstant,
                            Type::Float => DynamicType::FloatConstant,
                            Type::String => DynamicType::StringConstant,
                            Type::Colour => DynamicType::ColorConstant,
                            Type::Vector => DynamicType::VectorConstant,
                        }),
                    });
                }

                let blocks = proc.body.iter().map(|_| self.id_gen.next_i32()).collect();

                self.proc_map.insert(
                    proc.name,
                    ProcSignature {
                        call_block_id,
                        param_ids,
                        blocks,
                    },
                );
            });

        for toplevel in low_ir {
            match toplevel {
                TopLevel::Procedure(proc) => {
                    self.codegen_proc(proc);
                }
            }
        }

        // let main_proc_id = ProcId(0);

        // self.exolvl.level_data.nova_scripts.push(NovaScript {
        //     script_id: self.id_gen.next_i32(),
        //     script_name: "run main".to_string(),
        //     is_function: false,
        //     activation_count: 0,
        //     condition: new_novavalue(DynamicType::BoolConstant, NewValue::Bool(true)),
        //     activation_list: vec![Activator {
        //         activator_type: 0,
        //         parameters: vec![],
        //     }],
        //     parameters: vec![],
        //     variables: vec![],
        //     actions: vec![new_action(ActionType::RunFunction {
        //         function: FunctionCall {
        //             id: self.proc_map[&main_proc_id].call_block_id,
        //             parameters: vec![],
        //         },
        //     })],
        // });
    }

    fn codegen_proc(&mut self, proc: &Procedure) {
        let signature = self.proc_map[&proc.name].clone();

        self.current_proc = Some(proc.name);

        self.codegen_body(&proc.body, &signature.blocks);

        let actions = proc
            .args
            .iter()
            .zip(&signature.param_ids)
            .map(|((arg, ty), param_id)| {
                new_action(ActionType::SetVariable {
                    variable: self.var_map[arg],
                    value: Some(new_novavalue(
                        match ty {
                            Type::Boolean => DynamicType::BoolParameter,
                            Type::Integer => DynamicType::IntParameter,
                            Type::Float => DynamicType::FloatParameter,
                            Type::String => DynamicType::StringParameter,
                            Type::Colour => DynamicType::ColorParameter,
                            Type::Vector => DynamicType::VectorParameter,
                        },
                        NewValue::Int(*param_id),
                    )),
                })
            })
            .chain(std::iter::once(new_action(ActionType::RunFunction {
                function: FunctionCall {
                    id: signature.blocks[0],
                    parameters: vec![],
                },
            })))
            .collect();

        let call_block = NovaScript {
            script_id: signature.call_block_id,
            script_name: format!("proc_{}", proc.name.0),
            is_function: true,
            activation_count: 0,
            condition: new_novavalue(DynamicType::BoolConstant, NewValue::Bool(true)),
            activation_list: vec![],
            parameters: Self::proc_args(&proc.args, signature.param_ids),
            variables: vec![],
            actions,
        };

        self.exolvl.level_data.nova_scripts.push(call_block);
    }

    fn proc_args(args: &[(VarId, Type)], param_ids: Vec<i32>) -> Vec<Parameter> {
        args.iter()
            .zip(param_ids)
            .map(|((arg, ty), parameter_id)| {
                let (static_type, default_value) = match ty {
                    Type::Boolean => (
                        StaticType::Bool,
                        default_novavalue(DynamicType::BoolConstant),
                    ),
                    Type::Integer => (StaticType::Int, default_novavalue(DynamicType::IntConstant)),
                    Type::Float => (
                        StaticType::Float,
                        default_novavalue(DynamicType::FloatConstant),
                    ),
                    Type::String => (
                        StaticType::String,
                        default_novavalue(DynamicType::StringConstant),
                    ),
                    Type::Colour => (
                        StaticType::Color,
                        default_novavalue(DynamicType::ColorConstant),
                    ),
                    Type::Vector => (
                        StaticType::Vector,
                        default_novavalue(DynamicType::VectorConstant),
                    ),
                };

                Parameter {
                    parameter_id,
                    name: format!("var_{}", arg.0),
                    static_type,
                    default_value,
                }
            })
            .collect()
    }

    fn codegen_body(&mut self, body: &[BasicBlock], block_ids: &[i32]) {
        for (block, script_id) in body.iter().zip(block_ids) {
            self.codegen_block(block, *script_id);
        }
    }

    fn codegen_block(&mut self, block: &BasicBlock, script_id: i32) {
        let mut actions = vec![];

        for instr in block.instructions() {
            actions.extend(self.codegen_instruction(instr));
        }

        actions.extend(self.codegen_terminator(block.terminator(), script_id));

        let script = NovaScript {
            script_id,
            script_name: format!("bb_{}", block.id()),
            is_function: true,
            activation_count: 0,
            condition: new_novavalue(DynamicType::BoolConstant, NewValue::Bool(true)),
            activation_list: vec![],
            parameters: vec![],
            variables: vec![],
            actions,
        };

        self.exolvl.level_data.nova_scripts.push(script);
    }

    fn codegen_instruction(&mut self, instruction: &Instruction) -> Vec<Action> {
        match instruction {
            Instruction::Expr(_expr) => vec![],
            Instruction::Let { name, value } => {
                let static_type = match value.ty {
                    Type::Boolean => StaticType::Bool,
                    Type::Integer => StaticType::Int,
                    Type::Float => StaticType::Float,
                    Type::String => StaticType::String,
                    Type::Colour => StaticType::Color,
                    Type::Vector => StaticType::Vector,
                };

                let initial_value = default_novavalue(match value.ty {
                    Type::Boolean => DynamicType::BoolConstant,
                    Type::Integer => DynamicType::IntConstant,
                    Type::Float => DynamicType::FloatConstant,
                    Type::String => DynamicType::StringConstant,
                    Type::Colour => DynamicType::ColorConstant,
                    Type::Vector => DynamicType::VectorConstant,
                });

                let variable_id = self.id_gen.next_i32();

                self.exolvl.level_data.global_variables.push(Variable {
                    variable_id,
                    name: format!("var_{}", name.0),
                    static_type,
                    initial_value,
                });

                self.var_map.insert(*name, variable_id);

                vec![self.codegen_assign(*name, value)]
            }
            Instruction::Assign { name, value } => vec![self.codegen_assign(*name, value)],
            Instruction::Action { name, args } => {
                let mut args = args
                    .iter()
                    .map(|arg| (self.codegen_expression(arg), arg.ty));

                vec![new_action(match name {
                    mir::Action::Wait => {
                        let duration = args.next().unwrap().0;

                        ActionType::Wait { duration }
                    }
                    mir::Action::WaitFrames => {
                        let frames = args.next().unwrap().0;

                        ActionType::WaitFrames { frames }
                    }
                    mir::Action::Print => {
                        let (value, ty) = args.next().unwrap();

                        Self::print_action(value, ty)
                    }
                })]
            }
        }
    }

    fn print_action(value: NovaValue, ty: Type) -> ActionType {
        let duration = new_novavalue(DynamicType::FloatConstant, NewValue::Float(1.0));

        match ty {
            Type::Boolean => ActionType::ConditionBlock {
                if_actions: vec![new_action(ActionType::GameTextShow {
                    text: new_novavalue(
                        DynamicType::StringConstant,
                        NewValue::String("true".to_string()),
                    ),
                    duration: new_novavalue(DynamicType::FloatConstant, NewValue::Float(1.0)),
                })],
                else_actions: vec![new_action(ActionType::GameTextShow {
                    text: new_novavalue(
                        DynamicType::StringConstant,
                        NewValue::String("false".to_string()),
                    ),
                    duration,
                })],
                condition: value,
            },
            Type::Integer => ActionType::GameTextShow {
                text: new_novavalue(DynamicType::StringFromInt, NewValue::SubValues(vec![value])),
                duration,
            },
            Type::Float => ActionType::GameTextShow {
                text: new_novavalue(
                    DynamicType::StringFromFloat,
                    NewValue::SubValues(vec![value]),
                ),
                duration,
            },
            Type::String => ActionType::GameTextShow {
                text: value,
                duration,
            },
            Type::Colour | Type::Vector => unreachable!(),
        }
    }

    fn codegen_assign(&self, name: VarId, value: &TypedExpression) -> Action {
        new_action(ActionType::SetVariable {
            variable: self.var_map[&name],
            value: Some(self.codegen_expression(value)),
        })
    }

    fn codegen_terminator(&self, terminator: &Terminator, script_id: i32) -> Vec<Action> {
        match terminator {
            Terminator::Goto(goto) => self.codegen_goto(goto, script_id),
            Terminator::If {
                condition,
                then_block,
                else_block,
            } => vec![new_action(ActionType::ConditionBlock {
                if_actions: self.codegen_goto(then_block, script_id),
                else_actions: self.codegen_goto(else_block, script_id),
                condition: self.codegen_expression(condition),
            })],
            Terminator::Call {
                proc,
                args,
                continuation,
            } => {
                let proc_signature = self.proc_map[proc].clone();

                let parameters = args
                    .iter()
                    .zip(&proc_signature.param_ids)
                    .map(|(arg, param_id)| CallParameter {
                        parameter_id: *param_id,
                        value: self.codegen_expression(arg),
                    })
                    .collect();

                std::iter::once(new_action(ActionType::RunFunction {
                    function: FunctionCall {
                        id: proc_signature.call_block_id,
                        parameters,
                    },
                }))
                .chain(self.codegen_goto(continuation, script_id))
                .collect()
            }
        }
    }

    fn codegen_goto(&self, goto: &Goto, script_id: i32) -> Vec<Action> {
        match goto {
            Goto::Block(block_id) => vec![new_action(ActionType::RunFunction {
                function: FunctionCall {
                    id: self.proc_map[&self.current_proc.unwrap()].blocks[*block_id],
                    parameters: vec![],
                },
            })],
            Goto::Return => vec![
                new_action(ActionType::StopScript {
                    script: new_novavalue(DynamicType::ScriptConstant, NewValue::Int(script_id)),
                }),
                new_action(ActionType::WaitFrames {
                    frames: new_novavalue(DynamicType::IntConstant, NewValue::Int(1)),
                }),
            ],
        }
    }

    fn codegen_expression(&self, expr: &TypedExpression) -> NovaValue {
        match &expr.expr {
            Expression::Variable(var) => {
                let dynamic_type = match expr.ty {
                    Type::Boolean => DynamicType::BoolVariable,
                    Type::Integer => DynamicType::IntVariable,
                    Type::Float => DynamicType::FloatVariable,
                    Type::String => DynamicType::StringVariable,
                    Type::Colour => DynamicType::ColorVariable,
                    Type::Vector => DynamicType::VectorVariable,
                };

                new_novavalue(dynamic_type, NewValue::Int(self.var_map[var]))
            }
            Expression::Boolean(value) => {
                new_novavalue(DynamicType::BoolConstant, NewValue::Bool(*value))
            }
            Expression::Integer(value) => {
                new_novavalue(DynamicType::IntConstant, NewValue::Int(*value))
            }
            Expression::Float(value) => {
                new_novavalue(DynamicType::FloatConstant, NewValue::Float(*value))
            }
            Expression::String(value) => new_novavalue(
                DynamicType::StringConstant,
                NewValue::String((*value).to_string()),
            ),
            Expression::Colour { r, g, b, a } => new_novavalue(
                DynamicType::ColorConstant,
                NewValue::Color(Colour {
                    r: f32::from(*r) / 255.0,
                    g: f32::from(*g) / 255.0,
                    b: f32::from(*b) / 255.0,
                    a: f32::from(*a) / 255.0,
                }),
            ),
            Expression::Vector { x, y } => new_novavalue(
                DynamicType::VectorValues,
                NewValue::SubValues(vec![self.codegen_expression(x), self.codegen_expression(y)]),
            ),
            Expression::Unary { op, value } => {
                let ty = value.ty;
                let value = self.codegen_expression(value);

                match op {
                    UnaryOp::Negate => match ty {
                        Type::Integer => new_novavalue(
                            DynamicType::IntMultiply,
                            NewValue::SubValues(vec![
                                value,
                                new_novavalue(DynamicType::IntConstant, NewValue::Int(-1)),
                            ]),
                        ),
                        Type::Float => new_novavalue(
                            DynamicType::FloatMultiply,
                            NewValue::SubValues(vec![
                                value,
                                new_novavalue(DynamicType::FloatConstant, NewValue::Float(-1.0)),
                            ]),
                        ),
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                    UnaryOp::Not => match ty {
                        Type::Boolean => {
                            new_novavalue(DynamicType::BoolNot, NewValue::SubValues(vec![value]))
                        }
                        Type::Integer
                        | Type::Float
                        | Type::String
                        | Type::Colour
                        | Type::Vector => unreachable!(),
                    },
                }
            }
            Expression::Binary { lhs, op, rhs } => {
                assert_eq!(lhs.ty, rhs.ty);

                let ty = lhs.ty;

                let lhs = self.codegen_expression(lhs);
                let rhs = self.codegen_expression(rhs);

                let dyn_ty = match op {
                    BinaryOp::Equals => match ty {
                        Type::Boolean => DynamicType::BoolEqualBool,
                        Type::Integer | Type::Float => DynamicType::BoolEqualNumber,
                        Type::String => DynamicType::BoolEqualString,
                        Type::Colour | Type::Vector => unreachable!(),
                    },
                    BinaryOp::NotEquals => match ty {
                        Type::Boolean => DynamicType::BoolNotEqualBool,
                        Type::Integer | Type::Float => DynamicType::BoolNotEqualNumber,
                        Type::String => DynamicType::BoolNotEqualString,
                        Type::Colour | Type::Vector => unreachable!(),
                    },
                    BinaryOp::Plus => match ty {
                        Type::Integer => DynamicType::IntAdd,
                        Type::Float => DynamicType::FloatAdd,
                        Type::String => DynamicType::StringConcat,
                        Type::Boolean | Type::Colour | Type::Vector => unreachable!(),
                    },
                    BinaryOp::Minus => match ty {
                        Type::Integer => DynamicType::IntSubtract,
                        Type::Float => DynamicType::FloatSubtract,
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                    BinaryOp::Multiply => match ty {
                        Type::Integer => DynamicType::IntMultiply,
                        Type::Float => DynamicType::FloatMultiply,
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                    BinaryOp::Divide => match ty {
                        Type::Integer => DynamicType::IntDivide,
                        Type::Float => DynamicType::FloatDivide,
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                    BinaryOp::GreaterThanEquals => match ty {
                        Type::Integer | Type::Float => DynamicType::BoolGreaterOrEqual,
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                    BinaryOp::LessThanEquals => match ty {
                        Type::Integer | Type::Float => DynamicType::BoolLessOrEqual,
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                    BinaryOp::GreaterThan => match ty {
                        Type::Integer | Type::Float => DynamicType::BoolGreater,
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                    BinaryOp::LessThan => match ty {
                        Type::Integer | Type::Float => DynamicType::BoolLess,
                        Type::Boolean | Type::String | Type::Colour | Type::Vector => {
                            unreachable!()
                        }
                    },
                };

                new_novavalue(dyn_ty, NewValue::SubValues(vec![lhs, rhs]))
            }
        }
    }
}

fn new_novavalue(dynamic_type: DynamicType, value: NewValue) -> NovaValue {
    match value {
        NewValue::Bool(value) => NovaValue {
            dynamic_type,
            bool_value: value,
            int_value: 0,
            float_value: 0.0,
            string_value: None,
            color_value: Colour {
                r: 0.0,
                g: 0.0,
                b: 0.0,
                a: 0.0,
            },
            vector_value: Vec2 { x: 0.0, y: 0.0 },
            int_list_value: None,
            sub_values: None,
        },
        NewValue::Int(value) => NovaValue {
            dynamic_type,
            bool_value: false,
            int_value: value,
            float_value: 0.0,
            string_value: None,
            color_value: Colour {
                r: 0.0,
                g: 0.0,
                b: 0.0,
                a: 0.0,
            },
            vector_value: Vec2 { x: 0.0, y: 0.0 },
            int_list_value: None,
            sub_values: None,
        },
        NewValue::Float(value) => NovaValue {
            dynamic_type,
            bool_value: false,
            int_value: 0,
            float_value: value,
            string_value: None,
            color_value: Colour {
                r: 0.0,
                g: 0.0,
                b: 0.0,
                a: 0.0,
            },
            vector_value: Vec2 { x: 0.0, y: 0.0 },
            int_list_value: None,
            sub_values: None,
        },
        NewValue::String(value) => NovaValue {
            dynamic_type,
            bool_value: false,
            int_value: 0,
            float_value: 0.0,
            string_value: Some(value),
            color_value: Colour {
                r: 0.0,
                g: 0.0,
                b: 0.0,
                a: 0.0,
            },
            vector_value: Vec2 { x: 0.0, y: 0.0 },
            int_list_value: None,
            sub_values: None,
        },
        NewValue::Color(value) => NovaValue {
            dynamic_type,
            bool_value: false,
            int_value: 0,
            float_value: 0.0,
            string_value: None,
            color_value: value,
            vector_value: Vec2 { x: 0.0, y: 0.0 },
            int_list_value: None,
            sub_values: None,
        },
        NewValue::SubValues(value) => NovaValue {
            dynamic_type,
            bool_value: false,
            int_value: 0,
            float_value: 0.0,
            string_value: None,
            color_value: Colour {
                r: 0.0,
                g: 0.0,
                b: 0.0,
                a: 0.0,
            },
            vector_value: Vec2 { x: 0.0, y: 0.0 },
            int_list_value: None,
            sub_values: Some(value),
        },
    }
}

enum NewValue {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
    Color(Colour),
    SubValues(Vec<NovaValue>),
}

const fn default_novavalue(dynamic_type: DynamicType) -> NovaValue {
    NovaValue {
        dynamic_type,
        bool_value: false,
        int_value: 0,
        float_value: 0.0,
        string_value: None,
        color_value: Colour {
            r: 0.0,
            g: 0.0,
            b: 0.0,
            a: 0.0,
        },
        vector_value: Vec2 { x: 0.0, y: 0.0 },
        int_list_value: None,
        sub_values: None,
    }
}

const fn new_action(action_type: ActionType) -> Action {
    Action {
        closed: true,
        wait: true,
        action_type,
    }
}
