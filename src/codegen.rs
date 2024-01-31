use crate::{
    low_ir::{
        BasicBlock, BasicBlockId, Expression, Goto, Instruction, Object, Operation, Terminator,
        Type, TypedExpression, VarId,
    },
    mir_no_span,
};
use levelfile::{
    scripts::{
        Action, ActionType, Activator, DynamicType, FunctionCall, NovaScript, NovaValue,
        StaticType, Variable,
    },
    Colour, Exolvl, MyString, MyVec, Vec2,
};
use rustc_hash::FxHashMap;

pub fn codegen(low_ir: &[BasicBlock], exolvl: &mut Exolvl) {
    let mut codegen = Codegen::new(exolvl);

    codegen.codegen(low_ir);
}

struct Codegen<'a> {
    exolvl: &'a mut Exolvl,
    id_gen: IdGen,
    block_ids: FxHashMap<BasicBlockId, i32>,
    var_ids: FxHashMap<VarId, i32>,
}

impl Codegen<'_> {
    fn new(exolvl: &mut Exolvl) -> Codegen {
        Codegen {
            exolvl,
            id_gen: IdGen::new(),
            block_ids: FxHashMap::default(),
            var_ids: FxHashMap::default(),
        }
    }

    fn codegen(&mut self, low_ir: &[BasicBlock]) {
        for bb in low_ir {
            self.block_ids.insert(bb.id(), self.id_gen.next());
        }

        let mut low_ir = low_ir.iter();

        let start_block = low_ir.next().unwrap();

        self.codegen_bb(start_block);

        for bb in low_ir {
            self.codegen_bb(bb);
        }

        let main_script = NovaScript {
            script_id: self.id_gen.next(),
            script_name: MyString("main".into()),
            is_function: false,
            activation_count: 1,
            condition: new_novavalue(DynamicType::BoolConstant, NewValue::Bool(true)),
            activation_list: MyVec(vec![Activator {
                activator_type: 28,
                parameters: MyVec(vec![]),
            }]),
            parameters: MyVec(vec![]),
            variables: MyVec(vec![]),
            actions: MyVec(vec![new_action(ActionType::RunFunction {
                function: FunctionCall {
                    id: self.block_ids[&start_block.id()],
                    parameters: MyVec(vec![]),
                },
            })]),
        };

        self.exolvl.level_data.nova_scripts.0.push(main_script);
    }

    fn codegen_bb(&mut self, bb: &BasicBlock) {
        let mut script = NovaScript {
            script_id: self.block_ids[&bb.id()],
            script_name: MyString(format!("bb{}", bb.id())),
            is_function: true,
            activation_count: 0,
            condition: new_novavalue(DynamicType::BoolConstant, NewValue::Bool(true)),
            activation_list: MyVec(vec![]),
            parameters: MyVec(vec![]),
            variables: MyVec(vec![]),
            actions: MyVec(vec![]),
        };

        for instruction in bb.instructions() {
            match instruction {
                Instruction::Expr(_expr) => {} // no side effects
                Instruction::Let { name, value } => {
                    let value = self.codegen_expr(value);

                    let variable_id = self.id_gen.next();

                    self.var_ids.insert(*name, variable_id);

                    let variable = Variable {
                        variable_id,
                        name: MyString(format!("var_{name}")),
                        static_type: match value.1 {
                            Type::Boolean => StaticType::Bool,
                            Type::Integer => StaticType::Int,
                            Type::Float => StaticType::Float,
                            Type::Colour => StaticType::Color,
                            Type::Vector => StaticType::Vector,
                            Type::Object => StaticType::Object,
                            Type::ObjectSet => StaticType::ObjectSet,
                        },
                        initial_value: new_novavalue(DynamicType::IntConstant, NewValue::Int(0)),
                    };

                    self.exolvl.level_data.global_variables.0.push(variable);

                    script.actions.0.push(new_action(ActionType::SetVariable {
                        variable: variable_id,
                        value: Some(value.0),
                    }));
                }
                Instruction::Assign { name, value } => {
                    let value = self.codegen_expr(value);

                    let variable = self.var_ids[name];

                    script.actions.0.push(new_action(ActionType::SetVariable {
                        variable,
                        value: Some(value.0),
                    }));
                }
                Instruction::Action { name, args } => {
                    let mut args = args.iter().map(|arg| (self.codegen_expr(arg), arg.ty));

                    let action = match name {
                        mir_no_span::Action::Wait => {
                            let duration = args.next().unwrap().0 .0;

                            new_action(ActionType::Wait { duration })
                        }
                        mir_no_span::Action::WaitFrames => {
                            let frames = args.next().unwrap().0 .0;

                            new_action(ActionType::WaitFrames { frames })
                        }
                        mir_no_span::Action::Move => {
                            let target_objects = args.next().unwrap().0 .0;
                            let position = args.next().unwrap().0 .0;
                            let global = args.next().unwrap().0 .0;
                            let duration = args.next().unwrap().0 .0;

                            new_action(ActionType::Move {
                                target_objects,
                                position,
                                global,
                                duration,
                                easing: new_novavalue(
                                    DynamicType::EasingConstant,
                                    NewValue::Int(0),
                                ),
                            })
                        }
                        mir_no_span::Action::Print => {
                            let expr = args.next().unwrap().0;

                            let duration =
                                new_novavalue(DynamicType::FloatConstant, NewValue::Float(1.0));

                            match expr.1 {
                                Type::Boolean => new_action(ActionType::ConditionBlock {
                                    if_actions: MyVec(vec![new_action(ActionType::GameTextShow {
                                        text: new_novavalue(
                                            DynamicType::StringConstant,
                                            NewValue::String(MyString("true".into())),
                                        ),
                                        duration: new_novavalue(
                                            DynamicType::FloatConstant,
                                            NewValue::Float(1.0),
                                        ),
                                    })]),
                                    else_actions: MyVec(vec![new_action(
                                        ActionType::GameTextShow {
                                            text: new_novavalue(
                                                DynamicType::StringConstant,
                                                NewValue::String(MyString("false".into())),
                                            ),
                                            duration,
                                        },
                                    )]),
                                    condition: expr.0,
                                }),
                                Type::Integer => new_action(ActionType::GameTextShow {
                                    text: new_novavalue(
                                        DynamicType::StringFromInt,
                                        NewValue::SubValues(MyVec(vec![expr.0])),
                                    ),
                                    duration,
                                }),
                                Type::Float => new_action(ActionType::GameTextShow {
                                    text: new_novavalue(
                                        DynamicType::StringFromFloat,
                                        NewValue::SubValues(MyVec(vec![expr.0])),
                                    ),
                                    duration,
                                }),
                                Type::Colour | Type::Vector | Type::Object | Type::ObjectSet => {
                                    unreachable!()
                                }
                            }
                        }
                    };

                    script.actions.0.push(action);
                }
            }
        }

        match bb.terminator() {
            Terminator::Goto(goto) => match goto {
                Goto::Block(id) => {
                    let id = self.block_ids[id];

                    script.actions.0.push(new_action(ActionType::RunFunction {
                        function: FunctionCall {
                            id,
                            parameters: MyVec(vec![]),
                        },
                    }));
                }
                Goto::Finish => {}
            },
            Terminator::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.codegen_expr(condition);

                let then_block = match then_block {
                    Goto::Block(then_block) => {
                        let id = self.block_ids[then_block];

                        vec![new_action(ActionType::RunFunction {
                            function: FunctionCall {
                                id,
                                parameters: MyVec(vec![]),
                            },
                        })]
                    }
                    Goto::Finish => vec![],
                };

                let else_block = match else_block {
                    Goto::Block(else_block) => {
                        let id = self.block_ids[else_block];

                        vec![new_action(ActionType::RunFunction {
                            function: FunctionCall {
                                id,
                                parameters: MyVec(vec![]),
                            },
                        })]
                    }
                    Goto::Finish => vec![],
                };

                script
                    .actions
                    .0
                    .push(new_action(ActionType::ConditionBlock {
                        if_actions: MyVec(then_block),
                        else_actions: MyVec(else_block),
                        condition: condition.0,
                    }));
            }
        }

        self.exolvl.level_data.nova_scripts.0.push(script);
    }

    fn codegen_expr(&mut self, expr: &TypedExpression) -> (NovaValue, Type) {
        (
            match &expr.expr {
                Expression::Variable(name) => {
                    new_novavalue(DynamicType::IntVariable, NewValue::Int(self.var_ids[name]))
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
                Expression::Colour { r, g, b, a } => new_novavalue(
                    DynamicType::ColorConstant,
                    NewValue::Color(Colour {
                        r: f32::from(*r) / 255.0,
                        g: f32::from(*g) / 255.0,
                        b: f32::from(*b) / 255.0,
                        a: f32::from(*a) / 255.0,
                    }),
                ),
                Expression::Vector { x, y } => {
                    let x = self.codegen_expr(x);
                    let y = self.codegen_expr(y);

                    new_novavalue(
                        DynamicType::VectorValues,
                        NewValue::SubValues(MyVec(vec![x.0, y.0])),
                    )
                }
                Expression::Object(object) => match object {
                    Object::Player => new_novavalue(
                        DynamicType::ObjectPlayer,
                        NewValue::SubValues(MyVec(vec![])),
                    ),
                },
                Expression::Operation(operation) => {
                    #[allow(clippy::match_same_arms)]
                    let dyn_type = match operation.as_ref() {
                        Operation::IntegerEquals(_, _) => DynamicType::BoolEqualNumber,
                        Operation::IntegerNotEquals(_, _) => DynamicType::BoolNotEqualNumber,
                        Operation::IntegerPlus(_, _) => DynamicType::IntAdd,
                        Operation::IntegerMinus(_, _) => DynamicType::IntSubtract,
                        Operation::IntegerMultiply(_, _) => DynamicType::IntMultiply,
                        Operation::IntegerDivide(_, _) => DynamicType::IntDivide,
                        Operation::IntegerGreaterThanEquals(_, _) => {
                            DynamicType::BoolGreaterOrEqual
                        }
                        Operation::IntegerLessThanEquals(_, _) => DynamicType::BoolLessOrEqual,
                        Operation::IntegerGreaterThan(_, _) => DynamicType::BoolGreater,
                        Operation::IntegerLessThan(_, _) => DynamicType::BoolLess,
                        Operation::FloatEquals(_, _) => DynamicType::BoolEqualNumber,
                        Operation::FloatNotEquals(_, _) => DynamicType::BoolNotEqualNumber,
                        Operation::FloatPlus(_, _) => DynamicType::FloatAdd,
                        Operation::FloatMinus(_, _) => DynamicType::FloatSubtract,
                        Operation::FloatMultiply(_, _) => DynamicType::FloatMultiply,
                        Operation::FloatDivide(_, _) => DynamicType::FloatDivide,
                        Operation::FloatGreaterThanEquals(_, _) => DynamicType::BoolGreaterOrEqual,
                        Operation::FloatLessThanEquals(_, _) => DynamicType::BoolLessOrEqual,
                        Operation::FloatGreaterThan(_, _) => DynamicType::BoolGreater,
                        Operation::FloatLessThan(_, _) => DynamicType::BoolLess,
                        Operation::BooleanEquals(_, _) => DynamicType::BoolEqualNumber,
                        Operation::BooleanNotEquals(_, _) => DynamicType::BoolNotEqualNumber,
                        Operation::IntegerNegate(_) => DynamicType::IntMultiply,
                        Operation::FloatNegate(_) => DynamicType::FloatMultiply,
                        Operation::BooleanNot(_) => DynamicType::BoolNot,
                    };

                    match operation.as_ref() {
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
                            let lhs = self.codegen_expr(lhs);
                            let rhs = self.codegen_expr(rhs);

                            new_novavalue(dyn_type, NewValue::SubValues(MyVec(vec![lhs.0, rhs.0])))
                        }
                        Operation::IntegerNegate(rhs) => {
                            let rhs = self.codegen_expr(rhs);

                            new_novavalue(
                                dyn_type,
                                NewValue::SubValues(MyVec(vec![
                                    rhs.0,
                                    new_novavalue(DynamicType::IntConstant, NewValue::Int(-1)),
                                ])),
                            )
                        }
                        Operation::FloatNegate(rhs) => {
                            let rhs = self.codegen_expr(rhs);

                            new_novavalue(
                                dyn_type,
                                NewValue::SubValues(MyVec(vec![
                                    rhs.0,
                                    new_novavalue(
                                        DynamicType::FloatConstant,
                                        NewValue::Float(-1.0),
                                    ),
                                ])),
                            )
                        }
                        Operation::BooleanNot(rhs) => {
                            let rhs = self.codegen_expr(rhs);

                            new_novavalue(dyn_type, NewValue::SubValues(MyVec(vec![rhs.0])))
                        }
                    }
                }
            },
            expr.ty,
        )
    }
}

struct IdGen {
    next_id: i32,
}

impl IdGen {
    const fn new() -> Self {
        Self { next_id: 0 }
    }

    fn next(&mut self) -> i32 {
        let id = self.next_id;

        self.next_id += 1;

        id
    }
}

const fn new_action(action_type: ActionType) -> Action {
    Action {
        closed: true,
        wait: true,
        action_type,
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
    String(MyString),
    Color(Colour),
    SubValues(MyVec<NovaValue>),
}
