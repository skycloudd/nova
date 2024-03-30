use crate::{
    error::{Error, Warning},
    mir::{Expression, Function, Mir, Statement, TopLevel, TypedExpression},
    span::Spanned,
};

pub fn analyse(mir: &Mir) -> (Vec<Warning>, Vec<Error>) {
    let mut warnings = vec![];
    let mut errors = vec![];

    let mut analysis = Analysis::new(&mut warnings, &mut errors);

    analysis.analyse(mir);

    (warnings, errors)
}

struct Analysis<'warning, 'error> {
    warnings: &'warning mut Vec<Warning>,
    errors: &'error mut Vec<Error>,

    loop_depth: usize,
}

impl<'warning, 'error> Analysis<'warning, 'error> {
    fn new(
        warnings: &'warning mut Vec<Warning>,
        errors: &'error mut Vec<Error>,
    ) -> Analysis<'warning, 'error> {
        Analysis {
            warnings,
            errors,
            loop_depth: 0,
        }
    }

    fn analyse(&mut self, mir: &Mir) {
        for top_level in &mir.top_levels {
            self.analyse_top_level(top_level);
        }
    }

    fn analyse_top_level(&mut self, top_level: &Spanned<TopLevel>) {
        match &top_level.0 {
            TopLevel::Function(function) => self.analyse_function(function),
        }
    }

    fn analyse_function(&mut self, function: &Spanned<Function>) {
        if function.0.params.0.len() > 10 {
            self.warnings.push(Warning::Lint {
                span: function.0.params.1,
                message: "function has too many parameters".to_string(),
                note: Some("consider refactoring into multiple functions".to_string()),
            });
        }

        let mut finished = Finished::No;

        for statement in &function.0.body.0 {
            let res = self.analyse_statement(statement);

            if finished < Finished::Return && res == Finished::Return {
                finished = Finished::Return;
            } else if finished == Finished::Return {
                self.warnings.push(Warning::Lint {
                    span: statement.1,
                    message: "this statement is unreachable".to_string(),
                    note: None,
                });
            }
        }

        if finished != Finished::Return {
            self.errors.push(Error::MissingReturn {
                span: function.0.body.1,
            });
        }
    }

    #[allow(clippy::too_many_lines)]
    fn analyse_statement(&mut self, statement: &Spanned<Statement>) -> Finished {
        match &statement.0 {
            Statement::Error => Finished::No,
            Statement::Expr(expr) => {
                if expression_is_trivially_pure(&expr.0.expr) {
                    self.warnings.push(Warning::Lint {
                        span: statement.1,
                        message: "this statement has no effect".to_string(),
                        note: Some("removing it won't change the program's behaviour".to_string()),
                    });
                }

                self.analyse_expression(expr);

                Finished::No
            }
            Statement::Block(stmts) => {
                let mut finished = Finished::No;

                for stmt in &stmts.0 {
                    let res = self.analyse_statement(stmt);

                    if finished == Finished::No && res > Finished::No {
                        finished = res;
                    } else if finished != Finished::No {
                        self.warnings.push(Warning::Lint {
                            span: stmt.1,
                            message: "this statement is unreachable".to_string(),
                            note: None,
                        });
                    }
                }

                finished
            }
            #[allow(clippy::arithmetic_side_effects)]
            Statement::Loop(stmts) => {
                if stmts.0.len() == 1
                    && matches!(
                        stmts.0.first().unwrap().0,
                        Statement::Break | Statement::Continue
                    )
                {
                    self.warnings.push(Warning::Lint {
                        span: statement.1,
                        message: "this loop has no effect".to_string(),
                        note: Some("removing it won't change the program's behaviour".to_string()),
                    });
                }

                let mut finished = Finished::No;

                self.loop_depth += 1;

                for stmt in &stmts.0 {
                    let res = self.analyse_statement(stmt);

                    if finished == Finished::No && res > Finished::No {
                        finished = res;
                    } else if finished > Finished::No {
                        self.warnings.push(Warning::Lint {
                            span: stmt.1,
                            message: "this statement is unreachable".to_string(),
                            note: None,
                        });
                    }
                }

                self.loop_depth -= 1;

                finished
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.analyse_expression(condition);

                if let ExpressionTruthness::Known(known) =
                    check_expression_thruthness(&condition.0.expr)
                {
                    self.warnings.push(Warning::Lint {
                        span: condition.1,
                        message: format!("this `if` condition is always {known}"),
                        note: None,
                    });
                }

                let mut if_branch_finished = Finished::No;

                for stmt in &then_branch.0 {
                    let res = self.analyse_statement(stmt);

                    if if_branch_finished == Finished::No && res > Finished::No {
                        if_branch_finished = res;
                    } else if if_branch_finished > Finished::No {
                        self.warnings.push(Warning::Lint {
                            span: stmt.1,
                            message: "this statement is unreachable".to_string(),
                            note: None,
                        });
                    }
                }

                let mut else_branch_finished = Finished::No;

                if let Some(else_branch) = else_branch {
                    for stmt in &else_branch.0 {
                        let res = self.analyse_statement(stmt);

                        if else_branch_finished == Finished::No && res > Finished::No {
                            else_branch_finished = res;
                        } else if else_branch_finished > Finished::No {
                            self.warnings.push(Warning::Lint {
                                span: stmt.1,
                                message: "this statement is unreachable".to_string(),
                                note: None,
                            });
                        }
                    }
                }

                if_branch_finished.min(else_branch_finished)
            }
            Statement::For {
                name: _,
                start,
                end,
                inclusive,
                body,
            } => {
                self.analyse_expression(start);
                self.analyse_expression(end);

                if (expression_is_trivially_pure(&start.0.expr)
                    && expression_is_trivially_pure(&end.0.expr))
                    && (start.0.expr == end.0.expr)
                    && !inclusive
                {
                    self.warnings.push(Warning::Lint {
                        span: start.1,
                        message: "this loop will never execute and has no effect".to_string(),
                        note: Some("removing it won't change the program's behaviour".to_string()),
                    });
                }

                let mut finished = Finished::No;

                for stmt in &body.0 {
                    let res = self.analyse_statement(stmt);

                    if finished == Finished::No && res > Finished::No {
                        finished = res;
                    } else if finished > Finished::No {
                        self.warnings.push(Warning::Lint {
                            span: stmt.1,
                            message: "this statement is unreachable".to_string(),
                            note: None,
                        });
                    }
                }

                finished
            }
            Statement::Let { name: _, value } | Statement::Assign { name: _, value } => {
                self.analyse_expression(value);

                Finished::No
            }
            Statement::Break | Statement::Continue => {
                if self.loop_depth == 0 {
                    self.errors
                        .push(Error::BreakOrContinueOutsideLoop { span: statement.1 });
                }

                Finished::Loop
            }
            Statement::Return(expr) => {
                self.analyse_expression(expr);

                Finished::Return
            }
        }
    }

    fn analyse_expression(&mut self, expr: &Spanned<TypedExpression>) {
        match &expr.0.expr {
            Expression::Error
            | Expression::Variable(_)
            | Expression::Boolean(_)
            | Expression::Integer(_)
            | Expression::Float(_) => {}
            Expression::Unary { op: _, rhs } => {
                self.analyse_expression(&Spanned(*rhs.0.clone(), rhs.1));
            }
            Expression::Binary { lhs, op: _, rhs } => {
                self.analyse_expression(&Spanned(*lhs.0.clone(), lhs.1));
                self.analyse_expression(&Spanned(*rhs.0.clone(), rhs.1));
            }
            Expression::Convert { ty, expr } => {
                if ty.0 == expr.0.ty {
                    self.warnings.push(Warning::Lint {
                        span: expr.1,
                        message: "this conversion has no effect".to_string(),
                        note: Some(
                            "consider replacing it with just the inner expression".to_string(),
                        ),
                    });
                }

                self.analyse_expression(&Spanned(*expr.0.clone(), expr.1));
            }
            Expression::Call { func: _, args } => {
                for arg in &args.0 {
                    self.analyse_expression(arg);
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Finished {
    Return,
    Loop,
    No,
}

impl PartialOrd for Finished {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Finished {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        #[allow(clippy::match_same_arms)]
        match (self, other) {
            (Self::Return, Self::Return) => core::cmp::Ordering::Equal,
            (Self::Return, _) => core::cmp::Ordering::Greater,
            (_, Self::Return) => core::cmp::Ordering::Less,
            (Self::Loop, Self::Loop) => core::cmp::Ordering::Equal,
            (Self::Loop, _) => core::cmp::Ordering::Greater,
            (_, Self::Loop) => core::cmp::Ordering::Less,
            (Self::No, Self::No) => core::cmp::Ordering::Equal,
        }
    }
}

fn expression_is_trivially_pure(expr: &Expression) -> bool {
    #[allow(clippy::match_same_arms)]
    match expr {
        Expression::Error => false,
        Expression::Variable(_)
        | Expression::Boolean(_)
        | Expression::Integer(_)
        | Expression::Float(_) => true,
        Expression::Unary { op: _, rhs } => expression_is_trivially_pure(&rhs.0.expr),
        Expression::Binary { lhs, op: _, rhs } => {
            expression_is_trivially_pure(&lhs.0.expr) && expression_is_trivially_pure(&rhs.0.expr)
        }
        Expression::Convert { ty: _, expr } => expression_is_trivially_pure(&expr.0.expr),
        Expression::Call { func: _, args: _ } => false,
    }
}

#[derive(Debug)]
enum ExpressionTruthness {
    Known(bool),
    Unknown,
}

const fn check_expression_thruthness(expr: &Expression) -> ExpressionTruthness {
    match expr {
        Expression::Boolean(bool) => ExpressionTruthness::Known(*bool),
        _ => ExpressionTruthness::Unknown,
    }
}
