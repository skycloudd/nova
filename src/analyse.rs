use crate::{
    error::{Error, Warning},
    mir::{Expression, Function, Statement, TopLevel, TypedExpression},
    span::Spanned,
};

pub fn analyse(mir: &[Spanned<TopLevel>]) -> (Vec<Warning>, Vec<Error>) {
    let mut warnings = vec![];
    let mut errors = vec![];

    let mut analysis = Analysis::new(&mut warnings, &mut errors);

    analysis.analyse(mir);

    (warnings, errors)
}

struct Analysis<'warning, 'error> {
    warnings: &'warning mut Vec<Warning>,
    errors: &'error mut Vec<Error>,
}

impl<'warning, 'error> Analysis<'warning, 'error> {
    fn new(
        warnings: &'warning mut Vec<Warning>,
        errors: &'error mut Vec<Error>,
    ) -> Analysis<'warning, 'error> {
        Analysis { warnings, errors }
    }

    fn analyse(&mut self, mir: &[Spanned<TopLevel>]) {
        for top_level in mir {
            self.analyse_top_level(top_level);
        }
    }

    fn analyse_top_level(&mut self, top_level: &Spanned<TopLevel>) {
        match &top_level.0 {
            TopLevel::Function(function) => self.analyse_function(function),
        }
    }

    fn analyse_function(&mut self, function: &Function) {
        if function.params.0.len() > 10 {
            self.warnings.push(Warning::Lint {
                span: function.params.1,
                message: "function has too many parameters".to_string(),
                note: Some("consider refactoring into multiple functions".to_string()),
            });
        }

        let mut finished = Finished::No;

        for statement in &function.body.0 {
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
                span: function.body.1,
            });
        }
    }

    fn analyse_statement(&mut self, statement: &Spanned<Statement>) -> Finished {
        match &statement.0 {
            Statement::Expr(expr) => {
                if expression_is_trivially_pure(&expr.0.expr) {
                    self.warnings.push(Warning::Lint {
                        span: statement.1,
                        message: "this statement has no effect".to_string(),
                        note: None,
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
                        note: None,
                    });
                }

                let mut finished = Finished::No;

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

                finished
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if let Expression::Boolean(value) = condition.0.expr {
                    self.warnings.push(Warning::Lint {
                        span: condition.1,
                        message: format!("this `if` condition is always {value}"),
                        note: None,
                    });
                }

                self.analyse_expression(condition);

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
                        note: None,
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
            Statement::Let { name: _, value } => {
                self.analyse_expression(value);

                Finished::No
            }
            Statement::Assign { name: _, value } => {
                self.analyse_expression(value);

                Finished::No
            }
            Statement::Break | Statement::Continue => Finished::Loop,
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
            | Expression::Float(_)
            | Expression::String(_) => {}
            Expression::Unary { op: _, rhs } => {
                self.analyse_expression(&Spanned(*rhs.0.clone(), rhs.1));
            }
            Expression::Binary { lhs, op: _, rhs } => {
                self.analyse_expression(&Spanned(*lhs.0.clone(), lhs.1));
                self.analyse_expression(&Spanned(*rhs.0.clone(), rhs.1));
            }
            Expression::Convert { ty, expr } => {
                if &ty.0 == &expr.0.ty {
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
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Finished::Return, Finished::Return) => Some(std::cmp::Ordering::Equal),
            (Finished::Return, _) => Some(std::cmp::Ordering::Greater),
            (_, Finished::Return) => Some(std::cmp::Ordering::Less),
            (Finished::Loop, Finished::Loop) => Some(std::cmp::Ordering::Equal),
            (Finished::Loop, _) => Some(std::cmp::Ordering::Greater),
            (_, Finished::Loop) => Some(std::cmp::Ordering::Less),
            (Finished::No, Finished::No) => Some(std::cmp::Ordering::Equal),
        }
    }
}

impl Ord for Finished {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn expression_is_trivially_pure(expr: &Expression) -> bool {
    match expr {
        Expression::Error => false,
        Expression::Variable(_)
        | Expression::Boolean(_)
        | Expression::Integer(_)
        | Expression::Float(_)
        | Expression::String(_) => true,
        Expression::Unary { op: _, rhs } => expression_is_trivially_pure(&rhs.0.expr),
        Expression::Binary { lhs, op: _, rhs } => {
            expression_is_trivially_pure(&lhs.0.expr) && expression_is_trivially_pure(&rhs.0.expr)
        }
        Expression::Convert { ty: _, expr } => expression_is_trivially_pure(&expr.0.expr),
        Expression::Call { func: _, args: _ } => false,
    }
}
