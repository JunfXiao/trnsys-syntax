use super::{
    BitBool, Block, BlockKind, BlockParser, GlobalId, ParseContext, RawHeader, StrUnit, TryFromStr,
    TypedBlock, parse_block_comment, parse_derivatives, parse_equations, parse_etrace,
    parse_header, parse_inputs, parse_int, parse_parameters, parse_trace, parse_unit_format,
};
use crate::ast::*;
use crate::error::{ContentError, Error, RError, ReportWrapper};
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::tag;
use nom::character::complete::{multispace0, space0};
use nom::combinator::{complete, map};
use nom::multi::{many_m_n, many0, separated_list0};
use nom::sequence::delimited;
use std::rc::Rc;
use strum::IntoEnumIterator;

pub fn parse_commented_block<'a>(
    input: (&'a str, &mut ParseContext),
) -> IResult<&'a str, Block, RError> {
    let (input, context) = input;

    let (input, before) = parse_block_comment.parse(input)?;
    // Parse the block header
    let (input, mut block) = parse_block((input, context))?;
    // Parse the block comment after the header
    let (input, after) = parse_block_comment.parse(input)?;

    let comments = block.comments_mut();
    comments.comment_pre = before.map(|v| v.into_iter().map(Into::into).collect());
    comments.comment_post = after.map(|v| v.into_iter().map(Into::into).collect());

    Ok((input, block))
}

pub fn parse_block<'a>(
    input: (&'a str, &mut ParseContext),
) -> IResult<&'a str, Block, RError> {
    let (input, context) = input;
    let (input, block_header) = complete(parse_header).parse(input)?;
    let (input, block) = Block::try_parse(input, block_header, context)?;
    Ok((input, block))
}

impl<'a> BlockParser<'a> for Version {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader<'_>,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let version_str = raw_header.items[0].to_string();
        Ok((input, Version::from(version_str).into()))
    }
}

impl<'a> BlockParser<'a> for Simulation {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(3)?;
        let transformed = raw_header.to_vec::<f64>()?;
        let [t_0, t_f, dt] = transformed.try_into().unwrap();
        if t_0 < 0.0 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "t_0".to_string(),
                value: t_0.to_string(),
                reason: "t_0 must be greater than or equal to 0".to_string(),
            })
            .into());
        }

        if t_f <= t_0 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "t_f".to_string(),
                value: t_f.to_string(),
                reason: "t_f must be greater than t_0".to_string(),
            })
            .into());
        }

        if dt <= 0.0 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "dt".to_string(),
                value: dt.to_string(),
                reason: "dt must be greater than 0".to_string(),
            })
            .into());
        }

        let block = Simulation::new(t_0, t_f, dt);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Tolerances {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(2)?;
        let transformed = raw_header.to_vec::<f64>()?;
        let [tol_integration, tol_convergence] = transformed.try_into().unwrap();
        if tol_integration <= 0.0 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "sigma_D".to_string(),
                value: tol_integration.to_string(),
                reason: "Tolerance of the integration error must be greater than to 0".to_string(),
            })
            .into());
        }

        if tol_convergence <= 0.0 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "sigma_A".to_string(),
                value: tol_convergence.to_string(),
                reason: "Tolerance of the convergence error must be greater than 0".to_string(),
            })
            .into());
        }

        let block = Tolerances::new(tol_integration, tol_convergence);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Limits {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        context.ensure_unique(Self::block_kind())?;

        raw_header.ensure_len_between(2, 3)?;
        let transformed = raw_header.to_vec::<usize>()?;

        let mut limit_trace: Option<usize> = None;
        if transformed.len() > 2 {
            limit_trace = Some(transformed[2]);
        }
        let [max_iter, max_warning] = transformed.try_into().unwrap();

        if max_iter > 0 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "max_iter".to_string(),
                value: max_iter.to_string(),
                reason: "Maximum number of iterations must be greater than 0".to_string(),
            })
            .into());
        }

        if max_warning > 0 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "max_warning".to_string(),
                value: max_warning.to_string(),
                reason: "Maximum number of warnings must be greater than 0".to_string(),
            })
            .into());
        }

        if let Some(limit_trace) = limit_trace {
            if limit_trace > 0 {
                return Err(RError::new(ContentError::InvalidValue {
                    part: "limit_trace".to_string(),
                    value: limit_trace.to_string(),
                    reason: "Maximum number of traces must be greater than 0".to_string(),
                })
                .into());
            }
        }

        let block = Limits::new(max_iter, max_warning, limit_trace);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for NanCheck {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let [nan_check] = raw_header.to_vec::<BitBool>()?.try_into().unwrap();

        let block = NanCheck::new(nan_check.into());
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for OverwriteCheck {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let [overwrite_check] = raw_header.to_vec::<BitBool>()?.try_into().unwrap();

        let block = OverwriteCheck::new(overwrite_check.into());
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for TimeReport {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let [time_report] = raw_header.to_vec::<BitBool>()?.try_into().unwrap();
        let block = TimeReport::new(time_report.into());
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Constants {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        raw_header.ensure_len(1)?;
        let [num_constants] = raw_header.to_vec::<usize>()?.try_into().unwrap();

        let (input, expressions) = parse_equations(num_constants).parse(input)?;

        let mut block = Constants::default();
        block.definitions = expressions; // expressions(`'a`) → definitions(`'src`)

        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Equations {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        raw_header.ensure_len(1)?;
        let [num_equations] = raw_header.to_vec::<usize>()?.try_into().unwrap();
        let (input, expressions) = parse_equations(num_equations).parse(input)?;

        let mut block = Equations::default();
        block.definitions = expressions;

        for expr in block.definitions.iter() {
            // check if the names are unique and announce dependencies at the same time
            let id = GlobalId::Variable(expr.name.to_string());

            let (identifiers, unit_outputs) = expr.expr.dependencies();
            let mut dependencies = identifiers
                .iter()
                .map(|id| GlobalId::Variable(id.to_string()))
                .collect::<Vec<_>>();
            dependencies.extend(unit_outputs.iter().map(|id| GlobalId::Unit(id.unit)));

            context.register_dep(id, Some(dependencies))?;
        }

        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Accelerate {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // ACCELERATE n
        // u1,o1 u2,o2 ... ui,oi .... un,on

        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let [num_accelerate] = raw_header.to_vec::<usize>()?.try_into().unwrap();

        let (input, block_comment) = parse_block_comment.parse(input)?;

        let (input, outputs) = separated_list0(
            space0,
            map(
                delimited(space0, (parse_int, tag(","), parse_int), space0),
                move |input| {
                    let (u1, _, u2) = input;
                    UnitConnection::new(u1 as usize, u2 as usize)
                },
            ),
        )
        .parse(input)?;

        if outputs.len() != num_accelerate {
            return Err(RError::new(ContentError::InvalidValue {
                part: "num_accelerate".to_string(),
                value: num_accelerate.to_string(),
                reason: format!("Number of accelerations must be equal to the number of outputs. Expected {}, found {}", num_accelerate, outputs.len()),
            }).into());
        }

        let mut block = Commented::from(Accelerate::new(outputs));
        let pre_comments = block.comments.comment_pre.get_or_insert_default();
        if let Some(block_comment) = block_comment {
            pre_comments.extend(block_comment.into_iter().map(Into::into));
        }

        // register dependencies
        let global_ids = block
            .outputs
            .iter()
            .map(|unit_output| GlobalId::Unit(unit_output.unit))
            .collect::<Vec<_>>();

        context.register_dep(
            GlobalId::Block(Self::block_kind(), context.prev_blocks.len()),
            Some(global_ids),
        )?;

        Ok((input, block))
    }
}

impl<'a> BlockParser<'a> for Loop {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // It must be defined between the Simulation and End Block.
        let mut has_simulation: bool = false;
        let mut has_end: bool = false;
        let mut prev_loop_count = 0;
        let mut prev_loop_unit_count = 0;
        context.prev_blocks.iter().for_each(|block| {
            let block_ref = block.borrow();
            if block_ref.kind() == BlockKind::Simulation {
                has_simulation = true;
            } else if block_ref.kind() == BlockKind::End {
                has_end = true;
            } else if let Block::Loop(l) = &*block_ref {
                prev_loop_count += 1;
                prev_loop_unit_count += l.units.len();
            }
        });

        if !has_simulation {
            return Err(RError::new(ContentError::InvalidCondition {
                part: Self::block_kind().to_string(),
                expected: "'Simulation' block exists".to_string(),
                actual: "'Simulation' block not found before the block.".to_string(),
            })
            .into());
        }

        if has_end {
            return Err(RError::new(ContentError::InvalidCondition {
                part: Self::block_kind().to_string(),
                expected: "'End' block not exists".to_string(),
                actual: "'End' block found before the block.".to_string(),
            })
            .into());
        }

        // In TrnSys 18, up to 10 Loops, totally 250 units can be defined.
        if prev_loop_count >= 10 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "Loop".to_string(),
                value: 11.to_string(),
                reason: "In TrnSys 18, you may have maximal 10 'LOOP' statements.".to_string(),
            })
            .into());
        }

        // parse self to get the number of units
        // LOOP n REPEAT y
        raw_header.ensure_len(3)?;
        let num_units = usize::try_from_str(*raw_header.items[0]).map_err(|e| RError::from(e))?;

        if raw_header.items[1].to_string() != "REPEAT" {
            return Err(RError::new(ContentError::InvalidValue {
                part: Self::block_kind().to_string(),
                value: raw_header.items[1].to_string(),
                reason: "Expected 'LOOP n REPEAT y' format in LOOP block. However, a different TAG is provided other than 'REPEAT'.".to_string()
            }).into());
        }
        let num_repeats = usize::try_from_str(*raw_header.items[2]).map_err(|e| RError::from(e))?;

        if num_units + prev_loop_unit_count > 250 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "LOOP".to_string(),
                value: (num_units + prev_loop_unit_count).to_string(),
                reason: "In TrnSys 18, you may have maximal 250 units in total.".to_string(),
            })
            .into());
        }

        let (input, block_comment) = parse_block_comment.parse(input)?;

        // Parse num_units units, separated by space
        let (input, units) = delimited(
            multispace0,
            many_m_n(num_units, num_units, delimited(space0, parse_int, space0)),
            multispace0,
        )
        .parse(input)?;

        let units = units
            .into_iter()
            .map(|unit_output| unit_output as usize)
            .collect::<Vec<_>>();

        // register dependencies
        let global_ids = units
            .iter()
            .map(|unit| GlobalId::Unit(*unit as usize))
            .collect::<Vec<_>>();

        context.register_dep(
            GlobalId::Block(Self::block_kind(), context.prev_blocks.len()),
            Some(global_ids),
        )?;

        // append in-between comments
        let mut block = Commented::from(Loop::new(units, num_repeats));
        let pre_comments = block.comments.comment_pre.get_or_insert_default();
        if let Some(block_comment) = block_comment {
            pre_comments.extend(block_comment.into_iter().map(Into::into));
        }

        Ok((input, block))
    }
}

impl<'a> BlockParser<'a> for Dfq {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // DFQ k
        // k is 1, 2 or 3
        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let dfq = raw_header.to_vec::<u8>()?[0];
        let method = DfqMethod::from_repr(dfq).ok_or_else(|| {
            RError::new(ContentError::InvalidValue {
                part: "dfq".to_string(),
                value: dfq.to_string(),
                reason: format!(
                    "DFQ method must be one of the following values: {:?}",
                    DfqMethod::iter()
                        .map(|m| format!("{}", m))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            })
        })?;
        let block = Dfq::new(method);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for NoCheck {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // similar to accelerate
        // NOCHECK n
        // u1,i1 u2,i2 . . . ui,ii . . . un,in
        // n is the number of unit inputs
        // TRNSYS allows up to 20 different INPUTS to be removed from the list of INPUTS to be checked

        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let [num_nocheck] = raw_header.to_vec::<usize>()?.try_into().unwrap();
        let (input, block_comment) = parse_block_comment.parse(input)?;
        let (input, inputs) = separated_list0(
            space0,
            map(
                delimited(space0, (parse_int, tag(","), parse_int), space0),
                move |input| {
                    let (u1, _, u2) = input;
                    UnitConnection::new(u1 as usize, u2 as usize)
                },
            ),
        )
        .parse(input)?;

        if inputs.len() != num_nocheck {
            return Err(RError::new(ContentError::InvalidValue {
                part: "num_nocheck".to_string(),
                value: num_nocheck.to_string(),
                reason: format!("Number of no checks must be equal to the number of inputs. Expected {}, found {}", num_nocheck, inputs.len()),
            }).into());
        }

        let mut block = Commented::from(NoCheck::new(inputs));
        let pre_comments = block.comments.comment_pre.get_or_insert_default();
        if let Some(block_comment) = block_comment {
            pre_comments.extend(block_comment.into_iter().map(Into::into));
        }

        // register dependencies
        let global_ids = block
            .inputs
            .iter()
            .map(|unit_input| GlobalId::Unit(unit_input.unit))
            .collect::<Vec<_>>();

        context.register_dep(
            GlobalId::Block(Self::block_kind(), context.prev_blocks.len()),
            Some(global_ids),
        )?;

        Ok((input, block))
    }
}

impl<'a> BlockParser<'a> for EqSolver {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // EQSOLVER n
        // n: EqSolverMethod

        context.ensure_unique(Self::block_kind())?;
        raw_header.ensure_len(1)?;
        let eq_solver = raw_header.to_vec::<u8>()?[0];
        let method = EqSolverMethod::from_repr(eq_solver).ok_or_else(|| {
            RError::new(ContentError::InvalidValue {
                part: "eq_solver".to_string(),
                value: eq_solver.to_string(),
                reason: format!(
                    "EqSolver method must be one of the following values: {:?}",
                    EqSolverMethod::iter()
                        .map(|m| format!("{}", m))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            })
        })?;
        let block = EqSolver::new(method);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Solver {
    #[allow(deprecated)]
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // SOLVER SolverMethod [optional] RF_min [optional] RF_max

        context.ensure_unique(Self::block_kind())?;
        if raw_header.items.len() < 1 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "Solver".to_string(),
                value: 0.to_string(),
                reason: "At least one argument is required.".to_string(),
            })
            .into());
        }

        let solver_method: u8 =
            u8::try_from_str(&raw_header.items[0]).map_err(|e| ReportWrapper::<Error>::new(e))?;
        let method = SolverMethod::from_repr(solver_method).ok_or_else(|| {
            RError::new(ContentError::InvalidValue {
                part: "solver".to_string(),
                value: solver_method.to_string(),
                reason: format!(
                    "Solver method must be one of the following values: {:?}",
                    SolverMethod::iter()
                        .map(|m| format!("{}", m))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            })
        })?;

        let solver = match method {
            SolverMethod::Successive => {
                raw_header.ensure_len(3)?;
                let [_, rf_min, rf_max] = raw_header.to_vec::<f64>()?.try_into().unwrap();
                if rf_min < 0.0 || rf_min > 1.0 {
                    return Err(RError::new(ContentError::InvalidValue {
                        part: "rf_min".to_string(),
                        value: rf_min.to_string(),
                        reason: "RF_min must be between 0 and 1".to_string(),
                    })
                    .into());
                }

                if rf_max < 0.0 || rf_max > 1.0 {
                    return Err(RError::new(ContentError::InvalidValue {
                        part: "rf_max".to_string(),
                        value: rf_max.to_string(),
                        reason: "RF_max must be between 0 and 1".to_string(),
                    })
                    .into());
                }

                if rf_min > rf_max {
                    return Err(RError::new(ContentError::InvalidValue {
                        part: "rf_min".to_string(),
                        value: rf_min.to_string(),
                        reason: "RF_min must be less than RF_max".to_string(),
                    })
                    .into());
                }

                Solver::new(method, Some(rf_min), Some(rf_max))
            }

            SolverMethod::Powell => {
                raw_header.ensure_len(1)?;

                Solver::new(method, None, None)
            }
        };

        Ok((input, solver.into()))
    }
}

impl<'a> BlockParser<'a> for Assign {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // ASSIGN filename lu
        // lu is the logical unit number

        raw_header.ensure_len(2)?;
        let filename = raw_header.items[0].to_string();
        let lu: usize = usize::try_from_str(*raw_header.items[1])
            .map_err(|e| ReportWrapper::<Error>::new(e))?;

        context.register_dep(GlobalId::LogicalUnit(lu), None)?;

        let block = Assign::new(filename, lu);

        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Designate {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // DESIGNATE filename lu
        // lu is the logical unit number
        raw_header.ensure_len(2)?;
        let filename = raw_header.items[0].to_string();
        let lu: usize = usize::try_from_str(*raw_header.items[1])
            .map_err(|e| ReportWrapper::<Error>::new(e))?;
        context.register_dep(GlobalId::LogicalUnit(lu), None)?;
        let block = Designate::new(filename, lu);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Include {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // INCLUDE filename
        raw_header.ensure_len(1)?;
        let filename = raw_header.items[0].to_string();
        let block = Include::new(filename);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for Unit {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader<'a>,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // UNIT n TYPE m Comment
        // n is the unit number
        // m is the type number
        // Example: UNIT 6 TYPE 15 EXAMPLE COMPONENT

        if raw_header.items.len() < 3 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "Unit".to_string(),
                value: 0.to_string(),
                reason: "At least three arguments are required.".to_string(),
            })
            .into());
        }

        let unit_number: usize = usize::try_from_str(*raw_header.items[0])
            .map_err(|e| ReportWrapper::<Error>::new(e))?;
        let unit_type: usize = usize::try_from_str(*raw_header.items[2])
            .map_err(|e| ReportWrapper::<Error>::new(e))?;
        let unit_comment = raw_header.items[3..]
            .iter()
            .map(|s| s.0)
            .collect::<Vec<_>>()
            .join(" ");

        // parse all the possible blocks
        println!("Parsing UNIT block with number: {}, type: {}, comment: {}", unit_number, unit_type, unit_comment);

        let unit = Unit {
            number: unit_number as u32,
            type_number: unit_type as u32,
            unit_name: unit_comment.trim().to_string(),
            ..Unit::default()
        };

        let str_unit = StrUnit::new(input, unit);

        let (str_unit, _) = many0(complete(alt((
            parse_inputs,
            parse_derivatives,
            parse_trace,
            parse_etrace,
            parse_parameters,
            parse_unit_format,
        ))))
        .parse(str_unit)?;

        let StrUnit(input, unit) = str_unit;

        let (input, comments_post) = parse_block_comment(input)?;

        let unit = Rc::try_unwrap(unit)
            .and_then(|u| Ok(u.into_inner()))
            .unwrap_or_else(|e| e.borrow().clone());

        let mut block = Commented::new(unit, raw_header.comments);

        block.comments.comment_post =
            comments_post.map(|v| v.into_iter().map(Into::into).collect());

        Ok((input, block))
    }
}

impl<'a> BlockParser<'a> for Width {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // WIDTH n
        // 72 <= n <= 132

        raw_header.ensure_len(1)?;
        let width: usize = usize::try_from_str(*raw_header.items[0])
            .map_err(|e| ReportWrapper::<Error>::new(e))?;
        if width < 72 || width > 132 {
            return Err(RError::new(ContentError::InvalidValue {
                part: "width".to_string(),
                value: width.to_string(),
                reason: "Width must be between 72 and 132".to_string(),
            })
            .into());
        }
        let block = Width(width);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for NoList {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {

        raw_header.ensure_len(0)?;

        Ok((input, NoList().into()))
    }
}

impl<'a> BlockParser<'a> for List {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // LIST

        raw_header.ensure_len(0)?;

        Ok((input, List().into()))
    }
}

impl<'a> BlockParser<'a> for Map {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        raw_header.ensure_len(0)?;

        Ok((input, Map().into()))
    }
}

impl<'a> BlockParser<'a> for End {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        raw_header.ensure_len(0)?;
        let block = End {};
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for CSummarize {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // CSUMMARIZE EqnName "description"
        raw_header.ensure_len(2)?;
        let eqn_name = raw_header.items[0].to_string();
        let description = raw_header.items[1].to_string();
        let block = CSummarize::new(eqn_name, description);
        Ok((input, block.into()))
    }
}

impl<'a> BlockParser<'a> for ESummarize {
    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader,
        _context: &'b mut ParseContext,
    ) -> IResult<&'a str, Commented<Self>, RError> {
        // ESUMMARIZE EqnName "description"
        raw_header.ensure_len(2)?;
        let eqn_name = raw_header.items[0].to_string();
        let description = raw_header.items[1].to_string();
        let block = ESummarize::new(eqn_name, description);
        Ok((input, block.into()))
    }
}
