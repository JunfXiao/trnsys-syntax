use crate::ast::{Commented, Expr, Format, Trace, Unit, UnitInput};
use crate::error::{ContentError, Error, RError};
use crate::parse::{
    map_report, parse_header_of_kind,
    parse_identifier, parse_literal, parse_mixed_param, parse_unconnected,
    parse_unit_output,
};
use derive_more::Display;
use nom::branch::alt;
use nom::bytes::complete::take_till;
use nom::bytes::{tag_no_case, take_until};
use nom::character::complete::multispace0;
use nom::multi::many_m_n;
use nom::sequence::preceded;
use nom::{IResult, Input, Needed, Parser};
use phf::{phf_set, Set};
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

#[derive(Debug, Display, Clone)]
#[display("Unit {} of Type {}", self.1.as_ref().borrow().number, self.1.as_ref().borrow().type_number)]
pub struct StrUnit<'a>(pub &'a str, pub Rc<RefCell<Unit>>);

impl<'a> StrUnit<'a> {
    pub fn new(input: &'a str, unit: Unit) -> Self {
        Self(input, Rc::new(RefCell::new(unit)))
    }

    pub fn unit(&self) -> Ref<Unit> {
        self.1.as_ref().borrow()
    }

    pub fn unit_mut(&self) -> RefMut<Unit> {
        self.1.as_ref().borrow_mut()
    }
}

impl<'a> nom::Input for StrUnit<'a> {
    type Item = <&'a str as Input>::Item;
    type Iter = <&'a str as Input>::Iter;
    type IterIndices = <&'a str as Input>::IterIndices;

    fn input_len(&self) -> usize {
        self.0.input_len()
    }

    fn take(&self, index: usize) -> Self {
        Self(self.0.take(index), self.1.clone())
    }

    fn take_from(&self, index: usize) -> Self {
        Self(self.0.take_from(index), self.1.clone())
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        let (left, right) = self.0.take_split(index);
        (Self(left, self.1.clone()), Self(right, self.1.clone()))
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.position(predicate)
    }

    fn iter_elements(&self) -> Self::Iter {
        self.0.iter_elements()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.0.iter_indices()
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.0.slice_index(count)
    }
}

pub fn parse_parameters(input_unit: StrUnit) -> IResult<StrUnit, (), RError> {
    let mut unit = input_unit.1.as_ref().borrow_mut();
    let input = input_unit.0;
    if unit.parameters.is_some() {
        return Err(nom::Err::Error(RError::new(
            ContentError::DuplicateDefinition {
                name: "Parameters".to_string(),
            },
        )));
    }
    let (input, header) = parse_header_of_kind(Some(tag_no_case("Parameters")), Some(1))
        .parse(input)
        .map_err(|e| e.map(|e| e.attach_printable("Parsing parameters header")))?;
    if header.items.len() != 1 {
        return Err(nom::Err::Failure(RError::new(
            ContentError::ArgumentCount {
                expected: "1".to_string(),
                actual: header.items.len(),
                part: "Parameters".to_string(),
            },
        )));
    }

    let param_num: usize = header.to_vec()?[0];

    let param_parser = many_m_n(
        param_num,
        param_num,
        preceded(
            multispace0,
            parse_mixed_param(alt((parse_literal, parse_identifier)), None),
        ),
    );

    let (input, params) = map_report(param_parser, |err| {
        err.change_context(
            ContentError::InvalidValue {
                value: "Parameters".to_string(),
                reason: format!(
                    "{} parameters expected, but couldn't parse such many.",
                    param_num
                ),
                part: "Parameters".to_string(),
            }
            .into(),
        )
    })
    .parse(input)?;

    unit.parameters = Some(params);

    Ok((StrUnit(input, input_unit.1.clone()), ()))
}

pub static TYPE_WITH_LABELS: Set<u32> = phf_set! {25u32,26u32,27u32,46u32,65u32};

pub fn parse_inputs(input_unit: StrUnit) -> IResult<StrUnit, (), RError> {
    let mut unit = input_unit.unit_mut();
    let input = input_unit.0;

    if unit.inputs.is_some() {
        return Err(nom::Err::Error(RError::new(
            ContentError::DuplicateDefinition {
                name: "Inputs".to_string(),
            },
        )));
    }

    let (input, header) =
        parse_header_of_kind(Some(tag_no_case("Inputs")), Some(1)).parse(input)?;

    let param_num: usize = header.to_vec()?[0];

    let inputs_parser = many_m_n(
        param_num,
        param_num,
        preceded(
            multispace0,
            parse_mixed_param(
                alt((
                    // First consider "0,0"
                    parse_unconnected,
                    // Then consider "1,2"
                    parse_unit_output,
                    // then consider "1" before ",2"
                    parse_literal,
                    // Finally consider identifiers
                    parse_identifier,
                )),
                None,
            ),
        ),
    );

    let (input, input_conns) = map_report(inputs_parser, |err| {
        err.change_context(
            ContentError::InvalidValue {
                value: "Inputs".to_string(),
                reason: format!(
                    "{} inputs expected, but couldn't parse such many.",
                    param_num
                ),
                part: "Inputs".to_string(),
            }
            .into(),
        )
    })
    .parse(input)?;


    // Parse initial values

    let (input, init_values) = parse_input_init_values(param_num).parse(input)?;
    unit.inputs = Some(input_conns
        .into_iter()
        .zip(init_values)
        .map(|(conn, label)| UnitInput::new(conn, label))
        .collect::<Vec<_>>());

    Ok((StrUnit(input, input_unit.1.clone()), ()))
}

pub fn parse_labels(input_unit: StrUnit) -> IResult<StrUnit, (), RError> {
    let mut unit = input_unit.unit_mut();
    let input = input_unit.0;

    if unit.labels.is_some() {
        return Err(nom::Err::Error(RError::new(
            ContentError::DuplicateDefinition {
                name: "Labels".to_string(),
            },
        )));
    }

    let (input, header) =
        parse_header_of_kind(Some(tag_no_case("Labels")), Some(1)).parse(input)?;

    let num: usize = header.to_vec()?[0];

    let labels_parser = many_m_n(
        num,
        num,
        preceded(
            multispace0,
            parse_mixed_param(
                take_till(|c| c == '\0'),
                None,
            ),
        ),
    );
    let (input, labels) = map_report(labels_parser, |e: RError| {
        e.change_context(
            ContentError::InvalidValue {
                value: "Labels".to_string(),
                reason: format!("{} labels expected, but couldn't parse such many.", num),
                part: "Inputs".to_string(),
            }
                .into(),
        )
    })
        .parse(input)?;

    unit.labels = Some(labels.into_iter().map(|s|Commented::new(s.value.to_string(), s.comments)).collect());

    Ok((StrUnit(input, input_unit.1.clone()), ()))
}


fn parse_input_init_values<'a>(
    num: usize,
) -> impl Parser<&'a str, Output = Vec<Commented<Expr>>, Error = RError> {
    move |input: &'a str| {
        let parser = many_m_n(
            num,
            num,
            preceded(
                multispace0,
                parse_mixed_param(
                    alt((parse_literal, parse_unconnected, parse_identifier)),
                    None,
                ),
            ),
        );
        let (input, init_values) = map_report(parser, |err| {
            err.change_context(
                ContentError::InvalidValue {
                    value: "Initial values".to_string(),
                    reason: format!(
                        "{} initial values expected, but couldn't find such many.",
                        num
                    ),
                    part: "Inputs".to_string(),
                }
                .into(),
            )
        })
        .parse(input)?;

        Ok((input, init_values))
    }
}

pub fn parse_derivatives(input_unit: StrUnit) -> IResult<StrUnit, (), RError> {
    let mut unit = input_unit.unit_mut();
    let input = input_unit.0;

    if unit.derivatives.is_some() {
        return Err(nom::Err::Error(RError::new(
            ContentError::DuplicateDefinition {
                name: "Derivatives".to_string(),
            },
        )));
    }

    let (input, header) =
        parse_header_of_kind(Some(tag_no_case("Derivatives")), Some(1)).parse(input)?;

    let param_num: usize = header.to_vec()?[0];

    let param_parser = many_m_n(
        param_num,
        param_num,
        preceded(
            multispace0,
            parse_mixed_param(parse_literal, Some(", \t\n\r!")),
        ),
    );

    let (input, derivatives) = map_report(param_parser, |err| {
        err.change_context(
            ContentError::InvalidValue {
                value: "Derivatives".to_string(),
                reason: format!(
                    "{} derivatives expected, but couldn't find such many.",
                    param_num
                ),
                part: "Derivatives".to_string(),
            }
            .into(),
        )
    })
    .parse(input)?;

    unit.derivatives = Some(derivatives);

    Ok((StrUnit(input, input_unit.1.clone()), ()))
}

/// Parse the trace block inside an unit.
/// Format:
pub fn parse_trace<'a>(input_unit: StrUnit) -> IResult<StrUnit, (), RError> {
    let mut unit = input_unit.unit_mut();
    let input = input_unit.0;

    if unit.trace.is_some() {
        return Err(nom::Err::Error(RError::new(
            ContentError::DuplicateDefinition {
                name: "Trace".to_string(),
            },
        )));
    }

    let (input, header) = parse_header_of_kind(Some(tag_no_case("Trace")), Some(2)).parse(input)?;

    let [start, end] = header.to_vec::<f64>()?.try_into().map_err(|_| {
        nom::Err::Failure(
            Error::ConversionError {
                input: "Trace".to_string(),
                target: "float number".to_string(),
            }
            .into(),
        )
    })?;

    unit.trace = Some(Commented::new(Trace::new(start, end), header.comments));

    Ok((StrUnit(input, input_unit.1.clone()), ()))
}

pub fn parse_etrace<'a>(input_unit: StrUnit) -> IResult<StrUnit, (), RError> {
    let mut unit = input_unit.unit_mut();
    let input = input_unit.0;

    if unit.etrace.is_some() {
        return Err(nom::Err::Error(RError::new(
            ContentError::DuplicateDefinition {
                name: "ETrace".to_string(),
            },
        )));
    }

    let (input, header) =
        parse_header_of_kind(Some(tag_no_case("ETrace")), Some(2)).parse(input)?;


    let [start, end] = header.to_vec::<f64>()?.try_into().map_err(|_| {
        nom::Err::Failure(
            Error::ConversionError {
                input: "ETrace".to_string(),
                target: "float number".to_string(),
            }
            .into(),
        )
    })?;

    unit.etrace = Some(Commented::new(Trace::new(start, end), header.comments));

    Ok((StrUnit(input, input_unit.1.clone()), ()))
}

/// Parse the format block inside an unit.
/// Example:
/// ```txt
/// FORMAT (1(F10.2,1X),22(F10.2,1X))
/// ```
pub fn parse_unit_format(input_unit: StrUnit) -> IResult<StrUnit, (), RError> {
    let mut unit = input_unit.unit_mut();
    let input = input_unit.0;

    if unit.format.is_some() {
        return Err(nom::Err::Error(RError::new(
            ContentError::DuplicateDefinition {
                name: "Format".to_string(),
            },
        )));
    }

    let (mut input, header) =
        parse_header_of_kind(Some(tag_no_case("Format")), None).parse(input)?;
    let format: String;
    if header.items.len() > 0 {
        format = header.to_vec::<String>()?.join("").trim().to_string();
    } else {
        // parse the next none-empty line
        let (rest, format_line) = preceded(multispace0, take_until("\n")).parse(input)?;
        format = format_line.trim().to_string();
        input = rest;
    }
    unit.format = Some(Commented::new(Format::new(format), header.comments));

    Ok((StrUnit(input, input_unit.1.clone()), ()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Unit, UnitConnection};
    use crate::parse::{parse_commented_block, Block, DocContext};

    #[test]
    fn test_parse_parameters() -> Result<(), RError> {
        let input = "\
! Pre comment 1
* Pre comment 2
    PaRaMETers 3
! Inline comment
1.0 2.0 ! first 2 params
! Third param
3.0
";
        let unit = Unit::default();
        let input_unit = StrUnit::new(input, unit);
        let (input_unit, _) = parse_parameters(input_unit)?;
        let input = input_unit.0;
        let mut unit = input_unit.unit_mut();

        assert!(
            input.trim().is_empty(),
            "Input not fully consumed: `{}`",
            input
        );
        let params = &mut unit.parameters;
        assert!(params.is_some());
        let params = params.as_mut().unwrap();
        assert_eq!(params.len(), 3);
        assert_eq!(params[0].value, Expr::Literal(1.0));
        assert_eq!(params[1].value, Expr::Literal(2.0));
        assert_eq!(params[2].value, Expr::Literal(3.0));
        Ok(())
    }

    #[test]
    fn test_parse_inputs_with_init_val() -> Result<(), RError> {
        let input = "
        ! Pre comment 1
        INPUTS 3
        ! Inline comment
        1,2 0,0 STOP
        *** Initial Values
        1.0 2.0
        ! Comment XXX
        START !?!
        ";

        let unit = Unit::default();
        let input_unit = StrUnit::new(input, unit);
        let (input_unit, _) = parse_inputs(input_unit)?;
        let input = input_unit.0;
        let mut unit = input_unit.unit_mut();

        assert!(
            input.trim().is_empty(),
            "Input not fully consumed: `{}`",
            input
        );
        let inputs = &mut unit.inputs;
        assert!(inputs.is_some());
        let inputs = inputs
            .as_mut()
            .unwrap()
            .clone();

        assert_eq!(inputs.len(), 3);
        assert_eq!(
            inputs[0].connection.value,
            Expr::UnitOutput(UnitConnection::new(1, 2))
        );
        assert_eq!(inputs[1].connection.value, Expr::Unconnected);
        assert_eq!(
            inputs[2].connection.value,
            Expr::Identifier("STOP".to_string())
        );

        assert_eq!(inputs[0].initial.value, Expr::Literal(1.0));
        assert_eq!(inputs[1].initial.value, Expr::Literal(2.0));
        assert_eq!(
            inputs[2].initial.value,
            Expr::Identifier("START".to_string())
        );

        Ok(())
    }

    #[test]
    fn test_parse_derivatives() -> Result<(), RError> {
        let input = "\
! Pre comment 1
Derivatives 3
! Inline comment
1.0 2.0 ! first 2 derivatives
3.0
! Third derivative
";
        let unit = Unit::default();
        let input_unit = StrUnit::new(input, unit);
        let (input_unit, _) = parse_derivatives(input_unit)?;
        let input = input_unit.0;
        let mut unit = input_unit.unit_mut();

        assert!(
            input.trim().is_empty(),
            "Input not fully consumed: `{}`",
            input
        );
        let derivatives = &mut unit.derivatives;
        assert!(derivatives.is_some());
        let derivatives = derivatives.as_mut().unwrap();
        assert_eq!(derivatives.len(), 3);
        assert_eq!(derivatives[0].value, Expr::Literal(1.0));
        assert_eq!(derivatives[1].value, Expr::Literal(2.0));
        assert_eq!(derivatives[2].value, Expr::Literal(3.0));
        Ok(())
    }


    #[test]
    fn test_parse_input_labels() -> Result<(), RError> {
        let input = r#"
        LABELS 3
        *** Labels for printers/plotters
        Label1 "Label 2" ! Comment for Label2
        Label3 ! Comment for Label3
        "#;

        let unit = Unit::default();
        let input_unit = StrUnit::new(input, unit);
        let (input_unit, _) = parse_labels(input_unit)?;
        let input = input_unit.0;
        let mut unit = input_unit.unit_mut();
        let labels = unit.labels.as_ref().expect("Expected labels");
        println!("{:?}", labels);
        assert!(
            input.trim().is_empty(),
            "Input not fully consumed: `{}`",
            input
        );
        assert_eq!(labels.len(), 3);
        assert_eq!(labels[0].value, "Label1");
        assert_eq!(labels[1].value, "Label 2");
        assert_eq!(labels[2].value, "Label3");
        Ok(())
    }
    #[test]
    fn test_parse_trace() -> Result<(), RError> {
        let input = "\
! Pre comment 1
Trace 0.0 10.0
! Inline comment
";
        let unit = Unit::default();
        let input_unit = StrUnit::new(input, unit);
        let (input_unit, _) = parse_trace(input_unit)?;
        let input = input_unit.0;
        let unit = input_unit.unit_mut();

        assert!(
            input.trim().is_empty(),
            "Input not fully consumed: `{}`",
            input
        );
        assert!(unit.trace.is_some());
        let trace = unit.trace.as_ref().unwrap();
        assert_eq!(trace.start_time, 0.0);
        assert_eq!(trace.stop_time, 10.0);
        Ok(())
    }

    #[test]
    fn test_parse_format() -> Result<(), RError> {
        let input = "\
! Pre comment 1
Format \t(1(F10.2 , 1X),\t22(F10.2,1X))
        ! Inline comment
";
        let unit = Unit::default();
        let input_unit = StrUnit::new(input, unit);
        let (input_unit, _) = parse_unit_format(input_unit)?;
        let input = input_unit.0;
        let unit = input_unit.unit_mut();

        assert!(
            input.trim().is_empty(),
            "Input not fully consumed: `{}`",
            input
        );
        assert!(unit.format.is_some());
        let format = unit.format.as_ref().unwrap();
        assert_eq!(format.format_string, "(1(F10.2,1X),22(F10.2,1X))");
        Ok(())
    }

    #[test]
    fn test_parse_unit() -> Result<(), RError> {
        let input = r#"
*------------------------------------------------------------------------------


* Model "Type55" (Type 55)
* 

UNIT 3 TYPE 55	 Type55
*$UNIT_NAME Type55
*$MODEL .\Utility\Integrators\Periodic Integrator\Type55.tmf
*$POSITION 387 159
*$LAYER Main # 
PARAMETERS 7
1		! 1 Integrate or sum input
1.0		! 2 Relative starting hour for input
1.0		! 3 Duration for input
24.0		! 4 Cycle repeat time for input
-1		! 5 Reset time for input
0		! 6 Absolute starting hour for input
8760		! 7 Absolute stopping hour for input 
INPUTS 1
3,1		! [unconnected] Input
*** INITIAL INPUT VALUES
0. 
LABELS 1
"!AB*CD"	! Label for input
! POST COMMENTS
*------------------------------------------------------------------------------
        "#;
        let mut context = DocContext::new();
        let (input, block) = parse_commented_block((input, &mut context))?;
        assert!(input.trim().is_empty(), 
            "Input not fully consumed: `{}`", input);
        // to Block::Unit(Unit) Struct
        let Block::Unit(unit) = block else {
            panic!("Expected a Unit block, found: {:?}", block);
        };
        assert_eq!(unit.number, 3);
        assert_eq!(unit.type_number, 55);
        assert_eq!(unit.unit_name, "Type55");
        assert_eq!(unit.inputs.as_ref().unwrap().len(), 1);
        assert_eq!(unit.inputs.as_ref().unwrap()[0].connection.value, Expr::UnitOutput(UnitConnection::new(3, 1)));
        assert_eq!(unit.labels.as_ref().unwrap().len(), 1);
        assert_eq!(unit.labels.as_ref().unwrap()[0].value, "!AB*CD");
        
        
        println!("{:#?}", unit);
        Ok(())
    }
}
