use crate::ast::*;
use crate::error::RError;
use crate::parse::{BlockKind, TypedBlock};
use crate::serialize::DeckWrite;
use std::fmt::Write;

impl DeckWrite for Version {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} {}", Self::block_kind(), self.version)?;
        Ok(())
    }
}

impl DeckWrite for Simulation {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} {} {} {}",
            Self::block_kind(),
            self.start_time,
            self.stop_time,
            self.time_step
        )?;
        Ok(())
    }
}

impl DeckWrite for Tolerances {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} {} {}",
            Self::block_kind(),
            self.integration_tolerance,
            self.convergence_tolerance
        )
        .map_err(|e| RError::new(e))?;
        Ok(())
    }
}

impl DeckWrite for Limits {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} ", Self::block_kind())?;
        self.max_iterations.write_to(writer, Self::block_kind())?;
        write!(writer, " ")?;
        self.max_warnings.write_to(writer, Self::block_kind())?;
        if let Some(trace_limit) = &self.trace_limit {
            write!(writer, " ")?;
            trace_limit.write_to(writer, Self::block_kind())?;
        }
        Ok(())
    }
}

impl DeckWrite for UnitConnection {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{},{}", self.unit, self.index)?;
        Ok(())
    }
}

impl DeckWrite for NanCheck {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} {}",
            Self::block_kind(),
            if self.enabled { 1 } else { 0 }
        )?;
        Ok(())
    }
}

impl DeckWrite for OverwriteCheck {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} {}",
            Self::block_kind(),
            if self.enabled { 1 } else { 0 }
        )?;
        Ok(())
    }
}

impl DeckWrite for TimeReport {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} {}",
            Self::block_kind(),
            if self.enabled { 1 } else { 0 }
        )?;
        Ok(())
    }
}

impl DeckWrite for EquationDef {
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        write!(writer, " {} = ", self.name)?;
        self.expr.write_to(writer, kind)?;
        Ok(())
    }
}

impl DeckWrite for Constants {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        writeln!(writer, "{} {}", Self::block_kind(), self.definitions.len())?;
        for def in &self.definitions {
            def.write_to(writer, Self::block_kind())?;
        }

        Ok(())
    }
}

impl DeckWrite for Equations {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        writeln!(writer, "{} {}", Self::block_kind(), self.definitions.len())?;
        for def in &self.definitions {
            def.write_to(writer, Self::block_kind())?;
            if def.comments.is_empty() {
                writeln!(writer)?;
            }
        }
        Ok(())
    }
}

impl DeckWrite for Accelerate {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} {}", Self::block_kind(), self.outputs.len())?;
        for output in &self.outputs {
            write!(writer, " ")?;
            output.write_to(writer, Self::block_kind())?;
        }
        Ok(())
    }
}

impl DeckWrite for Loop {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} {} REPEAT {}",
            Self::block_kind(),
            self.units.len(),
            self.repeat
        )?;
        for unit in &self.units {
            write!(writer, " {}", unit)?;
        }
        Ok(())
    }
}

impl DeckWrite for Dfq {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} {}", Self::block_kind(), self.method as u8)?;
        Ok(())
    }
}

impl DeckWrite for NoCheck {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} {}", Self::block_kind(), self.inputs.len())?;
        for input in &self.inputs {
            write!(writer, " ")?;
            input.write_to(writer, Self::block_kind())?;
        }
        Ok(())
    }
}

impl DeckWrite for EqSolver {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "EQSOLVER {}", self.method as u8)?;
        Ok(())
    }
}

impl DeckWrite for Solver {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "SOLVER {}", self.method as u8)?;
        if let Some(ref rf_min) = self.rf_min {
            write!(writer, " {}", rf_min)?;
        }
        if let Some(ref rf_max) = self.rf_max {
            write!(writer, " {}", rf_max)?;
        }
        Ok(())
    }
}

impl DeckWrite for Assign {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "ASSIGN \"{}\" {}", self.filename, self.logical_unit)?;
        Ok(())
    }
}

impl DeckWrite for Designate {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "DESIGNATE \"{}\" {}",
            self.filename, self.logical_unit
        )?;
        Ok(())
    }
}

impl DeckWrite for Include {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "INCLUDE \"{}\"", self.filename)?;
        Ok(())
    }
}

impl DeckWrite for End {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "END")?;
        Ok(())
    }
}

impl DeckWrite for Metadata {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        if let Some(ref unit_name) = self.unit_name {
            write!(writer, "*$UNIT_NAME {}\n", unit_name)?;
        }
        if let Some(ref model) = self.model {
            write!(writer, "*$MODEL {}\n", model)?;
        }
        if let Some(ref position) = self.position {
            write!(writer, "*$POSITION {} {}\n", position.0, position.1)?;
        }
        if let Some(ref layer) = self.layer {
            write!(writer, "*$LAYER {}\n", layer)?;
        }
        for (k, v) in self.other.iter() {
            write!(writer, "*${} {}\n", k.to_uppercase(), v)?;
        }
        Ok(())
    }
}

impl DeckWrite for Trace {
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} {} {}", kind, self.start_time, self.stop_time)?;
        Ok(())
    }
}

impl DeckWrite for Format {
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} \"{}\"", kind, self.format_string)?;
        Ok(())
    }
}

impl DeckWrite for Unit {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} {} TYPE {} {}\n",
            Self::block_kind(),
            self.number,
            self.type_number,
            self.unit_name
        )?;
        if let Some(ref metadata) = self.metadata {
            metadata.write_to(writer, Self::block_kind())?;
        }
        if let Some(ref parameters) = self.parameters {
            write!(writer, "PARAMETERS {}\n", parameters.len())?;
            for param in parameters {
                param.write_to(writer, Self::block_kind())?;
            }
        }

        if let Some(ref inputs) = self.inputs {
            write!(writer, "INPUTS {}\n", inputs.len())?;
            for input in inputs {
                input.connection.write_to(writer, Self::block_kind())?;
            }
            write!(writer, "*** INITIAL INPUT VALUES\n")?;
            for input in inputs {
                input.initial.write_to(writer, Self::block_kind())?;
            }
        }

        if let Some(ref labels) = self.labels {
            write!(writer, "LABELS {}\n", labels.len())?;
            for label in labels {
                write!(writer, " ")?;
                label.write_to(writer, Self::block_kind())?;
            }
        }

        if let Some(ref derivatives) = self.derivatives {
            write!(writer, "DERIVATIVES {}\n", derivatives.len())?;
            for derivative in derivatives {
                derivative.write_to(writer, Self::block_kind())?;
            }
        }

        if let Some(ref trace) = self.trace {
            trace.write_to(writer, BlockKind::Trace)?;
        }

        if let Some(ref etrace) = self.etrace {
            etrace.write_to(writer, BlockKind::ETrace)?;
        }

        if let Some(ref format) = self.format {
            format.write_to(writer, BlockKind::Format)?;
        }

        Ok(())
    }
}

impl DeckWrite for Width {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{} {}", Self::block_kind(), self.0)?;

        Ok(())
    }
}

impl DeckWrite for NoList {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{}", Self::block_kind())?;
        Ok(())
    }
}

impl DeckWrite for List {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{}", Self::block_kind())?;
        Ok(())
    }
}

impl DeckWrite for Map {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(writer, "{}", Self::block_kind())?;
        Ok(())
    }
}

impl DeckWrite for CSummarize {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} \"{}\" \"{}\"",
            Self::block_kind(),
            self.const_name,
            self.description
        )?;
        Ok(())
    }
}

impl DeckWrite for ESummarize {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        write!(
            writer,
            "{} \"{}\" \"{}\"",
            Self::block_kind(),
            self.eq_name,
            self.description
        )?;
        Ok(())
    }
}
