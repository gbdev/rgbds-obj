use crate::util::{read_str, read_u32le, read_u8};
use crate::RpnExpr;
use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Read};

/// A link-time assertion.
/// Functions mostly like a [`Patch`][crate::Patch].
#[derive(Debug)]
pub struct Assertion {
    source_file_id: u32,
    line_no: u32,
    offset: u32,
    pc_section_id: u32,
    pc_offset: u32,
    err_type: AssertionType,
    expr: RpnExpr,
    message: Vec<u8>,
}
impl Assertion {
    pub(crate) fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        let source_file_id = read_u32le(&mut input)?;
        let line_no = read_u32le(&mut input)?;
        let offset = read_u32le(&mut input)?;
        let pc_section_id = read_u32le(&mut input)?;
        let pc_offset = read_u32le(&mut input)?;
        let err_type = AssertionType::try_from(read_u8(&mut input)?)?;

        let expr_size = read_u32le(&mut input)?.try_into().unwrap();
        let mut expr = vec![0; expr_size];
        input.read_exact(&mut expr)?;
        let expr = RpnExpr::from_bytes(expr);
        let message = read_str(input)?;

        Ok(Self {
            source_file_id,
            line_no,
            offset,
            pc_section_id,
            pc_offset,
            err_type,
            expr,
            message,
        })
    }

    /// Where the assertion was defined.
    /// That is, the [file stack node][crate::Node] ID, and the line number.
    pub fn source(&self) -> (u32, u32) {
        (self.source_file_id, self.line_no)
    }

    /// The offset within the [section][crate::Section]'s data where the assertion's expression
    /// shall be computed.
    ///
    /// This doesn't serve much purpose, since assertions only compute an expression, and don't
    /// patch the data.
    /// It should be equal to the [PC offset][Self::pc_offset].
    ///
    /// [PC offset]: #method.pc_offset
    pub fn offset(&self) -> u32 {
        self.offset
    }

    /// The assertion's error type (warning, error, fatal).
    pub fn err_type(&self) -> AssertionType {
        self.err_type
    }

    /// The ID of the expression's PC section. See [`pc_offset`][Self::pc_offset] for more info.
    pub fn pc_section_id(&self) -> u32 {
        self.pc_section_id
    }

    /// The expression's PC offset.
    ///
    /// This is separate from the [offset][Self::offset], probably as legacy due to assertions
    /// inheriting a lot from [patches][crate::Patch].
    pub fn pc_offset(&self) -> u32 {
        self.pc_offset
    }

    /// The assertion's RPN expression.
    pub fn expr(&self) -> &RpnExpr {
        &self.expr
    }

    /// The assertion's message.
    /// As with all text pulled from object files, this is not guaranteed to be valid UTF-8.
    pub fn message(&self) -> &[u8] {
        &self.message
    }
}

/// An assertion's error type.
#[derive(Debug, Clone, Copy)]
pub enum AssertionType {
    /// The assertion should produce a warning, but not fail linking.
    Warning,
    /// The assertion should produce an error, and cause the linker to exit unsuccessfully.
    Error,
    /// The assertion should produce an error, and abort linking immediately.
    Fatal,
}
impl AssertionType {
    fn try_from(byte: u8) -> Result<Self, io::Error> {
        use AssertionType::*;

        Ok(match byte {
            0 => Warning,
            1 => Error,
            2 => Fatal,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid assertion type",
                ))
            }
        })
    }

    /// The assertion error type's name.
    pub fn name(&self) -> &'static str {
        use AssertionType::*;

        match self {
            Warning => "warning",
            Error => "error",
            Fatal => "fatal",
        }
    }
}
impl Display for AssertionType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}", self.name())
    }
}
