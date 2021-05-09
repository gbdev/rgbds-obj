use crate::util::{read_u32le, read_u8};
use crate::RpnExpr;
use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Read};

/// A section patch.
/// Patches are bytes (or groups of bytes) whose computation was deferred to the linker.
#[derive(Debug)]
pub struct Patch {
    source_file_id: u32,
    line_no: u32,
    offset: u32,
    pc_section_id: u32,
    pc_offset: u32,
    patch_type: PatchType,
    expr: RpnExpr,
}
impl Patch {
    pub(crate) fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        let source_file_id = read_u32le(&mut input)?;
        let line_no = read_u32le(&mut input)?;
        let offset = read_u32le(&mut input)?;
        let pc_section_id = read_u32le(&mut input)?;
        let pc_offset = read_u32le(&mut input)?;
        let patch_type = PatchType::try_from(read_u8(&mut input)?)?;

        let expr_size = read_u32le(&mut input)?.try_into().unwrap();
        let mut expr = vec![0; expr_size];
        input.read_exact(&mut expr)?;
        let expr = RpnExpr::from_bytes(expr);

        Ok(Self {
            source_file_id,
            line_no,
            offset,
            pc_section_id,
            pc_offset,
            patch_type,
            expr,
        })
    }

    /// Where the patch was defined.
    /// That is, the [file stack node][crate::Node] ID, and the line number.
    pub fn source(&self) -> (u32, u32) {
        (self.source_file_id, self.line_no)
    }

    /// The offset within the [section][crate::Section]'s data where the patch will be written.
    pub fn offset(&self) -> u32 {
        self.offset
    }

    /// The patch's type, which notably determines its size.
    pub fn patch_type(&self) -> PatchType {
        self.patch_type
    }

    /// The ID of the expression's PC section. See [`pc_offset`][Self::pc_offset] for more info.
    pub fn pc_section_id(&self) -> u32 {
        self.pc_section_id
    }

    /// The expression's PC offset.
    ///
    /// This is separate from the [offset][Self::offset], because PC (`@`) may not point to the
    /// byte being written, and, in the case of [`LOAD` blocks], may be in a different section
    /// entirely!
    ///
    /// [`LOAD` blocks]: https://rgbds.gbdev.io/docs/v0.5.1/rgbasm.5.html#RAM_Code
    pub fn pc_offset(&self) -> u32 {
        self.pc_offset
    }

    /// The patch's RPN expression.
    pub fn expr(&self) -> &RpnExpr {
        &self.expr
    }
}

/// A patch's type.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PatchType {
    Byte,
    Word,
    Long,
    JrOfs,
}
impl PatchType {
    fn try_from(byte: u8) -> Result<Self, io::Error> {
        use PatchType::*;

        Ok(match byte {
            0 => Byte,
            1 => Word,
            2 => Long,
            3 => JrOfs,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid patch type",
                ))
            }
        })
    }

    /// The patch type's name.
    pub fn name(&self) -> &'static str {
        use PatchType::*;

        match self {
            Byte => "byte",
            Word => "word",
            Long => "long",
            JrOfs => "jr",
        }
    }

    /// How many bytes this patch type modifies.
    pub fn size(&self) -> u8 {
        use PatchType::*;

        match self {
            Byte => 1,
            Word => 2,
            Long => 4,
            JrOfs => 1,
        }
    }
}
impl Display for PatchType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}", self.name())
    }
}
