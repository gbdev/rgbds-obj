use crate::util::{opt_u32, read_str, read_u32le, read_u8};
use std::fmt::{self, Display, Formatter};
use std::io::{self, Read};

/// A symbol declaration, which may include a definition.
#[derive(Debug)]
pub struct Symbol {
    name: Vec<u8>,
    visibility: SymbolVisibility,
}
impl Symbol {
    pub(crate) fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        let name = read_str(&mut input)?;
        let visibility = SymbolVisibility::read_from(input)?;
        Ok(Self { name, visibility })
    }

    /// The symbol's name, as stored raw in the object file.
    /// Like all names pulled from object files, this is not guaranteed to be valid UTF-8.
    pub fn name(&self) -> &[u8] {
        &self.name
    }

    /// The symbol's visibility, including its definition data (if any).
    pub fn visibility(&self) -> &SymbolVisibility {
        &self.visibility
    }
}

/// A symbol's visibility, and accompanying data if applicable.
#[derive(Debug)]
pub enum SymbolVisibility {
    /// The symbol is defined in this object file, but is not visible outside of it.
    Local(SymbolDef),
    /// The symbol is not defined in this object file.
    Imported,
    /// The symbol is defined in this object file, and is visible in others as well.
    Exported(SymbolDef),
}
impl SymbolVisibility {
    fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        use SymbolVisibility::*;

        match read_u8(&mut input)? {
            0 => Ok(Local(SymbolDef::read_from(input)?)),
            1 => Ok(Imported),
            2 => Ok(Exported(SymbolDef::read_from(input)?)),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid symbol visibility type",
            )),
        }
    }

    /// The symbol visibility's name.
    pub fn name(&self) -> &'static str {
        use SymbolVisibility::*;

        match self {
            Local(..) => "local",
            Imported => "import",
            Exported(..) => "export",
        }
    }

    /// Returns the symbol's definition data, if any.
    pub fn data(&self) -> Option<&SymbolDef> {
        use SymbolVisibility::*;

        match self {
            Local(data) | Exported(data) => Some(data),
            Imported => None,
        }
    }
}
impl Display for SymbolVisibility {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}", self.name())
    }
}

/// A symbol definition's data.
#[derive(Debug)]
pub struct SymbolDef {
    source_file_id: u32,
    line_no: u32,
    section_id: Option<u32>,
    value: u32,
}
impl SymbolDef {
    fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        let source_file_id = read_u32le(&mut input)?;
        let line_no = read_u32le(&mut input)?;
        let section_id = opt_u32(read_u32le(&mut input)?);
        let value = read_u32le(input)?;

        Ok(Self {
            source_file_id,
            line_no,
            section_id,
            value,
        })
    }

    /// Where the symbol has been defined.
    /// That is, the [file stack node][crate::Node] ID, and the line number.
    pub fn source(&self) -> (u32, u32) {
        (self.source_file_id, self.line_no)
    }

    /// The ID of the [`Section`][crate::Section] the symbol was defined in.
    /// This is `None` for constants.
    pub fn section(&self) -> Option<u32> {
        self.section_id
    }

    /// The symbol's offset within its [`Section`][crate::Section], or its value if not attached to one.
    pub fn value(&self) -> u32 {
        self.value
    }
}
