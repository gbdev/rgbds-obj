use crate::util::{opt_u32, read_str, read_u32le, read_u8};
use crate::Patch;
use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Read};

/// A section definition.
#[derive(Debug)]
pub struct Section {
    name: Vec<u8>,
    source_file_id: Option<u32>,
    line_no: Option<u32>,
    size: u32,
    sect_type: SectionType,
    modifier: SectionMod,
    org: Option<u32>,
    bank: Option<u32>,
    align: u8,
    ofs: u32,
}
impl Section {
    pub(crate) fn read_from(mut input: impl Read, has_src: bool) -> Result<Self, io::Error> {
        let name = read_str(&mut input)?;
        let source_file_id = if has_src {
            Some(read_u32le(&mut input)?)
        } else {
            None
        };
        let line_no = if has_src {
            Some(read_u32le(&mut input)?)
        } else {
            None
        };
        let size = read_u32le(&mut input)?;
        let sect_type = read_u8(&mut input)?;
        let modifier = SectionMod::from(sect_type)?;
        let org = opt_u32(read_u32le(&mut input)?);
        let bank = opt_u32(read_u32le(&mut input)?);
        let align = read_u8(&mut input)?;
        let ofs = read_u32le(&mut input)?;

        let sect_type = SectionType::read_from(sect_type, input, size.try_into().unwrap())?;

        Ok(Self {
            name,
            source_file_id,
            line_no,
            size,
            sect_type,
            modifier,
            org,
            bank,
            align,
            ofs,
        })
    }

    /// The section's name.
    /// As with all names pulled from object files, this is not guaranteed to be valid UTF-8.
    pub fn name(&self) -> &[u8] {
        &self.name
    }

    /// Where the section has been defined.
    /// That is, the [file stack node][crate::Node] ID, and the line number.
    /// This is `None` for object files prior to v9 r11.
    pub fn source(&self) -> Option<(u32, u32)> {
        match (self.source_file_id, self.line_no) {
            (Some(source_file_id), Some(line_no)) => Some((source_file_id, line_no)),
            _ => None,
        }
    }

    /// The section's size.
    pub fn size(&self) -> u32 {
        self.size
    }

    /// The section's memory type, including data, if any.
    pub fn type_data(&self) -> &SectionType {
        &self.sect_type
    }

    /// The section's modifier (regular, union, etc.).
    pub fn modifier(&self) -> SectionMod {
        self.modifier
    }

    /// The address at which the section was fixed, or `None` if left floating.
    pub fn org(&self) -> Option<u32> {
        self.org
    }

    /// The bank the section was assigned, or `None` if left floating.
    pub fn bank(&self) -> Option<u32> {
        self.bank
    }

    /// The section's alignment, in bits. 0 if not specified.
    pub fn align(&self) -> u8 {
        self.align
    }

    /// The section's alignment offset.
    pub fn align_ofs(&self) -> u32 {
        self.ofs
    }
}

/// A section memory type, and associated data if applicable.
#[derive(Debug)]
pub enum SectionType {
    Wram0,
    Vram,
    Romx(SectionData),
    Rom0(SectionData),
    Hram,
    Wramx,
    Sram,
    Oam,
}
impl SectionType {
    fn read_from(byte: u8, input: impl Read, size: usize) -> Result<Self, io::Error> {
        use SectionType::*;

        Ok(match byte & 0x3F {
            0 => Wram0,
            1 => Vram,
            2 => Romx(SectionData::read_from(input, size)?),
            3 => Rom0(SectionData::read_from(input, size)?),
            4 => Hram,
            5 => Wramx,
            6 => Sram,
            7 => Oam,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid section type",
                ))
            }
        })
    }

    /// The [section][Section]'s ROM data, if any.
    pub fn data(&self) -> Option<&SectionData> {
        use SectionType::*;

        match self {
            Rom0(data) | Romx(data) => Some(data),
            _ => None,
        }
    }

    /// Returns whether the section type may be banked.
    /// Note that it's possible to configure this in RGBLINK (disabling VRAM banking with `-d`, for
    /// example), so this may return `true` but have RGBLINK say otherwise.
    pub fn is_banked(&self) -> bool {
        use SectionType::*;

        matches!(self, Romx(..) | Vram | Sram | Wramx)
    }

    /// The section type's name.
    pub fn name(&self) -> &'static str {
        use SectionType::*;

        match self {
            Wram0 => "WRAM0",
            Vram => "VRAM",
            Romx(..) => "ROMX",
            Rom0(..) => "ROM0",
            Hram => "HRAM",
            Wramx => "WRAMX",
            Sram => "SRAM",
            Oam => "OAM",
        }
    }
}
impl Display for SectionType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}", self.name())
    }
}

/// A section modifier.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SectionMod {
    Normal,
    Union,
    Fragment,
}
impl SectionMod {
    fn from(byte: u8) -> Result<Self, io::Error> {
        use SectionMod::*;

        Ok(match byte & 0xC0 {
            0x00 => Normal,
            0x80 => Union,
            0x40 => Fragment,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid section modifier",
                ))
            }
        })
    }

    /// The modifier's name, except that there is no such name for "regular" sections.
    pub fn name(&self) -> Option<&'static str> {
        use SectionMod::*;

        match self {
            Normal => None,
            Union => Some("UNION"),
            Fragment => Some("FRAGMENT"),
        }
    }
}

/// A ROM section's data.
#[derive(Debug)]
pub struct SectionData {
    data: Vec<u8>,
    patches: Vec<Patch>,
}
impl SectionData {
    fn read_from(mut input: impl Read, size: usize) -> Result<Self, io::Error> {
        let mut data = vec![0; size];
        input.read_exact(&mut data)?;

        let nb_patches = read_u32le(&mut input)?.try_into().unwrap();
        let mut patches = Vec::with_capacity(nb_patches);
        for _ in 0..nb_patches {
            patches.push(Patch::read_from(&mut input)?);
        }

        Ok(Self { data, patches })
    }

    /// The section's data.
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// The section's patches.
    pub fn patches(&self) -> &[Patch] {
        &self.patches
    }
}
