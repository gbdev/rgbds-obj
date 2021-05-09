use std::io::{self, Read};

/// Utility function to read a "LONG" (according to rgbds(5)) item
pub fn read_u32le(mut input: impl Read) -> Result<u32, io::Error> {
    let mut bytes = [0; 4];
    input.read_exact(&mut bytes)?;
    Ok(u32::from_le_bytes(bytes))
}

/// Utility function to read a "BYTE" (according to rgbds(5)) item
pub fn read_u8(mut input: impl Read) -> Result<u8, io::Error> {
    let mut byte = [0; 1];
    input.read_exact(&mut byte)?;
    Ok(byte[0])
}

/// Utility function to read a NUL-terminated string
pub fn read_str(mut input: impl Read) -> Result<Vec<u8>, io::Error> {
    let mut byte = [0];
    let mut bytes = Vec::new();

    loop {
        input.read_exact(&mut byte)?;
        if byte[0] == 0 {
            return Ok(bytes);
        }
        bytes.push(byte[0]);
    }
}

pub fn opt_u32(raw: u32) -> Option<u32> {
    if raw == u32::MAX {
        None
    } else {
        Some(raw)
    }
}
