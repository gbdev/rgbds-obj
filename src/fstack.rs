use crate::util::{opt_u32, read_str, read_u32le, read_u8};
use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Read};

/// A file stack node.
///
/// Historically, RGBDS stored locations as simple strings, which were extended to contain the
/// the full "file stack" (included files, macro calls, etc.); however, object files grew very
/// large, as the strings contained a lot of repetition.
///
/// To solve the bloat problem, file stack nodes were introduced in [format v9 r5]; they store each
/// scope in a unique way, building a tree.
/// However, scopes don't store line numbers, since several definitions may occur per scope, but
/// instead store the line number at which their parent scope was exited.
///
/// [format v9 r5]: https://rgbds.gbdev.io/docs/v0.4.2/rgbds.5
#[derive(Debug)]
pub struct Node {
    parent_id: Option<u32>,
    parent_line_no: u32,
    node_type: NodeType,
}
impl Node {
    pub(crate) fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        let parent_id = opt_u32(read_u32le(&mut input)?);
        let parent_line_no = read_u32le(&mut input)?;
        let node_type = NodeType::read_from(input)?;

        Ok(Node {
            parent_id,
            parent_line_no,
            node_type,
        })
    }

    /// Returns the ID of the parent node, and the line number at which it was exited.
    /// If the node is a root node, `None` is returned instead.
    pub fn parent(&self) -> Option<(u32, u32)> {
        self.parent_id.map(|id| (id, self.parent_line_no))
    }

    /// The node's type and associated data.
    pub fn type_data(&self) -> &NodeType {
        &self.node_type
    }
}

/// A file stack node's type, and associated type-dependent data.
#[derive(Debug)]
pub enum NodeType {
    /// The node represents one or more `REPT` blocks, and contains the iteration count for each of
    /// them.
    Rept(Vec<u32>),
    /// The node represents a top-level or `INCLUDE`d file, and contains the file's path (which may
    /// not be valid UTF-8).
    File(Vec<u8>),
    /// The node represents a macro invocation, and contains the macro's name (which may not be
    /// valid UTF-8).
    Macro(Vec<u8>),
}
impl NodeType {
    fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        let node_type = read_u8(&mut input)?;
        match node_type {
            0 => {
                let depth = read_u32le(&mut input)?.try_into().unwrap();
                let mut iters = Vec::with_capacity(depth);
                for _ in 0..depth {
                    iters.push(read_u32le(&mut input)?);
                }
                Ok(NodeType::Rept(iters))
            }
            1 => Ok(NodeType::File(read_str(input)?)),
            2 => Ok(NodeType::Macro(read_str(input)?)),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid fstack node type",
            )),
        }
    }
}
impl Display for NodeType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        use NodeType::*;

        match self {
            Rept(iters) => {
                for iter in iters {
                    write!(fmt, "::REPT~{iter}")?;
                }
            }
            File(name) | Macro(name) => write!(fmt, "{}", String::from_utf8_lossy(name))?,
        };
        Ok(())
    }
}
