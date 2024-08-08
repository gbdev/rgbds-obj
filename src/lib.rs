//! This crate allows working with [RGBDS] object files.
//! Currently, only version 9 revisions 6–10 are supported, but more should be added in the
//! future.
//!
//! # Object file revision table
//!
//! The object file format has changed several times over RGBDS' lifespan.
//! The following table indicates which object file version each release of RGBDS uses.
//!
//! Note that a "revision" field was introduced in version 9, so it's not listed earlier.
//!
//! RGBDS release                                          | Object file format
//! -------------------------------------------------------|-------------------
//! [v0.8.0](https://rgbds.gbdev.io/docs/v0.8.0/rgbds.5)   | v9 r10
//! [v0.7.0](https://rgbds.gbdev.io/docs/v0.7.0/rgbds.5)   | v9 r9 (reported), v9 r10 (actual)
//! [v0.6.1](https://rgbds.gbdev.io/docs/v0.6.1/rgbds.5)   | v9 r9
//! [v0.6.0](https://rgbds.gbdev.io/docs/v0.6.0/rgbds.5)   | v9 r9
//! [v0.5.1](https://rgbds.gbdev.io/docs/v0.5.1/rgbds.5)   | v9 r8
//! [v0.5.0](https://rgbds.gbdev.io/docs/v0.5.0/rgbds.5)   | v9 r7
//! [v0.4.2](https://rgbds.gbdev.io/docs/v0.4.2/rgbds.5)   | v9 r6
//! [v0.4.1](https://rgbds.gbdev.io/docs/v0.4.1/rgbds.5)   | v9 r5
//! [v0.4.0](https://rgbds.gbdev.io/docs/v0.4.0/rgbds.5)   | v9 r3
//! [v0.3.10](https://rgbds.gbdev.io/docs/v0.3.10/rgbds.5) | v6
//!
//! Earlier releases use v6, up to and including v0.3.4.
//!
//! [RGBDS]: https://rgbds.gbdev.io

#![doc(html_root_url = "https://docs.rs/rgbds-obj/0.2.1")]

use std::convert::TryInto;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Read};

mod assertion;
pub use assertion::*;
mod fstack;
pub use fstack::*;
mod patch;
pub use patch::*;
mod rpn;
pub use rpn::*;
mod section;
pub use section::*;
mod symbol;
pub use symbol::*;
mod util;
use util::*;

/// A RGBDS object file.
#[derive(Debug)]
pub struct Object {
    version: u8,
    revision: u32,
    fstack_nodes: Vec<Node>,
    symbols: Vec<Symbol>,
    sections: Vec<Section>,
    assertions: Vec<Assertion>,
}

impl Object {
    /// Reads a serialized object.
    ///
    /// # Errors
    ///
    /// This function returns any errors that occurred while reading data, as well as if the object
    /// data itself cannot be deserialized.
    /// Note that not all consistency checks are performed when reading the file; for example, RPN
    /// expressions may be invalid, the file stack node tree may be malformed, etc.
    ///
    /// Note that maximum upwards compatibility is assumed: for example, currently, RPN data is
    /// parsed using the v9 r8 spec, even if the file reports an earlier revision.
    /// This should change in the future.
    pub fn read_from(mut input: impl Read) -> Result<Self, io::Error> {
        let mut magic = [0; 4];
        input.read_exact(&mut magic)?;

        if &magic[0..3] != b"RGB" || !magic[3].is_ascii_digit() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "This does not appear to be a valid RGBDS object",
            ));
        }

        let version = magic[3];
        if version != b'9' {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Object file version {} is not supported (must be 9)",
                    version as char
                ),
            ));
        }

        let revision = read_u32le(&mut input)?;
        if !(6..=10).contains(&revision) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Object file {} revision {revision} is not supported (must be between 6 and 10)",
                    version as char
                ),
            ));
        }

        let nb_symbols = read_u32le(&mut input)?.try_into().unwrap();
        let nb_sections = read_u32le(&mut input)?.try_into().unwrap();
        let nb_fstack_nodes = read_u32le(&mut input)?.try_into().unwrap();

        let mut obj = Self {
            version,
            revision,
            fstack_nodes: Vec::with_capacity(nb_fstack_nodes),
            symbols: Vec::with_capacity(nb_symbols),
            sections: Vec::with_capacity(nb_sections),
            assertions: Vec::new(), // We don't have the assertion count yet
        };

        for _ in 0..nb_fstack_nodes {
            obj.fstack_nodes.push(Node::read_from(&mut input)?);
        }
        for _ in 0..nb_symbols {
            obj.symbols.push(Symbol::read_from(&mut input)?);
        }
        for _ in 0..nb_sections {
            obj.sections.push(Section::read_from(&mut input)?);
        }

        let nb_assertions = read_u32le(&mut input)?.try_into().unwrap();
        obj.assertions.reserve_exact(nb_assertions);
        for _ in 0..nb_assertions {
            obj.assertions.push(Assertion::read_from(&mut input)?);
        }

        Ok(obj)
    }

    /// The object's version.
    pub fn version(&self) -> u8 {
        self.version
    }

    /// The object's revision.
    pub fn revision(&self) -> u32 {
        self.revision
    }

    /// Retrieves one of the object's [file stack nodes][crate::Node] by ID.
    /// Returns `None` if the ID is invalid (too large).
    pub fn node(&self, id: u32) -> Option<&Node> {
        let id: usize = id.try_into().unwrap();
        if id < self.fstack_nodes.len() {
            Some(&self.fstack_nodes[self.fstack_nodes.len() - 1 - id])
        } else {
            None
        }
    }

    /// Walks the node tree, from its root up to the node with the given ID, running a callback on
    /// each node encountered.
    ///
    /// The functon may return an error, which aborts the walk.
    /// If the function does not fail, you can (and probably will have to) use [`Infallible`][std::convert::Infallible]:
    ///
    /// ```no_run
    /// # use rgbds_obj::Object;
    /// # use std::convert::Infallible;
    /// # use std::fs::File;
    /// #
    /// # let input = File::open("camera.o").unwrap();
    /// # let object = Object::read_from(&input).unwrap();
    /// object.walk_nodes::<Infallible, _>(0, &mut |node| {
    ///     println!("{node:?}");
    ///     Ok(())
    /// });
    /// ```
    pub fn walk_nodes<E, F>(&self, id: u32, callback: &mut F) -> Result<(), NodeWalkError<E>>
    where
        F: FnMut(&Node) -> Result<(), NodeWalkError<E>>,
    {
        let node = self
            .node(id)
            .ok_or_else(|| NodeWalkError::bad_id(id, self))?;

        if let Some((id, _)) = node.parent() {
            self.walk_nodes(id, callback)?;
        }
        callback(node)
    }

    /// The object's symbols.
    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }

    /// The object's sections.
    pub fn sections(&self) -> &[Section] {
        &self.sections
    }

    /// The object's assertions.
    pub fn assertions(&self) -> &[Assertion] {
        &self.assertions
    }
}

/// An error that occurs while walking the node tree.
#[derive(Debug)]
pub enum NodeWalkError<E> {
    /// Requested a node number that is out of bounds.
    BadId(u32, usize),
    /// Inner error thrown by the callback.
    Custom(E),
}
impl<E> NodeWalkError<E> {
    /// Constructs an "out of bounds node ID" error that occurred while walking a given object.
    pub fn bad_id(id: u32, object: &Object) -> Self {
        Self::BadId(id, object.fstack_nodes.len())
    }
}
impl<E: Display> Display for NodeWalkError<E> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        use NodeWalkError::*;

        match self {
            BadId(id, len) => write!(fmt, "Requested node #{id} of {len}"),
            Custom(err) => err.fmt(fmt),
        }
    }
}
impl<E: Error + 'static> Error for NodeWalkError<E> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use NodeWalkError::*;

        if let Custom(err) = self {
            Some(err)
        } else {
            None
        }
    }
}
impl<E> From<E> for NodeWalkError<E> {
    fn from(inner: E) -> Self {
        Self::Custom(inner)
    }
}
