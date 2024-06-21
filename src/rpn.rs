use std::cmp::Ordering;
use std::convert::TryInto;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::iter;

/// A [Reverse Polish Notation] expression.
///
/// # Printing
///
/// An expression can be printed in one of two ways.
/// By default, the expression is printed using [Reverse Polish Notation] still; however, if the
/// [`#` "alternate printing" flag][fmt#sign0] is passed, the expression will be pretty-printed using infix
/// notation instead, with a minimal amount of parentheses.
///
/// [Reverse Polish Notation]: https://en.wikipedia.org/wiki/Reverse_Polish_notation
#[derive(Debug)]
pub struct RpnExpr(Vec<u8>);

/// A section memory type.
#[derive(Debug, PartialEq)]
pub enum SectType {
    Wram0,
    Vram,
    Romx,
    Rom0,
    Hram,
    Wramx,
    Sram,
    Oam,
}
impl SectType {
    /// The section type's name.
    pub fn name(&self) -> &'static str {
        use SectType::*;

        match self {
            Wram0 => "WRAM0",
            Vram => "VRAM",
            Romx => "ROMX",
            Rom0 => "ROM0",
            Hram => "HRAM",
            Wramx => "WRAMX",
            Sram => "SRAM",
            Oam => "OAM",
        }
    }
}

impl RpnExpr {
    /// Constructs a RPN expression from its byte serialization.
    /// This does not check the expression's correctness.
    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    /// Retrieves the expression's serialized bytes.
    pub fn bytes(&self) -> &[u8] {
        &self.0
    }

    /// Yields an iterator over the expression's "operations".
    /// "Operations" may not mean what you think; refer to [`RpnOp`] for more information.
    pub fn iter(&self) -> Iter {
        Iter::new(self)
    }
}

/// An iterator over a [RPN expression][RpnExpr]'s operations (this includes literals).
///
/// Since a RPN expression does not validate the serialized data it's given when constructed,
/// the iteration may fail at any point.
pub struct Iter<'a>(&'a RpnExpr, usize); // Expression, pointer
impl<'a> Iter<'a> {
    fn new(expr: &'a RpnExpr) -> Self {
        Self(expr, 0)
    }

    fn read_u32(&mut self) -> Option<u32> {
        self.0.bytes().get(self.1..self.1 + 4).map(|bytes| {
            let val = u32::from_le_bytes(bytes.try_into().unwrap());
            self.1 += 4;
            val
        })
    }

    fn read_string(&mut self) -> Option<&'a [u8]> {
        let start = self.1;
        loop {
            let c = *self.0.bytes().get(self.1)?;
            self.1 += 1;

            if c == 0 {
                return Some(&self.0.bytes()[start..self.1]);
            }
        }
    }    

    fn read_sect_type(&mut self) -> Option<SectType> {
        use SectType::*;

        let val = match self.0.bytes().get(self.1) {
           Some(0) => Some(Wram0),
           Some(1) => Some(Vram),
           Some(2) => Some(Romx),
           Some(3) => Some(Rom0),
           Some(4) => Some(Hram),
           Some(5) => Some(Wramx),
           Some(6) => Some(Sram),
           Some(7) => Some(Oam),
           _ => None
        };
        self.1 += 1;
        val
    }
}
impl<'a> Iterator for Iter<'a> {
    type Item = Result<RpnOp<'a>, RpnIterError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 == self.0.bytes().len() {
            return None;
        }
        let err = Err(RpnIterError::new(self.1)); // What to return in case of error

        let operator = self.0.bytes()[self.1];
        self.1 += 1;
        Some(match operator {
            0x00 => Ok(RpnOp::Add),
            0x01 => Ok(RpnOp::Sub),
            0x02 => Ok(RpnOp::Mul),
            0x03 => Ok(RpnOp::Div),
            0x04 => Ok(RpnOp::Mod),
            0x05 => Ok(RpnOp::Neg),
            0x06 => Ok(RpnOp::Pow),
            0x10 => Ok(RpnOp::BinOr),
            0x11 => Ok(RpnOp::BinAnd),
            0x12 => Ok(RpnOp::Xor),
            0x13 => Ok(RpnOp::Cpl),
            0x21 => Ok(RpnOp::And),
            0x22 => Ok(RpnOp::Or),
            0x23 => Ok(RpnOp::Not),
            0x30 => Ok(RpnOp::Eq),
            0x31 => Ok(RpnOp::Neq),
            0x32 => Ok(RpnOp::Gt),
            0x33 => Ok(RpnOp::Lt),
            0x34 => Ok(RpnOp::Gte),
            0x35 => Ok(RpnOp::Lte),
            0x40 => Ok(RpnOp::Lsh),
            0x41 => Ok(RpnOp::Rsh),
            0x42 => Ok(RpnOp::Ursh),
            0x50 => self.read_u32().map_or(err, |id| Ok(RpnOp::BankSym(id))),
            0x51 => self
                .read_string()
                .map_or(err, |string| Ok(RpnOp::BankSect(string))),
            0x52 => Ok(RpnOp::BankSelf),
            0x53 => self
                .read_string()
                .map_or(err, |string| Ok(RpnOp::SizeofSect(string))),
            0x54 => self
                .read_string()
                .map_or(err, |string| Ok(RpnOp::StartofSect(string))),
            0x55 => self
                .read_sect_type()
                .map_or(err, |sect_type| Ok(RpnOp::SizeofSectType(sect_type))),
            0x56 => self
                .read_sect_type()
                .map_or(err, |sect_type| Ok(RpnOp::StartofSectType(sect_type))),
            0x60 => Ok(RpnOp::HramCheck),
            0x61 => Ok(RpnOp::RstCheck),
            0x80 => self.read_u32().map_or(err, |id| Ok(RpnOp::Int(id))),
            0x81 => self.read_u32().map_or(err, |id| Ok(RpnOp::Sym(id))),
            _ => err,
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.1 == self.0.bytes().len() {
            return (0, Some(0));
        }

        // At any point, there may be a single element left (if it takes a string as argument);
        // however, each element consumes at least one byte, so we have an upper bound.
        (1, Some(self.0.bytes().len() - self.1))
    }
}

/// An error produced while iterating on a [RPN expression][RpnExpr].
/// This can be an early EOF, an operator trying to popping an item off of an empty RPN stack, etc.
#[derive(Debug)]
pub struct RpnIterError(usize);
impl RpnIterError {
    fn new(ofs: usize) -> Self {
        Self(ofs)
    }

    /// The offset within the expression at which the error was encountered.
    pub fn offset(&self) -> usize {
        self.0
    }
}
impl Display for RpnIterError {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "Error parsing RPN expression at offset {}", self.0)
    }
}
impl Error for RpnIterError {}

// RPN expression printing

impl RpnExpr {
    fn fmt_rpn(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        for (op, prefix) in self.iter().zip(iter::successors(Some(""), |_| Some(" "))) {
            match op {
                Err(err) => write!(fmt, "{}<RPN error@${:04x}>", prefix, err.offset())?,
                Ok(op) => write!(fmt, "{}{}", prefix, op)?,
            }
        }
        Ok(())
    }

    fn fmt_infix(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        let mut nodes = Vec::new();
        // Node ID stack
        let mut stack = Vec::new();
        // Pops a node ID off the stack, erroring out of the whole function if it's empty
        // Additionally, gives the popped node its parent's ID
        macro_rules! pop {
            ($parent:expr) => {
                if let Some(tree) = stack.pop() {
                    tree
                } else {
                    return write!(fmt, "<bad RPN expr, emptied stack>");
                }
            };
        }

        // First, build the expression tree from the RPN
        for op in self.iter() {
            match op {
                Err(err) => return write!(fmt, "<RPN error@${:04x}>", err.offset()),
                Ok(op) => {
                    let next_id = nodes.len();

                    let children = match op.arity() {
                        Arity::Literal => RpnTreeNodeType::Literal,
                        Arity::Unary => {
                            let operand = pop!(next_id);
                            RpnTreeNodeType::Unary(operand)
                        }
                        Arity::Binary => {
                            let rhs = pop!(next_id);
                            let lhs = pop!(next_id);
                            RpnTreeNodeType::Binary { lhs, rhs }
                        }
                    };
                    nodes.push(RpnTreeNode::new(op, children));
                    stack.push(next_id);
                }
            }
        }

        if stack.len() != 1 {
            return write!(fmt, "<bad RPN expr, finished with {} elems>", stack.len());
        }
        // Now, traverse the expression tree
        write_node(&nodes, stack[0], fmt)
    }
}
impl Display for RpnExpr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        if fmt.alternate() {
            self.fmt_infix(fmt)
        } else {
            self.fmt_rpn(fmt)
        }
    }
}

/// A RPN operation; since this means "operation on the RPN stack" here, this includes literals,
/// not just operators.
// We don't have `Eq` because symbol IDs are file-relative
#[derive(Debug, PartialEq)]
pub enum RpnOp<'a> {
    /// `+` operator.
    Add,
    /// `-` operator.
    Sub,
    /// `*` operator.
    Mul,
    /// `/` operator.
    Div,
    /// `%` operator.
    Mod,
    /// Unary `-` operator.
    Neg,
    /// `**` operator.
    Pow,
    /// `|` operator.
    BinOr,
    /// `&` operator.
    BinAnd,
    /// `^` operator.
    Xor,
    /// `~` operator.
    Cpl,
    /// `&&` operator.
    And,
    /// `||` operator.
    Or,
    /// `!` operator.
    Not,
    /// `==` operator.
    Eq,
    /// `!=` operator.
    Neq,
    /// `>` operator.
    Gt,
    /// `<` operator.
    Lt,
    /// `>=` operator.
    Gte,
    /// `<=` operator.
    Lte,
    /// `<<` operator.
    Lsh,
    /// `>>` operator.
    Rsh,
    /// `>>>` operator.
    Ursh,
    /// `BANK(Symbol)`
    BankSym(u32),
    /// `BANK("section")`
    BankSect(&'a [u8]),
    /// `BANK(@)`
    BankSelf,
    /// `SIZEOF("section")`
    SizeofSect(&'a [u8]),
    /// `STARTOF("section")`
    StartofSect(&'a [u8]),
    /// `SIZEOF(SectionType)`
    SizeofSectType(SectType),
    /// `STARTOF(SectionType)`
    StartofSectType(SectType),
    /// HRAM check (check if the value is in HRAM range, then `& 0xFF`).
    HramCheck,
    /// `rst` check (check if the value is a `rst` target, then `| 0xC7`).
    RstCheck,
    /// 32-bit literal.
    Int(u32),
    /// Symbol (referenced by 32-bit ID).
    Sym(u32),
}
/// A RPN operation's [arity].
///
/// [arity]: https://en.wikipedia.org/wiki/Arity
pub enum Arity {
    Literal,
    Unary,
    Binary,
}
impl RpnOp<'_> {
    /// The operation's arity.
    pub fn arity(&self) -> Arity {
        use Arity::*;
        use RpnOp::*;

        match self {
            Add => Binary,
            Sub => Binary,
            Mul => Binary,
            Div => Binary,
            Mod => Binary,
            Neg => Unary,
            Pow => Binary,
            BinOr => Binary,
            BinAnd => Binary,
            Xor => Binary,
            Cpl => Unary,
            And => Binary,
            Or => Binary,
            Not => Unary,
            Eq => Binary,
            Neq => Binary,
            Gt => Binary,
            Lt => Binary,
            Gte => Binary,
            Lte => Binary,
            Lsh => Binary,
            Rsh => Binary,
            Ursh => Binary,
            BankSym(..) => Literal,
            BankSect(..) => Literal,
            BankSelf => Literal,
            SizeofSect(..) => Literal,
            StartofSect(..) => Literal,
            SizeofSectType(..) => Literal,
            StartofSectType(..) => Literal,
            HramCheck => Unary,
            RstCheck => Unary,
            Int(..) => Literal,
            Sym(..) => Literal,
        }
    }

    /// The operation's precedence.
    ///
    /// # Panics
    ///
    /// This function panics if the operation is not a binary operator.
    pub fn precedence(&self) -> u8 {
        use RpnOp::*;

        // "Operators" in rgbasm(5)
        match self {
            Pow => 6,
            Mul | Div | Mod => 5,
            Lsh | Rsh | Ursh => 4,
            BinAnd | BinOr | Xor => 3,
            Add | Sub => 2,
            Eq | Neq | Gt | Lt | Gte | Lte => 1,
            And | Or => 0,

            // There is no precedence for non-binary operators...
            Neg | Cpl | Not | BankSym(..) | BankSect(..) | BankSelf | SizeofSect(..)
            | StartofSect(..) | SizeofSectType(..) | StartofSectType(..) | HramCheck | RstCheck
            | Int(..) | Sym(..) => unreachable!(),
        }
    }

    /// Whether this operation is associative; that is, if `A op (B op C) == (A op B) op C`.
    ///
    /// # Panics
    ///
    /// This function panics if the operation is not a binary operator.
    pub fn is_associative(&self) -> bool {
        use RpnOp::*;

        match self {
            Add | Mul | BinOr | BinAnd | Xor | And | Or => true,
            Sub | Div | Mod | Pow | Eq | Neq | Gt | Lt | Gte | Lte | Lsh | Rsh | Ursh => false,

            // There is no associativity for non-binary operators...
            Neg | Cpl | Not | BankSym(..) | BankSect(..) | BankSelf | SizeofSect(..)
            | StartofSect(..) | SizeofSectType(..) | StartofSectType(..) | HramCheck | RstCheck
            | Int(..) | Sym(..) => unreachable!(),
        }
    }

    /// Computes whether parens are needed (for pretty-printing) around a child expression, with
    /// the given parent.
    pub fn needs_parens(&self, parent: &RpnOp<'_>, is_left: bool) -> bool {
        use Arity::*;

        match self.arity() {
            // Literals have no precedence, and unary operations always have priority over binary
            Literal | Unary => false,
            Binary => {
                // For binary, we need to compare the precedences
                use Ordering::*;

                match parent.precedence().cmp(&self.precedence()) {
                    Less => false,   // The child has priority
                    Greater => true, // The parent has priority, so override that by using parens
                    Equal => {
                        // Parens are only required for the right branch if the parent operation is
                        // *not* associative with the child one (which implies they're the same)
                        !is_left && (self != parent || !self.is_associative())
                    }
                }
            }
        }
    }
}

impl Display for RpnOp<'_> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        use RpnOp::*;

        match self {
            Add => write!(fmt, "+"),
            Sub => write!(fmt, "-"),
            Mul => write!(fmt, "*"),
            Div => write!(fmt, "/"),
            Mod => write!(fmt, "%"),
            Neg => write!(fmt, "-()"),
            Pow => write!(fmt, "**"),
            BinOr => write!(fmt, "|"),
            BinAnd => write!(fmt, "&"),
            Xor => write!(fmt, "^"),
            Cpl => write!(fmt, "~"),
            And => write!(fmt, "&&"),
            Or => write!(fmt, "||"),
            Not => write!(fmt, "!"),
            Eq => write!(fmt, "=="),
            Neq => write!(fmt, "!="),
            Gt => write!(fmt, ">"),
            Lt => write!(fmt, "<"),
            Gte => write!(fmt, ">="),
            Lte => write!(fmt, "<="),
            Lsh => write!(fmt, "<<"),
            Rsh => write!(fmt, ">>"),
            Ursh => write!(fmt, ">>>"),
            BankSym(id) => write!(fmt, "BANK(Sym#{})", id),
            BankSect(name) => write!(fmt, "BANK(\"{}\")", String::from_utf8_lossy(&name)),
            BankSelf => write!(fmt, "BANK(@)"),
            SizeofSect(name) => write!(fmt, "SIZEOF(\"{}\")", String::from_utf8_lossy(&name)),
            StartofSect(name) => write!(fmt, "STARTOF(\"{}\")", String::from_utf8_lossy(&name)),
            SizeofSectType(sect_type) => write!(fmt, "SIZEOF({})", sect_type.name()),
            StartofSectType(sect_type) => write!(fmt, "STARTOF({})", sect_type.name()),
            HramCheck => write!(fmt, "HRAM?"),
            RstCheck => write!(fmt, "RST?"),
            Int(val) => write!(fmt, "${:04x}", val),
            Sym(id) => write!(fmt, "Sym#{}", id),
        }
    }
}

#[derive(Debug)]
struct RpnTreeNode<'op> {
    op: RpnOp<'op>,
    children: RpnTreeNodeType,
}
impl<'op> RpnTreeNode<'op> {
    pub fn new(op: RpnOp<'op>, children: RpnTreeNodeType) -> Self {
        Self { op, children }
    }
}
#[derive(Debug)]
enum RpnTreeNodeType {
    Literal,
    Unary(usize),
    Binary { lhs: usize, rhs: usize },
}

fn write_node(nodes: &[RpnTreeNode], id: usize, fmt: &mut Formatter) -> Result<(), fmt::Error> {
    use RpnOp::*;
    use RpnTreeNodeType::*;
    let node = &nodes[id];

    let write_child_node = |id, fmt: &mut Formatter, is_left: bool| {
        let child_node: &RpnTreeNode = &nodes[id];
        let needs_parens = child_node.op.needs_parens(&node.op, is_left);
        if needs_parens {
            write!(fmt, "(")?;
        }
        write_node(nodes, id, fmt)?;
        if needs_parens {
            write!(fmt, ")")?;
        }
        Ok(())
    };

    match node.children {
        // Literals are printed just the same
        Literal => write!(fmt, "{}", node.op),
        // Gulp, these can be a bit funky
        Unary(operand) => match node.op {
            // These two are printed like functions
            HramCheck | RstCheck => {
                write!(fmt, "{}(", node.op)?;
                write_child_node(operand, fmt, true)?;
                write!(fmt, ")")
            }
            // The rest is simply affixed to the node
            _ => {
                if matches!(node.op, Neg) {
                    write!(fmt, "-")?;
                } else {
                    write!(fmt, "{}", node.op)?;
                }
                write_child_node(operand, fmt, true)
            }
        },
        // Fairly uniform
        Binary { lhs, rhs } => {
            write_child_node(lhs, fmt, true)?;
            write!(fmt, " {} ", node.op)?;
            write_child_node(rhs, fmt, false)
        }
    }
}
