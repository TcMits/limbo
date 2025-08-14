use std::error;
use std::fmt;

use crate::token::TokenType;

/// SQL lexer and parser errors
#[non_exhaustive]
#[derive(Debug, miette::Diagnostic)]
#[diagnostic()]
pub enum Error {
    /// Lexer error
    UnrecognizedToken(#[label("here")] miette::SourceSpan),
    /// Missing quote or double-quote or backtick
    UnterminatedLiteral(#[label("here")] miette::SourceSpan),
    /// Missing `]`
    UnterminatedBracket(#[label("here")] miette::SourceSpan),
    /// Missing `*/`
    UnterminatedBlockComment(#[label("here")] miette::SourceSpan),
    /// Invalid parameter name
    BadVariableName(#[label("here")] miette::SourceSpan),
    /// Invalid number format
    BadNumber(#[label("here")] miette::SourceSpan),
    // Bad fractional part of a number
    BadFractionalPart(#[label("here")] miette::SourceSpan),
    // Bad exponent part of a number
    BadExponentPart(#[label("here")] miette::SourceSpan),
    /// Invalid or missing sign after `!`
    ExpectedEqualsSign(#[label("here")] miette::SourceSpan),
    /// Hexadecimal integer literals follow the C-language notation of "0x" or "0X" followed by hexadecimal digits.
    MalformedHexInteger(#[label("here")] miette::SourceSpan),
    // parse errors
    // Unexpected end of file
    ParseUnexpectedEOF,
    // Unexpected token
    ParseUnexpectedToken {
        #[label("parsed to here")]
        parsed_offset: miette::SourceSpan,

        got: TokenType,
        expected: &'static [TokenType],
    },
    // Custom error message
    Custom(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::UnrecognizedToken(pos) => {
                write!(f, "unrecognized token at {pos:?}")
            }
            Self::UnterminatedLiteral(pos) => {
                write!(f, "non-terminated literal at {pos:?}")
            }
            Self::UnterminatedBracket(pos) => {
                write!(f, "non-terminated bracket at {pos:?}")
            }
            Self::UnterminatedBlockComment(pos) => {
                write!(f, "non-terminated block comment at {pos:?}")
            }
            Self::BadVariableName(pos) => write!(f, "bad variable name at {pos:?}"),
            Self::BadNumber(pos) => write!(f, "bad number at {pos:?}"),
            Self::BadFractionalPart(pos) => {
                write!(f, "bad fractional part at {pos:?}")
            }
            Self::BadExponentPart(pos) => {
                write!(f, "bad exponent part at {pos:?}")
            }
            Self::ExpectedEqualsSign(pos) => write!(f, "expected = sign at {pos:?}"),
            Self::MalformedHexInteger(pos) => {
                write!(f, "malformed hex integer at {pos:?}")
            }
            Self::ParseUnexpectedEOF => {
                write!(f, "unexpected end of file")
            }
            Self::ParseUnexpectedToken {
                parsed_offset,
                got,
                expected,
            } => {
                write!(
                    f,
                    "got unexpected token after parsing to offset {parsed_offset:?}: expected {expected:?}, found {got}",
                )
            }
            Self::Custom(ref s) => {
                write!(f, "custom error: {s}")
            }
        }
    }
}

impl error::Error for Error {}
