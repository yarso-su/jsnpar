//! A simple JSON parser using parser combinators (Educational purposes)
//!
//! # Example
//! ```
//! let json = r#"{"name": "yarso", "awesome": true}"#;
//! let result = jsnpar::parse(json);
//! ```
//! Returns a `Result` with the parsed value or an error message (`Result<JsonValue, String>`).
//!
//! The `JsonValue` type is a simple enum that represents the JSON value.
//!
//! ## Warning
//! This crate is not intended to be used in production, it is only for educational purposes.

mod parser;
mod value;

use parser::Parser;

pub use value::JsonValue;

/// Parse a JSON string into a `JsonValue` type.
pub fn parse(input: &str) -> Result<JsonValue, String> {
    match parser::json_value().parse(input) {
        Ok((_, value)) => Ok(value),
        Err(err) => Err(err.to_string()),
    }
}
