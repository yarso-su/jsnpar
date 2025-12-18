use std::collections::HashMap;

/// Represents a JSON value.
/// - Null
/// - Boolean(bool)
/// - Number(f64)
/// - String(String)
/// - Array(Vec<JsonValue>)
/// - Object(HashMap<String, JsonValue>)
#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}
