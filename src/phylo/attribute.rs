use crate::TreeFloat;
use crate::TreeInt;

use std::fmt::Debug;
use std::fmt::Display;
use std::str::FromStr;

// =============================================================================
// Type definitions
// =============================================================================

/// Represents an attribute that can be associated with tree nodes or branches.
///
/// Supports multiple data types and automatic type unification during tree validation.
#[derive(Clone, PartialEq)]
pub enum Attribute {
    Integer(TreeInt),
    Decimal(TreeFloat),
    Color(String),
    Text(String),
    List(Vec<AttributeValue>),
}

/// Represents a single attribute value within a list.
#[derive(Clone, PartialEq, Debug)]
pub enum AttributeValue {
    Integer(TreeInt),
    Decimal(TreeFloat),
    Color(String),
    Text(String),
}

/// Type signature for attribute values.
#[derive(Clone, PartialEq, Debug)]
pub enum AttributeValueType {
    Integer,
    Decimal,
    Color,
    Text,
}

/// Type signature for attributes.
#[derive(Clone, PartialEq, Debug)]
pub enum AttributeType {
    Integer,
    Decimal,
    Color,
    Text,
    List(Vec<AttributeValueType>),
}

// =============================================================================
// Helper functions
// =============================================================================

/// Check if a string represents a valid hex color pattern.
fn is_hex_color(s: &str) -> bool {
    if !s.starts_with('#') || s.len() != 7 {
        return false;
    }

    s[1..].chars().all(|c| c.is_ascii_hexdigit())
}

/// Convert a hex color string to uppercase format.
fn normalize_hex_color(s: &str) -> String {
    format!("#{}", &s[1..].to_uppercase())
}

// =============================================================================
// AttributeValue implementations
// =============================================================================

impl Display for AttributeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeValue::Integer(integer) => write!(f, "{}", integer),
            AttributeValue::Decimal(decimal) => write!(f, "{}", decimal),
            AttributeValue::Color(color) => write!(f, "{}", color),
            AttributeValue::Text(text) => write!(f, "{}", text),
        }
    }
}

impl From<String> for AttributeValue {
    fn from(s: String) -> Self {
        if let Ok(integer) = s.parse() {
            AttributeValue::Integer(integer)
        } else if let Ok(decimal) = s.parse() {
            AttributeValue::Decimal(decimal)
        } else if is_hex_color(&s) {
            AttributeValue::Color(normalize_hex_color(&s))
        } else {
            AttributeValue::Text(s)
        }
    }
}

impl From<&str> for AttributeValue {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

// =============================================================================
// Attribute implementations
// =============================================================================

impl Attribute {
    pub fn get_type(&self) -> AttributeType {
        match self {
            Attribute::Integer(_) => AttributeType::Integer,
            Attribute::Decimal(_) => AttributeType::Decimal,
            Attribute::Color(_) => AttributeType::Color,
            Attribute::Text(_) => AttributeType::Text,
            Attribute::List(values) => {
                let value_types = values
                    .iter()
                    .map(|v| match v {
                        AttributeValue::Integer(_) => {
                            AttributeValueType::Integer
                        }
                        AttributeValue::Decimal(_) => {
                            AttributeValueType::Decimal
                        }
                        AttributeValue::Color(_) => AttributeValueType::Color,
                        AttributeValue::Text(_) => AttributeValueType::Text,
                    })
                    .collect();
                AttributeType::List(value_types)
            }
        }
    }

    /// Convert an integer attribute to decimal if needed for type unification.
    pub fn unify_to_decimal(self) -> Self {
        match self {
            Attribute::Integer(i) => Attribute::Decimal(i as TreeFloat),
            Attribute::List(values) => {
                let unified_values = values
                    .into_iter()
                    .map(|v| match v {
                        AttributeValue::Integer(i) => {
                            AttributeValue::Decimal(i as TreeFloat)
                        }
                        other => other,
                    })
                    .collect();
                Attribute::List(unified_values)
            }
            other => other,
        }
    }

    /// Check if two attribute types can be unified.
    pub fn can_unify_types(
        type1: &AttributeType,
        type2: &AttributeType,
    ) -> bool {
        match (type1, type2) {
            // Same types are always compatible.
            (a, b) if a == b => true,

            // Integer and Decimal can be unified to Decimal.
            (AttributeType::Integer, AttributeType::Decimal)
            | (AttributeType::Decimal, AttributeType::Integer) => true,

            // Lists can be unified if they have the same length and compatible item types.
            (AttributeType::List(items1), AttributeType::List(items2)) => {
                items1.len() == items2.len()
                    && items1
                        .iter()
                        .zip(items2.iter())
                        .all(|(t1, t2)| Self::can_unify_value_types(t1, t2))
            }

            // All other combinations are incompatible.
            _ => false,
        }
    }

    /// Check if two attribute value types can be unified.
    fn can_unify_value_types(
        type1: &AttributeValueType,
        type2: &AttributeValueType,
    ) -> bool {
        match (type1, type2) {
            (a, b) if a == b => true,
            (AttributeValueType::Integer, AttributeValueType::Decimal)
            | (AttributeValueType::Decimal, AttributeValueType::Integer) => {
                true
            }
            _ => false,
        }
    }

    /// Get the unified type for two compatible attribute types.
    pub fn get_unified_type(
        type1: &AttributeType,
        type2: &AttributeType,
    ) -> Option<AttributeType> {
        if !Self::can_unify_types(type1, type2) {
            return None;
        }

        match (type1, type2) {
            (a, b) if a == b => Some(a.clone()),

            // Integer + Decimal = Decimal.
            (AttributeType::Integer, AttributeType::Decimal)
            | (AttributeType::Decimal, AttributeType::Integer) => {
                Some(AttributeType::Decimal)
            }

            // Lists: unify corresponding items.
            (AttributeType::List(items1), AttributeType::List(items2)) => {
                let unified_items: Vec<AttributeValueType> = items1
                    .iter()
                    .zip(items2.iter())
                    .map(|(t1, t2)| Self::get_unified_value_type(t1, t2))
                    .collect();
                Some(AttributeType::List(unified_items))
            }

            _ => None,
        }
    }

    /// Get the unified type for two compatible attribute value types.
    pub fn get_unified_value_type(
        type1: &AttributeValueType,
        type2: &AttributeValueType,
    ) -> AttributeValueType {
        match (type1, type2) {
            (a, b) if a == b => a.clone(),
            (AttributeValueType::Integer, AttributeValueType::Decimal)
            | (AttributeValueType::Decimal, AttributeValueType::Integer) => {
                AttributeValueType::Decimal
            }
            _ => type1.clone(), // Should not happen if can_unify_value_types returned true.
        }
    }
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text(arg0) => f.debug_tuple("Text").field(arg0).finish(),
            Self::Decimal(arg0) => {
                f.debug_tuple("Decimal").field(arg0).finish()
            }
            Self::Integer(arg0) => {
                f.debug_tuple("Integer").field(arg0).finish()
            }
            Self::Color(arg0) => f.debug_tuple("Color").field(arg0).finish(),
            Self::List(values) => f.debug_tuple("List").field(values).finish(),
        }
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Attribute::Integer(integer) => format!("{integer}"),
                Attribute::Decimal(decimal) => format!("{decimal}"),
                Attribute::Color(color) => color.to_string(),
                Attribute::Text(text) => text.to_string(),
                Attribute::List(values) => {
                    let items: Vec<String> = values
                        .iter()
                        .map(|v| match v {
                            AttributeValue::Integer(i) => i.to_string(),
                            AttributeValue::Decimal(d) => d.to_string(),
                            AttributeValue::Color(c) => c.clone(),
                            AttributeValue::Text(t) => t.clone(),
                        })
                        .collect();
                    format!("[{}]", items.join(","))
                }
            }
        )
    }
}

impl From<&str> for Attribute {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

impl From<String> for Attribute {
    fn from(s: String) -> Self {
        match s.parse() {
            Ok(attr) => attr,
            Err(_) => Attribute::Text(s),
        }
    }
}

// =============================================================================
// Parsing
// =============================================================================

/// Parse list syntax: [item1,item2,...]
fn parse_list_syntax(inner: &str) -> Result<Vec<AttributeValue>, ()> {
    if inner.is_empty() {
        return Ok(Vec::new());
    }

    let items: Vec<&str> = inner.split(',').collect();
    let mut values = Vec::new();

    for item in items {
        let trimmed = item.trim();
        if let Ok(integer) = trimmed.parse::<TreeInt>() {
            values.push(AttributeValue::Integer(integer));
        } else if let Ok(decimal) = trimmed.parse::<TreeFloat>() {
            values.push(AttributeValue::Decimal(decimal));
        } else if is_hex_color(trimmed) {
            values.push(AttributeValue::Color(normalize_hex_color(trimmed)));
        } else {
            values.push(AttributeValue::Text(trimmed.to_owned()));
        }
    }

    Ok(values)
}

/// Parse curly brace syntax: {item1,item2,...}
fn parse_curly_brace_syntax(inner: &str) -> Result<Vec<AttributeValue>, ()> {
    if inner.is_empty() {
        return Ok(Vec::new());
    }

    let items: Vec<&str> = inner.split(',').collect();
    let mut values = Vec::new();

    for item in items {
        let trimmed = item.trim();

        if (trimmed.starts_with('"') && trimmed.ends_with('"'))
            || (trimmed.starts_with('\'') && trimmed.ends_with('\''))
        {
            let unquoted = &trimmed[1..trimmed.len() - 1];
            values.push(AttributeValue::Text(unquoted.to_owned()));
        } else if is_hex_color(trimmed) {
            values.push(AttributeValue::Color(normalize_hex_color(trimmed)));
        } else if let Ok(integer) = trimmed.parse::<TreeInt>() {
            values.push(AttributeValue::Integer(integer));
        } else if let Ok(decimal) = trimmed.parse::<TreeFloat>() {
            values.push(AttributeValue::Decimal(decimal));
        } else {
            values.push(AttributeValue::Text(trimmed.to_owned()));
        }
    }

    Ok(values)
}

impl FromStr for Attribute {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('[') && s.ends_with(']') {
            let inner = &s[1..s.len() - 1];
            let values = parse_list_syntax(inner)?;
            Ok(Attribute::List(values))
        } else if s.starts_with('{') && s.ends_with('}') {
            let inner = &s[1..s.len() - 1];
            let values = parse_curly_brace_syntax(inner)?;
            Ok(Attribute::List(values))
        } else if let Ok(integer) = s.parse::<TreeInt>() {
            Ok(Attribute::Integer(integer))
        } else if let Ok(decimal) = s.parse::<TreeFloat>() {
            Ok(Attribute::Decimal(decimal))
        } else if is_hex_color(s) {
            Ok(Attribute::Color(normalize_hex_color(s)))
        } else {
            Ok(Attribute::Text(s.to_owned()))
        }
    }
}
