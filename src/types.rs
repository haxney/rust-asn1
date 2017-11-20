use std::collections::HashSet;

/// An `identifier` lexical item as specified by X.680 §12.3.
#[derive(Debug, PartialEq, Eq)]
pub struct Identifier(String);

impl Identifier {
    pub fn new<S>(name: S) -> Identifier
    where
        S: Into<String>,
    {
        Identifier(name.into())
    }
}

/// A `valuereference` lexical item as specified by X.680 §12.4.
#[derive(Debug, PartialEq, Eq)]
pub struct ValueReference(String);

impl ValueReference {
    pub fn new<S>(name: S) -> ValueReference
    where
        S: Into<String>,
    {
        ValueReference(name.into())
    }
}

/// A `typereference` lexical item as specified by X.680 §12.2.
#[derive(Debug, PartialEq, Eq)]
pub struct TypeReference(String);

impl TypeReference {
    pub fn new<S>(name: S) -> TypeReference
    where
        S: Into<String>,
    {
        TypeReference(name.into())
    }
}

/// A `modulereference` lexical item as specified by X.680 §12.5.
#[derive(Debug, PartialEq, Eq)]
pub struct ModuleReference(String);

impl ModuleReference {
    pub fn new<S>(name: S) -> ModuleReference
    where
        S: Into<String>,
    {
        ModuleReference(name.into())
    }
}

/// An `encodingreference` lexical item as specified by X.680 §12.25.
#[derive(Debug, Eq, PartialEq)]
pub struct EncodingReference(String);

impl EncodingReference {
    pub fn new<S: Into<String>>(name: S) -> EncodingReference {
        EncodingReference(name.into())
    }
}

/// A `non-integerUnicodeLabel` lexical item as specified by X.680 §12.27 and X.660 §7.5.
#[derive(Debug, Eq, PartialEq)]
pub struct NonIntegerUnicodeLabel(String);

impl NonIntegerUnicodeLabel {
    pub fn new<S: Into<String>>(name: S) -> NonIntegerUnicodeLabel {
        NonIntegerUnicodeLabel(name.into())
    }
}

lazy_static! {
/// Reserved keywords as specified in §12.38
pub static ref RESERVED_WORDS: HashSet<&'static str> = vec![
    "ABSENT",
    "ENCODED",
    "INTERSECTION",
    "SEQUENCE",
    "ABSTRACT-SYNTAX",
    "ENCODING-CONTROL",
    "ISO646String",
    "SET",
    "ALL",
    "END",
    "MAX",
    "SETTINGS",
    "APPLICATION",
    "ENUMERATED",
    "MIN",
    "SIZE",
    "AUTOMATIC",
    "EXCEPT",
    "MINUS-INFINITY",
    "STRING",
    "BEGIN",
    "EXPLICIT",
    "NOT-A-NUMBER",
    "SYNTAX",
    "BIT",
    "EXPORTS",
    "NULL",
    "T61String",
    "BMPString",
    "EXTENSIBILITY",
    "NumericString",
    "TAGS",
    "BOOLEAN",
    "EXTERNAL",
    "OBJECT",
    "TeletexString",
    "BY",
    "FALSE",
    "ObjectDescriptor",
    "TIME",
    "CHARACTER",
    "FROM",
    "OCTET",
    "TIME-OF-DAY",
    "CHOICE",
    "GeneralizedTime",
    "OF",
    "TRUE",
    "CLASS",
    "GeneralString",
    "OID-IRI",
    "TYPE-IDENTIFIER",
    "COMPONENT",
    "GraphicString",
    "OPTIONAL",
    "UNION",
    "COMPONENTS",
    "IA5String",
    "PATTERN",
    "UNIQUE",
    "CONSTRAINED",
    "IDENTIFIER",
    "PDV",
    "UNIVERSAL",
    "CONTAINING",
    "IMPLICIT",
    "PLUS-INFINITY",
    "UniversalString",
    "DATE",
    "IMPLIED",
    "PRESENT",
    "UTCTime",
    "DATE-TIME",
    "IMPORTS",
    "PrintableString",
    "UTF8String",
    "DEFAULT",
    "INCLUDES",
    "PRIVATE",
    "VideotexString",
    "DEFINITIONS",
    "INSTANCE",
    "REAL",
    "VisibleString",
    "DURATION",
    "INSTRUCTIONS",
    "RELATIVE-OID",
    "WITH",
    "EMBEDDED",
    "INTEGER",
    "RELATIVE-OID-IRI",
  ].into_iter().collect();
}
