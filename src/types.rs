use std::collections::HashSet;

/// An `identifier` lexical item as specified by X.680 §12.3.
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Identifier(String);

impl Identifier {
    pub fn new<S>(name: S) -> Identifier
        where S: Into<String> {
        Identifier(name.into())
    }
}

/// A `typereference` lexical item as specified by X.680 §12.2.
#[derive(Debug)]
#[derive(PartialEq)]
pub struct TypeReference(String);

impl TypeReference {
    pub fn new<S>(name: S) -> TypeReference
        where S: Into<String> {
        TypeReference(name.into())
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