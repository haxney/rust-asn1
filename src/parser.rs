/*!
 * Taken from http://www.itu.int/rec/T-REC-X.680-200811-I
 *
 */

#![feature(phase)]

#[phase(plugin)]
extern crate peg_syntax_ext;

peg! asn1_grammar(r#"

// Section 12.38
reserved_word = "ABSENT" / "ENCODED" / "INTERSECTION" / "SEQUENCE"
   / "ABSTRACT-SYNTAX" / "ENCODING-CONTROL" / "ISO646String" / "SET"
   / "ALL"  / "END"  / "MAX"  / "SETTINGS"
   / "APPLICATION"  / "ENUMERATED"  / "MIN"  / "SIZE"
   / "AUTOMATIC"  / "EXCEPT"  / "MINUS-INFINITY"  / "STRING"
   / "BEGIN"  / "EXPLICIT"  / "NOT-A-NUMBER"  / "SYNTAX"
   / "BIT"  / "EXPORTS"  / "NULL"  / "T61String"
   / "BMPString"  / "EXTENSIBILITY"  / "NumericString"  / "TAGS"
   / "BOOLEAN"  / "EXTERNAL"  / "OBJECT"  / "TeletexString"
   / "BY"  / "FALSE"  / "ObjectDescriptor"  / "TIME"
   / "CHARACTER"  / "FROM"  / "OCTET"  / "TIME-OF-DAY"
   / "CHOICE"  / "GeneralizedTime"  / "OF"  / "TRUE"
   / "CLASS"  / "GeneralString"  / "OID-IRI"  / "TYPE-IDENTIFIER"
   / "COMPONENT"  / "GraphicString"  / "OPTIONAL"  / "UNION"
   / "COMPONENTS"  / "IA5String"  / "PATTERN"  / "UNIQUE"
   / "CONSTRAINED"  / "IDENTIFIER"  / "PDV"  / "UNIVERSAL"
   / "CONTAINING"  / "IMPLICIT"  / "PLUS-INFINITY"  / "UniversalString"
   / "DATE"  / "IMPLIED"  / "PRESENT"  / "UTCTime"
   / "DATE-TIME"  / "IMPORTS"  / "PrintableString"  / "UTF8String"
   / "DEFAULT"  / "INCLUDES"  / "PRIVATE"  / "VideotexString"
   / "DEFINITIONS"  / "INSTANCE"  / "REAL"  / "VisibleString"
   / "DURATION"  / "INSTRUCTIONS"  / "RELATIVE-OID"  / "WITH"
   / "EMBEDDED"  / "INTEGER"  / "RELATIVE-OID-IRI"

// Section 12.1.6
newline = [\n\x0B\x0C\r]

// Section 12.1.6
whitespace = [\t ] / newline

// Section 12.2
#[export]
typereference -> String
  = !reserved_word [A-Z] ("-"? [a-zA-Z0-9])* { match_str.to_string() }
"#)


#[cfg(test)]
mod tests {
    use super::asn1_grammar::typereference;

    #[test]
    pub fn test_typereference() {
        assert_eq!(typereference("B"), Ok("B".to_string()));
        assert_eq!(typereference("Bob"), Ok("Bob".to_string()));
        assert_eq!(typereference("B-ob"), Ok("B-ob".to_string()));
        // Must start with an uppercase letter
        assert!(typereference("b").is_err());
        assert!(typereference("bob").is_err());

        // Must not end with a hyphen
        assert!(typereference("B-").is_err());

        // Must not contain adjacent hyphens
        assert!(typereference("B--ob").is_err());

        // Must not contain whitespace
        assert!(typereference("B- ob").is_err());

        // Must not be a reserved word
        assert!(typereference("INTEGER").is_err());
    }
}
