/*!
 * Taken from http://www.itu.int/rec/T-REC-X.680-200811-I
 *
 */

#[phase(plugin)]
extern crate peg_syntax_ext;

extern crate collections;
use std::collections::bitv::Bitv;

#[deriving(Clone, PartialEq, Eq, Show)]
struct RealNumber {
    int_part: uint,
    frac_part: Option<String>,
    exponent: Option<int>,
}

fn bstring_to_bitv(bs: Vec<char>) -> Bitv {
    bs.iter().filter_map(|&x| match x {
        '0' => Some(false),
        '1' => Some(true),
        _ => None,
    }).collect()
}

peg! asn1_grammar(r#"
{
  use super::RealNumber;
  use std::collections::bitv::Bitv;
}

use super::{RealNumber, bstring_to_bitv};
use std::collections::bitv::Bitv;

digit = [0-9]
digits = digit+

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

// Section 12.3
identifier -> String
  = [a-z] ("-"? [a-zA-Z0-9])* { match_str.to_string() }

// Section 12.4
#[export]
valuereference -> String
  = identifier

// Section 12.5
modulereference -> String
  = typereference

// Section 12.6
#[export]
comment
  = single_line_comment
  / multi_line_comment

single_line_comment
  = "--" (!newline .)*

multi_line_comment
  = "/*" (!"*/" .)* "*/"

// Section 12.8
number -> uint
  = "0" { 0 }
  / [1-9] digit* { from_str::<uint>(match_str).unwrap() }

signed_number -> int
  = "-"? number { from_str::<int>(match_str).unwrap() }

// Section 12.9
#[export]
realnumber -> RealNumber
  = n:number "."? !digit
    e:([eE] sn:signed_number { sn } )? {
                  RealNumber { int_part: n, frac_part: None, exponent: e}
    }
  / n:number
    f:("." digits { match_str.slice_from(1).to_string() })?
    e:([eE] sn:signed_number { sn } )? {
                  RealNumber { int_part: n, frac_part: f, exponent: e}
    }

// Sort of a hack to get individual characters
bstring_char -> char
  = ("0" / "1" / whitespace) { match_str.char_at(0) }

// Section 12.10
#[export]
bstring -> Bitv
  = "'" bs:bstring_char* "'B" { bstring_to_bitv(bs) }

// Section 12.11
xmlbstring -> Bitv
  = bs:bstring_char* { bstring_to_bitv(bs) }
"#)


#[cfg(test)]
mod tests {
    use super::asn1_grammar::{typereference, valuereference, comment,
                              realnumber, bstring};
    use super::RealNumber;
    use std::collections::bitv::Bitv;

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

    #[test]
    pub fn test_valuereference() {
        assert_eq!(valuereference("b"), Ok("b".to_string()));
        assert_eq!(valuereference("bob"), Ok("bob".to_string()));
        assert_eq!(valuereference("b-ob"), Ok("b-ob".to_string()));
        // Must start with a lowercase letter
        assert!(valuereference("B").is_err());
        assert!(valuereference("Bob").is_err());

        // Must not end with a hyphen
        assert!(valuereference("b-").is_err());

        // Must not contain adjacent hyphens
        assert!(valuereference("b--ob").is_err());

        // Must not contain whitespace
        assert!(valuereference("b- ob").is_err());
    }

    #[test]
    pub fn test_comment() {
        assert!(comment("-- stuff").is_ok());
        assert!(comment("-- stuff --").is_ok());
        assert!(comment("-- stuff -\n").is_err());
        assert!(comment("/* stuff\n\n */").is_ok());

        // A "/*" in a comment is not treated specially
        assert!(comment("-- stuff /* --").is_ok());
        assert!(comment("/* stuff").is_err());
    }

    fn realnum<T>(n: uint) -> Result<RealNumber, T> {
        Ok(RealNumber { int_part: n, frac_part: None, exponent: None })
    }

    fn realnum_f<T>(n: uint, f: &str) -> Result<RealNumber, T> {
        Ok(RealNumber { int_part: n, frac_part: Some(f.to_string()), exponent: None })
    }

    fn realnum_f_e<T>(n: uint, f: &str, e: int) -> Result<RealNumber, T> {
        Ok(RealNumber { int_part: n, frac_part: Some(f.to_string()), exponent: Some(e) })
    }

    fn realnum_e<T>(n: uint, e: int) -> Result<RealNumber, T> {
        Ok(RealNumber { int_part: n, frac_part: None, exponent: Some(e) })
    }

    #[test]
    pub fn test_realnumber() {
        assert_eq!(realnumber("1"), realnum(1));
        assert_eq!(realnumber("0"), realnum(0));

        // Cannot have a leading zero
        assert!(realnumber("00").is_err());
        assert!(realnumber("01").is_err());

        // Can have a trailing period without a factional part
        assert_eq!(realnumber("1."), realnum(1));
        assert_eq!(realnumber("1.0"), realnum_f(1, "0"));
        assert_eq!(realnumber("1.001"), realnum_f(1, "001"));

        // Exponents
        assert_eq!(realnumber("1.1e3"), realnum_f_e(1, "1", 3));
        assert_eq!(realnumber("1.1e-3"), realnum_f_e(1, "1", -3));
        assert_eq!(realnumber("1.e3"), realnum_e(1, 3));
        assert_eq!(realnumber("1e3"), realnum_e(1, 3));
        assert_eq!(realnumber("1.1E3"), realnum_f_e(1, "1", 3));
        assert_eq!(realnumber("1.1E-3"), realnum_f_e(1, "1", -3));

        // Leading zero in exponent
        assert!(realnumber("1.1E-03").is_err());
    }

    #[test]
    pub fn test_bstring() {
        assert_eq!(bstring("'000'B"), Ok(Bitv::with_capacity(3, false)));
        assert_eq!(bstring("''B"), Ok(Bitv::new()));

        let expected: Bitv = vec![false, true, false].iter().map(|n| *n).collect();
        let expect_ok = Ok(expected);
        assert_eq!(bstring("'010'B"), expect_ok);
        assert_eq!(bstring("'0 1\t0\n'B"), expect_ok);

        assert!(bstring("'010' B").is_err());
        assert!(bstring("'0 1\t0\n'b").is_err());
        assert!(bstring("'0 1\t0\n' B").is_err());
    }
}
