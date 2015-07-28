/*!
 * Taken from http://www.itu.int/rec/T-REC-X.680-200811-I
 *
 */

#![allow(dead_code)] // Until parser is complete. Too noisy otherwise

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

pub struct ModuleDefinition {
    identifier: ModuleIdentifier,
    enc_ref: EncodingReference,
    tag: Tag,
    extension: bool,
    body: Option<ModuleBody>,
    enc_controls: Vec<EncodingControl>,
}

pub enum ModuleIdentifier {
    ModuleReference(String),
    DefinitiveIdent(Option<DefinitiveIdentification>),
}

pub enum DefinitiveIdentification {
    DefinitiveOID(Vec<DefinitiveObjIdComponent>),
    DefinitiveOIDandIRI(Vec<DefinitiveOI>, Vec<ArcIdentifier>),
}

pub enum ArcIdentifier {
    IntegerArc(uint),
    NonIntArc(String),
}

pub enum DefinitiveObjIdComponent {
    NameForm(String),
    DefinitiveNumberForm(uint),
    DefinitiveNameAndNumberForm(String, uint),
}

pub enum Tag {
    ExplicitTag,
    ImplicitTag,
    AutomaticTag,
}

pub struct ModuleBody {
    exports: Option<Exports>,
    imports: Option<Imports>,
    assignments: Vec<Assignment>,
}

pub enum Exports {
    SymbolsExported(Vec<Symbol>),
    AllExported,
}

pub enum Imports {
    SymbolsImported(Vec<SymbolsFromModule>),
}

pub struct SymbolsFromModule {
    symbols: Vec<Symbol>,
    module: GlobalModuleReference,
}

pub struct GlobalModuleReference {
    module_ref: String,
    assigned_id: Option<AssignedIdentifier>,
}

pub enum AssignedIdentifier {
    ObjectIdentifierValue,
    DefinedValue,
}

pub enum Symbol {
    ReferenceSymbol(Reference),
    ParameterizedReferenceSymbol,
}

pub enum Reference {
    TypeReference(String),
    ValueReference(String),
    ObjectClassReference(String),
    ObjectReference(String),
    ObjectSetReference(String),
}

pub enum Assignment {
    TypeAssignment,
    ValueAssignment,
    XMLValueAssignment,
    ValueSetTypeAssignment,
    ObjectClassAssignment,
    ObjectAssignment,
    ObjectSetAssignment,
    ParameterizedAssignment,

}

peg_file! asn1_grammar("grammar.rustpeg")

#[cfg(test)]
mod tests {
    use super::asn1_grammar::{typereference, valuereference, comment,
                              realnumber, bstring, cstring};
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

    #[test]
    pub fn test_cstring() {
        assert_eq!(cstring(r#""""#), Ok("".to_string()));
        assert_eq!(cstring(r#""a""#), Ok("a".to_string()));
        assert_eq!(cstring(r#""s""t""#), Ok("s\"t".to_string()));
        assert_eq!(cstring("\"ABCDE FGH\nIJK\"\"XYZ\""),
                   Ok("ABCDE FGHIJK\"XYZ".to_string()));
        assert_eq!(cstring("\"A\tB \n C\n D\t\""),
                   Ok("A\tBCD\t".to_string()));
    }
}
