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
    enc_controls: Vec<EncodingControl>
}

pub enum DefinitiveIdentification {
    DefinitiveOID(Vec<DefinitiveObjId>),
    DefinitiveOIDandIRI(Vec<DefinitiveOI>, Vec<ArcIdentifier>)
}

pub enum DefinitiveObjIdComponent {
    NameForm(String),
    DefinitiveNumberForm(uint),
    DefinitiveNameAndNumberForm(String, uint)
}

pub enum Tag {
    ExplicitTag,
    ImplicitTag,
    AutomaticTag
}

pub struct ModuleBody {
    exports: Option<Exports>,
    imports: Option<Imports>,
    assignments: Vec<Assignment>
}

pub enum Exports {
    SymbolsExported(Vec<Symbol>),
    AllExported
}

pub enum Imports {
    SymbolsImported(Vec<SymbolsFromModule>)
}

pub struct SymbolsFromModule {
    symbols: Vec<Symbol>,
    module: GlobalModuleReference
}

pub struct GlobalModuleReference {
    module_ref: String,
    assigned_id: Option<AssignedIdentifier>
}

pub enum AssignedIdentifier {
    ObjectIdentifierValue,
    DefinedValue
}

pub enum Symbol {
    ReferenceSymbol(Reference),
    ParameterizedReferenceSymbol
}

pub enum Reference {
    TypeReference(String),
    ValueReference(String),
    ObjectClassReference(String),
    ObjectReference(String),
    ObjectSetReference(String)
}

pub enum Assignment {
    TypeAssignment,
    ValueAssignment,
    XMLValueAssignment,
    ValueSetTypeAssignment,
    ObjectClassAssignment,
    ObjectAssignment,
    ObjectSetAssignment,
    ParameterizedAssignment

}

peg! asn1_grammar(r#"
{
  use super::{RealNumber, ModuleDefinition};
  use std::collections::bitv::Bitv;
}

use super::{RealNumber, bstring_to_bitv, ModuleDefinition};
use std::collections::bitv::Bitv;
use std::str;

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

spacing = [\t ]

whitespace = spacing / newline

__ = whitespace*

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

// Section 12.12
hstring_char -> Option<char>
  = ([A-F0-9] / whitespace) {{
                  let c = match_str.char_at(0);
                  if c.is_whitespace() {
                    None
                  } else {
                    Some(c)
                  }
  }}

hstring -> String
  = "'" hs:hstring_char* "'H" { hs.iter().filter_map(|x| *x).collect() }

// Section 12.13
xmlhstring_char -> Option<char>
  = ([a-fA-F0-9] / whitespace) {{
                  let c = match_str.char_at(0).to_uppercase();
                  if c.is_whitespace() {
                    None
                  } else {
                    Some(c)
                  }
  }}

xmlhstring -> String
  = hs:xmlhstring_char* { hs.iter().filter_map(|x| *x).collect() }


cstring_char -> char
  = (!("\"" / newline) . / '""') {
                  if match_str.len() == 1 {
                    match_str.char_at(0)
                  } else {
                    '\x22' // Double quote
                  }
  }

cstring_basic -> String
  = cs:cstring_char* { cs.move_iter().collect() }

#[export]
// Section 12.14
//
// Yes, this is complex. Don't blame me! The spec calls for stripping spacing
// characters before and after newlines, but allowing them at the beginning and
// end of the string.
cstring -> String
  = "\"" cs:cstring_basic "\"" { cs }

  // Multi-line string. Always has at least one string in `lines` and one in
  // `tail`. If I were more clever, I could figure out a way to do most of this
  // within the PEG expressions.
  / "\"" lines:(cs:cstring_basic newline {cs})+ tail:cstring_basic "\"" {{
                  let mut mlines = lines.clone();
                  let head = mlines.shift().unwrap();
                  for line in mlines.mut_iter() {
                    *line = line.as_slice().trim().to_string();
                  }
                  mlines.unshift(head.as_slice().trim_right().to_string());
                  mlines.append_one(tail.as_slice().trim_left().to_string()).concat()
  }}

// Section 12.15
// TODO: xmlcstring

// Section 12.16
// TODO: simplestring

// Section 12.17
// TODO: tstring
// Oh god, it's horrible. It makes me want to re-think this whole endeavor.

// Section 12.18
// TODO: xmltstring

// Section 12.19
psname -> String
  = [A-Z] ("-"? [a-zA-Z0-9])* { match_str.to_string() }

// Section 12.20
asgn = "::="

// Section 12.21
range = ".."

// Section 12.22
ellipsis = "..."

// Section 12.23
lversion_brack = "[["

// Section 12.24
rversion_brack = "]]"

// Section 12.25
encodingreference -> String
  = [A-Z] ("-"? [A-Z0-9])* { match_str.to_string() }

// Section 12.26
integerUnicodeLabel -> uint
  = number

// Intentionally does not include HYPHEN MINUS
non_int_uni_label
  = [._~0-9A-Za-z]
                  / [\u00A0-\uDFFE]
                  / [\uF900-\uFDCF]
                  / [\uFDF0-\uFFEF]
                  / [\U00010000-\U0001FFFD]
                  / [\U00020000-\U0002FFFD]
                  / [U00030000-\U0003FFFD]
                  / [U00040000-\U0004FFFD]
                  / [U00050000-\U0005FFFD]
                  / [U00060000-\U0006FFFD]
                  / [U00070000-\U0007FFFD]
                  / [U00080000-\U0008FFFD]
                  / [U00090000-\U0009FFFD]
                  / [U000A0000-\U000AFFFD]
                  / [U000B0000-\U000BFFFD]
                  / [U000C0000-\U000CFFFD]
                  / [U000D0000-\U000DFFFD]
                  / [U000E1000-\U000EFFFD]

// Section 12.27
// References X.660 Section 7.5

// This does not quite match the rules. It is overly permissive in that it
// allows digit-only strings, which the spec says are forbidden.
non_integerUnicodeLabel -> String
  = non_int_uni_label ("-"? non_int_uni_label)* { match_str.to_string() }

// Section 12.28
xml_end_tag_start = "</"

// Section 12.29
xml_single_tag_end = "/>"

// Section 12.30
xml_true -> bool
  = "true" { true }

// Section 12.31
xml_xtrue -> bool
  = "true" / "1" { true }

// Section 12.32
xml_false -> bool
  = "false" { false }

// Section 12.33
xml_xfalse -> bool
  = "false" / "0" { false }

// Section 12.34
// TODO: xml_nan

// Section 12.35
// TODO: xml_inf

// Section 13.1
#[export]
ModuleDefinition -> ModuleDefinition
  = mi:ModuleIdentifier __
    "DEFINITIONS" __
    er:EncodingReference? __
    tag:Tag? __
    ext:ExtensionDefault
    "::=" __
    "BEGIN" __
    mb: ModuleBody? __
    ecs:EncodingControlSection* __
    "END" {
                  ModuleDefinition { identifier: mi,
                    enc_ref: er,
                    tag: tag,
                    extension: ext,
                    body: mb,
                    enc_controls: ecs
                  }
    }

ModuleIdentifier -> ModuleIdentifier
  = mr:modulereference __ di:DefinitiveIdentification? {
                  ModuleIdentifier {
                    mod_ref: mr,
                    def_id: di
                  }
  }

DefinitiveIdentification -> DefinitiveIdentification
  = doid:DefinitiveOID { DefinitiveOID(doid) }
  / doai:DefinitiveOIDandIRI { doai }

DefinitiveOID -> Vec<DefinitiveObjId>
  = "{" __ cl:DefinitiveObjIdComponent* __ "}" { cl }

DefinitiveOIDandIRI -> DefinitiveOIDandIRI
  = doid:DefinitiveOID __ iri:IRIValue { DefinitiveOIDandIRI(doid, iri) }

DefinitiveObjIdComponent -> DefinitiveObjIdComponent
  = nf:NameForm { NameForm(nf) }
  / dnf:DefinitiveNumberForm { DefinitiveNumberForm(dnf) }
  / dnanf:DefinitiveNameAndNumberForm {{
                  let (name, num) = dnanf;
                  DefinitiveNameAndNumberForm(name, num)
  }}

DefinitiveNumberForm -> uint
  = n:number { n }

DefinitiveNameAndNumberForm -> (String, uint)
  = id:identifier __ "(" __ dnf:DefinitiveNumberForm __ ")" {
                  (id, dnf)
  }

// `EncodingReferenceDefault` from spec
EncodingReference -> String
  = er:encodingreference __ "INSTRUCTIONS" { er }

// `TagDefault` from spec
Tag -> Tag
  = "EXPLICIT" __ "TAGS" { ExplicitTag}
  / "IMPLICIT" __ "TAGS" { ImplicitTag }
  / "AUTOMATIC" __ "TAGS" { AutomaticTag }

ExtensionDefault -> bool
  = "EXTENSIBILITY" __ "IMPLIED" { true }
  / "" { false }

ModuleBody -> ModuleBody
  = ex:Exports __ im:Imports __ al:AssignmentList {
                  ModuleBody {
                    exports: ex,
                    imports: im,
                    assignments: al
                  }
  }

Exports -> Option<Exports>
  = "EXPORTS" __ sl:SymbolList __ ";" { Some(SymbolsExported(sl)) }
  / "EXPORTS" __ "ALL" __ ";" { Some(AllExported) }
  / "" { None }

Imports -> Option<Imports>
  = "IMPORTS" __ si:SymbolsFromModule* __ ";" { Some(SymbolsImported(si)) }
  / "" { None }

SymbolsFromModule -> SymbolsFromModule
  = sl:SymbolList __ "FROM" __ gmr: GlobalModuleReference {
                  SymbolsFromModule{ symbols: sl, module: gmr }
  }

GlobalModuleReference -> GlobalModuleReference
  = mr:modulereference __ ai:AssignedIdentifier {
                  GlobalModuleReference {
                    module_ref: mr,
                    assigned_id: ai
                  }
  }

AssignedIdentifier -> Option<AssignedIdentifier>
  = oiv:ObjectIdentifierValue { Some(ObjectIdentifierValue(oiv)) }
  / dv:DefinedValue { Some(DefinedValue(dv)) }
  / "" { None }

SymbolList -> Vec<Symbol>
  = s:Symbol { vec![s] }
  / head:Symbol rest:("," __ s:Symbol { s }) {{
                  rest.unshift(head);
                  rest
  }}

Symbol -> Symbol
  = r:Reference { ReferenceSymbol(r) }
  / pr:ParameterizedReference { ParameterizedReferenceSymbol(pr) }

Reference -> Reference
  = typereference
  / valuereference
  / objectclassreference
  / objectreference
  / objectsetreference

Assignment -> Assignment
  = TypeAssignment
  / ValueAssignment
  / XMLValueAssignment
  / ValueSetTypeAssignment
  / ObjectClassAssignment
  / ObjectAssignment
  / ObjectSetAssignment
  / ParameterizedAssignment

"#)


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
