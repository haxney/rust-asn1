//! Defines the AST of the ASN.1 file.

use lexer::{number, identifier, identifier_str, modulereference, whitespace,
            noninteger_unicode_label, typereference_str, typereference_upper_str, valuereference};
use types::{ModuleIdentifier, DefinitiveObjIdComponent, DefinitiveIdentification, ArcIdentifier,
            UnresolvedSymbol, Exports, NamedBit, NamedBitValue, DefinedValue, ActualParameter,
            BuiltinType, CharacterString};

named!(definitive_oid_component<&str, DefinitiveObjIdComponent>,
    alt!(
        complete!(asn_ws!(pair!(identifier, delimited!(tag_s!("("), number, tag_s!(")")))))
            => { |(name, num)|
                DefinitiveObjIdComponent::NameAndNumberForm(name, num)
            } |
        identifier => { |name| DefinitiveObjIdComponent::NameForm(name) } |
        number => { |num| DefinitiveObjIdComponent::NumberForm(num) }));

named!(definitive_oid_list<&str, Vec<DefinitiveObjIdComponent> >,
    asn_ws!(
        delimited!(
            tag_s!("{"),
            many1!(definitive_oid_component),
            tag_s!("}"))));

named!(arc_identifier<&str, ArcIdentifier>,
    alt!(
        number => { |num| ArcIdentifier::IntegerLabel(num) } |
        noninteger_unicode_label => { |name| ArcIdentifier::NonIntegerLabel(name) }));

named!(iri_value<&str, Vec<ArcIdentifier> >,
    asn_ws!(
        delimited!(
            tag_s!("\"/"),
            separated_nonempty_list!(tag_s!("/"), arc_identifier),
            tag_s!("\""))));

named!(definitive_identification<&str, DefinitiveIdentification>,
    asn_ws!(
        do_parse!(
            oids: definitive_oid_list >>
            iri_list: opt!(iri_value) >>
            (DefinitiveIdentification {
                obj_id_components: oids,
                iri: iri_list.unwrap_or(vec![]),
            }))));

named!(module_identifier<&str, ModuleIdentifier>,
   asn_ws!(
       do_parse!(
           name: modulereference >>
           idents: opt!(definitive_identification) >>
           (ModuleIdentifier {
               module_reference: name,
               definitive_identification: idents
           }))));

named!(reference<&str, &str>,
    alt!(typereference_str | identifier_str | typereference_upper_str));

named!(symbol<&str, UnresolvedSymbol>,
    alt!(
        complete!(asn_ws!(terminated!(reference, tuple!(tag_s!("{"), tag_s!("}")))))
            => { |val| UnresolvedSymbol::parameterized(val) } |
        reference => { |val| UnresolvedSymbol::plain(val) }));

named!(symbol_list<&str, Vec<UnresolvedSymbol> >,
    // Doesn't work with `separated_list_complete!` for some reason.
    asn_ws!(separated_list!(complete!(tag_s!(",")), complete!(symbol))));

named!(exports<&str, Exports>,
    asn_ws!(
        alt!(
            delimited!(
                tag_s!("EXPORTS"),
                symbol_list,
                tag_s!(";")) => { |list| Exports::SymbolsExported(list) } |
            delimited!(
                tag_s!("EXPORTS"),
                tag_s!("ALL"),
                tag_s!(";")) => { |_| Exports::AllExported })));

/// `ActualParameter` as defined by X.683 §9.5.
named!(
    actual_parameter<&str, ActualParameter>,
    // Temporary placeholders
    alt!(
        tag_s!("VALUE") => { |_| ActualParameter::Value } |
        tag_s!("VALUESET") => { |_| ActualParameter::ValueSet }));

/// `ActualParamterList` as defined in X.683 §9.5.
named!(
    actual_parameter_list<&str, Vec<ActualParameter> >,
    asn_ws!(
        delimited!(
            tag_s!("{"),
            separated_nonempty_list!(tag_s!(","), actual_parameter),
            tag_s!("}"))));

/// Parser for `DefinedValue` as defined in X.680 §14.1.
named!(
    defined_value<&str, DefinedValue>,
    asn_ws!(
        do_parse!(
            mod_ref: opt!(terminated!(modulereference, tag_s!("."))) >>
            val_ref: valuereference >>
            param_val: opt!(actual_parameter_list) >>
            (DefinedValue::new(mod_ref, val_ref, param_val.unwrap_or_else(Vec::new))))));

named!(
    named_bit<&str, NamedBit>,
    map!(
        asn_ws!(
            tuple!(
                identifier,
                delimited!(
                    tag_s!("("),
                    alt!(number => { |num| NamedBitValue::Num(num) } |
                         defined_value => { |val| NamedBitValue::Ref(val) }),
                    tag_s!(")")
                )
            )
        ),
        NamedBit::new_tuple));

/// Parser for `BitStringType` as defined by X.680 §22.1.
named!(
    bitstring_type<&str, Vec<NamedBit> >,
    asn_ws!(
        do_parse!(
            tag_s!("BIT") >>
            tag_s!("STRING") >>
            bit_list: opt!(
                delimited!(
                    tag_s!("{"),
                    separated_nonempty_list!(
                        tag_s!(","),
                        named_bit),
                    tag_s!("}"))) >>
            (bit_list.unwrap_or(vec![])))));

/// Parser for `CharacterStringType` as defined in X.680 §40.1.
named!(
    character_string<&str, CharacterString>,
    alt!(
        tag_s!("BMPString") => { |_| CharacterString::BMPString } |
        tag_s!("GeneralString") => { |_| CharacterString::GeneralString } |
        tag_s!("GraphicString") => { |_| CharacterString::GraphicString } |
        tag_s!("IA5String") => { |_| CharacterString::IA5String } |
        tag_s!("ISO646String") => { |_| CharacterString::ISO646String } |
        tag_s!("NumericString") => { |_| CharacterString::NumericString } |
        tag_s!("PrintableString") => { |_| CharacterString::PrintableString } |
        tag_s!("TeletexString") => { |_| CharacterString::TeletexString } |
        tag_s!("T61String") => { |_| CharacterString::T61String } |
        tag_s!("UniversalString") => { |_| CharacterString::UniversalString } |
        tag_s!("UTF8String") => { |_| CharacterString::UTF8String } |
        tag_s!("VideotexString") => { |_| CharacterString::VideotexString } |
        tag_s!("VisibleString") => { |_| CharacterString::VisibleString } |
        asn_ws!(tuple!(tag_s!("CHARACTER"), tag_s!("STRING"))) => {
            |_| CharacterString::UnrestrictedString }));

named!(
    builtin_type<&str, BuiltinType>,
    alt_complete!(
        bitstring_type => { |bitstrings| BuiltinType::BitStringType(bitstrings) } |
        tag_s!("BOOLEAN") => { |_| BuiltinType::BooleanType } |
        character_string => { |str_type| BuiltinType::CharacterStringType(str_type) }
    )
);

#[cfg(test)]
mod tests {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    use super::*;
    use types;
    use types::{Identifier, ValueReference};
    use types::DefinitiveObjIdComponent::NumberForm;
    use types::ArcIdentifier::IntegerLabel;
    use nom::{IResult, Needed, ErrorKind};
    use nom::IResult::{Done, Incomplete, Error};

    fn done<T>(res: T) -> IResult<&'static str, T> {
        IResult::Done("", res)
    }

    fn name_form(name: &str) -> DefinitiveObjIdComponent {
        DefinitiveObjIdComponent::NameForm(types::Identifier::new(name))
    }

    fn name_and_number_form(name: &str, num: u64) -> DefinitiveObjIdComponent {
        DefinitiveObjIdComponent::NameAndNumberForm(types::Identifier::new(name), num)
    }

    fn non_int_label(name: &str) -> ArcIdentifier {
        ArcIdentifier::NonIntegerLabel(types::NonIntegerUnicodeLabel::new(name))
    }

    #[test]
    fn test_definitive_oid_component() {
        assert_eq!(definitive_oid_component("1"), done(NumberForm(1)));

        assert_eq!(
            definitive_oid_component("some-cool-name"),
            done(name_form("some-cool-name")));

        assert_eq!(
            definitive_oid_component("some-cool-name(4)"),
            done(name_and_number_form("some-cool-name", 4)));

        assert_eq!(
            definitive_oid_component("some-cool-name ( 4 )"),
            done(name_and_number_form("some-cool-name", 4)));

        assert_eq!(definitive_oid_component("some(4"), Done("(4", name_form("some")));
    }

    #[test]
    fn test_iri_value() {
        assert_eq!(iri_value(r#""/1""#), done(vec![IntegerLabel(1)]));
        assert_eq!(iri_value(r#""/1/abc""#),
                   done(vec![IntegerLabel(1), non_int_label("abc")]));

        // Missing closing quote
        assert_eq!(iri_value(r#""/1"#), Incomplete(Needed::Size(4)));

        assert_eq!(iri_value(r#""1""#), Error(ErrorKind::Tag));
    }

    #[test]
    fn test_definitive_oid_list() {
        // Normal test. Ending open paren allows nom to decide that there is no IRI.
        assert_eq!(
            definitive_oid_list("{1}"),
            done(vec![NumberForm(1)]));

        assert_eq!(definitive_oid_list("{ 1 }"), done(vec![NumberForm(1)]));

        assert_eq!(definitive_oid_list("{ 1 2 }"), done(vec![NumberForm(1), NumberForm(2)]));

        assert_eq!(
            definitive_oid_list("{ alice bob }"),
            done(vec![name_form("alice"), name_form("bob")]));

        assert_eq!(
            definitive_oid_list("{ alice (1) }"),
            done(vec![name_and_number_form("alice", 1)]));

        assert_eq!(
            definitive_oid_list("{ alice (1) bob }"),
            done(vec![name_and_number_form("alice", 1), name_form("bob")]));

        assert_eq!(
            definitive_oid_list("{ alice (1) bob carol (23) stuff }"),
            done(
                vec![
                    name_and_number_form("alice", 1),
                    name_form("bob"),
                    name_and_number_form("carol", 23),
                    name_form("stuff")]));
    }

    #[test]
    fn test_definitive_identification() {
        // Trailing `(` is to convince nom that there is no IRI
        assert_eq!(
            definitive_identification("{ alice (1) bob carol (23) stuff } ("),
            Done("(",
                 DefinitiveIdentification::new_no_iri(
                     vec![
                         name_and_number_form("alice", 1),
                         name_form("bob"),
                         name_and_number_form("carol", 23),
                         name_form("stuff")])));

        assert_eq!(
            definitive_identification(r#"{ alice (1) bob carol (23) stuff } "/thing/other/4" "#),
            done(
                DefinitiveIdentification::new(
                    vec![
                        name_and_number_form("alice", 1),
                        name_form("bob"),
                        name_and_number_form("carol", 23),
                        name_form("stuff")],
                    vec![
                        non_int_label("thing"),
                        non_int_label("other"),
                        IntegerLabel(4)])));
    }

    fn plain_sym(name: &str) -> UnresolvedSymbol {
        UnresolvedSymbol::plain(name)
    }

    fn param_sym(name: &str) -> UnresolvedSymbol {
        UnresolvedSymbol::parameterized(name)
    }

    #[test]
    fn test_symbol() {
        assert_eq!(symbol("alice"), done(plain_sym("alice")));

        assert_eq!(symbol("BOB"), done(plain_sym("BOB")));

        assert_eq!(symbol("Carol { }"), done(param_sym("Carol")));
    }

    #[test]
    fn test_symbol_list() {
        assert_eq!(
            symbol_list("OPERATION,ERROR,Rose-PDU{}"),
            done(
                vec![
                    plain_sym("OPERATION"),
                    plain_sym("ERROR"),
                    param_sym("Rose-PDU")]));

        assert_eq!(
            symbol_list("OPERATION, ERROR, Rose-PDU{}"),
            done(
                vec![
                    plain_sym("OPERATION"),
                    plain_sym("ERROR"),
                    param_sym("Rose-PDU")]));
    }

    #[test]
    fn test_exports() {
        assert_eq!(exports("EXPORTS ALL ;"), done(Exports::AllExported));

        assert_eq!(
            exports("EXPORTS OPERATION, ERROR, Rose-PDU{};"),
            done(Exports::SymbolsExported(vec![
                plain_sym("OPERATION"),
                plain_sym("ERROR"),
                param_sym("Rose-PDU")])));
    }

    #[test]
    fn test_bitstring_type() {
        fn num_bit(name: &str, num: u64) -> NamedBit {
            NamedBit::new(Identifier::new(name), NamedBitValue::Num(num))
        }

        fn ref_bit(name: &str, ref_name: &str) -> NamedBit {
            NamedBit::new(
                Identifier::new(name),
                NamedBitValue::Ref(DefinedValue::new_val(ValueReference::new(ref_name))),
            )
        }

        // Exclamation to indicate no parameters
        assert_eq!(bitstring_type("BIT STRING !"), Done("!", vec![]));
        assert_eq!(bitstring_type("BIT   \nSTRING!"), Done("!", vec![]));

        assert_eq!(bitstring_type("BIT   \nSTRING { a(3), b(a) }"),
                   done(vec![num_bit("a", 3), ref_bit("b", "a")]));
    }
}
