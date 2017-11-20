//! Defines the AST of the ASN.1 file.

use lexer::{number, identifier, modulereference, whitespace, noninteger_unicode_label};
use types::{ModuleIdentifier, DefinitiveObjIdComponent, DefinitiveIdentification, ArcIdentifier};

named!(module_identifier<&str, ModuleIdentifier>,
    do_parse!(
        name: modulereference >>
        idents: opt!(definitive_identification) >>
        (ModuleIdentifier {
            module_reference: name,
            definitive_identification: idents
        })));

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

#[cfg(test)]
mod tests {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    use super::*;
    use types;
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
}
