use std::str::FromStr;
use nom::{IResult, Needed};
use types::{Identifier, TypeReference, RESERVED_WORDS, ValueReference, ModuleReference,
            EncodingReference, NonIntegerUnicodeLabel};
use bit_vec::BitVec;

/// Returns `true` if the character is a newline character according to ASN.1.
///
/// Defined in X.680 §12.1.6
fn is_newline(ch: char) -> bool {
    match ch {
        // LINE FEED
        '\n' => true,

        // VERTICAL TABULATION
        '\x0B' => true,

        // FORM FEED
        '\x0C' => true,

        // CARRIAGE RETURN
        '\x0D' => true,
        _ => false,
    }
}

named!(newline<&str, &str>,
       alt!(
           tag_s!("\n") |
           tag_s!("\x0B") |
           tag_s!("\x0C") |
           tag_s!("\x0D")
       )
);

/// Returns `true` if the character is a non-newline whitespace character.
///
/// Defined in §12.1.6
fn is_separator(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\u{A0}' => true,
        _ => false,
    }
}

named!(separator<&str, &str>, take_while_s!(is_separator));

/// Returns `true` if the character is a whitespace character according to ASN.1.
///
/// Defined in X.680 §12.1.6
fn is_whitespace(ch: char) -> bool {
    is_separator(ch) || is_newline(ch)
}

/// Parse whitespace according to §12.1.6
named!(pub whitespace<&str, &str>, take_while_s!(is_whitespace));

/// Input consuming macro to wrap a matcher. This is a version of nom's `ws!` macro using the
/// whitespace set of ASN.1.
macro_rules! asn_ws (
  ($i:expr, $($args:tt)*) => (
    {
      sep!($i, whitespace, $($args)*)
    }
  )
);

/// An identifier-like sequence that begins with an uppercase letter.
named!(upper_identifier<&str, &str>,
    re_find!(r"^[A-Z]([a-zA-Z0-9]+|-[a-zA-Z0-9])*"));

// Parse a `typereference` string and filters out `RESERVED_WORDS`.
named!(typereference_str<&str, &str>,
    verify!(
        upper_identifier,
        |ref_name| !RESERVED_WORDS.contains(ref_name)));

/// Parse a `TypeReference` according to §12.2.
named!(typereference<&str, TypeReference>, map!(typereference_str, TypeReference::new));


// Parse an identifier as a string.
named!(identifier_str<&str, &str>, re_find!(r"^[a-z]([a-zA-Z0-9]+|-[a-zA-Z0-9])*"));

/// Parse an `Identifier` according to §12.3.
///
/// Starts with a lowercase letter, can contain letters, digits, or hyphens, but cannot contain a
/// double hyphen or end with a hyphen.
named!(pub identifier<&str, Identifier>, map!(identifier_str, Identifier::new));

/// Parse a `ValueReference` according to §12.4.
named!(valuereference<&str, ValueReference>, map!(identifier_str, ValueReference::new));

/// Parse a `ModuleReference` according to §12.5.
named!(pub modulereference<&str, ModuleReference>, map!(typereference_str, ModuleReference::new));

/// Takes input until it finds the end of a single-line comment. This will not consume the ending
/// character(s) of the comment.
fn take_until_single_line_comment_end(input: &str) -> IResult<&str, &str> {
    let mut offset = input.len();
    let mut chars = input.char_indices().peekable();
    loop {
        match chars.next() {
            Some((i, c)) => {
                match c {
                    '-' => {
                        match chars.peek() {
                            Some(&(_, pc)) => {
                                if pc == '-' {
                                    offset = i;
                                    break;
                                } else {
                                    // Safe to advance because we know that the peeked char is not
                                    // a dash.
                                    chars.next();
                                    continue;
                                }
                            }
                            // Got a single "-" at the end of the string. Consider it to be part of
                            // the comment.
                            None => {
                                // Normally, adding 1 to a byte offset in a `&str` could lead to an
                                // invalid access of a non-character boundary. In this case, we
                                // know that the last character was a "-", which is a single byte.
                                offset = i + 1;
                                break;
                            }
                        }
                    }
                    _ if is_newline(c) => {
                        offset = i;
                        break;
                    }
                    _ => continue,
                }
            }
            None => break,
        }
    }

    if offset < input.len() {
        IResult::Done(&input[offset..], &input[..offset])
    } else {
        IResult::Done("", input)
    }
}

/// Parse a single-line comment.
///
/// Defined in X.680 §12.6.3
named!(single_line_comment<&str, &str>,
    delimited!(
        tag_s!("--"),
        take_until_single_line_comment_end,
        alt!(newline | tag_s!("--"))
    )
);

fn offset<T: AsRef<[u8]>>(base: T, sub: T) -> usize {
    let fst = base.as_ref().as_ptr();
    let snd = sub.as_ref().as_ptr();

    snd as usize - fst as usize
}

/// Parse a multi-line comment. Multi-line comments in ASN.1 can be nested arbitrarily, so the
/// following is a valid comment:
///
/// ```notrust
/// /* Outer
///  * /* Inner */
///  * Still a comment
///  */
/// ```
///
/// Multi-line comments must be closed. If the end of the file is reached before the closing tag is
/// found, this is an error.
///
/// Defined in X.680 §12.6.4
fn multi_line_comment(input: &str) -> IResult<&str, &str> {
    let mut offset = input.len();

    let (inside_comment, _) = try_parse!(input, tag_s!("/*"));
    let mut chars = inside_comment.char_indices().peekable();

    // The previous `try_parse!` has already consumed an opening tag, so the beginning nesting
    // level is 1.
    let mut nest_level: u32 = 1;
    loop {
        let next_char = chars.next();

        match next_char {
            Some((i, c)) => {
                match c {
                    '/' => {
                        match chars.peek() {
                            Some(&(_, pc)) => {
                                if pc == '*' {
                                    nest_level += 1;
                                } else {
                                    continue;
                                }
                            }
                            // Got a single "/" at the end of the string. This is a parse error, as
                            // all multi-line comments must be closed.
                            None => {
                                return IResult::Incomplete(Needed::Unknown);
                            }
                        }
                    }
                    '*' => {
                        match chars.peek() {
                            Some(&(_, pc)) => {
                                if pc == '/' {
                                    // About to close the comment.
                                    if nest_level == 1 {
                                        offset = i;
                                        break;
                                    }
                                    nest_level -= 1;
                                } else {
                                    continue;
                                }
                            }
                            // Got a single "*" at the end of the string. This is a parse error, as
                            // all multi-line comments must be closed.
                            None => {
                                return IResult::Incomplete(Needed::Size(1));
                            }
                        }
                    }
                    _ => continue,
                }
            }
            None => break,
        }
    }

    if offset + 2 <= inside_comment.len() {
        // Consume the closing comment characters
        IResult::Done(&inside_comment[(offset + 2)..], &inside_comment[..offset])
    } else {
        IResult::Done("", inside_comment)
    }
}

/// Parse a comment.
///
/// Defined in X.680 §12.6.
named!(comment<&str, &str>,
    alt!(
        single_line_comment |
        multi_line_comment
    )
);

/// Parse an integer.
///
/// Defined in X.680 §12.8.
named!(pub number<&str, u64>,
    map_res!(
        re_find!(r"^(0|[1-9]\d*)"),
        FromStr::from_str
    )
);

/// Parse an `f64`.
///
/// Defined in X.680 §12.9.
named!(realnumber<&str, f64>,
   map_res!(
       re_find!(r"^\d+(\.\d*)?([eE]-?(0|[1-9]\d*))?"),
       FromStr::from_str
   )
);

/// Parse a bit string without any wrapping. Used for the various bstring types.
named!(plain_bstring<&str, BitVec>,
    fold_many0!(
        asn_ws!(alt!(tag_s!("0") => { |_| false } | tag_s!("1") => { |_| true })),
        BitVec::new(),
        |mut acc: BitVec, item| {
            acc.push(item);
            acc
        }));

/// Parse a `bstring` as defined in §12.10.
named!(bstring<&str, BitVec>,
    delimited!(
        tag_s!("'"),
        plain_bstring,
        tag_s!("'B")));

/// Parse an `xmlbstring` as defined in §12.11.
named!(xmlbstring<&str, BitVec>, call!(plain_bstring));

/// Parses a single hex character to a `u8`, uppercase only.
named!(hstring_single<&str, u8>,
    map_res!(
        re_find!(r"^[0-9A-F]"),
        |chr| u8::from_str_radix(chr, 16)));

/// Parses a single hex character, upper or lowercase.
named!(hstring_single_lower<&str, u8>,
    map_res!(
        re_find!(r"^[0-9A-Fa-f]"),
        |chr| u8::from_str_radix(chr, 16)));

/// Parse an `hstring` as defined in §12.12.
///
/// Parses a plain hex string to a vector of `u8`s. Note that each element of the vector corresponds
/// to a single digit of the input hex, so the maximum value of each `u8` is `15`. This is done to
/// support the requirement that `hstring` types be able to have an odd number of digits.
named!(hstring<&str, Vec<u8> >,
    delimited!(
        tag_s!("'"),
        fold_many0!(
            asn_ws!(hstring_single),
            Vec::new(),
            |mut acc: Vec<_>, item| {
                acc.push(item);
                acc
            }),
        tag_s!("'H")));

/// Parse an `xmlhstring` as defined in §12.13.
///
/// See format of [`hstring`]
named!(xmlhstring<&str, Vec<u8> >,
    fold_many0!(
        asn_ws!(hstring_single_lower),
        Vec::new(),
        |mut acc: Vec<_>, item| {
            acc.push(item);
            acc
        }));

/// A single-line string that ends in a newline and does not have any escaped `"`.
named!(string_segment_newline_end<&str, &str>,
    map!(
        re_capture!(r"^([^\x22\n\x0B\x0C\x0D]*?)([ \t\xA0]*[\n\x0B\x0C\x0D])"),
    |v| v[1]));

/// Parses an unwrapped `cstring` up to an unmatched `"`
named!(plain_cstring<&str, String>,
    fold_many0!(
        alt!(
            string_segment_newline_end |
            is_not_s!("\"\n\x0B\x0C\x0D") |
            map!(
                complete!(tag_s!("\"\"")),
                |_| "\"")),
        String::new(),
        |mut acc: String, item| {
            acc.push_str(item);
            acc
        }));

/// Parse the characters of a `cstring` as defined in §12.14.
///
/// The `cstring` type may span multiple lines and is enclosed by double quotes. A pair of double
/// quotes is used to escape a single `"` character. Newlines and the spaces around them are
/// ignored.
///
/// Does not handle any of the "printed representation" special cases documented in §12.14.2. Why a
/// computer protocol specification would worry so much about printed representations is a mystery.
named!(cstring<&str, String>,
    delimited!(
        tag_s!("\""),
        plain_cstring,
        tag_s!("\"")));

// Skipping xmlcstring for now. It is super weird.

/// Parser for `simplestring` type as defined in §12.16.
///
/// It is similar to `cstring`, but it does not have escaping for quotation mark characters.
named!(simplestring<&str, &str>,
    delimited!(
        tag_s!("\""),
        is_not_s!("\""),
        tag_s!("\"")
    ));

/// Parser for `tstring` as defined in §12.17.
named!(tstring<&str, &str>,
    delimited!(
        tag_s!("\""),
        is_a_s!("0123456789+-:.,/CDHMRPSTWYZ"),
        tag_s!("\"")));

named!(xmltstring<&str, &str>, call!(tstring));


/// Parser for `psname` as defined in §12.19.
named!(psname<&str, &str>, call!(upper_identifier));

/// Parser for `encodingreference` as defined in §12.25.
named!(encodingreference<&str, EncodingReference>,
    map!(
        verify!(
            re_find!(r"^[A-Z]([A-Z0-9]+|-[a-zA-Z0-9])*"),
            |ref_name| !RESERVED_WORDS.contains(ref_name)),
        EncodingReference::new));

/// Lexer for the character ranges for a non-integer unicode label as defined in X.660 §7.5.2.
fn non_integer_unicode_char(ch: char) -> bool {
    match ch {
        '-' |
        '.' |
        '_' |
        '~' |
        '0'...'9' |
        'A'...'Z' |
        'a'...'z' |
        '\u{000A0}'...'\u{0D7AF}' |
        '\u{0F900}'...'\u{0FDCF}' |
        '\u{0FDF0}'...'\u{0FFEF}' |
        '\u{10000}'...'\u{1FFFD}' |
        '\u{20000}'...'\u{2FFFD}' |
        '\u{30000}'...'\u{3FFFD}' |
        '\u{40000}'...'\u{4FFFD}' |
        '\u{50000}'...'\u{5FFFD}' |
        '\u{60000}'...'\u{6FFFD}' |
        '\u{70000}'...'\u{7FFFD}' |
        '\u{80000}'...'\u{8FFFD}' |
        '\u{90000}'...'\u{9FFFD}' |
        '\u{A0000}'...'\u{AFFFD}' |
        '\u{B0000}'...'\u{BFFFD}' |
        '\u{C0000}'...'\u{CFFFD}' |
        '\u{D0000}'...'\u{DFFFD}' |
        '\u{E1000}'...'\u{EFFFD}' => true,
        _ => false,
    }
}

/// Returns `true` if `name` adheres to the rules of X.660 §7.5. Assumes that the characters are in
/// the proper ranges as verified by `non_integer_unicode_char`.
fn non_integer_unicode_valid(name: &str) -> bool {
    // TODO(haxney): Use one iteration for whole function.

    // §7.5.1: not all characters are 0-9
    if !name.chars().any(|ch| !ch.is_digit(10)) {
        return false;
    }

    // §7.5.4: Does not start with `HYPHEN MINUS`
    let mut chars = name.chars();
    if let Some(ch) = chars.next() {
        if ch == '-' {
            return false;
        }
    }

    // The third and fourth characters may not be `HYPHEN MINUS`.
    chars.next();
    let third = chars.next();
    let fourth = chars.next();
    if let (Some(third_char), Some(fourth_char)) = (third, fourth) {
        if third_char == '-' && fourth_char == '-' {
            return false;
        }
    }

    // Does not end with `HYPHEN MINUS`.
    if let Some(last) = name.chars().last() {
        last != '-'
    } else {
        true
    }
}

/// Lexer for `non-integerUnicodeLabel` as defined in §12.27 and X.660 §7.5.
named!(pub noninteger_unicode_label<&str, NonIntegerUnicodeLabel>,
    map!(
        verify!(
            take_while1_s!(non_integer_unicode_char),
            non_integer_unicode_valid),
        NonIntegerUnicodeLabel::new));

#[cfg(test)]
mod tests {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    use super::*;

    use nom::IResult::{Done, Incomplete, Error};
    use nom::{IResult, Needed, ErrorKind};
    use types::{Identifier, ValueReference, TypeReference, ModuleReference};
    use std::fmt::Debug;
    use bit_vec::BitVec;

    /// Simple way of specifying an `IResult::Done` with no remaining input.
    macro_rules! done_result (
        ($res:expr) => (
            IResult::Done("", $res)
        )
    );

    fn done_string(s: &str) -> IResult<&str, String> {
        IResult::Done("", s.to_string())
    }

    /// Run tests for parsers which behave like `TypeReference`. Currently, this is only
    /// `TypeReference` and `ModuleReference`.
    fn typereference_like_tests<F, T, N>(parser: F, struct_maker: N)
    where
        T: Debug + PartialEq,
        F: Fn(&str) -> IResult<&str, T>,
        N: Fn(&'static str) -> T,
    {
        assert_eq!(parser("A"), done_result!(struct_maker("A")));
        assert_eq!(parser("Good"), done_result!(struct_maker("Good")));
        assert_eq!(
            parser("With-Dashes"),
            done_result!(struct_maker("With-Dashes"))
        );
        assert_eq!(parser("SpaCe "), Done(" ", struct_maker("SpaCe")));
        assert_eq!(
            parser("DOUBLE--hypen"),
            Done("--hypen", struct_maker("DOUBLE"))
        );
        assert_eq!(parser("-start"), Error(ErrorKind::RegexpFind));
        assert_eq!(parser("lower"), Error(ErrorKind::RegexpFind));
        assert_eq!(parser("lower"), Error(ErrorKind::RegexpFind));

        // ABSENT is a reserved word
        assert_eq!(parser("ABSENT"), Error(ErrorKind::Verify));
    }

    /// Run tests for parsers which behave like `Identifier`. Currently, this is only `Identifier`
    /// and `ValueReference`.
    fn identifier_like_tests<F, T, N>(parser: F, struct_maker: N)
    where
        T: Debug + PartialEq,
        F: Fn(&str) -> IResult<&str, T>,
        N: Fn(&'static str) -> T,
    {
        assert_eq!(parser("a"), done_result!(struct_maker("a")));
        assert_eq!(parser("aB3"), done_result!(struct_maker("aB3")));
        assert_eq!(parser("aB3-"), Done("-", struct_maker("aB3")));
        assert_eq!(
            parser("dash-middle"),
            done_result!(struct_maker("dash-middle"))
        );
        assert_eq!(parser("space "), Done(" ", struct_maker("space")));
        assert_eq!(
            parser("double--hypen"),
            Done("--hypen", struct_maker("double"))
        );
        assert_eq!(parser("-start"), Error(ErrorKind::RegexpFind));
        assert_eq!(parser("Capital"), Error(ErrorKind::RegexpFind));
    }

    #[test]
    fn test_typereference() {
        typereference_like_tests(typereference, TypeReference::new);
    }

    #[test]
    fn test_identifier() {
        identifier_like_tests(identifier, Identifier::new);
    }

    #[test]
    fn test_valuereference() {
        identifier_like_tests(valuereference, ValueReference::new)
    }

    #[test]
    fn test_modulereference() {
        typereference_like_tests(modulereference, ModuleReference::new);
    }

    #[test]
    fn test_newline() {
        assert_eq!(newline("\n"), done_result!("\n"));
        assert_eq!(newline("\x0B"), done_result!("\x0B"));
    }


    #[test]
    fn test_take_until_single_line_comment_end() {
        assert_eq!(
            take_until_single_line_comment_end(" thing\nmore"),
            Done("\nmore", " thing")
        );
        assert_eq!(
            take_until_single_line_comment_end(" thing\n"),
            Done("\n", " thing")
        );
        assert_eq!(
            take_until_single_line_comment_end(" thing--"),
            Done("--", " thing")
        );
        assert_eq!(
            take_until_single_line_comment_end(" dash-in-middle--"),
            Done("--", " dash-in-middle")
        );
        assert_eq!(
            take_until_single_line_comment_end(" dash-in-middle\x0C"),
            Done("\x0C", " dash-in-middle")
        );

        // Empty comments
        assert_eq!(take_until_single_line_comment_end("\n"), Done("\n", ""));
        assert_eq!(take_until_single_line_comment_end("--"), Done("--", ""));

        assert_eq!(
            take_until_single_line_comment_end("stuff-"),
            done_result!("stuff-")
        );
        assert_eq!(take_until_single_line_comment_end("-"), done_result!("-"));
    }

    fn single_line_comment_tests<F>(parser: F)
    where
        F: Fn(&str) -> IResult<&str, &str>,
    {
        assert_eq!(parser("-- thing\nmore"), Done("more", " thing"));
        assert_eq!(parser("-- thing--"), done_result!(" thing"));
        assert_eq!(parser("-- thing\n"), done_result!(" thing"));
        assert_eq!(
            parser("-- dash-in-middle--"),
            done_result!(" dash-in-middle")
        );
        assert_eq!(
            parser("-- dash-in-middle\x0C"),
            done_result!(" dash-in-middle")
        );

        // "/*" and "*/ have no special meaning in single-line comment.
        assert_eq!(
            single_line_comment("--/* still\nsingle"),
            Done("single", "/* still")
        );
        assert_eq!(
            single_line_comment("--*/* still\nsingle"),
            Done("single", "*/* still")
        );
        assert_eq!(
            single_line_comment("--*/ *//* still\nsingle"),
            Done("single", "*/ *//* still")
        );

        assert_eq!(single_line_comment("--\n"), done_result!(""));
        assert_eq!(single_line_comment("----"), done_result!(""));
    }

    #[test]
    fn test_single_line_comment() {
        single_line_comment_tests(single_line_comment);
    }

    fn multi_line_comment_tests<F>(parser: F)
    where
        F: Fn(&str) -> IResult<&str, &str>,
    {
        assert_eq!(parser("/* stuff */"), done_result!(" stuff "));
        assert_eq!(parser("/* line1\nline2 */"), done_result!(" line1\nline2 "));
        assert_eq!(parser("/**/"), done_result!(""));

        // "--" in multi-line comment has no special meaning.
        assert_eq!(parser("/* -- */"), done_result!(" -- "));

        // Incomplete
        assert_eq!(parser("/*/"), Incomplete(Needed::Unknown));
        assert_eq!(parser("/**"), Incomplete(Needed::Size(1)));

        // Nested comments
        assert_eq!(
            parser("/* Outer /* Inner */ Outer again */"),
            done_result!(" Outer /* Inner */ Outer again ")
        );
        assert_eq!(
            parser("/* Out /* In */ Out2 /* In2 */ Out3 */"),
            done_result!(" Out /* In */ Out2 /* In2 */ Out3 ")
        );
        assert_eq!(
            parser("/* Out /* In */ Out */ Extra */"),
            Done(" Extra */", " Out /* In */ Out ")
        );
    }

    #[test]
    fn test_multi_line_comment() {
        multi_line_comment_tests(multi_line_comment);
    }

    #[test]
    fn test_comment() {
        single_line_comment_tests(comment);
        multi_line_comment_tests(comment);
    }

    #[test]
    fn test_number() {
        assert_eq!(number("0"), done_result!(0));
        assert_eq!(number("1"), done_result!(1));

        // Matches the first zero, but considers the second digit to be leftover input.
        assert_eq!(number("01"), Done("1", 0));
    }

    #[test]
    fn test_realnumber() {
        assert_eq!(realnumber("1"), done_result!(1.0));
        assert_eq!(realnumber("0"), done_result!(0.0));

        // Can have a leading zero
        assert_eq!(realnumber("00"), done_result!(0.0));
        assert_eq!(realnumber("01"), done_result!(1.0));

        // Can have a trailing period without a factional part
        assert_eq!(realnumber("1."), done_result!(1.0));
        assert_eq!(realnumber("1.0"), done_result!(1.0));
        assert_eq!(realnumber("1.001"), done_result!(1.001));

        // Exponents
        assert_eq!(realnumber("1.1e3"), done_result!(1100.0));
        assert_eq!(realnumber("1.1e-3"), done_result!(0.0011));
        assert_eq!(realnumber("1.e3"), done_result!(1000.0));
        assert_eq!(realnumber("1e3"), done_result!(1000.0));
        assert_eq!(realnumber("1.1E3"), done_result!(1100.0));
        assert_eq!(realnumber("1.1E-3"), done_result!(0.0011));

        // Leading zero in exponent
        assert_eq!(realnumber("1.1E-03"), Done("3", 1.1));
        assert_eq!(realnumber("1.1E03"), Done("3", 1.1));
    }

    #[test]
    fn test_asn_ws() {
        named!(tuple<&str, (&str, &str)>,
            asn_ws!(tuple!(take_s!(3), tag_s!("de"))));

        assert_eq!(tuple(" \t abc\u{A0} de fg"), Done("fg", ("abc", "de")));
    }

    #[test]
    fn test_bstring() {
        assert_eq!(
            bstring("'01101100'B"),
            done_result!(BitVec::from_bytes(&[0b01101100]))
        );
        assert_eq!(
            bstring("'01 1\u{A0}0\n1\t100'B"),
            done_result!(BitVec::from_bytes(&[0b01101100]))
        );
        assert_eq!(
            bstring("'\x0B01101100 \t\x0D'B"),
            done_result!(BitVec::from_bytes(&[0b01101100]))
        );

        // Missing initial quote
        assert_eq!(bstring("01101100'B"), Error(ErrorKind::Tag));

        // Missing ending "B"
        assert_eq!(bstring("'01101100'"), Incomplete(Needed::Size(11)));
    }

    #[test]
    fn test_xmlbstring() {
        assert_eq!(
            xmlbstring("01101100"),
            done_result!(BitVec::from_bytes(&[0b01101100]))
        );
        assert_eq!(
            xmlbstring("01 1\u{A0}0\n1\t100"),
            done_result!(BitVec::from_bytes(&[0b01101100]))
        );
        assert_eq!(
            xmlbstring("\x0B01101100 \t\x0D"),
            done_result!(BitVec::from_bytes(&[0b01101100]))
        );

        assert_eq!(
            xmlbstring("01101100'B"),
            Done("'B", BitVec::from_bytes(&[0b01101100]))
        );

        assert_eq!(xmlbstring("'01101100'"), Done("'01101100'", BitVec::new()));
    }

    #[test]
    fn test_hstring() {
        assert_eq!(hstring("'AB0196'H"), done_result!(vec![10u8, 11, 0, 1, 9, 6]));
        assert_eq!(hstring("'A B0\u{A0}19'H"), done_result!(vec![10u8, 11, 0, 1, 9]));
        assert_eq!(hstring("' A\u{A0}\n\tB0196\x0B'H"), done_result!(vec![10u8, 11, 0, 1, 9, 6]));

        // Missing initial quote
        assert_eq!(hstring("01101100'B"), Error(ErrorKind::Tag));

        // Missing ending "H"
        assert_eq!(hstring("'01101100'"), Incomplete(Needed::Size(11)));
    }

    #[test]
    fn test_xmlhstring() {
        assert_eq!(
            xmlhstring("AB0196"),
            done_result!(vec![10u8, 11, 0, 1, 9, 6])
        );
        assert_eq!(
            xmlhstring("AB \u{A0}0\n\t196"),
            done_result!(vec![10u8, 11, 0, 1, 9, 6])
        );
        assert_eq!(
            xmlhstring("\x0BAB0196 \t\x0D"),
            done_result!(vec![10u8, 11, 0, 1, 9, 6])
        );

        assert_eq!(
            xmlhstring("01101100'B"),
            Done("'B", vec![0u8, 1, 1, 0, 1, 1, 0, 0])
        );

        assert_eq!(xmlhstring("'01101100'"), Done("'01101100'", vec![]));
    }

    #[test]
    fn test_string_segment_newline_end() {
        assert_eq!(string_segment_newline_end("hi there\n"), done_result!("hi there"));

        // Stops at newline
        assert_eq!(string_segment_newline_end("some\nmore"), Done("more", "some"));

        // Strips spaces before newline
        assert_eq!(string_segment_newline_end("some \t\nmore"), Done("more", "some"));

        assert_eq!(string_segment_newline_end(" \t\ntrailing"), Done("trailing", ""));

        // Errors at quote
        assert_eq!(string_segment_newline_end("some\"\"more"), Error(ErrorKind::RegexpCapture));
    }

    #[test]
    fn test_cstring() {
        assert_eq!(cstring(r#""hi there""#), done_string("hi there"));
        assert_eq!(cstring(r#""quote "" escape""#), done_string("quote \" escape"));
        assert_eq!(cstring("\"some \"\"\nmore\""), done_string(r#"some "more"#));

        // Strips spaces before newline
        assert_eq!(cstring("\"some \"\" \t\nmore\""), done_string(r#"some "more"#));
    }

    #[test]
    fn test_simplestring() {
        assert_eq!(simplestring("\"hi there\n\""), done_result!("hi there\n"));
        assert_eq!(simplestring("\"some\nmore\""), done_result!("some\nmore"));

        assert_eq!(simplestring("\"missing close"), Incomplete(Needed::Size(15)));
    }

    #[test]
    fn test_encodingreference() {
        assert_eq!(encodingreference("A"), done_result!(EncodingReference::new("A")));

        // Contains lowercase letters
        assert_eq!(encodingreference("Good"), Done("ood", EncodingReference::new("G")));
        assert_eq!(
            encodingreference("WITH-DASHES"),
            done_result!(EncodingReference::new("WITH-DASHES"))
        );
        assert_eq!(encodingreference("SpaCe "), Done("paCe ", EncodingReference::new("S")));
        assert_eq!(
            encodingreference("DOUBLE--hypen"),
            Done("--hypen", EncodingReference::new("DOUBLE"))
        );
        assert_eq!(encodingreference("-start"), Error(ErrorKind::RegexpFind));
        assert_eq!(encodingreference("lower"), Error(ErrorKind::RegexpFind));
        assert_eq!(encodingreference("lower"), Error(ErrorKind::RegexpFind));

        // ABSENT is a reserved word
        assert_eq!(encodingreference("ABSENT"), Error(ErrorKind::Verify));
    }

    #[test]
    fn test_noninteger_unicode_label() {
        assert_eq!(noninteger_unicode_label("A"), done_result!(NonIntegerUnicodeLabel::new("A")));

        assert_eq!(noninteger_unicode_label("A0\u{FDF0}"),
                   done_result!(NonIntegerUnicodeLabel::new("A0\u{FDF0}")));

        // Must not contain only digits
        assert_eq!(noninteger_unicode_label("012349"), Error(ErrorKind::Verify));

        // Must not start or end with `HYPHEN MINUS`
        assert_eq!(noninteger_unicode_label("-A"), Error(ErrorKind::Verify));
        assert_eq!(noninteger_unicode_label("A-"), Error(ErrorKind::Verify));
        assert_eq!(noninteger_unicode_label("-A-"), Error(ErrorKind::Verify));

        // Must not have hyphens in both third and fourth position
        assert_eq!(noninteger_unicode_label("Ab--DE"), Error(ErrorKind::Verify));

        // May have hyphens elsewhere
        assert_eq!(noninteger_unicode_label("Stuff--things"),
                   done_result!(NonIntegerUnicodeLabel::new("Stuff--things")));
    }
}
