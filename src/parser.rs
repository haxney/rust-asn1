use std::str::FromStr;
use nom::{IResult, Needed};
use types::{Identifier, TypeReference, RESERVED_WORDS, ValueReference, ModuleReference};

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

/// Returns `true` if the character is a whitespace character according to ASN.1.
///
/// Defined in X.680 §12.1.6
fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\t' => true,

        // All newline characters are also whitespace
        _ if is_newline(ch) => true,
        _ => false,
    }
}

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
                                break
                            }
                        }
                    }
                    _ if is_newline(c) => {
                        offset = i;
                        break
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
/// Defined in X.680 §12.6a
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
/// Defined in X.680 §12.6b
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
/// Defined in X.680 §12.6
named!(comment<&str, &str>,
    alt!(
        single_line_comment |
        multi_line_comment
    )
);

/// Parse an integer.
///
/// Defined in X.680 §12.8
named!(number<&str, u64>,
    map_res!(
        re_find!(r"^(0|[1-9]\d*)"),
        FromStr::from_str
    )
);

/// Parse an `f64`.
///
/// Defined in X.680 §12.9
named!(realnumber<&str, f64>,
   map_res!(
       re_find!(r"^\d+(\.\d*)?([eE]-?(0|[1-9]\d*))?"),
       FromStr::from_str
   )
);

// Parse an identifier as a string.
named!(identifier_str<&str, &str>, re_find!(r"^[a-z]([a-zA-Z0-9]+|-[a-zA-Z0-9])*"));

/// Parse an `Identifier`.
///
/// Starts with a lowercase letter, can contain letters, digits, or hyphens, but cannot contain a
/// double hyphen or end with a hyphen.
named!(identifier<&str, Identifier>, map!(identifier_str, Identifier::new));

/// Parse a `ValueReference`.
named!(valuereference<&str, ValueReference>, map!(identifier_str, ValueReference::new));

// Parse a `typereference` string and filters out `RESERVED_WORDS`.
named!(typereference_str<&str, &str>,
    verify!(
        re_find!(r"^[A-Z]([a-zA-Z0-9]+|-[a-zA-Z0-9])*"),
        |ref_name| !RESERVED_WORDS.contains(ref_name)));

/// Parse a `TypeReference`.
named!(typereference<&str, TypeReference>, map!(typereference_str, TypeReference::new));

/// Parse a `ModuleReference`.
named!(modulereference<&str, ModuleReference>, map!(typereference_str, ModuleReference::new));

#[cfg(test)]
mod tests {
    use super::{number, realnumber, single_line_comment, take_until_single_line_comment_end,
                newline, multi_line_comment, comment, identifier, valuereference, typereference,
                modulereference};
    use nom::IResult::{Done, Incomplete, Error};
    use nom::{IResult, Needed, ErrorKind};
    use types::{Identifier, ValueReference, TypeReference, ModuleReference};
    use std::fmt::Debug;

    /// Simple way of specifying an `IResult::Done` with no remaining input.
    macro_rules! done_result (
        ($res:expr) => (
            IResult::Done("", $res)
        )
    );

    #[test]
    fn test_newline() {
        assert_eq!(newline("\n"), done_result!("\n"));
        assert_eq!(newline("\x0B"), done_result!("\x0B"));
    }

    #[test]
    fn test_take_until_single_line_comment_end() {
        assert_eq!(take_until_single_line_comment_end(" thing\nmore"), Done("\nmore", " thing"));
        assert_eq!(take_until_single_line_comment_end(" thing\n"), Done("\n", " thing"));
        assert_eq!(take_until_single_line_comment_end(" thing--"), Done("--", " thing"));
        assert_eq!(take_until_single_line_comment_end(" dash-in-middle--"),
                   Done("--", " dash-in-middle"));
        assert_eq!(take_until_single_line_comment_end(" dash-in-middle\x0C"),
                   Done("\x0C", " dash-in-middle"));

        // Empty comments
        assert_eq!(take_until_single_line_comment_end("\n"), Done("\n", ""));
        assert_eq!(take_until_single_line_comment_end("--"), Done("--", ""));

        assert_eq!(take_until_single_line_comment_end("stuff-"), done_result!("stuff-"));
        assert_eq!(take_until_single_line_comment_end("-"), done_result!("-"));
    }

    fn single_line_comment_tests<F>(parser: F)
        where F: Fn(&str) -> IResult<&str, &str> {
        assert_eq!(parser("-- thing\nmore"), Done("more", " thing"));
        assert_eq!(parser("-- thing--"), done_result!(" thing"));
        assert_eq!(parser("-- thing\n"), done_result!(" thing"));
        assert_eq!(parser("-- dash-in-middle--"), done_result!(" dash-in-middle"));
        assert_eq!(parser("-- dash-in-middle\x0C"), done_result!(" dash-in-middle"));

        // "/*" and "*/ have no special meaning in single-line comment.
        assert_eq!(single_line_comment("--/* still\nsingle"), Done("single", "/* still"));
        assert_eq!(single_line_comment("--*/* still\nsingle"), Done("single", "*/* still"));
        assert_eq!(single_line_comment("--*/ *//* still\nsingle"),
                   Done("single", "*/ *//* still"));

        assert_eq!(single_line_comment("--\n"), done_result!(""));
        assert_eq!(single_line_comment("----"), done_result!(""));
    }

    #[test]
    fn test_single_line_comment() {
        single_line_comment_tests(single_line_comment);
    }

    fn multi_line_comment_tests<F>(parser: F)
        where F: Fn(&str) -> IResult<&str, &str> {
        assert_eq!(parser("/* stuff */"), done_result!(" stuff "));
        assert_eq!(parser("/* line1\nline2 */"), done_result!(" line1\nline2 "));
        assert_eq!(parser("/**/"), done_result!(""));

        // "--" in multi-line comment has no special meaning.
        assert_eq!(parser("/* -- */"), done_result!(" -- "));

        // Incomplete
        assert_eq!(parser("/*/"), Incomplete(Needed::Unknown));
        assert_eq!(parser("/**"), Incomplete(Needed::Size(1)));

        // Nested comments
        assert_eq!(parser("/* Outer /* Inner */ Outer again */"),
                   done_result!(" Outer /* Inner */ Outer again "));
        assert_eq!(parser("/* Out /* In */ Out2 /* In2 */ Out3 */"),
                   done_result!(" Out /* In */ Out2 /* In2 */ Out3 "));
        assert_eq!(parser("/* Out /* In */ Out */ Extra */"),
                   Done(" Extra */", " Out /* In */ Out "));
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

    /// Run tests for parsers which behave like `Identifier`. Currently, this is only `Identifier`
    /// and `ValueReference`.
    fn identifier_like_tests<F, T, N>(parser: F, struct_maker: N)
        where T: Debug + PartialEq,
              F: Fn(&str) -> IResult<&str, T>,
              N: Fn(&'static str) -> T
    {
        assert_eq!(parser("a"), done_result!(struct_maker("a")));
        assert_eq!(parser("aB3"), done_result!(struct_maker("aB3")));
        assert_eq!(parser("aB3-"), Done("-", struct_maker("aB3")));
        assert_eq!(parser("dash-middle"), done_result!(struct_maker("dash-middle")));
        assert_eq!(parser("space "), Done(" ", struct_maker("space")));
        assert_eq!(parser("double--hypen"), Done("--hypen", struct_maker("double")));
        assert_eq!(parser("-start"), Error(ErrorKind::RegexpFind));
        assert_eq!(parser("Capital"), Error(ErrorKind::RegexpFind));
    }

    #[test]
    fn test_identifier() {
        identifier_like_tests(identifier, Identifier::new);
    }

    #[test]
    fn test_valuereference() {
        identifier_like_tests(valuereference, ValueReference::new)
    }

    /// Run tests for parsers which behave like `TypeReference`. Currently, this is only
    /// `TypeReference` and `ModuleReference`.
    fn typereference_like_tests<F, T, N>(parser: F, struct_maker: N)
        where T: Debug + PartialEq,
              F: Fn(&str) -> IResult<&str, T>,
              N: Fn(&'static str) -> T
    {
        assert_eq!(parser("A"), done_result!(struct_maker("A")));
        assert_eq!(parser("Good"), done_result!(struct_maker("Good")));
        assert_eq!(parser("With-Dashes"), done_result!(struct_maker("With-Dashes")));
        assert_eq!(parser("SpaCe "), Done(" ", struct_maker("SpaCe")));
        assert_eq!(parser("DOUBLE--hypen"), Done("--hypen", struct_maker("DOUBLE")));
        assert_eq!(parser("-start"), Error(ErrorKind::RegexpFind));
        assert_eq!(parser("lower"), Error(ErrorKind::RegexpFind));
        assert_eq!(parser("lower"), Error(ErrorKind::RegexpFind));

        // ABSENT is a reserved word
        assert_eq!(parser("ABSENT"), Error(ErrorKind::Verify));
    }

    #[test]
    fn test_typereference() {
        typereference_like_tests(typereference, TypeReference::new);
    }

    #[test]
    fn test_modulereference() {
        typereference_like_tests(modulereference, ModuleReference::new);
    }
}
