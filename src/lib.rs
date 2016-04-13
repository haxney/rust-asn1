//! Taken from http://www.itu.int/rec/T-REC-X.680-200811-I

#![allow(dead_code)] // Until parser is complete. Too noisy otherwise

#[macro_use]
extern crate nom;

extern crate regex;

use std::str::FromStr;


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

#[cfg(test)]
mod tests {

    use super::{number, realnumber};
    use nom::IResult;
    use nom::IResult::Done;

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

    #[test]
    fn test_single_line_comment() {
        assert_eq!(single_line_comment("-- thing\nmore"), Done("more", " thing"));
        assert_eq!(single_line_comment("-- thing--"), done_result!(" thing"));
        assert_eq!(single_line_comment("-- thing\n"), done_result!(" thing"));
        assert_eq!(single_line_comment("-- dash-in-middle--"), done_result!(" dash-in-middle"));
        assert_eq!(single_line_comment("-- dash-in-middle\x0C"), done_result!(" dash-in-middle"));
        assert_eq!(single_line_comment("--\n"), done_result!(""));
        assert_eq!(single_line_comment("----"), done_result!(""));
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

}
