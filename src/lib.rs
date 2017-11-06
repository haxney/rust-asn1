//! An ASN.1 parser written in Rust.
//!
//! Taken from http://www.itu.int/rec/T-REC-X.680-200811-I

#![allow(dead_code)] // Until parser is complete. Too noisy otherwise

#[macro_use]
extern crate nom;

extern crate regex;

mod parser;
pub mod types;