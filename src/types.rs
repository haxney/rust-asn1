/// An identifier lexical item as specified by X.680 §12.3
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Identifier(String);

impl Identifier {
    pub fn new<S>(name: S) -> Identifier
        where S: Into<String> {
        Identifier(name.into())
    }
}