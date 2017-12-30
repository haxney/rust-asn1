use std::collections::HashSet;

/// An `identifier` lexical item as specified by X.680 §12.3.
#[derive(Debug, PartialEq, Eq)]
pub struct Identifier(String);

impl Identifier {
    pub fn new<S>(name: S) -> Identifier
    where
        S: Into<String>,
    {
        Identifier(name.into())
    }
}

/// A `valuereference` lexical item as specified by X.680 §12.4.
#[derive(Debug, PartialEq, Eq)]
pub struct ValueReference(String);

impl ValueReference {
    pub fn new<S>(name: S) -> ValueReference
    where
        S: Into<String>,
    {
        ValueReference(name.into())
    }
}

/// A `typereference` lexical item as specified by X.680 §12.2.
#[derive(Debug, PartialEq, Eq)]
pub struct TypeReference(String);

impl TypeReference {
    pub fn new<S>(name: S) -> TypeReference
    where
        S: Into<String>,
    {
        TypeReference(name.into())
    }
}

/// A `modulereference` lexical item as specified by X.680 §12.5.
#[derive(Debug, PartialEq, Eq)]
pub struct ModuleReference(String);

impl ModuleReference {
    pub fn new<S>(name: S) -> ModuleReference
    where
        S: Into<String>,
    {
        ModuleReference(name.into())
    }
}

/// An `encodingreference` lexical item as specified by X.680 §12.25.
#[derive(Debug, Eq, PartialEq)]
pub struct EncodingReference(String);

impl EncodingReference {
    pub fn new<S: Into<String>>(name: S) -> EncodingReference {
        EncodingReference(name.into())
    }
}

/// A `non-integerUnicodeLabel` lexical item as specified by X.680 §12.27 and X.660 §7.5.
#[derive(Debug, Eq, PartialEq)]
pub struct NonIntegerUnicodeLabel(String);

impl NonIntegerUnicodeLabel {
    pub fn new<S: Into<String>>(name: S) -> NonIntegerUnicodeLabel {
        NonIntegerUnicodeLabel(name.into())
    }
}

/// An `objectclassreference` lexical item as specified by X.681 §7.1
#[derive(Debug, PartialEq, Eq)]
pub struct ObjectClassReference(String);

impl ObjectClassReference {
    pub fn new<S>(name: S) -> ObjectClassReference
    where
        S: Into<String>,
    {
        ObjectClassReference(name.into())
    }
}

/// An `objectreference` lexical item as specified by X.681 §7.2
#[derive(Debug, PartialEq, Eq)]
pub struct ObjectReference(String);

impl ObjectReference {
    pub fn new<S>(name: S) -> ObjectReference
    where
        S: Into<String>,
    {
        ObjectReference(name.into())
    }
}

/// An `objectsetreference` lexical item as specified by X.681 §7.3
#[derive(Debug, Eq, PartialEq)]
pub struct ObjectSetReference(String);


impl ObjectSetReference {
    pub fn new<S>(name: S) -> ObjectSetReference
    where
        S: Into<String>,
    {
        ObjectSetReference(name.into())
    }
}

lazy_static! {
/// Reserved keywords as specified in §12.38
pub static ref RESERVED_WORDS: HashSet<&'static str> = vec![
    "ABSENT",
    "ENCODED",
    "INTERSECTION",
    "SEQUENCE",
    "ABSTRACT-SYNTAX",
    "ENCODING-CONTROL",
    "ISO646String",
    "SET",
    "ALL",
    "END",
    "MAX",
    "SETTINGS",
    "APPLICATION",
    "ENUMERATED",
    "MIN",
    "SIZE",
    "AUTOMATIC",
    "EXCEPT",
    "MINUS-INFINITY",
    "STRING",
    "BEGIN",
    "EXPLICIT",
    "NOT-A-NUMBER",
    "SYNTAX",
    "BIT",
    "EXPORTS",
    "NULL",
    "T61String",
    "BMPString",
    "EXTENSIBILITY",
    "NumericString",
    "TAGS",
    "BOOLEAN",
    "EXTERNAL",
    "OBJECT",
    "TeletexString",
    "BY",
    "FALSE",
    "ObjectDescriptor",
    "TIME",
    "CHARACTER",
    "FROM",
    "OCTET",
    "TIME-OF-DAY",
    "CHOICE",
    "GeneralizedTime",
    "OF",
    "TRUE",
    "CLASS",
    "GeneralString",
    "OID-IRI",
    "TYPE-IDENTIFIER",
    "COMPONENT",
    "GraphicString",
    "OPTIONAL",
    "UNION",
    "COMPONENTS",
    "IA5String",
    "PATTERN",
    "UNIQUE",
    "CONSTRAINED",
    "IDENTIFIER",
    "PDV",
    "UNIVERSAL",
    "CONTAINING",
    "IMPLICIT",
    "PLUS-INFINITY",
    "UniversalString",
    "DATE",
    "IMPLIED",
    "PRESENT",
    "UTCTime",
    "DATE-TIME",
    "IMPORTS",
    "PrintableString",
    "UTF8String",
    "DEFAULT",
    "INCLUDES",
    "PRIVATE",
    "VideotexString",
    "DEFINITIONS",
    "INSTANCE",
    "REAL",
    "VisibleString",
    "DURATION",
    "INSTRUCTIONS",
    "RELATIVE-OID",
    "WITH",
    "EMBEDDED",
    "INTEGER",
    "RELATIVE-OID-IRI",
  ].into_iter().collect();
}

#[derive(Debug, Eq, PartialEq)]
pub struct ModuleIdentifier {
    pub module_reference: ModuleReference,
    pub definitive_identification: Option<DefinitiveIdentification>,
}

/// An identifier for one segment of an OID hierarchy.
#[derive(Debug, Eq, PartialEq)]
pub enum ArcIdentifier {
    IntegerLabel(u64),
    NonIntegerLabel(NonIntegerUnicodeLabel),
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinitiveIdentification {
    pub obj_id_components: Vec<DefinitiveObjIdComponent>,

    /// IRI path. May be empty
    pub iri: Vec<ArcIdentifier>,
}

impl DefinitiveIdentification {
    pub fn new_no_iri(obj_ids: Vec<DefinitiveObjIdComponent>) -> DefinitiveIdentification {
        DefinitiveIdentification {
            obj_id_components: obj_ids,
            iri: vec![],
        }
    }

    pub fn new(
        obj_ids: Vec<DefinitiveObjIdComponent>,
        iri: Vec<ArcIdentifier>,
    ) -> DefinitiveIdentification {
        DefinitiveIdentification {
            obj_id_components: obj_ids,
            iri: iri,
        }
    }
}

/// A component of a Definitive Object ID list.
#[derive(Debug, Eq, PartialEq)]
pub enum DefinitiveObjIdComponent {
    NameForm(Identifier),
    NumberForm(u64),
    NameAndNumberForm(Identifier, u64),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Reference {
    Type(TypeReference),
    Value(ValueReference),
    ObjectClass(ObjectClassReference),
    Object(ObjectReference),
    ObjectSet(ObjectSetReference),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol {
    Ref(Reference),

    /// A `ParameterizedRef` is the same as a `Reference`, but is distinguished in the grammar.
    ParameterizedRef(Reference),
}

/// A parsed symbol. Resolution of the symbol to a specific type depends resolving the name to a
/// specific definition, and so must come at a later stage of analysis.
#[derive(Debug, Eq, PartialEq)]
pub enum UnresolvedSymbol {
    Plain(String),
    Parameterized(String),
}

impl UnresolvedSymbol {
    pub fn plain<S>(name: S) -> UnresolvedSymbol
    where
        S: Into<String>,
    {
        UnresolvedSymbol::Plain(name.into())
    }
    pub fn parameterized<S>(name: S) -> UnresolvedSymbol
    where
        S: Into<String>,
    {
        UnresolvedSymbol::Parameterized(name.into())
    }
}

/// Exported symbols.
#[derive(Debug, Eq, PartialEq)]
pub enum Exports {
    /// All symbols are exported.
    AllExported,

    /// Specific list of symbols. May be empty.
    SymbolsExported(Vec<UnresolvedSymbol>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ActualParameter {
    Type(Type),
    Value,
    ValueSet,
    DefinedObjectClass,
    Object,
    ObjectSet,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinedValue {
    external_module: Option<ModuleReference>,
    value: ValueReference,
    parameters: Vec<ActualParameter>,
}

impl DefinedValue {
    pub fn new(
        external_module: Option<ModuleReference>,
        value: ValueReference,
        parameters: Vec<ActualParameter>,
    ) -> DefinedValue {
        DefinedValue {
            external_module: external_module,
            value: value,
            parameters: parameters,
        }
    }

    pub fn new_val(value: ValueReference) -> DefinedValue {
        DefinedValue {
            external_module: Option::None,
            value: value,
            parameters: vec![],
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct NamedBit {
    ident: Identifier,
    value: NamedBitValue,
}

impl NamedBit {
    pub fn new(ident: Identifier, value: NamedBitValue) -> NamedBit {
        NamedBit {
            ident: ident,
            value: value,
        }
    }

    pub fn new_tuple((ident, value): (Identifier, NamedBitValue)) -> NamedBit {
        NamedBit::new(ident, value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum NamedBitValue {
    Num(u64),
    Ref(DefinedValue),
}

/// The type of a CharacterString. Combines both restricted CharacterString types (X.680 §41) and
/// unrestricted character strings (X.680 §44)
#[derive(Debug, Eq, PartialEq)]
pub enum CharacterString {
    BMPString,
    GeneralString,
    GraphicString,
    IA5String,
    ISO646String,
    NumericString,
    PrintableString,
    TeletexString,
    T61String,
    UniversalString,
    UTF8String,
    VideotexString,
    VisibleString,
    UnrestrictedString,
}

/// NamedType structure as defined in X.680 §17.5.
#[derive(Debug, Eq, PartialEq)]
pub struct NamedType {
    name: Identifier,

    /// This does not need to be boxed because any recursive which would contain this already have
    /// indirection, such as the elements of a `ChoiceType` being in a `Vec`.
    ty: Type
}

impl NamedType {
    pub fn new(ident: Identifier, ty: Type) -> NamedType {
        NamedType {
            name: ident,
            ty: ty,
        }
    }

    pub fn new_tuple((ident, ty): (Identifier, Type)) -> NamedType {
        NamedType::new(ident, ty)
    }
}

/// An ExceptionIdentification type as defined in X.680 §53.4.
#[derive(Debug, Eq, PartialEq)]
pub enum ExceptionIdentification {
    SignedNumber(i64),
    DefinedVal(DefinedValue),
    TypeVal(Type, Value)
}

/// An `ExtensionAdditionAlternative` type as defined in X.680 §29.1.
#[derive(Debug, Eq, PartialEq)]
pub enum ExtensionAlternative {
    /// `ExtensionAdditionAlternativesGroup` with a `VersionNumber` and `AlternativeTypeList`.
    Group(Option<u64>, Vec<NamedType>),
    Named(NamedType)
}

#[derive(Debug, Eq, PartialEq)]
pub struct ChoiceExtension {
    /// The `ExtensionAndException` parse type. Boxed to prevent recursive datatype.
    exception: Option<Box<ExceptionIdentification>>,
    extension_addition: Vec<ExtensionAlternative>,
    extension_marker: bool
}

/// A `Choice` type as defined in X.680 §29.
#[derive(Debug, Eq, PartialEq)]
pub struct Choice {
    root_alternatives: Vec<NamedType>,
    extension: Option<ChoiceExtension>,
}

impl Choice {
    pub fn new(root: Vec<NamedType>, extension: Option<ChoiceExtension>) -> Choice {
        Choice {
            root_alternatives: root,
            extension: extension
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum BuiltinType {
    /// BitString type as defined in X.680 §22.1.
    BitStringType(Vec<NamedBit>),

    /// Boolean type as defined in X.680 §18.1.
    BooleanType,

    /// CharacterString type as defined in X.680 §40.1.
    CharacterStringType(CharacterString),

    /// X.680 §29
    ChoiceType(Choice),
    DateType,
    DateTimeType,
    DurationType,
    EmbeddedPDVType,
    EnumeratedType,
    ExternalType,
    InstanceOfType,
    IntegerType,
    IRIType,
    NullType,
    ObjectClassFieldType,
    ObjectIdentifierType,
    OctetStringType,
    RealType,
    RelativeIRIType,
    RelativeOIDType,
    SequenceType,
    SequenceOfType,
    SetType,
    SetOfType,
    PrefixedType,
    TimeType,
    TimeOfDayType,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ReferencedType {
    DefinedType,
    UsefulType,
    SelectionType,
    TypeFromObject,
    ValueSetFromObjects,
}

/// `ConstrainedType` as defined in X.680 §49.1
#[derive(Debug, Eq, PartialEq)]
pub enum ConstrainedType {
    Builtin(BuiltinType, Constraint),
    Referenced(ReferencedType, Constraint),
    // TypeWith(TypeWithConstraint)
}

#[derive(Debug, Eq, PartialEq)]
pub struct Constraint();

/// Enum to describe a type.
#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Builtin(BuiltinType),
    Referenced(ReferencedType),
    Constrained(ConstrainedType),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Value {
}
