use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while},
    character::{
        complete::{char, multispace0, multispace1},
        streaming::digit1,
    },
    combinator::{map, map_res},
    error::{ErrorKind, ParseError},
    multi::separated_list0,
    number::complete::double,
    sequence::{delimited, pair, separated_pair, terminated},
    AsChar, IResult, InputTakeAtPosition,
};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct StepFile {
    pub header: Header,
    pub data: Data,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Header {
    pub file_description: FileDescription,
    pub file_name: FileName,
    pub file_schema: FileSchema,
    pub file_population: Option<FilePopulation>,
    pub section_language: Option<SectionLanguage>,
    pub section_context: Option<SectionContext>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileDescription {
    pub description: String,
    /// TODO: get proper format
    pub implementation_level: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileName {
    pub name: String,
    pub time_stamp: String,
    pub author: String,
    pub organization: String,
    pub preprocessor_version: String,
    pub originating_system: String,
    pub authorization: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileSchema {}

#[derive(Debug, Clone, PartialEq)]
pub struct FilePopulation {}

#[derive(Debug, Clone, PartialEq)]
pub struct SectionLanguage {}

#[derive(Debug, Clone, PartialEq)]
pub struct SectionContext {}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    pub entities: Vec<Entity>,
}

// pub struct Entity {
//     pub id: NonZeroU64,
//     pub record:EntityRecord,
// }

#[derive(Debug, Clone, PartialEq)]
pub struct Entity {
    pub id: u64,
    pub name: String,
    pub s: Vec<Parameter>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntityRecord {
    Simple(SimpleRecord),
    Complex(Vec<SimpleRecord>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleRecord {
    pub keyword: String,
    pub parameters: ParameterList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterList(Vec<String>);

#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    TypedParameter(TypedParameter),
    UntypedParameter(UntypedParameter),
    Omitted,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UntypedParameter {
    SimpleParameter(SimpleParameter),
    NotProvided,
    List(Vec<Parameter>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct TypedParameter {
    pub type_: String,
    pub value: Rc<Parameter>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleParameter {
    Integer(i64),
    Real(f64),
    String(String),
    OccurrenceName(OccurrenceName),
    EnumerationValue(String),
    Binary(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum OccurrenceName {
    ConstantInstanceName(String),
    ConstantValueName(String),
    EntityInstanceName(u64),
    ValueInstanceName(u64),
}

pub fn ifc_file(input: &str) -> IResult<&str, (Vec<(&str, Vec<Parameter>)>, Data)> {
    let (i, (headers, entities)) = delimited(
        delimited(multispace0, tag("ISO-10303-21;"), multispace0),
        ifc_contents,
        delimited(multispace0, tag("END-ISO-10303-21;"), multispace0),
    )(input)?;
    Ok((i, (headers, Data { entities })))
}

pub fn ifc_contents(input: &str) -> IResult<&str, (Vec<(&str, Vec<Parameter>)>, Vec<Entity>)> {
    pair(
        delimited(multispace0, ifc_header_sec, multispace0),
        delimited(multispace0, ifc_data_sec, multispace0),
    )(input)
}

pub fn ifc_header_sec(input: &str) -> IResult<&str, Vec<(&str, Vec<Parameter>)>> {
    delimited(
        delimited(multispace0, tag("HEADER;"), multispace0),
        ifc_header_list,
        delimited(multispace0, tag("ENDSEC;"), multispace0),
    )(input)
}

pub fn ifc_data_sec(input: &str) -> IResult<&str, Vec<Entity>> {
    delimited(
        delimited(multispace0, tag("DATA;"), multispace0),
        ifc_data_list,
        delimited(multispace0, tag("ENDSEC;"), multispace0),
    )(input)
}

pub fn ifc_data_list(input: &str) -> IResult<&str, Vec<Entity>> {
    separated_list0(multispace1, ifc_entity_terminated)(input)
}

pub fn ifc_header_list(input: &str) -> IResult<&str, Vec<(&str, Vec<Parameter>)>> {
    separated_list0(multispace1, terminated(ifc_entity, char(';')))(input)
}

fn ifc_entity_terminated(input: &str) -> IResult<&str, Entity> {
    terminated(ifc_entity_ided, char(';'))(input)
}

fn ifc_entity_ided(input: &str) -> IResult<&str, Entity> {
    let (input, (id, (name, entity))) = separated_pair(
        ifc_id,
        delimited(multispace0, tag("="), multispace0),
        ifc_entity,
    )(input)?;
    let entity = Entity {
        id,
        name: name.to_string(),
        s: entity,
    };
    Ok((input, entity))
}

fn ifc_id(input: &str) -> IResult<&str, u64> {
    map_res(pair(char('#'), digit1), |(_, s): (char, &str)| {
        s.parse::<u64>()
    })(input)
}

// TODO: consider all of the types
fn ifc_occurrence_name(input: &str) -> IResult<&str, Parameter> {
    let (i, n) = map_res(pair(char('#'), digit1), |(_, s): (char, &str)| {
        s.parse::<u64>()
    })(input)?;
    Ok((
        i,
        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
            SimpleParameter::OccurrenceName(OccurrenceName::EntityInstanceName(n)),
        )),
    ))
}

fn ifc_entity(input: &str) -> IResult<&str, (&str, Vec<Parameter>)> {
    pair(ifc_name, ifc_parameter_list)(input)
}

pub fn ifc_name<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar + Copy,
{
    input.split_at_position1_complete(
        |item| !item.is_alphanum() && item.as_char() != '_',
        ErrorKind::AlphaNumeric,
    )
}

fn ifc_string(input: &str) -> IResult<&str, Parameter> {
    // TODO: allow escaping
    let (i, s) = delimited(char('\''), take_while(|c| c != '\''), char('\''))(input)?;
    Ok((
        i,
        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(SimpleParameter::String(
            s.to_string(),
        ))),
    ))
}

fn ifc_parameter_list(input: &str) -> IResult<&str, Vec<Parameter>> {
    delimited(char('('), ifc_parameter_list_inner, char(')'))(input)
}

fn ifc_typed_parameter(input: &str) -> IResult<&str, TypedParameter> {
    let (i, (name, value)) = pair(ifc_name, delimited(char('('), ifc_parameter, char(')')))(input)?;
    Ok((
        i,
        TypedParameter {
            type_: name.to_string(),
            value: Rc::new(value),
        },
    ))
}

fn ifc_parameter_list_p(input: &str) -> IResult<&str, Parameter> {
    let (i, list) = ifc_parameter_list(input)?;
    Ok((i, Parameter::UntypedParameter(UntypedParameter::List(list))))
}

fn ifc_parameter_list_inner(input: &str) -> IResult<&str, Vec<Parameter>> {
    separated_list0(char(','), ifc_parameter)(input)
}

fn ifc_omitted(input: &str) -> IResult<&str, Parameter> {
    let (i, _) = char('*')(input)?;
    Ok((i, Parameter::Omitted))
}

fn ifc_asterisk(input: &str) -> IResult<&str, Parameter> {
    let (i, _) = char('$')(input)?;
    Ok((
        i,
        Parameter::UntypedParameter(UntypedParameter::NotProvided),
    ))
}

fn ifc_enum(input: &str) -> IResult<&str, Parameter> {
    let (i, enum_value) = delimited(char('.'), is_not("."), char('.'))(input)?;
    Ok((
        i,
        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
            SimpleParameter::EnumerationValue(enum_value.to_string()),
        )),
    ))
}

fn ifc_num(input: &str) -> IResult<&str, Parameter> {
    let (i, n) = double(input)?;
    Ok((
        i,
        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(SimpleParameter::Real(n))),
    ))
}

fn ifc_parameter(input: &str) -> IResult<&str, Parameter> {
    alt((
        ifc_string,
        ifc_omitted,
        ifc_asterisk,
        ifc_enum,
        ifc_num,
        ifc_occurrence_name,
        ifc_parameter_list_p,
        map(ifc_typed_parameter, Parameter::TypedParameter),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_DATA: &str = include_str!("test.ifc");

    #[test]
    fn count_doors() {
        let (input, (_headers, data)) = ifc_file(TEST_DATA).unwrap();
        assert_eq!(input.trim(), "", "check that there is only whitespace left");
        let mut doors = vec![];
        for entry in data.entities.iter() {
            if entry.name == "IFCDOOR" {
                doors.push(entry.clone());
            }
        }
        assert_eq!(14, doors.len());
    }

    #[test]
    fn parse_ifc_data_list_many() {
        let (input, (_headers, data)) = ifc_file(TEST_DATA).unwrap();
        for entry in data.entities.iter() {
            println!("{entry:?}");
        }
        println!("{input}");
        assert_eq!(38898, data.entities.len());
    }

    #[test]
    fn parse_ifc_data_list() {
        assert_eq!(
            ifc_data_list(
                "#1= IFCORGANIZATION($,'Autodesk Revit Architecture 2011',$,$,$);\n\n#2=IFCAPPLICATION(#1,'2011','Autodesk Revit Architecture 2011','Revit');"
            ),
            Ok((
                "",
                vec![Entity {
                     id: 1,
                     name: "IFCORGANIZATION".to_string(),
                     s: vec![
                        Parameter::UntypedParameter(UntypedParameter::NotProvided),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(SimpleParameter::String("Autodesk Revit Architecture 2011".to_string()))),
                        Parameter::UntypedParameter(UntypedParameter::NotProvided),
                        Parameter::UntypedParameter(UntypedParameter::NotProvided),
                        Parameter::UntypedParameter(UntypedParameter::NotProvided),
                    ],
                }
                ,Entity{
                    id:2,
                    name:"IFCAPPLICATION".to_string(),
                    s:vec![
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(SimpleParameter::OccurrenceName(
                        OccurrenceName::EntityInstanceName(1)
                    ))),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(SimpleParameter::String("2011".to_string()))),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(SimpleParameter::String("Autodesk Revit Architecture 2011".to_string()))),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(SimpleParameter::String("Revit".to_string()))),
                    ]}
    ]
            ))
        );
    }

    #[test]
    fn parse_ifc_entity_terminated() {
        assert_eq!(
            ifc_entity_terminated(
                "#2=IFCAPPLICATION(#1,'2011','Autodesk Revit Architecture 2011','Revit');"
            ),
            Ok((
                "",
                Entity {
                    id: 2,
                    name: "IFCAPPLICATION".to_string(),
                    s: vec![
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::OccurrenceName(OccurrenceName::EntityInstanceName(1))
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("2011".to_string())
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("Autodesk Revit Architecture 2011".to_string())
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("Revit".to_string())
                        ))
                    ]
                }
            ))
        );
    }

    #[test]
    fn parse_ifc_entity_ided() {
        assert_eq!(
            ifc_entity_ided(
                "#2=IFCAPPLICATION(#1,'2011','Autodesk Revit Architecture 2011','Revit')"
            ),
            Ok((
                "",
                Entity {
                    id: 2,
                    name: "IFCAPPLICATION".to_string(),
                    s: vec![
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::OccurrenceName(OccurrenceName::EntityInstanceName(1))
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("2011".to_string())
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("Autodesk Revit Architecture 2011".to_string())
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("Revit".to_string())
                        ))
                    ]
                }
            ))
        );
    }

    #[test]
    fn parse_ifc_header_entity() {
        assert_eq!(
            ifc_entity("FILE_DESCRIPTION(('ViewDefinition [CoordinationView]'),'2;1')"),
            Ok((
                "",
                (
                    "FILE_DESCRIPTION",
                    vec![
                        Parameter::UntypedParameter(UntypedParameter::List(vec![
                            Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                                SimpleParameter::String(
                                    "ViewDefinition [CoordinationView]".to_string()
                                )
                            ))
                        ])),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("2;1".to_string())
                        ))
                    ]
                )
            ))
        );
    }

    #[test]
    fn parse_ifc_entity() {
        assert_eq!(
            ifc_entity("IFCAPPLICATION(#1,'2011','Autodesk Revit Architecture 2011','Revit')"),
            Ok((
                "",
                (
                    "IFCAPPLICATION",
                    vec![
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::OccurrenceName(OccurrenceName::EntityInstanceName(1))
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("2011".to_string())
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("Autodesk Revit Architecture 2011".to_string())
                        )),
                        Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                            SimpleParameter::String("Revit".to_string())
                        ))
                    ]
                )
            ))
        );
    }

    #[test]
    fn parse_ifc_string() {
        assert_eq!(
            ifc_string("'Autodesk Revit Architecture 2011'"),
            Ok((
                "",
                Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                    SimpleParameter::String("Autodesk Revit Architecture 2011".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parse_ifc_parameter_list() {
        assert_eq!(
            ifc_parameter_list_inner("'a','b'"),
            Ok((
                "",
                vec![
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("a".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("b".to_string())
                    )),
                ]
            ))
        );
        assert_eq!(
            ifc_parameter_list_inner("#1,'2011','Autodesk Revit Architecture 2011','Revit'"),
            Ok((
                "",
                vec![
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::OccurrenceName(OccurrenceName::EntityInstanceName(1))
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("2011".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("Autodesk Revit Architecture 2011".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("Revit".to_string())
                    ))
                ]
            ))
        );
    }

    #[test]
    fn parse_ifc_parameter_list2() {
        assert_eq!(
            ifc_string("''"),
            Ok((
                "",
                Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                    SimpleParameter::String("".to_string())
                ))
            ))
        );
        assert_eq!(
            ifc_parameter_list_inner("$,'','',$,$"),
            Ok((
                "",
                vec![
                    Parameter::UntypedParameter(UntypedParameter::NotProvided),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::NotProvided),
                    Parameter::UntypedParameter(UntypedParameter::NotProvided),
                ]
            ))
        );
        assert_eq!(
            ifc_parameter_list_inner(
                "'Reference',$,IFCLABEL('Basic Wall:Interior - Partition (92mm Stud)'),$"
            ),
            Ok((
                "",
                vec![
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("Reference".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::NotProvided),
                    Parameter::TypedParameter(TypedParameter {
                        type_: "IFCLABEL".to_string(),
                        value: Rc::new(Parameter::UntypedParameter(
                            UntypedParameter::SimpleParameter(SimpleParameter::String(
                                "Basic Wall:Interior - Partition (92mm Stud)".to_string()
                            ))
                        )),
                    }),
                    Parameter::UntypedParameter(UntypedParameter::NotProvided),
                ]
            ))
        );
    }

    #[test]
    fn parse_ifc_parameter_list_inner() {
        assert_eq!(
            ifc_parameter_list("(#1,'2011','Autodesk Revit Architecture 2011','Revit')"),
            Ok((
                "",
                vec![
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::OccurrenceName(OccurrenceName::EntityInstanceName(1))
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("2011".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("Autodesk Revit Architecture 2011".to_string())
                    )),
                    Parameter::UntypedParameter(UntypedParameter::SimpleParameter(
                        SimpleParameter::String("Revit".to_string())
                    )),
                ]
            ))
        );
    }

    // #[test]
    // fn parse_test_file() {
    //     let f = std::fs::File::open("test.ifc").unwrap();
    //     let parser = StepParser::new(f);
    //     for i in parser {
    //         println!("{:?}", i);
    //     }
    // }
}
