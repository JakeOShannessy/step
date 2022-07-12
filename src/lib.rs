use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::{
        complete::{alphanumeric1, char, multispace1},
        streaming::digit1,
    },
    combinator::map_res,
    multi::separated_list0,
    number::complete::double,
    sequence::{delimited, pair, separated_pair},
    IResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StepFile {
    pub header: Header,
    pub data: Data,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilePopulation {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SectionLanguage {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SectionContext {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
    pub entities: Vec<Entity>,
}

// pub struct Entity {
//     pub id: NonZeroU64,
//     pub record:EntityRecord,
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entity {
    pub id: u64,
    pub name: String,
    pub s: Vec<String>,
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
    SimpleParameter(SimpleParameter),
    Dollar,
    Asterisk,
    // TypedParameter(TypedParameter),
    List(Vec<Parameter>),
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

fn ifc_data_list(input: &str) -> IResult<&str, Vec<Entity>> {
    separated_list0(multispace1, ifc_entity_terminated)(input)
    // let (input, (entity, _)) = pair(ifc_entity_ided, char(';'))(input)?;
    // Ok((input, entity))
}

fn ifc_entity_terminated(input: &str) -> IResult<&str, Entity> {
    let (input, (entity, _)) = pair(ifc_entity_ided, char(';'))(input)?;
    Ok((input, entity))
}

fn ifc_entity_ided(input: &str) -> IResult<&str, Entity> {
    let (input, (id, (name, entity))) = separated_pair(ifc_id, tag("="), ifc_entity)(input)?;
    let entity = Entity {
        id,
        name: name.to_string(),
        s: entity.iter().map(|s| s.to_string()).collect(),
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
        Parameter::SimpleParameter(SimpleParameter::OccurrenceName(
            OccurrenceName::EntityInstanceName(n),
        )),
    ))
}

fn ifc_entity(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    pair(ifc_name, ifc_parameter_list)(input)
}

fn ifc_name(input: &str) -> IResult<&str, &str> {
    alphanumeric1(input)
}

fn ifc_string(input: &str) -> IResult<&str, Parameter> {
    // TODO: allow escaping
    let (i, s) = delimited(char('\''), is_not("\'"), char('\''))(input)?;
    Ok((
        i,
        Parameter::SimpleParameter(SimpleParameter::String(s.to_string())),
    ))
}

fn ifc_parameter_list(input: &str) -> IResult<&str, Vec<&str>> {
    delimited(char('('), ifc_parameter_list_inner, char(')'))(input)
}

fn ifc_parameter_list_inner(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(char(','), is_not(",)"))(input)
}

fn ifc_dollar(input: &str) -> IResult<&str, Parameter> {
    let (i, _) = char('$')(input)?;
    Ok((i, Parameter::Dollar))
}

fn ifc_asterisk(input: &str) -> IResult<&str, Parameter> {
    let (i, _) = char('*')(input)?;
    Ok((i, Parameter::Asterisk))
}

fn ifc_enum(input: &str) -> IResult<&str, Parameter> {
    let (i, enum_value) = delimited(char('.'), is_not("."), char('.'))(input)?;
    Ok((
        i,
        Parameter::SimpleParameter(SimpleParameter::EnumerationValue(enum_value.to_string())),
    ))
}

fn ifc_num(input: &str) -> IResult<&str, Parameter> {
    let (i, n) = double(input)?;
    Ok((i, Parameter::SimpleParameter(SimpleParameter::Real(n))))
}

fn ifc_parameter(input: &str) -> IResult<&str, Parameter> {
    alt((
        ifc_string,
        ifc_dollar,
        ifc_asterisk,
        ifc_enum,
        ifc_num,
        ifc_occurrence_name,
    ))(input)
    // separated_list0(char(','), is_not(",)"))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_ifc_data_list_many() {
        let (input, entries) = ifc_data_list(include_str!("test_data.ifc")).unwrap();
        for entry in entries.iter() {
            println!("{entry:?}");
        }
        println!("{input}");
        assert_eq!(38898, entries.len());
    }

    #[test]
    fn parse_ifc_data_list() {
        assert_eq!(
            ifc_data_list(
                "#1=IFCORGANIZATION($,'Autodesk Revit Architecture 2011',$,$,$);\n\n#2=IFCAPPLICATION(#1,'2011','Autodesk Revit Architecture 2011','Revit');"
            ),
            Ok((
                "",
                vec![Entity {
                     id: 1,
                     name: "IFCORGANIZATION".to_string(),
                     s: vec![
                        "$","'Autodesk Revit Architecture 2011'","$","$","$"
                    ].iter().map(|s|s.to_string()).collect(),
                }
                ,Entity{
                    id:2,
                    name:"IFCAPPLICATION".to_string(),
                    s:vec![
                        "#1",
                        "'2011'",
                        "'Autodesk Revit Architecture 2011'",
                        "'Revit'"
                    ].iter().map(|s|s.to_string()).collect()}
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
                        "#1",
                        "'2011'",
                        "'Autodesk Revit Architecture 2011'",
                        "'Revit'"
                    ]
                    .iter()
                    .map(|s| s.to_string())
                    .collect()
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
                        "#1",
                        "'2011'",
                        "'Autodesk Revit Architecture 2011'",
                        "'Revit'"
                    ]
                    .iter()
                    .map(|s| s.to_string())
                    .collect()
                }
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
                        "#1",
                        "'2011'",
                        "'Autodesk Revit Architecture 2011'",
                        "'Revit'"
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
                Parameter::SimpleParameter(SimpleParameter::String(
                    "Autodesk Revit Architecture 2011".to_string()
                ))
            ))
        );
    }

    #[test]
    fn parse_ifc_parameter_list() {
        assert_eq!(ifc_parameter_list_inner("a,b"), Ok(("", vec!["a", "b",])));
        assert_eq!(
            ifc_parameter_list_inner("#1,'2011','Autodesk Revit Architecture 2011','Revit'"),
            Ok((
                "",
                vec![
                    "#1",
                    "'2011'",
                    "'Autodesk Revit Architecture 2011'",
                    "'Revit'"
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
                    "#1",
                    "'2011'",
                    "'Autodesk Revit Architecture 2011'",
                    "'Revit'"
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