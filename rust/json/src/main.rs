use env_logger::{from_env, Env};
use log::error;
use serde_derive::{Deserialize, Serialize};
use serde_json;

#[derive(Debug, Deserialize, Serialize)]
struct Person<'a> {
    #[serde(rename = "fullname")]
    name: &'a str,
    age: u8,
    phone: Option<Vec<&'a str>>,
}

fn main() {
    // Set logger to log at all levels
    from_env(Env::new().default_filter_or("trace")).init();

    let json_str =
        r#"{ "fullname": "John Doe", "age": 21, "phone": ["512-555-1234", "512-555-5555"] }"#;

    deserialize_value(json_str);
    deserialize_derived(json_str);

    let jane = Person {
        name: "Jane Doe",
        age: 40,
        phone: Some(vec!["512-555-9999", "512-555-5555"]),
    };

    match serde_json::to_string(&jane) {
        Ok(p) => println!("JSON String: '{}'", p),
        Err(error) => error!("unable to marshal struct as json string: {}", error),
    };
}

fn deserialize_value(json_str: &str) {
    let js_obj: Result<serde_json::Value, _> = serde_json::from_str(json_str);
    match js_obj {
        Ok(p) => {
            if let Some(name) = p["fullname"].as_str() {
                println!("Name = {}", name);
            } else {
                error!("unable to unmarshal field 'name' as string");
            }
        }
        Err(error) => {
            error!("unable to unmarshal string as json object: {}", error);
        }
    }
}

fn deserialize_derived(json_str: &str) {
    let js_obj: Result<Person, _> = serde_json::from_str(json_str);
    match js_obj {
        Ok(p) => println!("Person = {:#?}", p),
        Err(error) => error!("unable to unmarshal string as json object: {}", error),
    }
}
