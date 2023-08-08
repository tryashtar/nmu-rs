use std::collections::HashMap;
use std::fs::File;
use chrono::{DateTime, Utc};

fn main() {
    let dates = load_dates();
    println!("{}", dates.len());
}

fn load_dates() -> HashMap<String, DateTime<Utc>> {
    let f = File::open("/d/Music/.music-cache/datecache.yaml");
    match f {
        Err(_) => HashMap::new(),
        Ok(s) => {
            let yaml = serde_yaml::from_reader(s);
            match yaml {
                Err(e) =>  {
                    println!("{}", e);
                    HashMap::new()},
                Ok(s) => s
            }
        }
    }
}