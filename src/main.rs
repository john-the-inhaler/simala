use std::str::CharIndices;

use std::io::{self, Read};
use std::fs::File;
pub fn main() -> io::Result<()> {
    let mut file = File::open("test.sima")?;
    let mut buff = String::with_capacity(file.metadata()?.len() as usize);
    file.read_to_string(&mut buff)?;
    Ok(())
}
