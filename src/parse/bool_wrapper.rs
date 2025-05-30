use std::str::FromStr;
use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into};
use crate::error::Error;

#[derive(From, Into, Clone, Debug, AsRef, AsMut, Deref, DerefMut)]
pub struct BitBool(bool);

impl FromStr for BitBool {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "0" => Ok(BitBool(false)),
            "1" => Ok(BitBool(true)),
            _ => Err(Error::ConversionError {
                input: input.to_string(),
                target: "boolean".to_string(),
            })
        }
    }
}