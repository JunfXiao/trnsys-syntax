use std::fmt::Debug;
use crate::error::Error;

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into};

/// A trait for types that can be constructed from a string slice.
///
/// The aim of this trait is to avoid the potential conflict if TryFrom<&str> is implemented in std in the future.
pub trait TryFromStr<'a>: Sized {
    type Error;
    fn try_from_str(input: &'a str) -> Result<Self, Self::Error>;
}

impl<'a, T> TryFromStr<'a> for T
where
    T: std::str::FromStr + 'static,
    T::Err: Into<Error>,
{
    type Error = Error;
    fn try_from_str(input: &'a str) -> Result<Self, Self::Error> {
        input.parse::<T>().map_err(Into::into)
    }
}



#[derive(Clone, PartialEq, Eq, AsRef, AsMut, Deref, DerefMut, From, Into)]
pub struct StrRef<'a>(pub &'a str);

impl<'a> Debug for StrRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> TryFromStr<'a> for StrRef<'a> {
    type Error = Error;
    fn try_from_str(input: &'a str) -> Result<Self, Self::Error> {
        Ok(StrRef(input))
    }
}

impl<'a> PartialEq<&'a str> for StrRef<'a> {
    fn eq(&self, other: &&'a str) -> bool {
        self.0 == *other
    }
}

impl<'a> PartialEq<StrRef<'a>> for &'a str {
    fn eq(&self, other: &StrRef<'a>) -> bool {
        *self == other.0
    }
}


