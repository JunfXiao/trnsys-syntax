use std::borrow::Cow;
use derive_more::{AsMut, AsRef, Constructor, Deref, DerefMut, Display};
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};


#[derive(Debug, Clone, Serialize, Deserialize, Default, Display)]
#[display("Pre: {comment_pre:#?}, \nPost: {comment_post:#?}, Inline: \n{comment_inline:#?}")]
pub struct Comments<'a> {
    pub comment_inline: Option<Cow<'a,str>>,
    pub comment_pre: Option<Vec<Cow<'a,str>>>,
    pub comment_post: Option<Vec<Cow<'a,str>>>,
}

pub trait CommentTrait {}

impl<'a> Comments<'a> {
    pub fn with_inline<T>(comment_inline: Option<T>) -> Self
    where
        T: Into<Cow<'a, str>>,
    {
        Self {
            comment_inline: comment_inline.map(Into::into),
            comment_pre: None,
            comment_post: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, AsRef, AsMut, Deref, DerefMut, Constructor)]
pub struct Commented<'a, T: Debug> {
    #[as_ref]
    #[as_mut]
    #[deref]
    #[deref_mut]
    pub value: T,
    pub comments: Comments<'a>,
}

impl<'a, T> From<T> for Commented<'a, T>
where
    T: Debug,
{
    fn from(value: T) -> Self {
        Self {
            value,
            comments: Comments::default(),
        }
    }
}



impl<'a, T: Display + std::fmt::Debug> Display for Commented<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(ref pre) = self.comments.comment_pre {
            for line in pre {
                write!(f, "! {}\n", line)?;
            }
        }
        write!(f, "{}", self.value)?;
        if let Some(ref inline) = self.comments.comment_inline {
            write!(f, "\t! {}", inline)?;
        }
        if let Some(ref post) = self.comments.comment_post {
            for line in post {
                write!(f, "\n! {}", line)?;
            }
        }
        
        Ok(())
        
    }
}