use derive_more::{AsMut, AsRef, Constructor, Deref, DerefMut, Display};
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, AddAssign};

#[derive(Clone, Serialize, Deserialize, Default, Display)]
#[display("Pre: {comment_pre:#?}, \nPost: {comment_post:#?}, Inline: \n{comment_inline:#?}")]
pub struct Comments {
    pub comment_inline: Option<String>,
    pub comment_pre: Option<Vec<String>>,
    pub comment_post: Option<Vec<String>>,
}

impl Comments {
    pub fn with_inline<T>(comment_inline: Option<String>) -> Self {
        Self {
            comment_inline,
            comment_pre: None,
            comment_post: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.comment_inline.is_none() && self.comment_pre.is_none() && self.comment_post.is_none()
    }
}

impl Debug for Comments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut ds = f.debug_struct("Comments");
        let mut has_fields = false;
        if let Some(inline) = &self.comment_inline {
            ds.field("comment_inline", inline);
            has_fields = true;
        }
        if let Some(pre) = &self.comment_pre {
            ds.field("comment_pre", pre);
            has_fields = true;
        }
        if let Some(post) = &self.comment_post {
            ds.field("comment_post", post);
            has_fields = true;
        }

        if has_fields {
            ds.finish()
        } else {
            write!(f, "<Empty>")
        }
    }
}

impl Add for Comments {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign for Comments {
    fn add_assign(&mut self, rhs: Self) {
        if let Some(inline) = rhs.comment_inline {
            if self.comment_inline.is_none() {
                self.comment_inline = Some(inline);
            } else {
                let original = self.comment_inline.take().unwrap_or_default();
                self.comment_inline.replace(original + "\n" + &inline);
            }
        }
        if let Some(pre) = rhs.comment_pre {
            if self.comment_pre.is_none() {
                self.comment_pre = Some(pre);
            } else {
                self.comment_pre.as_mut().unwrap().extend(pre);
            }
        }
        if let Some(post) = rhs.comment_post {
            if self.comment_post.is_none() {
                self.comment_post = Some(post);
            } else {
                self.comment_post.as_mut().unwrap().extend(post);
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, AsRef, AsMut, Deref, DerefMut, Constructor)]
pub struct Commented<T: Debug> {
    #[as_ref]
    #[as_mut]
    #[deref]
    #[deref_mut]
    pub value: T,
    pub comments: Comments,
}

impl<T> From<T> for Commented<T>
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

impl<T: Display + std::fmt::Debug> Display for Commented<T> {
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
