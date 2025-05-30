use core::fmt;
use crate::error::{ErrorScope, Error as ErrorBase};
use error_stack::{Context, Report};
use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};
use std::error::Error as StdError;
use std::fmt::Debug;
use nom::Err as NomErr;
use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into};

#[derive(From, Into, AsMut, AsRef, Deref, DerefMut)]
#[repr(transparent)]
pub struct ReportWrapper<T>(pub Report<T>)
where
    T: Context + StdError + Sized;


impl<T> ReportWrapper<T>
where
    T: Context + StdError + Sized,
{
    #[track_caller]
    pub fn new<R>(err: R) -> Self 
    where R: Into<T>
    {
        Self(error_stack::Report::new(err.into()))
    }

    #[track_caller]
    pub fn attach_printable<A>(mut self, attachment: A) -> Self
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.0 = self.0.attach_printable(attachment);
        self
    }

    #[track_caller]
    pub fn attach<A>(mut self, attachment: A) -> Self
    where
        A: Send + Sync + 'static,
    {
        self.0 = self.0.attach(attachment);
        self
    }


    #[track_caller]
    pub fn change_context<C>(self, context: C) -> ReportWrapper<C>
    where
        C: Context + StdError + Sized,
    {
        ReportWrapper(self.0.change_context(context))
    }
    
    
}





pub trait NewFromErrorKind<T> {
    #[track_caller]
    fn new_from_error_kind(input: T, kind: ErrorKind, scope: ErrorScope) -> Self;
}

impl<I, T> ContextError<I> for ReportWrapper<T>
where
    T: NewFromErrorKind<I> + Context + StdError + Sized,
{
    fn add_context(_input: I, _ctx: &'static str, other: Self) -> Self {
        
        other.attach_printable(_ctx)
    }
}

impl<I, T> ParseError<I> for ReportWrapper<T>
where
    T: NewFromErrorKind<I> + Context + StdError + Sized,
{
    #[track_caller]
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        ReportWrapper(Report::new(T::new_from_error_kind(input, kind, ErrorScope::Document)))
    }

    #[track_caller]
    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        let report = other.0.change_context(T::new_from_error_kind(input, kind, ErrorScope::Document));
        ReportWrapper(report)
    }
}

impl<T> From<T> for ReportWrapper<T>
where
    T: Context + StdError + Sized,
{
    #[track_caller]
    fn from(err: T) -> Self {
        ReportWrapper(Report::new(err))
    }
}

impl<I, E, T> FromExternalError<I, E> for ReportWrapper<T>
where
    I: ToString,
    E: StdError + Send + Sync + 'static,
    T: StdError + Context + Send + Sync + ParseError<I>,
{
    #[track_caller]
    fn from_external_error(input: I, kind: ErrorKind, e: E) -> Self {
        ReportWrapper(Report::new(e).change_context(T::from_error_kind(input, kind)))
    }
}

impl<T> std::fmt::Display for ReportWrapper<T>
where
    T: Context + StdError + Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> Debug for ReportWrapper<T>
where
    T: Context + StdError + Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T> StdError for ReportWrapper<T>
where
    T: Context + StdError + Sized,
{
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.0.current_context().source()
    }
}

impl From<strum::ParseError> for ReportWrapper<ErrorBase> {
    #[track_caller]
    fn from(value: strum::ParseError) -> Self {
        ReportWrapper(Report::new(value.into()))
    }
}

impl<R> From<ReportWrapper<R>> for NomErr<ReportWrapper<R>,ReportWrapper<R>>
where R: StdError + Context + Send + Sync
{
    #[track_caller]
    fn from(value: ReportWrapper<R>) -> Self {
        NomErr::Error(value)
    }
}

impl<RW> From<NomErr<RW,RW>> for ReportWrapper<RW>
where RW: StdError + Context + Send + Sync + Default,
{
    #[track_caller]
    fn from(value: NomErr<RW,RW>) -> Self {
        match value {
            NomErr::Error(e) => e.into(),
            NomErr::Failure(e) => e.into(),
            NomErr::Incomplete(needed) => ReportWrapper::new(
                RW::default(),
            ).attach_printable(
                format!("Incomplete input. Need {:?} more chars", needed)
            ),
        }
    }
}

impl<T,K> NewFromErrorKind<K> for ReportWrapper<T> 
where T: NewFromErrorKind<K> + StdError + Sync + Send + 'static,
{
    #[track_caller]
    fn new_from_error_kind(input: K, kind: ErrorKind, error_scope: ErrorScope) -> Self {
        ReportWrapper(Report::new(T::new_from_error_kind(input, kind, error_scope)))
    }
}


impl<T> From<NomErr<ReportWrapper<T>>> for ReportWrapper<T>
where T: StdError + Send + Sync + 'static + NewFromErrorKind<String>,
{
    #[track_caller]
    fn from(value: NomErr<ReportWrapper<T>>) -> Self {
        match value {
            NomErr::Error(e) => e,
            NomErr::Failure(e) => e,
            NomErr::Incomplete(needed) => ReportWrapper(T::new_from_error_kind(
                format!("Incomplete input. Need {:?} more chars", needed),
                ErrorKind::Eof,
                ErrorScope::Document,
            ).into()),
        }
    }
}