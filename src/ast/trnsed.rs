// src/ast/trnsed.rs
use super::Span;
use serde::{Deserialize, Serialize};

/// Represents a TRNSED command (line starting with *|)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TrnsedCommand {
    Background {
        color: String,
        span: Span,
    },
    Color {
        id: usize,
        color: String,
        span: Span,
    },
    Size {
        id: usize,
        size: usize,
        span: Span,
    },
    Align {
        id: usize,
        alignment: String,
        span: Span,
    },
    Style {
        id: usize,
        style: Vec<String>,
        span: Span,
    },
    Header {
        content: String,
        span: Span,
    },
    Comment {
        content: String,
        span: Span,
    },
    Group {
        name: String,
        title: Option<String>,
        content: Vec<TrnsedCommand>,
        span: Span,
    },
    InputField {
        description: String,
        units1: String,
        units2: String,
        add: f64,
        mult: f64,
        min: f64,
        max: f64,
        help: usize,
        span: Span,
    },
    FileSelector {
        description: String,
        help: usize,
        span: Span,
    },
    Image {
        src: String,
        href: Option<String>,
        hint: Option<String>,
        span: Span,
    },
    TabWindow {
        name: String,
        content: Vec<TrnsedCommand>,
        span: Span,
    },
}
