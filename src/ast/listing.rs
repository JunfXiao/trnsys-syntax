use serde::{Deserialize, Serialize};

/// WIDTH n
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Width(pub usize);

/// NOLIST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NoList();

/// LIST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct List();

/// MAP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Map();
