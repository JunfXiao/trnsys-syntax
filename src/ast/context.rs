use crate::error::{ContentError, RError};
use crate::parse::{Block, BlockKind};
use crate::serialize::{DeckBinaryCodec, DeckJsonCodec};
use derive_more::Unwrap;
use serde::ser::SerializeSeq;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::rc::Rc;
use thiserror::__private::AsDisplay;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Unwrap, Serialize, Deserialize)]
pub enum GlobalId {
    Unit(usize),
    Variable(String),
    /// A block without a specific unit number and a string identifier.
    Block(BlockKind, String),
    LogicalUnit(usize),
}

impl GlobalId {
    pub fn as_err(&self) -> ContentError {
        ContentError::UndefinedVariable {
            name: self.as_display().to_string(),
        }
    }

    pub fn type_str(&self) -> String {
        match self {
            GlobalId::Unit(_) => "Unit".to_string(),
            GlobalId::Variable(_) => "Variable/Constant".to_string(),
            GlobalId::Block(k, _) => format!("Block {}", k),
            GlobalId::LogicalUnit(_) => "Logical Unit".to_string(),
        }
    }

    pub fn value_str(&self) -> String {
        match self {
            GlobalId::Unit(unit) => unit.to_string(),
            GlobalId::Variable(var) => var.clone(),
            GlobalId::Block(_, index) => index.to_string(),
            GlobalId::LogicalUnit(unit) => unit.to_string(),
        }
    }
}

impl Display for GlobalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalId::Unit(unit) => write!(f, "Unit {}", unit),
            GlobalId::Variable(expr) => write!(f, "Equation/Constant {}", expr),
            GlobalId::Block(block_kind, block_index) => {
                write!(f, "Block {:?} at index {}", block_kind, block_index)
            }
            GlobalId::LogicalUnit(unit) => write!(f, "Logical Unit {}", unit),
        }
    }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct DocContext {
    #[serde(
        serialize_with = "serialize_vec_rc_refcell",
        deserialize_with = "deserialize_vec_rc_refcell"
    )]
    pub prev_blocks: Vec<Rc<RefCell<Block>>>,
    /// The hashmap of dependencies.
    ///
    /// The key is the id of the block, and the value is a set of ids that this block depends on.
    #[serde(skip_serializing)]
    dependencies: HashMap<GlobalId, HashSet<GlobalId>>,
    /// The set of reserved global ids.
    reserved_ids: HashSet<GlobalId>,
}

impl<'a> DocContext {
    pub fn new() -> Self {
        let mut reserved = HashSet::new();

        // System Logical Units
        // 4: TRNSYS Log File
        // 5: Keyboard
        // 6: TRNSYS Listing File
        // 7: UNITS.LAB
        // 8: ASHRAE.COF (for use with TYPE 19 only)
        // 9: TRNSYS input (Deck) file
        reserved.extend((4..=9).into_iter().map(|i| GlobalId::LogicalUnit(i)));

        // system defined 'TIME'
        reserved.extend(vec![GlobalId::Variable("TIME".to_string())]);

        Self {
            prev_blocks: Vec::new(),
            dependencies: HashMap::new(),
            reserved_ids: reserved,
        }
    }

    /// Ensure that the block kind is unique in the context.
    pub fn ensure_unique_kind(&self, block_kind: BlockKind) -> Result<(), RError> {
        if self
            .prev_blocks
            .iter()
            .any(|block| block.borrow().kind() == block_kind)
        {
            return Err(RError::new(ContentError::DuplicateDefinition {
                name: block_kind.to_string(),
            }));
        }
        Ok(())
    }

    /// Register a new dependency
    pub fn register_dep(
        &mut self,
        self_id: GlobalId,
        dependency_id: Option<Vec<GlobalId>>,
    ) -> Result<(), RError> {
        if self.dependencies.contains_key(&self_id) {
            return Err(RError::new(ContentError::DuplicateDefinition {
                name: format!("{:?}", self_id),
            }));
        }

        if self.reserved_ids.contains(&self_id) {
            return Err(RError::new(ContentError::InvalidValue {
                part: self_id.type_str(),
                value: self_id.value_str(),
                reason: "It is reserved by TRNSYS system and cannot be redefined.".to_string(),
            }));
        }

        let dependencies = match dependency_id {
            Some(deps) => deps.into_iter().collect(),
            None => HashSet::new(),
        };

        self.dependencies.insert(self_id, dependencies);
        Ok(())
    }

    /// Returns the dependencies of a given id
    pub fn dependent(&self, id: GlobalId) -> Option<&HashSet<GlobalId>> {
        self.dependencies.get(&id)
    }

    /// Check all registered dependencies for correctness
    pub fn check_dep(&self) -> Result<(), RError> {
        for (id, deps) in &self.dependencies {
            for dep in deps {
                if !self.dependencies.contains_key(dep) {
                    return Err(
                        RError::new(dep.as_err()).attach_printable(format!("It is in {:}", id))
                    );
                }
            }
        }
        Ok(())
    }

    pub fn dep(&self) -> &HashMap<GlobalId, HashSet<GlobalId>> {
        &self.dependencies
    }

    pub fn dep_mut(&mut self) -> &mut HashMap<GlobalId, HashSet<GlobalId>> {
        &mut self.dependencies
    }
}

impl DeckJsonCodec<'_> for DocContext {}

impl DeckBinaryCodec<'_> for DocContext {}

fn serialize_vec_rc_refcell<T, S>(
    vec: &Vec<Rc<RefCell<T>>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    T: Serialize,
    S: Serializer,
{
    let mut seq = serializer.serialize_seq(Some(vec.len()))?;
    for rc_cell in vec {
        let inner_ref = rc_cell.borrow();
        seq.serialize_element(&*inner_ref)?;
    }
    seq.end()
}

fn deserialize_vec_rc_refcell<'de, T, D>(deserializer: D) -> Result<Vec<Rc<RefCell<T>>>, D::Error>
where
    T: Deserialize<'de>,
    D: Deserializer<'de>,
{
    let vec_t: Vec<T> = Vec::deserialize(deserializer)?;
    Ok(vec_t
        .into_iter()
        .map(|t| Rc::new(RefCell::new(t)))
        .collect())
}
