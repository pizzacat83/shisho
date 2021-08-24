use crate::{language::Queryable, pattern::Pattern};
use anyhow::Result;
use std::{
    convert::{TryFrom, TryInto},
    marker::PhantomData,
};

pub const SHISHO_NODE_METAVARIABLE_NAME: &str = "shisho_metavariable_name";
pub const SHISHO_NODE_METAVARIABLE: &str = "shisho_metavariable";
pub const SHISHO_NODE_ELLIPSIS_METAVARIABLE: &str = "shisho_ellipsis_metavariable";
pub const SHISHO_NODE_ELLIPSIS: &str = "shisho_ellipsis";

#[derive(Debug)]
pub struct Query<T>
where
    T: Queryable,
{
    query: tree_sitter::Tree,
    _marker: PhantomData<T>,
}

impl<T> AsRef<tree_sitter::Tree> for Query<T>
where
    T: Queryable,
{
    fn as_ref(&self) -> &tree_sitter::Tree {
        &self.query
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct MetavariableId(pub String);

impl<T> TryFrom<&str> for Query<T>
where
    T: Queryable,
{
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, anyhow::Error> {
        let p = Pattern::from(value);
        p.try_into()
    }
}

impl<T> TryFrom<Pattern<T>> for Query<T>
where
    T: Queryable,
{
    type Error = anyhow::Error;

    fn try_from(value: Pattern<T>) -> Result<Self, anyhow::Error> {
        let query = value.to_tstree()?;
        Ok(Query {
            query,
            _marker: PhantomData,
        })
    }
}
