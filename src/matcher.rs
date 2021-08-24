use anyhow::{anyhow, Result};
use tree_sitter::Point;

use crate::{
    constraint::{Constraint, Predicate},
    language::Queryable,
    query::{MetavariableId, Query},
    tree::PartialTree,
};
use std::{collections::HashMap, convert::TryFrom};

pub struct QueryMatcher<'tree, 'query, T>
where
    T: Queryable,
{
    query: &'query Query<T>,
    tree: &'tree PartialTree<'tree, T>,
}

impl<'tree, 'query, T> QueryMatcher<'tree, 'query, T>
where
    T: Queryable,
{
    pub fn new(tree: &'tree PartialTree<'tree, T>, query: &'query Query<T>) -> Self {
        QueryMatcher { query, tree }
    }
}

impl<'tree, 'query, T> Iterator for QueryMatcher<'tree, 'query, T>
where
    T: Queryable,
{
    type Item = MatchedItem<'tree>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!("foo")
    }
}

#[derive(Debug)]
pub struct MatchedItem<'tree> {
    pub raw: &'tree [u8],
    pub top: CaptureItem<'tree>,
    pub captures: HashMap<MetavariableId, CaptureItem<'tree>>,
}

impl<'tree> MatchedItem<'tree> {
    pub fn get_captured_string(&self, id: &MetavariableId) -> Option<&'tree str> {
        let capture = self.captures.get(&id)?;
        let t = capture.utf8_text(self.raw).unwrap();
        Some(t)
    }

    pub fn get_captured_items(&self, id: &MetavariableId) -> Option<&CaptureItem<'tree>> {
        self.captures.get(&id)
    }

    pub fn satisfies_all<T: Queryable>(&self, constraints: &Vec<Constraint<T>>) -> bool {
        constraints
            .iter()
            .all(|constraint| self.satisfies(constraint))
    }

    pub fn satisfies<T: Queryable>(&self, constraint: &Constraint<T>) -> bool {
        if !self.captures.contains_key(&constraint.target) {
            return false;
        }

        match &constraint.predicate {
            Predicate::MatchQuery(q) => self
                .get_captured_items(&constraint.target)
                .unwrap()
                .as_vec()
                .into_iter()
                .any(|node| {
                    let ptree = PartialTree::<T>::new(node.clone(), self.raw);
                    let matches = ptree.matches(q).collect::<Vec<MatchedItem>>();
                    matches.len() > 0
                }),
            Predicate::NotMatchQuery(q) => self
                .get_captured_items(&constraint.target)
                .unwrap()
                .as_vec()
                .into_iter()
                .all(|node| {
                    let ptree = PartialTree::<T>::new(node.clone(), self.raw);
                    let matches = ptree.matches(q).collect::<Vec<MatchedItem>>();
                    matches.len() == 0
                }),
            Predicate::MatchRegex(r) => {
                r.is_match(self.get_captured_string(&constraint.target).unwrap())
            }
            Predicate::NotMatchRegex(r) => {
                !r.is_match(self.get_captured_string(&constraint.target).unwrap())
            }
        }
    }
}

#[derive(Debug)]
pub struct CaptureItem<'tree>(Vec<tree_sitter::Node<'tree>>);

impl<'tree> TryFrom<Vec<tree_sitter::Node<'tree>>> for CaptureItem<'tree> {
    type Error = anyhow::Error;

    fn try_from(value: Vec<tree_sitter::Node<'tree>>) -> Result<Self, Self::Error> {
        if value.len() == 0 {
            return Err(anyhow!("no item included"));
        }

        // TODO (y0n3uchy): check all capture items are consecutive

        Ok(Self(value))
    }
}

impl<'tree> CaptureItem<'tree> {
    pub fn as_vec(&self) -> &Vec<tree_sitter::Node<'tree>> {
        &self.0
    }

    pub fn push(&mut self, n: tree_sitter::Node<'tree>) {
        self.0.push(n)
    }

    pub fn start_position(&self) -> Point {
        self.as_vec().first().unwrap().start_position()
    }

    pub fn end_position(&self) -> Point {
        self.as_vec().last().unwrap().end_position()
    }

    pub fn range_for_view<T: Queryable + 'static>(&self) -> (Point, Point) {
        (
            T::range_for_view(self.as_vec().first().unwrap()).0,
            T::range_for_view(self.as_vec().last().unwrap()).1,
        )
    }

    pub fn start_byte(&self) -> usize {
        self.as_vec().first().unwrap().start_byte()
    }

    pub fn end_byte(&self) -> usize {
        self.as_vec().last().unwrap().end_byte()
    }

    pub fn utf8_text<'a>(&self, source: &'a [u8]) -> Result<&'a str, core::str::Utf8Error> {
        core::str::from_utf8(&source[self.start_byte()..self.end_byte()])
    }
}
