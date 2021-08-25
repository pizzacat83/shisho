use anyhow::{anyhow, Result};
use tree_sitter::Point;

use crate::{
    constraint::{Constraint, Predicate},
    language::Queryable,
    query::{
        MetavariableId, Query, SHISHO_NODE_ELLIPSIS, SHISHO_NODE_ELLIPSIS_METAVARIABLE,
        SHISHO_NODE_METAVARIABLE, SHISHO_NODE_METAVARIABLE_NAME,
    },
    tree::PartialTree,
};
use std::collections::HashMap;

pub struct QueryMatcher<'tree, 'query, T>
where
    T: Queryable,
{
    cursor: Option<tree_sitter::TreeCursor<'tree>>,
    items: Vec<MatchedItem<'tree>>,

    tree: &'tree PartialTree<'tree, T>,
    query: &'query Query<T>,
}

type UnverifiedMetavariable<'tree> = (MetavariableId, CaptureItem<'tree>);

#[derive(Debug, Default)]
pub struct MatcherState<'tree> {
    subtree: Option<ConsecutiveNodes<'tree>>,
    captures: Vec<UnverifiedMetavariable<'tree>>,
}

impl<'tree, 'query, T> QueryMatcher<'tree, 'query, T>
where
    T: Queryable,
{
    fn yield_next_node(&mut self) -> Option<tree_sitter::Node<'tree>> {
        if let Some(cursor) = self.cursor.as_mut() {
            let node = cursor.node();
            if !cursor.goto_first_child() && !cursor.goto_next_sibling() {
                while cursor.goto_parent() {
                    if cursor.goto_next_sibling() {
                        return Some(node);
                    }
                }
                self.cursor = None;
            }
            Some(node)
        } else {
            None
        }
    }
}

impl<'tree, 'query, T> QueryMatcher<'tree, 'query, T>
where
    T: Queryable,
{
    pub fn new(tree: &'tree PartialTree<'tree, T>, query: &'query Query<T>) -> Self {
        QueryMatcher {
            query,
            tree,
            cursor: Some(tree.top.walk()),
            items: vec![],
        }
    }

    fn match_sibillings(
        &self,
        tsibilings: Vec<tree_sitter::Node<'tree>>,
        qsibilings: Vec<tree_sitter::Node<'query>>,
    ) -> Vec<MatcherState<'tree>> {
        // verify children
        let mut queue: Vec<(usize, usize, Vec<UnverifiedMetavariable>)> = vec![(0, 0, vec![])];
        let mut result: Vec<MatcherState> = vec![];

        while let Some((tidx, qidx, captures)) = queue.pop() {
            match (tsibilings.get(tidx), qsibilings.get(qidx)) {
                (None, None) => result.push(MatcherState {
                    subtree: Some(ConsecutiveNodes::from(tsibilings.clone())),
                    captures,
                }),
                (Some(tchild), Some(qchild))
                    if qchild.kind() == SHISHO_NODE_ELLIPSIS
                        || qchild.kind() == SHISHO_NODE_ELLIPSIS_METAVARIABLE =>
                {
                    let mid = MetavariableId(self.variable_name_of(&qchild).to_string());
                    let mut captured_nodes = vec![];
                    for tcidx in tidx..(tsibilings.len() + 1) {
                        queue.push((
                            tcidx,
                            qidx + 1,
                            [
                                vec![(mid.clone(), CaptureItem::from(captured_nodes.clone()))],
                                captures.clone(),
                            ]
                            .concat(),
                        ));
                        if let Some(tchild) = tsibilings.get(tcidx) {
                            captured_nodes.push(tchild.clone());
                        }
                    }
                }

                (Some(tchild), Some(qchild)) => {
                    for submatch in self.match_subtree(Some(tchild.clone()), Some(qchild.clone())) {
                        queue.push((
                            tidx + 1,
                            qidx + 1,
                            [captures.clone(), submatch.captures].concat(),
                        ));
                    }
                }
                _ => (),
            }
        }
        result
    }

    fn match_subtree(
        &self,
        tnode: Option<tree_sitter::Node<'tree>>,
        qnode: Option<tree_sitter::Node<'query>>,
    ) -> Vec<MatcherState<'tree>> {
        match (tnode, qnode) {
            (None, None) => {
                // base case
                vec![Default::default()]
            }
            (Some(tnode), Some(qnode)) => {
                let subtree = ConsecutiveNodes::from(vec![tnode]);

                match qnode.kind() {
                    s if s == SHISHO_NODE_METAVARIABLE => {
                        let mid = MetavariableId(self.variable_name_of(&qnode).to_string());
                        let item = CaptureItem::from(vec![tnode]);
                        vec![MatcherState {
                            subtree: Some(subtree),
                            captures: vec![(mid, item)],
                        }]
                    }
                    _ if qnode.child_count() == 0 || T::is_leaf(&qnode) => {
                        // TODO: care about patterns in string especially if string constraint
                        if self.tree.value_of(&tnode) == self.query.value_of(&qnode) {
                            vec![MatcherState {
                                subtree: Some(subtree),
                                captures: vec![],
                            }]
                        } else {
                            vec![]
                        }
                    }
                    _ => {
                        // verify tnode itself
                        if tnode.kind() != qnode.kind() {
                            return vec![];
                        }

                        let tchildren = tnode.children(&mut tnode.walk()).collect();
                        let qchildren = qnode.children(&mut qnode.walk()).collect();
                        self.match_sibillings(tchildren, qchildren)
                            .into_iter()
                            .map(|submatch| MatcherState {
                                subtree: Some(subtree.clone()),
                                captures: submatch.captures,
                            })
                            .collect()
                    }
                }
            }
            _ => vec![],
        }
    }

    fn variable_name_of(&self, qnode: &tree_sitter::Node) -> &str {
        qnode
            .named_children(&mut qnode.walk())
            .find(|child| child.kind() == SHISHO_NODE_METAVARIABLE_NAME)
            .map(|child| self.query.value_of(&child))
            .expect(
                format!(
                    "{} did not have {}",
                    SHISHO_NODE_ELLIPSIS_METAVARIABLE, SHISHO_NODE_METAVARIABLE_NAME
                )
                .as_str(),
            )
    }
}

impl<'tree, 'query, T> Iterator for QueryMatcher<'tree, 'query, T>
where
    T: Queryable,
{
    type Item = MatchedItem<'tree>;

    fn next(&mut self) -> Option<Self::Item> {
        let qnodes = self.query.tsnodes();

        loop {
            if let Some(mitem) = self.items.pop() {
                return Some(mitem);
            }

            if let Some(tnode) = self.yield_next_node() {
                let matches = self.match_subtree(Some(tnode), Some(qnodes[0]));

                // TODO: convert matcherState -> MatchedItem validating equivalence
                for mitem in matches {
                    self.items.push(MatchedItem {
                        raw: self.tree.as_ref(),
                        top: mitem.subtree.unwrap(),
                        captures: mitem
                            .captures
                            .into_iter()
                            .collect::<HashMap<MetavariableId, CaptureItem>>(),
                    });
                }
            } else {
                return None;
            }
        }
    }
}

#[derive(Debug)]
pub struct MatchedItem<'tree> {
    pub raw: &'tree [u8],
    pub top: ConsecutiveNodes<'tree>,
    pub captures: HashMap<MetavariableId, CaptureItem<'tree>>,
}

impl<'tree> MatchedItem<'tree> {
    pub fn get_captured_string(&'tree self, id: &MetavariableId) -> Option<&'tree str> {
        let capture = self.captures.get(&id)?;
        match capture {
            CaptureItem::Empty => None,
            CaptureItem::Literal(s) => Some(s.as_str()),
            CaptureItem::Nodes(n) => Some(n.utf8_text(self.raw).unwrap()),
        }
    }

    pub fn get_captured_items(&self, id: &MetavariableId) -> Option<&CaptureItem> {
        self.captures.get(&id)
    }

    pub fn satisfies_all<T: Queryable>(&self, constraints: &Vec<Constraint<T>>) -> Result<bool> {
        for c in constraints {
            if !self.satisfies(c)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub fn satisfies<T: Queryable>(&self, constraint: &Constraint<T>) -> Result<bool> {
        if !self.captures.contains_key(&constraint.target) {
            return Ok(false);
        }

        match &constraint.predicate {
            Predicate::MatchQuery(q) => {
                let captured_item = self.get_captured_items(&constraint.target).unwrap();
                match captured_item {
                    CaptureItem::Empty => Ok(false),
                    CaptureItem::Literal(_) => Err(anyhow!(
                        "match-query predicate for string literals is not supported"
                    )),
                    CaptureItem::Nodes(n) => Ok(n.as_vec().into_iter().any(|node| {
                        let ptree = PartialTree::<T>::new(node.clone(), self.raw);
                        let matches = ptree.matches(q).collect::<Vec<MatchedItem>>();
                        matches.len() > 0
                    })),
                }
            }
            Predicate::NotMatchQuery(q) => {
                let captured_item = self.get_captured_items(&constraint.target).unwrap();
                match captured_item {
                    CaptureItem::Empty => Ok(true),
                    CaptureItem::Literal(_) => Err(anyhow!(
                        "match-query predicate for string literals is not supported"
                    )),
                    CaptureItem::Nodes(n) => Ok(n.as_vec().into_iter().all(|node| {
                        let ptree = PartialTree::<T>::new(node.clone(), self.raw);
                        let matches = ptree.matches(q).collect::<Vec<MatchedItem>>();
                        matches.len() == 0
                    })),
                }
            }

            Predicate::MatchRegex(r) => {
                Ok(r.is_match(self.get_captured_string(&constraint.target).unwrap()))
            }
            Predicate::NotMatchRegex(r) => {
                Ok(!r.is_match(self.get_captured_string(&constraint.target).unwrap()))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum CaptureItem<'tree> {
    Empty,
    Literal(String),
    Nodes(ConsecutiveNodes<'tree>),
}

#[derive(Debug, Clone)]
pub struct ConsecutiveNodes<'tree>(Vec<tree_sitter::Node<'tree>>);

impl<'tree> From<Vec<tree_sitter::Node<'tree>>> for CaptureItem<'tree> {
    fn from(value: Vec<tree_sitter::Node<'tree>>) -> Self {
        if value.len() == 0 {
            Self::Empty
        } else {
            // TODO (y0n3uchy): check all capture items are consecutive
            Self::Nodes(ConsecutiveNodes(value))
        }
    }
}

impl<'tree> From<Vec<tree_sitter::Node<'tree>>> for ConsecutiveNodes<'tree> {
    fn from(value: Vec<tree_sitter::Node<'tree>>) -> Self {
        if value.len() == 0 {
            panic!("internal error; ConsecutiveNodes was generated from empty vec.");
        }
        ConsecutiveNodes(value)
    }
}

impl<'tree> ConsecutiveNodes<'tree> {
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
