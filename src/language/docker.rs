use anyhow::Result;

use super::Queryable;

#[derive(Debug, Clone)]
pub struct Dockerfile;

impl Queryable for Dockerfile {
    fn target_language() -> tree_sitter::Language {
        tree_sitter_dockerfile::language()
    }

    fn query_language() -> tree_sitter::Language {
        tree_sitter_dockerfile_query::language()
    }

    fn extract_query_nodes<'tree>(
        root: &'tree tree_sitter::Tree,
    ) -> Result<Vec<tree_sitter::Node<'tree>>> {
        // TODO (y0n3uchy): this should be done more strictly.

        // see `//third_party/tree-sitter-dockerfile-query/grammar.js`
        let source_file = root.root_node();

        let mut cursor = source_file.walk();
        Ok(source_file.named_children(&mut cursor).collect())
    }

    fn is_leaf(_node: &tree_sitter::Node) -> bool {
        false
    }

    fn range_for_view(node: &tree_sitter::Node) -> (tree_sitter::Point, tree_sitter::Point) {
        (node.start_position(), node.end_position())
    }

    fn normalize_leaf(s: &str) -> String {
        s.to_ascii_uppercase()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        query::{MetavariableId, Query, TSQueryString},
        tree::Tree,
    };
    use std::convert::TryFrom;

    #[test]
    fn test_from_instruction() {
        {
            let query = Query::<Dockerfile>::try_from(r#"FROM :[A]"#).unwrap();
            let tree = Tree::<Dockerfile>::try_from(r#"FROM name"#).unwrap();
            let ptree = tree.to_partial();
            let session = ptree.matches(&query);
            let c = session.collect();
            assert_eq!(c.len(), 1);
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("A".into())),
                Some("name")
            );
        }

        {
            let query = Query::<Dockerfile>::try_from(r#"FROM :[A]::[B]"#).unwrap();
            let tree = Tree::<Dockerfile>::try_from(r#"FROM name:tag"#).unwrap();
            let ptree = tree.to_partial();
            let session = ptree.matches(&query);
            let c = session.collect();
            assert_eq!(c.len(), 1);
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("A".into())),
                Some("name")
            );
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("B".into())),
                Some("tag")
            );
        }

        {
            let query = Query::<Dockerfile>::try_from(r#"FROM :[A]::[B]@:[HASH]"#).unwrap();

            let tree = Tree::<Dockerfile>::try_from(r#"FROM name:tag@hash"#).unwrap();
            let ptree = tree.to_partial();
            let session = ptree.matches(&query);
            let c = session.collect();
            assert_eq!(c.len(), 1);
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("A".into())),
                Some("name")
            );
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("B".into())),
                Some("tag")
            );
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("HASH".into())),
                Some("hash")
            );
        }

        {
            let query =
                Query::<Dockerfile>::try_from(r#"FROM :[A]::[B]@:[HASH] as :[ALIAS]"#).unwrap();

            let tree = Tree::<Dockerfile>::try_from(r#"FROM name:tag@hash as alias"#).unwrap();
            let ptree = tree.to_partial();
            let session = ptree.matches(&query);
            let c = session.collect();
            assert_eq!(c.len(), 1);
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("A".into())),
                Some("name")
            );
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("B".into())),
                Some("tag")
            );
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("HASH".into())),
                Some("hash")
            );
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("ALIAS".into())),
                Some("alias")
            );
        }
    }

    #[test]
    fn test_run_instruction() {
        {
            println!(
                "{}",
                TSQueryString::<Dockerfile>::try_from(r#"RUN :[X]"#)
                    .unwrap()
                    .query_string
            );
            let query = Query::<Dockerfile>::try_from(r#"RUN :[X]"#).unwrap();
            let tree =
                Tree::<Dockerfile>::try_from(r#"RUN echo "hosts: files dns" > /etc/nsswitch.conf"#)
                    .unwrap();
            let ptree = tree.to_partial();
            let session = ptree.matches(&query);
            let c = session.collect();
            assert_eq!(c.len(), 1);
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("X".into())),
                Some(r#"echo "hosts: files dns" > /etc/nsswitch.conf"#)
            );
        }
    }

    #[test]
    fn test_copy_instruction() {
        {
            let query = Query::<Dockerfile>::try_from(r#"COPY :[X] :[Y]"#).unwrap();

            let tree = Tree::<Dockerfile>::try_from(r#"COPY ./ /app"#).unwrap();
            let ptree = tree.to_partial();
            let session = ptree.matches(&query);
            let c = session.collect();
            assert_eq!(c.len(), 1);
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("X".into())),
                Some("./")
            );
            assert_eq!(
                c[0].get_captured_string(&MetavariableId("Y".into())),
                Some("/app")
            );
        }
    }
}
