mod docker;
mod go;
mod hcl;
use anyhow::Result;

use crate::query::CaptureId;

pub use self::docker::Dockerfile;
pub use self::go::Go;
pub use self::hcl::HCL;

pub trait Queryable {
    fn target_language() -> tree_sitter::Language;
    fn query_language() -> tree_sitter::Language;

    fn extract_query_nodes(root: &tree_sitter::Tree) -> Result<Vec<tree_sitter::Node>>;

    fn is_leaf(node: &tree_sitter::Node) -> bool;
    fn range_for_view(node: &tree_sitter::Node) -> (tree_sitter::Point, tree_sitter::Point);

    fn generate_node_constraints(
        _node: &tree_sitter::Node,
        node_value: &str,
        capture_id: &CaptureId,
    ) -> String {
        // node to regex 
        format!(
            r#"(#matches? @{} "{}")"#,
            capture_id.as_ref().to_string(),
            node_value.replace("\"", "\\\"")
        )
    }

    fn normalize_leaf(s: &str) -> String {
        s.to_string()
    }
}
