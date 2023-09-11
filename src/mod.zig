//! forwarded namespaces for module

pub const Ast = @import("Ast.zig");

const parser = @import("parser/parser.zig");
pub const ParseError = parser.Error;
pub const ParseFragmentInto = parser.FragmentInto;
pub const parseFragment = parser.parseFragment;
pub const parse = parser.parse;