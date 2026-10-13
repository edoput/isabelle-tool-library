theory MCP_Quoted_Field_Tests
  imports "MCP-Tool-Library.MCP_Tool_Library"
begin

mcp_tool quoted_field (description \<open>quoted field-name parser regression\<close>)
  for "theory" \<open>theory name\<close> :: "string"
  = run \<open>fn _ => fn {theory} => theory\<close>

mcp_tool unquoted_field (description \<open>unquoted field-name parser regression\<close>)
  for name \<open>name\<close> :: "string"
  = run \<open>fn _ => fn {name} => name\<close>

ML \<open>
val _ = \<^assert> (MCP_Protocol.run_tool \<^context> "MCP_Quoted_Field_Tests.quoted_field"
  (MCP_Codec.V_Object [("theory", MCP_Codec.V_String "Pure")]) =
  ("ok", "Pure"));
val _ = \<^assert> (MCP_Protocol.run_tool \<^context> "MCP_Quoted_Field_Tests.unquoted_field"
  (MCP_Codec.V_Object [("name", MCP_Codec.V_String "ordinary")]) =
  ("ok", "ordinary"));
\<close>

end
