theory Library_Inclusion
  imports "MCP-Tool-Library-HOL.MCP_Library" "HOL-Library.BNF_Corec"
begin

text \<open>The reusable import has no server mirrors or example tools.\<close>
ML \<open>\<^assert> (null (MCP_Tool.list (Context.Proof \<^context>)))\<close>

datatype inclusion_value = Inclusion_Value nat

mcp_tool echo (description \<open>standalone inclusion test\<close>)
  for input \<open>input\<close> :: "string"
  = run \<open>fn _ => fn {input} => input\<close>

ML \<open>
val _ = \<^assert> (MCP_Protocol.run_tool \<^context> "Library_Inclusion.echo"
  (MCP_Codec.V_Object [("input", MCP_Codec.V_String "standalone")]) =
  ("ok", "standalone"));
\<close>

end
