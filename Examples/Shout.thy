theory Shout
  imports "MCP-Tool-Library-HOL.MCP_Library"
begin

text \<open>
  A minimal verifier-native tool: declaration and implementation live in
  theory context, independently of any MCP server or transport.
\<close>

mcp_tool shout (description "uppercase the input")
  for "text" "text to uppercase" :: "string"
  = run \<open>fn _ => fn {text} => String.map Char.toUpper text\<close>

ML \<open>
  val input = MCP_Codec.V_Object [("text", MCP_Codec.V_String "hello verifier")];
  val result = MCP_Protocol.run_tool \<^context> "Shout.shout" input;
  val _ = \<^assert> (result = ("ok", "HELLO VERIFIER"));
  val _ = writeln ("Shout.shout: " ^ #2 (result));
\<close>

end
