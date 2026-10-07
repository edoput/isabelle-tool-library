theory MCP_Codecs
  imports MCP_Registry
  keywords "mcp_codec" :: thy_decl
begin

nonterminal codecs and codec_atom

syntax
  "_codec_name" :: "id_position \<Rightarrow> codec_atom" ("_" [1000] 1000)
  "_codec_long_name" :: "longid_position \<Rightarrow> codec_atom" ("_" [1000] 1000)
  "_codec_parentheses" :: "codecs \<Rightarrow> codec_atom" ("'(_')" [0] 1000)
  "" :: "codec_atom \<Rightarrow> codecs" ("_" [1000] 1000)
  "_codec_list" :: "codec_atom \<Rightarrow> codec_atom" ("_ list" [1000] 1000)
  "_codec_arrow" :: "codec_atom \<Rightarrow> codecs \<Rightarrow> codecs" ("_ \<Rightarrow> _" [1, 0] 0)

ML_file "mcp_codecs.ML"

setup \<open>fn thy =>
  thy |> Named_Target.theory_map (fn lthy =>
    lthy
    |> MCP_Codec.declare \<^binding>\<open>string\<close> "MCP_Codec.string" MCP_Codec.string |> #2
    |> MCP_Codec.declare \<^binding>\<open>bool\<close> "MCP_Codec.bool" MCP_Codec.bool |> #2
    |> MCP_Codec.declare \<^binding>\<open>int\<close> "MCP_Codec.integer" MCP_Codec.integer |> #2
    |> MCP_Codec.declare \<^binding>\<open>unit\<close> "MCP_Codec.unit" MCP_Codec.unit |> #2)\<close>

end
