theory Basic_Tools
  imports "MCP-Tool-Library-HOL.MCP_Library"
begin

text \<open>Small typed ML-backed tools advertised through the MCP catalogue.\<close>

mcp_tool hello_world
  (description "Return a hello-world greeting")
  :: "unit"
  = run \<open>fn _ => fn () => "Hello, world!"\<close>

mcp_tool current_time
  (description "Return the current local date and time")
  :: "unit"
  = run \<open>fn _ => fn () =>
      Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal (Time.now ()))\<close>

mcp_tool hello_name
  (description "Greet a person by name")
  for name "Name of the person to greet" :: "string"
  = run \<open>fn _ => fn {name} => "Hello, " ^ name ^ "!"\<close>

end
