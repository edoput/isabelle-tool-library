/* Shared tool results and catalogue codecs; independent of server/session ownership. */

package isabelle.mcp

import isabelle.XML

object McpToolData {
  sealed abstract class Result { def ok: Boolean }
  case class Ok(text: String) extends Result { def ok = true }
  case class Error(message: String) extends Result { def ok = false }

  sealed abstract class Tool_Schema
  case object Schema_String extends Tool_Schema
  case object Schema_Bool extends Tool_Schema
  case object Schema_Integer extends Tool_Schema
  case class Schema_Array(item: Tool_Schema) extends Tool_Schema
  case class Schema_Object(fields: List[Schema_Field]) extends Tool_Schema
  case class Schema_Field(
    name: String, schema: Tool_Schema, description: String, required: Boolean)

  def decode_schema(body: XML.Body): Tool_Schema = {
    import XML.Decode._
    variant[Tool_Schema](List(
      { case _ => Schema_String },
      { case _ => Schema_Bool },
      { case _ => Schema_Integer },
      { case (_, ts) => Schema_Array(decode_schema(ts)) },
      { case (_, ts) =>
          Schema_Object(list(pair(string, pair(decode_schema _, pair(string, bool))))(ts)
            .map { case (name, (schema, (description, required))) =>
              Schema_Field(name, schema, description, required) }) }))(body)
  }

  /* mirrors MCP_Tool.annotations (MCP_Tools.thy) exactly -- the four MCP
     hint flags, each independently absent (no premise proven) or set
     (plans/param_schema_v2 step 5). No tag/variant hazard here (unlike
     Ptyp): every field is an independent option, so there is no
     positional ambiguity between two encoded values to get wrong. */
  case class Tool_Annotations(
    read_only: Option[Boolean],
    idempotent: Option[Boolean],
    destructive: Option[Boolean],
    open_world: Option[Boolean])

  object Tool_Annotations {
    /* MCP_Tool.default_annotations's mirror: a declaration whose form
       tag proves nothing about its behavior (plans/param_schema_v2). */
    val default: Tool_Annotations = Tool_Annotations(None, None, None, Some(false))
  }

  def decode_annotations(body: XML.Body): Tool_Annotations = {
    import XML.Decode._
    val (read_only, (idempotent, (destructive, open_world))) =
      pair(option(bool), pair(option(bool), pair(option(bool), option(bool))))(body)
    Tool_Annotations(read_only, idempotent, destructive, open_world)
  }

  case class Tool_Row(
    name: String,
    description: String,
    form: String,
    schema: Tool_Schema,
    annotations: Tool_Annotations)

  /* The tools operation payload (plans/builtin_activation): ML rows (active,
     non-builtin ML tools) plus a builtins section -- (base name, active)
     for EVERY registered Builtin-form mirror in MCP_Tools.thy, inactive
     included, so an empty section is distinguishable from "every mirror
     del'd" (the AVAILABILITY FLOOR guardrail: empty -> scala serves the
     full builtin table). */
  case class Tools_Reply(rows: List[Tool_Row], builtin_activation: List[(String, Boolean)])

  def decode_tools(body: XML.Body): List[Tool_Row] = {
    import XML.Decode._
    list(pair(string, pair(string, pair(string,
      pair(decode_schema _, decode_annotations _)))))(body)
      .map({ case (name, (description, (form, (schema, annotations)))) =>
        Tool_Row(name, description, form, schema, annotations)
      })
  }

  def decode_tools_reply(body: XML.Body): Tools_Reply = {
    import XML.Decode._
    val (rows, activation) = pair(decode_tools, list(pair(string, bool)))(body)
    Tools_Reply(rows, activation)
  }

}
