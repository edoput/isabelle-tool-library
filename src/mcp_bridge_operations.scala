/*  Title:      mcp/src/mcp_bridge_operations.scala

Typed application payload codecs for the internal PIDE bridge.
*/

package isabelle.mcp

import isabelle.{JSON, XML}
import isabelle.mcp.pide.BridgeOperation

import scala.util.control.NonFatal


/** Reviewed bridge surface selected by the composition root.
  *
  * The startup hello may advertise additional extension operations, but it
  * must include every operation required by the selected application surface.
  */
final case class McpBridgeProfile private (
  name: String,
  requiredOperationNames: Set[String]
)

object McpBridgeProfile {
  val base: McpBridgeProfile =
    McpBridgeProfile("base", McpBridgeOperations.baseOperationNames)
}


object McpBridgeOperations {
  final class InvalidJsonNumber(message: String) extends IllegalArgumentException(message)

  private def invalidNumber(value: Any): Nothing =
    throw new InvalidJsonNumber(
      "MCP tool arguments support integer JSON numbers only; received " + value)

  private abstract class Operation[A](
    val name: String,
    val requestPayload: XML.Body
  ) extends BridgeOperation[A] {
    protected def decode(payload: XML.Body): A

    final def decodeReply(payload: XML.Body): Either[String, A] =
      try Right(decode(payload))
      catch {
        case NonFatal(exn) =>
          val detail = Option(exn.getMessage).filter(_.nonEmpty)
            .getOrElse(exn.getClass.getName)
          Left(detail + "; payload=" + payload)
      }
  }

  private def encodeJson(value: JSON.T): XML.Body = {
    import XML.Encode._
    variant[JSON.T](List(
      { case null => (Nil, Nil) },
      { case s: String => (Nil, string(s)) },
      { case b: Boolean => (Nil, bool(b)) },
      { case n: Int => (Nil, string(n.toString))
        case n: Long => (Nil, string(n.toString))
        case n: Double if n.isWhole && Math.abs(n) <= 9007199254740992.0 =>
          (Nil, string(BigDecimal(n).toBigInt.toString))
        case n: Double => invalidNumber(n)
        case n: Float => invalidNumber(n)
        case n: Number => invalidNumber(n) },
      { case xs: List[_] => (Nil, list(encodeJson)(xs)) },
      { case JSON.Object(obj) => (Nil, list(pair(string, encodeJson))(obj.toList)) }
    ))(value)
  }
  private val encodeArgs =
    XML.Encode.list(XML.Encode.pair(XML.Encode.string, encodeJson))
  private val decodeStatusBody =
    XML.Decode.pair(XML.Decode.string, XML.Decode.self)

  def tools(context: String): BridgeOperation[McpToolData.Tools_Reply] =
    new Operation[McpToolData.Tools_Reply]("tools", XML.Encode.string(context)) {
      protected def decode(payload: XML.Body): McpToolData.Tools_Reply =
        McpToolData.decode_tools_reply(payload)
    }

  def runTool(context: String, name: String,
    args: JSON.Object.T): BridgeOperation[McpToolData.Result] =
    statusText(
      "run_tool",
      XML.Encode.pair(XML.Encode.string,
        XML.Encode.pair(XML.Encode.string, encodeArgs))((context, (name, args.toList))))

  /** None asks ML for the canonical registry-root context. */
  def checkContext(context: Option[String]): BridgeOperation[McpToolData.Result] =
    statusText("check_context", XML.Encode.option(XML.Encode.string)(context))

  private def statusText(operation: String,
    request: XML.Body): BridgeOperation[McpToolData.Result] =
    new Operation[McpToolData.Result](operation, request) {
      protected def decode(payload: XML.Body): McpToolData.Result = {
        val (status, body) = decodeStatusBody(payload)
        statusResult(status, XML.content(body))
      }
    }

  private def statusResult(status: String, text: String): McpToolData.Result =
    if (status == "ok") McpToolData.Ok(text) else McpToolData.Error(text)

  val operationNames: Set[String] = Set("tools", "run_tool", "check_context")
  val baseOperationNames: Set[String] = operationNames
}
