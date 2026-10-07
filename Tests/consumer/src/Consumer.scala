/* This package intentionally has no isabelle.mcp private-package privileges. */
package library_consumer

import isabelle.{Headless, JSON}
import isabelle.mcp.{McpBridgeOperations, McpBridgeProfile, McpToolData}
import isabelle.mcp.control.DeadlineScheduler
import isabelle.mcp.pide._

object Consumer {
  def transport(session: Headless.Session): PideTransport =
    new SessionPideTransport(session, PideBridgeV1.resultFunctions)

  def bridge(transport: PideTransport, policy: PideBridgePolicy,
    scheduler: DeadlineScheduler, root: () => BridgeResult[PideRootSelector]): PideBridge =
    new PideBridge(transport, policy, scheduler, root,
      McpBridgeOperations.operationNames, McpBridgeProfile.base, PideBridgeV1)

  def invoke(bridge: PideBridge, context: String, name: String,
    args: JSON.Object.T, cancellation: BridgeCancellation): BridgeResult[McpToolData.Result] =
    bridge.call(McpBridgeOperations.runTool(context, name, args), cancellation)

  def catalogue(bridge: PideBridge, context: String,
    cancellation: BridgeCancellation): BridgeResult[McpToolData.Tools_Reply] =
    bridge.call(McpBridgeOperations.tools(context), cancellation)
}
