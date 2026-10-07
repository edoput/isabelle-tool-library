signature MCP_CANCELLATION =
sig
  val register: string -> Future.group -> unit
  val member: string -> bool
  val cancel: string -> bool
  val finish: string -> bool option
  val cleanup: string -> string list
  val drain: string -> string list
  val fork_group:
    string -> string -> (Future.group -> 'a) -> ('a Exn.result -> unit) ->
      (string list -> unit) -> unit
  val fork:
    string -> string -> (unit -> 'a) -> ('a Exn.result -> unit) ->
      (string list -> unit) -> unit
end;

structure MCP_Cancellation: MCP_CANCELLATION =
struct

datatype entry = Running of Future.group | Cancelled of Future.group;
type state = {accepting: bool, requests: (string * entry) list, waiters: string list};

val requests: state Synchronized.var =
  Synchronized.var "MCP_Cancellation.requests" {accepting = true, requests = [], waiters = []};

fun register id group =
  Synchronized.change requests (fn {accepting, requests = entries, waiters} =>
    if not accepting then error "MCP bridge admission is closed"
    else if AList.defined (op =) entries id
    then error ("Duplicate MCP bridge id " ^ quote id)
    else {accepting = accepting, requests = (id, Running group) :: entries, waiters = waiters});

fun member id = AList.defined (op =) (#requests (Synchronized.value requests)) id;

fun cancel id =
  let
    val group =
      Synchronized.change_result requests (fn {accepting, requests = entries, waiters} =>
        (case AList.lookup (op =) entries id of
          SOME (Running group) =>
            (SOME group, {accepting = accepting,
              requests = AList.update (op =) (id, Cancelled group) entries, waiters = waiters})
        | SOME (Cancelled _) => (NONE, {accepting = accepting, requests = entries, waiters = waiters})
        | NONE => (NONE, {accepting = accepting, requests = entries, waiters = waiters})));
    val _ = Option.app Future.cancel_group group;
  in is_some group end;

(*SOME true means cancellation won before result publication; SOME false is
  normal completion; NONE exposes an internal route-ownership error.  finish
  observes but deliberately retains the route until cleanup follows the
  publication attempt.*)
fun finish id =
  Synchronized.change_result requests (fn {accepting, requests = entries, waiters} =>
    let
      val result =
        (case AList.lookup (op =) entries id of
          SOME (Cancelled _) => SOME true
        | SOME (Running _) => SOME false
        | NONE => NONE);
    in (result, {accepting = accepting, requests = entries, waiters = waiters}) end);

fun cleanup id =
  Synchronized.change_result requests (fn {accepting, requests = entries, waiters} =>
    let
      val remaining = AList.delete (op =) id entries;
      val acknowledgements = if null remaining then waiters else [];
      val remaining_waiters = if null remaining then [] else waiters;
    in (acknowledgements,
      {accepting = accepting, requests = remaining, waiters = remaining_waiters}) end);

fun drain id =
  Synchronized.change_result requests (fn {requests = entries, waiters, ...} =>
    if null entries then ([id], {accepting = false, requests = entries, waiters = waiters})
    else ([], {accepting = false, requests = entries, waiters = waiters @ [id]}));

(*Every Scala-to-ML protocol round trip uses the same route lifecycle: a
  request-owned future group, an id-indexed cancellation entry, and a
  non-interruptible publication callback.  The shared executor performs
  finish, optional publication, and cleanup in that order, then hands released
  drain identifiers to the bridge-specific acknowledgement callback.*)
fun fork_group id name body publish release_drains =
  let
    val group = Future.new_group NONE;
    val _ = register id group;
    val result =
      (singleton o Future.forks)
        {name = name, group = SOME group, deps = [], pri = ~1, interrupts = true}
        (fn () => body group);
    val _ =
      (singleton o Future.forks)
        {name = name ^ "_result", group = NONE,
         deps = [Future.task_of result], pri = ~1, interrupts = false}
        (fn () =>
          let
            val joined = Future.join_result result;
            val cancelled =
              (case finish id of
                SOME value => value
              | NONE => error ("Missing MCP cancellation route " ^ quote id));
            val published = Exn.capture_body
              (fn () => if cancelled then () else publish joined);
            val acknowledgements = cleanup id;
            val _ = release_drains acknowledgements;
          in Exn.release published end);
  in () end;

fun fork id name body publish release_drains =
  fork_group id name (fn _ => body ()) publish release_drains;

end;
