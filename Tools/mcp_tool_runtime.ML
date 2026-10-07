signature MCP_OUTPUT =
sig
  val install_wrappers: unit -> unit
  val register: Future.group -> (unit -> string)
  val captured: (unit -> 'a) -> 'a Exn.result * string
  val reset: unit -> unit
end;

structure MCP_Output: MCP_OUTPUT =
struct

(* routing table: Future group id -> output buffer *)

val buffers: (int * string list Synchronized.var) list Synchronized.var =
  Synchronized.var "MCP_Output.buffers" [];

(*walk the worker's group ancestry*)
fun find_buffer () =
  (case Future.worker_group () of
    NONE => NONE
  | SOME group =>
      let
        val gs = Task_Queue.str_of_groups group;
        fun lookup [] = NONE
          | lookup ((gid, buf) :: rest) =
              if String.isSubstring (string_of_int gid) gs
              then SOME buf else lookup rest;
      in lookup (Synchronized.value buffers) end);


(* lazy Private_Output wrappers *)

val wrapped = Synchronized.var "MCP_Output.wrapped" false;

fun install_wrappers () =
  Synchronized.change wrapped (fn already =>
    if already then true
    else
      let
        fun wrap (r: (string list -> unit) Unsynchronized.ref) =
          let val orig = ! r in
            r := (fn ss =>
              (case find_buffer () of
                SOME buf => Synchronized.change buf (cons (implode ss))
              | NONE => orig ss))
          end;
        fun wrap_err (r: (serial * string list -> unit) Unsynchronized.ref) =
          let val orig = ! r in
            r := (fn (i, ss) =>
              (case find_buffer () of
                SOME buf => Synchronized.change buf (cons (implode ss))
              | NONE => orig (i, ss)))
          end;
      in
        wrap Private_Output.writeln_fn;
        wrap Private_Output.writeln_urgent_fn;
        wrap Private_Output.state_fn;
        wrap Private_Output.information_fn;
        wrap Private_Output.tracing_fn;
        wrap Private_Output.warning_fn;
        wrap Private_Output.legacy_fn;
        wrap_err Private_Output.error_message_fn;
        true
      end);

(*process startup (isabelle_process, build job) re-assigns the
  Private_Output functions, silently discarding wrappers inherited from the
  heap — so a persisted "wrapped = true" would disable routing for good.
  Reset after build-time use; re-installation in a live process is harmless
  because wrappers fall through when no buffer is registered.*)
fun reset () =
  (Synchronized.change wrapped (K false);
   Synchronized.change buffers (K []));

(*register a buffer for a group; the returned function unregisters it and
  yields the captured output*)
fun register group =
  let
    val gid = Task_Queue.group_id group;
    val buffer = Synchronized.var "MCP_Output.buffer" ([]: string list);
    val _ = Synchronized.change buffers (cons (gid, buffer));
  in
    fn () =>
      (Synchronized.change buffers (filter_out (fn (g, _) => g = gid));
       cat_lines (rev (Synchronized.value buffer)))
  end;

(*Synchronous capture: fork f into a fresh registered subgroup and join.
  Outside a future this still has no parent; inside MCP.run_tool it inherits
  the bridge group, so client cancellation reaches the actual user tool rather
  than merely interrupting the outer worker that is waiting for it. Interrupts
  are NOT swallowed here — callers inspect the Exn.result.*)
fun captured f =
  let
    val _ = install_wrappers ();
    val group = Future.worker_subgroup ();
    val finish = register group;
    val res =
      Future.join_result
        ((singleton o Future.forks)
          {name = "MCP_Output.captured", group = SOME group, deps = [],
           pri = 0, interrupts = true} f);
  in (res, finish ()) end;

end;

structure MCP_Tool_Execution =
struct
fun typed_capture description (codec: 'a MCP_Codec.T) annotations f : MCP_Tool.tool =
  let val plain = XML.content_of o YXML.parse_body in
    {description = description, schema = #schema codec,
     form = MCP_Tool.Capture, annotations = annotations,
     run = fn ctxt => fn value =>
      (case MCP_Output.captured (fn () =>
          Print_Mode.with_modes [] (fn () => f ctxt (MCP_Codec.decode codec ctxt value)) ()) of
        (Exn.Res (), output) => plain output
      | (Exn.Exn exn, output) =>
          if Exn.is_interrupt exn then Exn.reraise exn
          else error (if output = "" then Runtime.exn_message exn
            else plain output ^ "\n" ^ Runtime.exn_message exn))}
  end;

fun typed_run description (codec: 'a MCP_Codec.T) annotations f : MCP_Tool.tool =
  {description = description, schema = #schema codec,
   form = MCP_Tool.Run, annotations = annotations,
   run = fn ctxt => fn value => f ctxt (MCP_Codec.decode codec ctxt value)};
end;
