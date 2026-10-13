theory MCP_Tool_Library
  imports MCP_Codecs MCP_Cancellation
  keywords "mcp_tool" :: thy_decl
    and "print_mcp_tools" :: diag
    and "description" "run" "capture"
begin

section \<open>Context locators\<close>

ML_file "mcp_context_locator.ML"

setup \<open>MCP_Context_Locator.register \<^binding>\<open>theory\<close>
  MCP_Context_Locator.theory_resolver\<close>

section \<open>MCP tool registry\<close>

ML \<open>
signature MCP_TOOL =
sig
  datatype form = Run | Builtin | Capture
  val form_tag: form -> string
  type annotations =
    {read_only: bool option, idempotent: bool option, destructive: bool option,
     open_world: bool option}
  val default_annotations: annotations
  val read_only: annotations
  val read_only_non_idempotent: annotations
  val mutating: annotations
  val idempotent_mutating: annotations
  val destructive: annotations
  type tool =
    {description: string, schema: MCP_Codec.schema, form: form, annotations: annotations,
     run: Proof.context -> MCP_Codec.value -> string}
  val space_of: Context.generic -> Name_Space.T
  val declare: binding -> tool -> local_theory -> string * local_theory
  val check: Proof.context -> xstring * Position.T -> string
  val get: Context.generic -> string -> tool
  val defined: Context.generic -> string -> bool
  val list: Context.generic -> (string * tool) list
  val active: Context.generic -> (string * tool) list
  val is_active: Context.generic -> string -> bool
  val activate: string -> Context.generic -> Context.generic
  val deactivate: string -> Context.generic -> Context.generic
  val add_del_attribute: attribute context_parser
  val print: bool -> Proof.context -> (string * tool -> Pretty.T list) -> unit
end;

structure MCP_Tool: MCP_TOOL =
struct
datatype form = Run | Builtin | Capture;
fun form_tag Run = "run"
  | form_tag Builtin = "builtin"
  | form_tag Capture = "capture";

type annotations =
  {read_only: bool option, idempotent: bool option, destructive: bool option,
   open_world: bool option};
val default_annotations : annotations =
  {read_only = NONE, idempotent = NONE, destructive = NONE, open_world = SOME false};
val read_only : annotations =
  {read_only = SOME true, idempotent = SOME true, destructive = NONE, open_world = SOME false};
val read_only_non_idempotent : annotations =
  {read_only = SOME true, idempotent = SOME false, destructive = NONE, open_world = SOME false};
val mutating : annotations =
  {read_only = SOME false, idempotent = SOME false, destructive = NONE, open_world = SOME false};
val idempotent_mutating : annotations =
  {read_only = SOME false, idempotent = SOME true, destructive = NONE, open_world = SOME false};
val destructive : annotations =
  {read_only = SOME false, idempotent = SOME false, destructive = SOME true,
   open_world = SOME false};

type tool =
  {description: string, schema: MCP_Codec.schema, form: form, annotations: annotations,
   run: Proof.context -> MCP_Codec.value -> string};
structure Registry =
  MCP_Registry(val kind = "mcp_tool" val changed = "MCP.tools_changed" type value = tool);
open Registry;

fun declare binding (tool: tool) lthy =
  let
    val fields =
      (case #schema tool of MCP_Codec.Object fields => fields
      | _ => error "MCP tool input schema must be an object");
    val names = map #name fields;
    val duplicate_fields = duplicates (op =) names;
    val _ =
      if member (op =) names "context"
      then error "MCP tool parameter context is reserved for framework target selection"
      else ();
    val _ =
      if null duplicate_fields then ()
      else error ("Duplicate MCP tool schema field " ^ quote (hd duplicate_fields));
  in Registry.declare binding tool lthy end;
end;
\<close>

ML \<open>
val _ = Theory.setup
  (Attrib.setup \<^binding>\<open>mcp_tools\<close> MCP_Tool.add_del_attribute
    "activation of MCP tools in the context");

val _ =
  Outer_Syntax.command \<^command_keyword>\<open>print_mcp_tools\<close>
    "print MCP tools available in the context (\"!\" includes inactive)"
    (Parse.opt_bang >> (fn verbose =>
      Toplevel.keep (fn st =>
        MCP_Tool.print verbose (Toplevel.context_of st) (fn (_, tool) =>
          [Pretty.str (" [" ^ MCP_Tool.form_tag (#form tool) ^ "]: " ^ #description tool)]))));
\<close>

ML_file "mcp_tool_runtime.ML"

section \<open>The mcp_tool command\<close>

ML \<open>
local

val annotation_buckets =
  [("read_only", MCP_Tool.read_only),
   ("read_only_non_idempotent", MCP_Tool.read_only_non_idempotent),
   ("mutating", MCP_Tool.mutating),
   ("idempotent_mutating", MCP_Tool.idempotent_mutating),
   ("destructive", MCP_Tool.destructive)];

fun read_annotations name =
  (case AList.lookup (op =) annotation_buckets name of
    SOME a => a
  | NONE => error ("Unknown MCP tool annotations bucket " ^ quote name ^
      " (expected " ^ commas_quote (map #1 annotation_buckets) ^ ")"));

fun clause_block kw p =
  Parse.$$$ "(" |-- Parse.$$$ kw |-- Parse.!!! (p --| Parse.$$$ ")");
fun clause_block_ct kw p =
  Parse.$$$ "(" |-- Args.$$$ kw |-- Parse.!!! (p --| Parse.$$$ ")");

fun print_annotations (a: MCP_Tool.annotations) =
  "{read_only = " ^ ML_Syntax.print_option Bool.toString (#read_only a) ^
  ", idempotent = " ^ ML_Syntax.print_option Bool.toString (#idempotent a) ^
  ", destructive = " ^ ML_Syntax.print_option Bool.toString (#destructive a) ^
  ", open_world = " ^ ML_Syntax.print_option Bool.toString (#open_world a) ^ "}";

fun ml_declaration head source tail =
  ML_Context.expression (Input.pos_of source)
    (ML_Lex.read ("Theory.local_setup (fn lthy => #2 (" ^ head ^ " (") @
      ML_Lex.read_source source @ ML_Lex.read (")" ^ tail ^ " lthy))"))
  |> Context.proof_map;
fun binding_ml (name, pos) = ML_Syntax.atomic (ML_Syntax.make_binding (name, pos));

val _ =
  Outer_Syntax.local_theory \<^command_keyword>\<open>mcp_tool\<close>
    "register a typed MCP tool"
    (let
      val description = clause_block "description" Parse.embedded;
      val fields = Scan.optional (Parse.$$$ "for" |--
        Parse.and_list1 (Parse.name -- Parse.embedded)) [];
      val type_signature = Parse.$$$ "::" |-- Parse.embedded_input;
      datatype form = Run of Input.source | Capture of Input.source;
      val implementation = Parse.$$$ "=" |--
        (Parse.$$$ "run" |-- Parse.ML_source >> Run ||
         Parse.$$$ "capture" |-- Parse.ML_source >> Capture);

      fun record_codec ctxt fields types =
        if null fields then
          if (case types of [MCP_Codec.Name ("unit", _)] => true | _ => false)
          then "MCP_Codec.unit"
          else error "A parameterless mcp_tool signature must be unit"
        else if length fields <> length types then
          error ("mcp_tool field/type arity mismatch: " ^ string_of_int (length fields) ^
            " fields but " ^ string_of_int (length types) ^ " types")
        else
          let
            val rows = map2 (fn (name, description) => fn typ =>
              (name, description, MCP_Codec.source ctxt typ)) fields types;
            val schemas = rows |> map (fn (name, description, codec) =>
              "MCP_Codec.field " ^ ML_Syntax.print_string name ^ " " ^ codec ^ " " ^
                ML_Syntax.print_string description ^ " true");
            val members = rows |> map (fn (name, _, codec) =>
              name ^ " = MCP_Codec.required " ^ codec ^
                " ctxt path entries " ^ ML_Syntax.print_string name);
          in
            "(MCP_Codec.object [" ^ commas schemas ^ "] " ^
              "(fn ctxt => fn path => fn entries => {" ^ commas members ^ "}))"
          end;

      fun command (((((name, pos), descr), fields), typ_text), (form, annot)) lthy =
        let
          val codec = record_codec lthy fields
            (MCP_Codec.argument_types (MCP_Codec.parse_signature lthy typ_text));
          val annotations =
            (case (form, annot) of
              (Capture _, NONE) =>
                error ("Missing (annotations <bucket>) clause for capture tool " ^ quote name)
            | (_, SOME a) => a
            | _ => MCP_Tool.default_annotations);
          val constructor =
            (case form of Run _ => "MCP_Tool_Execution.typed_run"
            | Capture _ => "MCP_Tool_Execution.typed_capture");
          val source = (case form of Run s => s | Capture s => s);
        in
          ml_declaration
            ("MCP_Tool.declare " ^ binding_ml (name, pos) ^ " (" ^ constructor ^ " " ^
              ML_Syntax.print_string descr ^ " " ^ codec ^ " " ^ print_annotations annotations)
            source ")" lthy
        end;
    in
      Parse.position Parse.name -- description -- fields -- type_signature --
        (implementation -- Scan.option
          (clause_block_ct "annotations" Parse.name >> read_annotations))
      >> command
    end);

in end;
\<close>

section \<open>Protocol payloads\<close>

text \<open>Pure domain codecs and operations, kept apart from the common
bridge so they can be unit-tested without a PIDE transport.  The bridge
resolves an opaque context locator first and supplies the resulting
<^ML_type>\<open>Proof.context\<close>; tool selection uses the catalogue context and execution uses the target context.\<close>

ML \<open>
signature MCP_PROTOCOL =
sig
  val tools_body: Proof.context -> XML.body
  val run_tool: Proof.context -> string -> MCP_Codec.value -> string * string
end;

structure MCP_Protocol: MCP_PROTOCOL =
struct

(*Rows carry full name, description, form tag, recursive input schema,
  and annotations. Isabelle/Scala computes exposed names.
  Builtin-form rows (the scala builtin table's ML mirrors below, spec
  phase 3 "builtin tools in the activation layer") carry no real
  schemas and are EXCLUDED here -- they enter the builtins section
  instead, so they never reach exposure-name computation (guardrail
  A3, plans/builtin_activation).*)
fun encode_schema schema =
  let open XML.Encode in
    variant
      [fn MCP_Codec.String => ([], []),
       fn MCP_Codec.Bool => ([], []),
       fn MCP_Codec.Integer => ([], []),
       fn MCP_Codec.Array item => ([], encode_schema item),
       fn MCP_Codec.Object fields =>
         ([], list (fn {name, schema, description, required} =>
           pair string (pair encode_schema (pair string bool))
             (name, (schema, (description, required)))) fields)] schema
  end;

fun with_context_schema (MCP_Codec.Object fields) =
      MCP_Codec.Object (fields @
        [{name = "context", schema = MCP_Codec.String, required = false,
          description = "Optional target context URL; defaults to the startup root"}])
  | with_context_schema schema =
      error "MCP tool input schema must be an object";

(*the four hints as (option bool) each, in the SAME field order as the
  type/scala's Tool_Annotations: every field is independently present or absent, so
  there is no positional ambiguity between two encoded values.*)
fun encode_annotations (a: MCP_Tool.annotations) =
  let open XML.Encode in
    pair (option bool) (pair (option bool) (pair (option bool) (option bool)))
      (#read_only a, (#idempotent a, (#destructive a, #open_world a)))
  end;

fun encode_row (name, tool: MCP_Tool.tool) =
  let open XML.Encode in
    pair string (pair string (pair string
      (pair encode_schema encode_annotations)))
      (name, (#description tool, (MCP_Tool.form_tag (#form tool),
        (with_context_schema (#schema tool), #annotations tool))))
  end;

(*(base name, active): the BASE name, not the theory-qualified full
  name, so it matches the scala builtin table's bare names directly
  (the drift gate, plans/builtin_activation). ALL registered mirrors,
  inactive included, so scala can tell "hidden" (registered, del'd)
  from "absent" (no mirror at all) -- the AVAILABILITY FLOOR
  guardrail: an empty section means scala serves its full table.*)
fun encode_builtin (name, active) =
  let open XML.Encode in pair string bool (Long_Name.base_name name, active) end;

fun tools_body ctxt =
  let
    val context = Context.Proof ctxt;
    val ml_rows =
      MCP_Tool.active context
      |> filter (fn (_, tool) => #form tool <> MCP_Tool.Builtin);
    val builtin_rows =
      MCP_Tool.list context
      |> filter (fn (_, tool) => #form tool = MCP_Tool.Builtin)
      |> map (fn (name, _) => (name, MCP_Tool.is_active context name));
  in
    let open XML.Encode in pair (list encode_row) (list encode_builtin) (ml_rows, builtin_rows) end
  end;

(*(status, output) with status = "ok" | "error"; interrupts are reraised.
  Serving follows listing: a registered-but-inactive tool is not
  callable, matching its absence from tools_body.*)
fun run_tool catalogue name value =
  (case Exn.capture_body (fn () =>
      let
        val context = Context.Proof catalogue;
        val tool = MCP_Tool.get context name;
        val _ =
          MCP_Tool.is_active context name orelse
            error ("Inactive MCP tool " ^ quote name);
        val entries =
          (case value of MCP_Codec.V_Object xs => xs
          | _ => error "MCP tool arguments must be an object");
        val targets = map_filter (fn (key, x) =>
          if key = "context" then
            (case x of MCP_Codec.V_String s => SOME s
            | _ => error "Framework context argument must be a string")
          else NONE) entries;
        val ctxt =
          (case targets of
            [] => catalogue
          | [locator] => #2 (MCP_Context_Locator.resolve_string
              (Proof_Context.theory_of catalogue) locator)
          | _ => error "Duplicate framework context argument");
        val tool_value = MCP_Codec.V_Object (filter (fn (key, _) => key <> "context") entries);
      in #run tool ctxt tool_value end) of
    Exn.Res res => ("ok", res)
  | Exn.Exn exn =>
      if Exn.is_interrupt exn then Exn.reraise exn
      else ("error", Runtime.exn_message exn));

end;
\<close>

section \<open>Protocol commands for Isabelle/Scala\<close>
text \<open>The common bridge owns the versioned envelope, request id,
cancellation route, operation lookup, and exactly-one result publication.  A
registered operation owns only its typed payload and domain result.  The
registry is inherited theory data, so an importing theory may add operations
without changing this dispatcher.\<close>

ML_file "mcp_bridge.ML"

ML \<open>
structure MCP_Bridge_Base =
struct

fun decode_value body =
  XML.Decode.variant
    [fn _ => MCP_Codec.V_Null,
     fn (_, ts) => MCP_Codec.V_String (XML.Decode.string ts),
     fn (_, ts) => MCP_Codec.V_Bool (XML.Decode.bool ts),
     fn (_, ts) => MCP_Codec.V_Integer (XML.Decode.int ts),
     fn (_, ts) => MCP_Codec.V_Array (XML.Decode.list decode_value ts),
     fn (_, ts) => MCP_Codec.V_Object
       (XML.Decode.list (XML.Decode.pair XML.Decode.string decode_value) ts)] body;
val decode_args = XML.Decode.list (XML.Decode.pair XML.Decode.string decode_value);
fun encode_status (status, output) =
  XML.Encode.pair XML.Encode.string XML.Encode.self
    (status, YXML.parse_body output);

fun tools _ root payload =
  (XML.Decode.string payload; MCP_Protocol.tools_body (Proof_Context.init_global root));

fun run_tool _ root payload =
  let
    val (_, (name, args)) =
      XML.Decode.pair XML.Decode.string
        (XML.Decode.pair XML.Decode.string decode_args) payload;
  in encode_status (MCP_Protocol.run_tool (Proof_Context.init_global root) name
       (MCP_Codec.V_Object args)) end;

fun check_context _ root payload =
  let
    val candidate = XML.Decode.option XML.Decode.string payload;
    val result =
      (case Exn.capture_body (fn () =>
          (case candidate of
            SOME locator => #1 (MCP_Context_Locator.resolve_string root locator)
          | NONE => MCP_Context_Locator.print (MCP_Context_Locator.theory root))) of
        Exn.Res canonical => ("ok", canonical)
      | Exn.Exn exn =>
          if Exn.is_interrupt exn then Exn.reraise exn
          else ("error", Runtime.exn_message exn));
  in encode_status result end;

end;
\<close>

setup \<open>MCP_Bridge.register \<^binding>\<open>tools\<close> MCP_Bridge_Base.tools\<close>
setup \<open>MCP_Bridge.register \<^binding>\<open>run_tool\<close> MCP_Bridge_Base.run_tool\<close>
setup \<open>MCP_Bridge.register \<^binding>\<open>check_context\<close> MCP_Bridge_Base.check_context\<close>

end
