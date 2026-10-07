signature MCP_CODEC =
sig
  datatype schema =
    String | Bool | Integer | Array of schema |
    Object of {name: string, schema: schema, description: string, required: bool} list
  datatype value =
    V_Null | V_String of string | V_Bool of bool | V_Integer of int |
    V_Array of value list | V_Object of (string * value) list
  type 'a T = {schema: schema, decode: Proof.context -> string list -> value -> 'a}
  val string: string T
  val bool: bool T
  val integer: int T
  val unit: unit T
  val list: 'a T -> 'a list T
  val field: string -> 'a T -> string -> bool ->
    {name: string, schema: schema, description: string, required: bool}
  val object: {name: string, schema: schema, description: string, required: bool} list ->
    (Proof.context -> string list -> (string * value) list -> 'a) -> 'a T
  val required: 'a T -> Proof.context -> string list -> (string * value) list -> string -> 'a
  val decode: 'a T -> Proof.context -> value -> 'a
  type entry = {schema: schema, ml_name: string}
  val declare: binding -> string -> 'a T -> local_theory -> string * local_theory
  val declare_global: binding -> string -> 'a T -> theory -> string * theory
  val check: Proof.context -> xstring * Position.T -> string
  val get: Context.generic -> string -> entry
  datatype codec_type = Name of xstring * Position.T | List of codec_type | Arrow of codec_type * codec_type
  val parse_signature: Proof.context -> Input.source -> codec_type
  val argument_types: codec_type -> codec_type list
  val source: Proof.context -> codec_type -> string
end;

structure MCP_Codec: MCP_CODEC =
struct
datatype schema =
  String | Bool | Integer | Array of schema |
  Object of {name: string, schema: schema, description: string, required: bool} list;
datatype value =
  V_Null | V_String of string | V_Bool of bool | V_Integer of int |
  V_Array of value list | V_Object of (string * value) list;
type 'a T = {schema: schema, decode: Proof.context -> string list -> value -> 'a};
val string = {schema = String, decode = fn _ => fn _ => fn V_String x => x};
val bool = {schema = Bool, decode = fn _ => fn _ => fn V_Bool x => x};
val integer = {schema = Integer, decode = fn _ => fn _ => fn V_Integer x => x};
val unit = {schema = Object [], decode = fn _ => fn _ => fn V_Object _ => ()};
fun list (codec: 'a T) : 'a list T =
  {schema = Array (#schema codec),
   decode = fn ctxt => fn p => fn V_Array xs =>
     map_index (fn (i, x) => #decode codec ctxt (string_of_int i :: p) x) xs};
fun field name (codec: 'a T) description required =
  {name = name, schema = #schema codec, description = description, required = required};
fun object fields decode : 'a T =
  let
    val duplicate_fields = duplicates (op =) (map #name fields);
    val _ = if null duplicate_fields then ()
      else error ("Duplicate MCP codec field " ^ quote (hd duplicate_fields));
  in {schema = Object fields, decode = fn ctxt => fn p => fn V_Object entries => decode ctxt p entries} end;
fun required (codec: 'a T) ctxt p entries name =
  (case AList.lookup (op =) entries name of
    SOME value => #decode codec ctxt (name :: p) value
  | NONE => raise Match);
fun decode (codec: 'a T) ctxt value = #decode codec ctxt [] value;

type entry = {schema: schema, ml_name: string};
structure Registry =
  MCP_Registry(val kind = "mcp_codec" val changed = "MCP.tools_changed" type value = entry);
fun declare binding ml_name (codec: 'a T) lthy =
  Registry.declare binding {schema = #schema codec, ml_name = ml_name} lthy;
fun declare_global binding ml_name (codec: 'a T) thy =
  Registry.declare_global binding {schema = #schema codec, ml_name = ml_name} thy;
val check = Registry.check;
val get = Registry.get;

datatype codec_type = Name of xstring * Position.T | List of codec_type | Arrow of codec_type * codec_type;

fun token_text tree =
  let
    fun add (Parser.Tip tok) = cons (Lexicon.str_of_token tok)
      | add (Parser.Node (_, trees)) = fold add trees
      | add (Parser.Markup (_, trees)) = fold add trees;
  in String.concat (rev (add tree [])) end;

fun tree_pos (Parser.Tip tok) = Lexicon.pos_of_token tok
  | tree_pos (Parser.Node ({range = (pos, _), ...}, _)) = pos
  | tree_pos (Parser.Markup ({range = (pos, _), ...}, _)) = pos;

fun decode_tree tree =
  (case tree of
    Parser.Markup (_, [body]) => decode_tree body
  | Parser.Markup (_, bodies) =>
      (case filter (fn Parser.Tip _ => false | _ => true) bodies of
        [body] => decode_tree body
      | _ => error ("Malformed MCP codec markup" ^ Position.here (tree_pos tree)))
  | Parser.Node ({const = "", ...}, [body]) => decode_tree body
  | Parser.Node ({const, ...}, children) =>
      let
        fun unmark name = if Lexicon.is_const name then Lexicon.unmark_const name else name;
        val const' = unmark const;
        fun hidden (Parser.Node ({const, ...}, _)) = String.isSuffix "_position" (unmark const)
          | hidden _ = false;
        val semantic_children = filter (fn tree => not (hidden tree) andalso not (null (Parser.pretty_tree tree))) children;
      in
      (if String.isSuffix "_codec_name" const' orelse String.isSuffix "_codec_long_name" const'
       then (case children of body :: _ => Name (token_text body, tree_pos body)
              | _ => error "Malformed MCP codec name")
       else if const' = "" andalso not (null semantic_children)
       then decode_tree (hd semantic_children)
       else if String.isSuffix "_codec_arrow" const' andalso length semantic_children >= 2
       then Arrow (decode_tree (nth semantic_children 0), decode_tree (nth semantic_children 1))
       else if String.isSuffix "_codec_list" const' andalso not (null semantic_children)
       then List (decode_tree (hd semantic_children))
       else if String.isSuffix "_codec_parentheses" const' andalso not (null semantic_children)
       then decode_tree (hd semantic_children)
       else case semantic_children of
        [body] =>
          if String.isSuffix "_codec_parentheses" const' then decode_tree body
          else if String.isSuffix "_codec_list" const' then List (decode_tree body)
          else decode_tree body
      | [left, right] =>
          if String.isSuffix "_codec_arrow" const' then Arrow (decode_tree left, decode_tree right)
          else error ("Malformed MCP codec syntax constant " ^ quote const' ^ " " ^
            Pretty.string_of (Pretty.chunks (Parser.pretty_tree tree)) ^ Position.here (tree_pos tree))
      | _ => error ("Malformed MCP codec syntax constant " ^ quote const' ^ " with " ^
          string_of_int (length semantic_children) ^ " children " ^
          Pretty.string_of (Pretty.chunks (Parser.pretty_tree tree)) ^ Position.here (tree_pos tree))) end
  | _ => error ("Malformed MCP codec syntax " ^
      Pretty.string_of (Pretty.chunks (Parser.pretty_tree tree)) ^ Position.here (tree_pos tree)));

fun parse_signature ctxt source =
  let
    val syn = Proof_Context.syntax_of ctxt;
    val pos = Input.pos_of source;
    val toks = Syntax.tokenize syn {raw = false} (Input.source_explode source);
    val _ = Context_Position.reports ctxt (maps Lexicon.reports_of_token toks);
    val trees = Syntax.parse syn "MCP_Codecs.codecs" (filter Lexicon.is_proper toks)
      handle ERROR msg => error (msg ^ Position.here pos);
  in
    (case trees of [tree] => decode_tree tree
    | [] => error ("Malformed MCP codec syntax" ^ Position.here pos)
    | _ => error ("Ambiguous MCP codec syntax" ^ Position.here pos))
  end;

fun argument_types (Arrow (left, right)) = left :: argument_types right
  | argument_types typ = [typ];

fun source ctxt typ =
  let
    fun atom (Name (name, pos)) =
          (case name of
            "string" => "MCP_Codec.string"
          | "bool" => "MCP_Codec.bool"
          | "int" => "MCP_Codec.integer"
          | "unit" => "MCP_Codec.unit"
          | _ =>
              let
                val full = check ctxt (name, pos);
                val {ml_name, ...} = get (Context.Proof ctxt) full;
              in ml_name end)
      | atom (List body) = "(MCP_Codec.list " ^ atom body ^ ")"
      | atom (Arrow _) = error "Function-valued MCP tool fields are unsupported";
  in atom typ end;
end;

local
fun codec_cmd ((name, range), source) thy =
  let
    val ml_name = name;
    val tail =
      ") in Theory.setup_result (fn thy => let val (_, thy') = MCP_Codec.declare_global " ^
      ML_Syntax.atomic (ML_Syntax.make_binding (name, #1 range)) ^ " " ^
      ML_Syntax.print_string ml_name ^ " codec thy in (codec, thy') end) end";
  in
    ML_Context.expression (Input.pos_of source)
      (ML_Lex.read "val " @ ML_Lex.read_range range name @
       ML_Lex.read " = let val codec = (" @ ML_Lex.read_source source @ ML_Lex.read tail)
    |> Context.theory_map |> (fn f => f thy)
  end;
val _ = Outer_Syntax.command \<^command_keyword>\<open>mcp_codec\<close>
  "declare a named monomorphic MCP JSON codec"
  (Parse.name_range -- (Parse.$$$ "=" |-- Parse.ML_source) >> (Toplevel.theory o codec_cmd));
in end;
