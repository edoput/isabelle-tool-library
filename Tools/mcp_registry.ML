signature MCP_REGISTRY =
sig
  type value
  val space_of: Context.generic -> Name_Space.T
  val declare: binding -> value -> local_theory -> string * local_theory
  val declare_global: binding -> value -> theory -> string * theory
  val check: Proof.context -> xstring * Position.T -> string
  val get: Context.generic -> string -> value
  val defined: Context.generic -> string -> bool
  val list: Context.generic -> (string * value) list
  val active: Context.generic -> (string * value) list
  val is_active: Context.generic -> string -> bool
  val activate: string -> Context.generic -> Context.generic
  val deactivate: string -> Context.generic -> Context.generic
  val add_del_attribute: attribute context_parser
  val print: bool -> Proof.context -> (string * value -> Pretty.T list) -> unit
end;

functor MCP_Registry(val kind: string val changed: string type value): MCP_REGISTRY =
struct
type value = value;
structure Data = Generic_Data
(
  type T = value Name_Space.table * Symtab.set
  val empty = (Name_Space.empty_table kind, Symtab.empty)
  fun merge ((table1, active1), (table2, active2)) =
    (Name_Space.merge_tables (table1, table2), Symtab.merge (K true) (active1, active2))
);
val table_of = #1 o Data.get;
val active_set = #2 o Data.get;
val space_of = Name_Space.space_of_table o table_of;
fun notify_changed () =
  Output.protocol_message [Markup.function changed] [] handle Output.Protocol_Message _ => ();
fun new_entry binding x context =
  let val (name, table') = Name_Space.define context true (binding, x) (table_of context)
  in (name, Data.map (fn (_, active) => (table', Symtab.insert_set name active)) context) end;
fun declare binding x lthy =
  let
    val (name, lthy') = lthy
      |> Local_Theory.background_theory_result (fn thy =>
          let val (name, context') = new_entry binding x (Context.Theory thy)
          in (name, Context.the_theory context') end)
      ||> Local_Theory.map_contexts (fn _ => fn ctxt =>
          Context.the_proof (#2 (new_entry binding x (Context.Proof ctxt))));
    val _ = notify_changed ();
  in (name, lthy') end;
fun declare_global binding x thy =
  let val (name, context') = new_entry binding x (Context.Theory thy)
  in (notify_changed (); (name, Context.the_theory context')) end;
fun check ctxt arg = #1 (Name_Space.check (Context.Proof ctxt) (table_of (Context.Proof ctxt)) arg);
fun get context name = Name_Space.get (table_of context) name;
fun defined context name = is_some (Name_Space.lookup (table_of context) name);
fun list context = Name_Space.dest_table (table_of context);
fun is_active context name = Symtab.defined (active_set context) name;
fun active context = filter (is_active context o #1) (list context);
fun activate name = Data.map (apsnd (Symtab.insert_set name));
fun deactivate name = Data.map (apsnd (Symtab.remove_set name));
val add_del_attribute =
  Scan.optional (Scan.lift ((Args.add >> K true || Args.del >> K false) --| Args.colon)) true
    -- Args.context -- Scan.lift Parse.name_position >>
    (fn ((add, ctxt), arg) =>
      let val name = check ctxt arg in
        Thm.declaration_attribute (K (if add then activate name else deactivate name))
      end);
fun print verbose ctxt prt_value =
  let
    val context = Context.Proof ctxt;
    val table = table_of context;
    val space = Name_Space.space_of_table table;
    fun prt (name, x) = Pretty.block
      (Pretty.mark_str (Name_Space.markup space name, Name_Space.extern ctxt space name) ::
        Pretty.str (if is_active context name then "" else " (inactive)") :: prt_value (name, x));
    val entries = list context |> filter (fn (name, _) => verbose orelse is_active context name);
  in Pretty.writeln (Pretty.big_list (kind ^ (if verbose then " (all):" else " (active):"))
    (map prt entries)) end;
end;
