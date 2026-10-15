theory Change_Detection
  imports "MCP-Tool-Library-HOL.MCP_Library"
begin

text \<open>
  This theory contains the implementation and definition the \<open>theory_add\<close> and \<open>theory_diff\<close> MCP tools.

  Import this theory from the MCP server root tool theory to make it available to LLMs.

  \<open>theory_add\<close> and \<open>theory_diff\<close> can be used to track changes to the logical signature of a theory.
  The commands are inspired by the version control system \<open>git\<close> and work as follows:

  1. \<open>theory_add\<close> adds a theory to the logical staging area, this is the equivalent of \<open>git add\<close> on the first run.
  2. \<open>theory_diff\<close> compares the current version of a theory document with the one in the logical staging are. It is the equivalent of \<open>git diff\<close>.

  These two tools are implemented for the purpose of change detection in the following logical elements of a theory:

  - imports
  - types
  - constants
  - axioms
  - definitions
  - named propositions

  Non logical elements of a theory (syntax, abbreviations, classes, proof automation, theory data, proofs) are ignored.
\<close>

ML \<open>
signature MCP_DIFF =
sig
  type store
  val new_store: unit -> store
  val create: store -> theory list -> string
  val check: store -> theory list -> string
  val changes: theory -> theory -> string list
  val own_facts: theory -> (string * thm list) list
  val checkpoint: Proof.context -> string list -> string
  val diff: Proof.context -> string list -> string
end;

structure MCP_Diff:> MCP_DIFF =
struct
  type node =
    {imports: string list,
     types: (string * int) list,
     consts: (string * typ) list,
     axioms: (string * term) list,
     defs: (string * string) list,
     facts: (string * thm list) list};
  type graph = node Symtab.table;
  datatype store = Store of (theory * graph) Symtab.table Synchronized.var;

  fun new_store () = Store (Synchronized.var "MCP_Diff.checkpoints" Symtab.empty);
  val live_store = new_store ();
  fun name thy = Context.theory_long_name thy;

  fun owned space entries thy =
    filter (fn (n, _) =>
      not (exists (fn parent => Name_Space.declared (space parent) n)
        (Theory.parents_of thy))) entries;

  fun own_types thy =
    owned Sign.type_space
      (Name_Space.dest_table (#types (Type.rep_tsig (Sign.tsig_of thy)))) thy
    |> map_filter (fn (n, Type.Logical_Type arity) => SOME (n, arity) | _ => NONE);

  fun own_consts thy =
    owned Sign.const_space (#constants (Consts.dest (Sign.consts_of thy))) thy
    |> map_filter (fn (n, (T, NONE)) => SOME (n, T) | _ => NONE);

  fun own_axioms thy =
    owned Theory.axiom_space (Theory.all_axioms_of thy) thy;

  fun own_facts thy =
    Facts.dest_static true (map Global_Theory.facts_of (Theory.parents_of thy))
      (Global_Theory.facts_of thy);

  fun node thy: node =
    {imports = map name (Theory.parents_of thy),
     types = own_types thy,
     consts = own_consts thy,
     axioms = own_axioms thy,
     defs = Defs.dest_constdefs (map Theory.defs_of (Theory.parents_of thy))
       (Theory.defs_of thy),
     facts = own_facts thy};

  fun graph thy =
    fold (fn t => Symtab.update_new (name t, node t))
      (Theory.nodes_of thy) Symtab.empty;

  fun same_thms (xs, ys) =
    eq_list (fn (x, y) => Thm.prop_of x aconv Thm.prop_of y) (xs, ys);

  fun compare_entries owner category eq old new =
    let
      val old_tab = Symtab.make old;
      val new_tab = Symtab.make new;
      fun line action n = owner ^ ": " ^ category ^ " " ^ action ^ " " ^ quote n;
      val removed = map (line "removed" o #1)
        (filter_out (Symtab.defined new_tab o #1) old);
      val added = map (line "added" o #1)
        (filter_out (Symtab.defined old_tab o #1) new);
      val changed = map_filter (fn (n, v) =>
        (case Symtab.lookup old_tab n of
          SOME prior => if eq (prior, v) then NONE else SOME (line "changed" n)
        | NONE => NONE)) new;
    in removed @ added @ changed end;

  fun compare_node owner (old: node) (new: node) =
    (if #imports old = #imports new then []
     else [owner ^ ": direct imports changed"]) @
    compare_entries owner "logical type" (op =) (#types old) (#types new) @
    compare_entries owner "primitive constant" (op =) (#consts old) (#consts new) @
    compare_entries owner "axiom" (fn (x, y) => x aconv y)
      (#axioms old) (#axioms new) @
    compare_entries owner "definition record" (op =)
      (map (fn (c, d) => (c ^ " / " ^ d, ())) (#defs old))
      (map (fn (c, d) => (c ^ " / " ^ d, ())) (#defs new)) @
    compare_entries owner "named fact" same_thms (#facts old) (#facts new);

  fun compare_graph old new =
    Symtab.fold (fn (n, old_node) => fn lines =>
      (case Symtab.lookup new n of
        NONE => ("theory node removed " ^ quote n) :: lines
      | SOME new_node => compare_node n old_node new_node @ lines)) old [] @
    Symtab.fold (fn (n, _) => fn lines =>
      if Symtab.defined old n then lines
      else ("theory node added " ^ quote n) :: lines) new [];

  fun changes old current = compare_graph (graph old) (graph current);

  fun distinct_roots thys =
    let val names = map name thys in
      if null names then error "At least one root theory is required"
      else if length (distinct (op =) names) <> length names then
        error "Root theories must be distinct"
      else names
    end;

  fun create (Store table) thys =
    let
      val names = distinct_roots thys;
      val roots = map (fn thy => (name thy, (thy, graph thy))) thys;
      val _ = Synchronized.change_result table (fn entries =>
        let
          val occupied = filter (Symtab.defined entries) names;
          val _ = if null occupied then ()
            else error ("Checkpoint already exists for " ^ commas_quote occupied);
        in ((), fold Symtab.update_new roots entries) end);
    in "checkpoint saved for " ^ commas_quote names end;

  fun check (Store table) thys =
    let
      val names = distinct_roots thys;
      val entries = Synchronized.value table;
      val missing = filter_out (Symtab.defined entries) names;
      val _ = if null missing then ()
        else error ("No checkpoint for " ^ commas_quote missing);
      val current = map (fn thy => (name thy, graph thy)) thys;
      fun report (n, now) =
        let
          val (_, baseline) = the (Symtab.lookup entries n);
          val lines = compare_graph baseline now;
        in "Theory " ^ quote n ^ ":\n" ^
          (if null lines then "protected logical signature unchanged"
           else cat_lines lines)
        end;
    in cat_lines (map report current) end;

  (* Resolve imports of the PIDE catalogue root as well as globally loaded theories. *)
  fun loaded_theory ctxt name =
    let val root = Proof_Context.theory_of ctxt in
      if name = Context.theory_long_name root orelse
         name = Context.theory_base_name root then root
      else
        (case try (Context.get_theory {long = false} root) name of
          SOME thy => thy
        | NONE => Thy_Info.get_theory name)
    end;

  fun checkpoint ctxt names = create live_store (map (loaded_theory ctxt) names);
  fun diff ctxt names = check live_store (map (loaded_theory ctxt) names);
end;
\<close>

mcp_tool theory_add
  (description "Stage theory into the theory_diff index")
  for theories \<open>Theory names\<close> :: "string list"
  = run \<open>fn ctxt => fn {theories} => MCP_Diff.checkpoint ctxt theories\<close>
  (annotations mutating)

mcp_tool theory_diff
  (description "Show changes in current theory from staged version")
  for theories \<open>Theory names\<close> :: "string list"
  = run \<open>fn ctxt => fn {theories} => MCP_Diff.diff ctxt theories\<close>
  (annotations read_only)

end
