theory Proof_Tasks
  imports "MCP-Tool-Library-HOL.MCP_Library"
begin

text \<open>
  This theory contains the implementation and definition of the \<open>proof_tasks\<close> MCP tool.

  Import this theory from the MCP server root tool theory to make it available to LLMs.

  Given a theory name, this tool returns a list of named theorems to prove.
  The theorems are selected by walking the dependency tree and taking only the one where
  a direct oracle, e.g. \<open>sorry\<close>, application is present.

  The list is dynamic. As the oracles are replaced with proofs the theorems
  will be removed.
\<close>

section \<open>Direct oracle uses in named theorem dependencies\<close>

text \<open>
  Oracles can be used in a proof directly or indirectly. Moreover theorems are tagged
  as coming from an oracle even if they contain no direct oracle use and only depend
  on a theorem with a direct oracle use.

  This means that we need to distinguish between direct and indirect oracles in the theorem
  dependency tree to not over-approximate the actual proof tasks.

\<close>

ML \<open>
signature MCP_ORACLE_TREE =
sig
  val oracle_origin: thm -> bool * bool
  val oracle_tree: theory -> string -> string
  val oracle_trees: theory -> string
end;

structure MCP_Oracle_Tree: MCP_ORACLE_TREE =
struct

fun all_fact_entries thy =
  let
    val facts = Global_Theory.facts_of thy;
    fun entries (name, thms0) =
      map_index (fn (i, thm) =>
        (if length thms0 = 1 then name else name ^ "(" ^ string_of_int (i + 1) ^ ")",
         Thm.transfer thy thm)) thms0;
  in Facts.dest_static false [] facts |> maps entries end;

fun lookup_fact entries name =
  get_first (fn (candidate, thm) =>
    if candidate = name then SOME (candidate, thm) else NONE)
    entries;

fun dependency_names thy thm =
  Thm_Deps.thm_deps thy [thm] |> map (#1 o #2);

(* the same theorem statement can be present in more than one named theorem.
   The only reliable way to determine if we need to skip inspecting a theorem
   dependency tree is if we saw the same proof before. *)

(* Compare proof derivation identities, not merely theorem propositions. *)
fun same_derivation (thm1, thm2) =
  (case (Thm.derivation_id thm1, Thm.derivation_id thm2) of
    (SOME id1, SOME id2) =>
      #serial id1 = #serial id2 andalso #theory_name id1 = #theory_name id2
  | _ => false);

(* Local oracle use is in the named theorem's proof subgraph, up to the next
   named theorem boundary. The outer body of a named theorem has no oracles. *)
fun oracle_origin thm =
  (case Thm.derivation_id thm of
    NONE => error "The theorem has no identified proof node"
  | SOME {serial = root_serial, ...} =>
      let
        fun walk inherited_path (Proofterm.PBody {oracles, thms, ...})
            (seen, direct, inherited) =
          let
            val has_oracle = not (null oracles);
            val direct' = direct orelse (has_oracle andalso not inherited_path);
            val inherited' = inherited orelse (has_oracle andalso inherited_path);
            fun visit (node_serial, node) state =
              let
                val inherited_path' = inherited_path orelse
                  (node_serial <> root_serial andalso
                   not (Thm_Name.is_empty (Proofterm.thm_node_name node)));
                val key = (node_serial, inherited_path');
              in
                if member (op =) (#1 state) key then state
                else
                  let val (seen', direct'', inherited'') = state in
                    walk inherited_path'
                      (Future.join (Proofterm.thm_node_body node))
                      (key :: seen', direct'', inherited'')
                  end
              end;
          in fold visit thms (seen, direct', inherited') end;
        val (_, direct, inherited) =
          walk false (Thm.proof_body_of thm) ([], false, false);
      in (direct, inherited) end);

fun direct_oracle thm = #1 (oracle_origin thm);

(* Traverse named dependencies, retaining every directly oracle-bearing fact,
   including internal nodes. Unresolvable dependencies remain outside this
   addressable-fact view. *)
fun oracle_nodes thy lookup root =
  let
    fun visit seen (name, thm) =
      if exists (fn seen_thm => same_derivation (thm, seen_thm)) seen then []
      else
        let
          val seen' = thm :: seen;
          val names = dependency_names thy thm;
          val children = map_filter lookup names;
        in
          (if direct_oracle thm then [(name, thm)] else []) @
          maps (visit seen') children
        end;
  in visit [] root end;

fun oracle_tree_facts thy lookup (name, thms0) =
  map_index (fn (i, thm) =>
    (if length thms0 = 1 then name else name ^ "(" ^ string_of_int (i + 1) ^ ")",
     Thm.transfer thy thm)) thms0
  |> maps (oracle_nodes thy lookup)
  |> map (#1)
  |> String.concatWith "\n";

fun oracle_tree thy name =
  let
    val entries = all_fact_entries thy;
    val lookup = lookup_fact entries;
  in
    (case lookup name of
    NONE => error ("Unknown theorem fact " ^ quote name)
  | SOME root => oracle_tree_facts thy lookup (name, [#2 root]))
  end;

fun oracle_trees thy =
  let
    val entries = all_fact_entries thy;
    val lookup = lookup_fact entries;
    val own_facts =
      Facts.dest_static false
        (map Global_Theory.facts_of (Theory.parents_of thy))
        (Global_Theory.facts_of thy);
  in
    own_facts |> map (oracle_tree_facts thy lookup)
    |> String.concatWith "\n"
  end;

end;
\<close>


(*
ML \<open>
fun paper_loaded_theory ctxt name =
  let
    val root = Proof_Context.theory_of ctxt;
  in
    if name = Context.theory_long_name root orelse
       name = Context.theory_base_name root then root
    else
      (case try (Context.get_theory {long = false} root) name of
        SOME thy => thy
      | NONE => Thy_Info.get_theory name)
  end;
\<close>
*)

(* Listing 1.6 *)
mcp_tool proof_tasks
  (description "Theorems to be proved to formalize the input theories.")
  for theories \<open>Theory names\<close> :: "string list"
  = run \<open>fn ctxt => fn {theories} =>
      String.concatWith "\n\n" (map (fn name =>
        name ^ ":\n" ^ MCP_Oracle_Tree.oracle_trees (Thy_Info.get_theory name)) theories)\<close>
  (annotations read_only)
end
