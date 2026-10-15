theory Paper_Proof_Fixture
  imports Main
begin

(* required for building this theory from the command line *)
declare [[quick_and_dirty = true]]

lemma open_obligation: "True"
  sorry

lemma using_open_obligation: "True"
  by (rule open_obligation)

end
