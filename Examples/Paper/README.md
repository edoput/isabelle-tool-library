# Paper example tools

This session collects the verifier-resident tools used in the MCP paper. Its
catalogue root is `Paper_Tools.thy`, which imports:

- `MCP_Task_List.thy`: `proof_tasks` lists outstanding proof obligations for
  loaded theories.
- `MCP_Diff.thy`: `theory_checkpoint` stores a fixed in-process logical
  signature, and `theory_diff` compares later verified theory values with it.

Both theories import the HOL entry point of the extracted tool library. They
are examples, separate from the library's core sessions. The proof-task example
uses the direct-oracle dependency traversal from `isabelle-mcp`; the change
detector was adapted from its `theory-diff-implementation` experiment. The
server-specific `MCP` imports were replaced with `MCP_Library`.

Build with Isabelle2025-2 after registering this component:

```sh
isabelle build -d /absolute/path/to/isabelle-tool-library/Examples MCP-Tool-Library-Paper-Examples
```

For an MCP host, load the session `MCP-Tool-Library-Paper-Examples` and use
`Paper_Tools` as the catalogue root. The root imports both example fixtures.
`Paper_Proof_Fixture` enables `quick_and_dirty` locally to admit its example
lemma with `sorry`. The tools resolve imported theories from their execution
context, with the ordinary theory loader as a fallback. `theory_checkpoint`
and `theory_diff` operate within the same running prover process; a checkpoint
does not survive a restart.
