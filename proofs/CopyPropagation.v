(** * Copy Propagation: Verified Optimization Pass

    Proves correctness of copy propagation and defines the transformation
    so the Rust implementation is a mechanical transcription.

    Transformation:
      [Move D S] ; rest   ==>   [Nop] ; subst_src(D→S, rest)

    Preconditions (each maps to a runtime check in Rust):
      (P1) D <> S                 [check: d == s => skip]
      (P2) D <> 0                 [check: d == 0 => skip]
      (P3) D not written in rest  [check: defs[d] == 1]
      (P4) S not written in rest  [check: stop when dst == s]
      (P5) D not read by Opaque   [check: stop before Call/SaveRegs/RestoreRegs]

    The instruction model includes [Opaque], which represents instructions
    whose source operands cannot be individually substituted (e.g., Call,
    whose [args_start] denotes a contiguous register range). Precondition
    (P5) ensures D is not read by any such instruction; the Rust code
    enforces this by stopping propagation before them.

    Proof technique: bisimulation. After Move D S, registers D and S
    hold the same value. Every substitutable instruction that reads D
    sees the same value as reading S. Opaque instructions don't read D
    (by P5), so they also see the same inputs. After substitution, D is
    dead, so the Move can be NOPed.

    AXIOM-FREE: uses only functional extensionality (standard in Coq).
    Eliminates the [srcs_correct_agrees] axiom from MoveForwarding.v.
*)

Require Import Stdlib.Lists.List.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Arith.PeanoNat.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.Logic.FunctionalExtensionality.
Import ListNotations.

(** ** 1. VM Model *)

Definition Reg := nat.
Definition Val := nat.
Definition RegFile := Reg -> Val.

Definition reg_set (rf : RegFile) (r : Reg) (v : Val) : RegFile :=
  fun r' => if Nat.eqb r r' then v else rf r'.

Lemma reg_set_eq : forall rf r v, reg_set rf r v r = v.
Proof.
  intros. unfold reg_set. rewrite Nat.eqb_refl. reflexivity.
Qed.

Lemma reg_set_neq : forall rf r1 r2 v,
  r1 <> r2 -> reg_set rf r1 v r2 = rf r2.
Proof.
  intros. unfold reg_set.
  apply Nat.eqb_neq in H. rewrite H. reflexivity.
Qed.

(** Setting a register to its current value is the identity. *)
Lemma reg_set_same : forall rf r,
  reg_set rf r (rf r) = rf.
Proof.
  intros. apply functional_extensionality. intro r'.
  unfold reg_set. destruct (Nat.eqb r r') eqn:E.
  - apply Nat.eqb_eq in E. subst. reflexivity.
  - reflexivity.
Qed.

(** ** 2. Instructions

    Five instruction kinds:
    - [Compute]: reads declared sources, writes dst. Substitutable.
    - [Move]: copies src to dst. Substitutable.
    - [Nop]: no-op.
    - [Print]: observable output from src. Substitutable.
    - [Opaque]: reads from a register set that cannot be individually
      substituted (models Call, whose args_start denotes a contiguous
      range — replacing one register would shift the whole range).
      NOT substitutable: [subst_src_instr] returns it unchanged.

    The distinction between Compute and Opaque is purely about
    substitutability, not execution semantics. Both compute a value
    from the register file and write it to dst. *)

Inductive Instr : Type :=
  | Compute (dst : Reg) (srcs : list Reg) (f : RegFile -> Val)
  | Move (dst src : Reg)
  | Nop
  | Print (src : Reg)
  | Opaque (dst : Reg) (reads : list Reg) (f : RegFile -> Val).

Definition get_dst (ins : Instr) : option Reg :=
  match ins with
  | Compute dst _ _ => Some dst
  | Move dst _ => Some dst
  | Nop => None
  | Print _ => None
  | Opaque dst _ _ => Some dst
  end.

Definition step (ins : Instr) (rf : RegFile) : RegFile * option Val :=
  match ins with
  | Compute dst _ f => (reg_set rf dst (f rf), None)
  | Move dst src => (reg_set rf dst (rf src), None)
  | Nop => (rf, None)
  | Print src => (rf, Some (rf src))
  | Opaque dst _ f => (reg_set rf dst (f rf), None)
  end.

Definition Program := list Instr.

Fixpoint exec (prog : Program) (rf : RegFile) : RegFile * list Val :=
  match prog with
  | [] => (rf, [])
  | ins :: rest =>
      let (rf', obs) := step ins rf in
      let (rf_final, obs_rest) := exec rest rf' in
      match obs with
      | None => (rf_final, obs_rest)
      | Some o => (rf_final, o :: obs_rest)
      end
  end.

Fixpoint not_written_in (r : Reg) (prog : Program) : Prop :=
  match prog with
  | [] => True
  | ins :: rest => get_dst ins <> Some r /\ not_written_in r rest
  end.

(** ** 3. Independence and Well-formedness *)

(** [f] does not depend on register [r]. *)
Definition f_independent (f : RegFile -> Val) (r : Reg) : Prop :=
  forall rf v, f (reg_set rf r v) = f rf.

(** [f] only depends on registers in [reads]. *)
Definition reads_correct (reads : list Reg) (f : RegFile -> Val) : Prop :=
  forall r, ~ In r reads -> f_independent f r.

(** Well-formedness: Opaque instructions declare their dependencies.
    Compute does NOT need this — the bisimulation uses
    [regfile_reconstruct] for Compute, bypassing source-list reasoning.
    Only Opaque needs it because substitution doesn't rewrite Opaque. *)
Fixpoint wf_program (prog : Program) : Prop :=
  match prog with
  | [] => True
  | Opaque _ reads f :: rest => reads_correct reads f /\ wf_program rest
  | _ :: rest => wf_program rest
  end.

(** [D] is not in any Opaque instruction's reads list.
    Rust: [stop propagation before Call/SaveRegs/RestoreRegs]. *)
Fixpoint opaque_safe (D : Reg) (prog : Program) : Prop :=
  match prog with
  | [] => True
  | Opaque _ reads _ :: rest => ~ In D reads /\ opaque_safe D rest
  | _ :: rest => opaque_safe D rest
  end.

(** ** 4. Key Lemmas *)

(** Reconstruct rf1 from rf2 by setting D to rf2(S). *)
Lemma regfile_reconstruct : forall rf1 rf2 (D S : Reg),
  (forall r, r <> D -> rf1 r = rf2 r) ->
  rf1 D = rf2 S ->
  reg_set rf2 D (rf2 S) = rf1.
Proof.
  intros. apply functional_extensionality. intro r.
  unfold reg_set. destruct (Nat.eqb D r) eqn:E.
  - apply Nat.eqb_eq in E. subst. symmetry. exact H0.
  - apply Nat.eqb_neq in E. symmetry. apply H. exact E.
Qed.

(** If [f] is independent of [D], and two regfiles agree except on [D],
    then [f] produces the same result on both.

    This eliminates the [srcs_correct_agrees] axiom from MoveForwarding.v
    for the specific case of one differing register. *)
Lemma f_independent_agrees : forall f (D : Reg) rf1 rf2,
  f_independent f D ->
  (forall r, r <> D -> rf1 r = rf2 r) ->
  f rf1 = f rf2.
Proof.
  intros f D rf1 rf2 Hind Hagree.
  rewrite <- (Hind rf1 (rf2 D)).
  assert (Heq : reg_set rf1 D (rf2 D) = rf2). {
    apply functional_extensionality. intro r.
    unfold reg_set. destruct (Nat.eqb D r) eqn:E.
    - apply Nat.eqb_eq in E. subst. reflexivity.
    - apply Nat.eqb_neq in E. apply Hagree. exact E.
  }
  rewrite Heq. reflexivity.
Qed.

(** ** 5. Source Substitution — EXTRACTABLE

    Defines exactly what the Rust pass does to each instruction.
    Rust [replace_src_reg(d, s)] corresponds to [subst_src_instr D S].

    CRITICAL: [Opaque] is returned UNCHANGED. This models the Rust
    behavior of stopping propagation before Call/SaveRegs/RestoreRegs.
    The [opaque_safe D] precondition ensures D is never read by an
    un-substituted Opaque instruction. *)

Definition subst_src_instr (old new_ : Reg) (ins : Instr) : Instr :=
  match ins with
  | Compute dst srcs f =>
    Compute dst
      (map (fun r => if Nat.eqb r old then new_ else r) srcs)
      (fun rf => f (reg_set rf old (rf new_)))
  | Move dst src =>
    Move dst (if Nat.eqb src old then new_ else src)
  | Nop => Nop
  | Print src =>
    Print (if Nat.eqb src old then new_ else src)
  | Opaque dst reads f =>
    Opaque dst reads f  (* UNCHANGED — not safe to substitute *)
  end.

Fixpoint subst_src (old new_ : Reg) (prog : Program) : Program :=
  match prog with
  | [] => []
  | ins :: rest => subst_src_instr old new_ ins :: subst_src old new_ rest
  end.

(** ** 6. Bisimulation Theorem

    Executing [prog] with rf1 and [subst_src D S prog] with rf2 produces
    the same observations, when the regfiles agree except on D and
    rf1(D) = rf2(S).

    At each step, for each instruction kind:
    - Compute: [regfile_reconstruct] shows f sees identical input
    - Move/Print: direct register lookup shows same value
    - Nop: trivial (no register access)
    - Opaque: [f_independent_agrees] + [opaque_safe] shows f sees
      identical input despite D not being substituted

    This handles two DIFFERENT programs (before/after substitution). *)

Theorem copy_prop_simulation :
  forall prog D S rf1 rf2,
    D <> S ->
    wf_program prog ->
    opaque_safe D prog ->
    not_written_in D prog ->
    not_written_in S prog ->
    (forall r, r <> D -> rf1 r = rf2 r) ->
    rf1 D = rf2 S ->
    let (rf1', obs1) := exec prog rf1 in
    let (rf2', obs2) := exec (subst_src D S prog) rf2 in
    obs1 = obs2 /\ (forall r, r <> D -> rf1' r = rf2' r).
Proof.
  induction prog as [| ins rest IH];
  intros D S rf1 rf2 HDS Hwf Hos Hnw_D Hnw_S Hagree HDS_val.
  - simpl. auto.
  - simpl in Hnw_D. destruct Hnw_D as [Hd_D Hnw_D_rest].
    simpl in Hnw_S. destruct Hnw_S as [Hd_S Hnw_S_rest].
    assert (Hwf_rest : wf_program rest) by
      (destruct ins; simpl in Hwf; tauto).
    assert (Hos_rest : opaque_safe D rest) by
      (destruct ins; simpl in Hos; tauto).
    destruct ins; simpl.

    + (* Compute dst srcs f — substitutable *)
      (* reg_set rf2 D (rf2 S) = rf1, so f sees identical input *)
      assert (Hrecon : reg_set rf2 D (rf2 S) = rf1)
        by (apply regfile_reconstruct; auto).
      rewrite Hrecon.
      specialize (IH D S
        (reg_set rf1 dst (f rf1))
        (reg_set rf2 dst (f rf1))
        HDS Hwf_rest Hos_rest Hnw_D_rest Hnw_S_rest).
      destruct (exec rest (reg_set rf1 dst (f rf1))) as [rf1' obs1] eqn:E1.
      destruct (exec (subst_src D S rest) (reg_set rf2 dst (f rf1)))
        as [rf2' obs2] eqn:E2.
      apply IH.
      * intros r Hr. unfold reg_set.
        destruct (Nat.eqb dst r) eqn:Ed; auto.
      * unfold reg_set.
        assert (Nat.eqb dst D = false) as ->
          by (apply Nat.eqb_neq; intro; subst; apply Hd_D; reflexivity).
        assert (Nat.eqb dst S = false) as ->
          by (apply Nat.eqb_neq; intro; subst; apply Hd_S; reflexivity).
        exact HDS_val.

    + (* Move dst src — substitutable *)
      assert (Hsrc_eq : rf1 src = rf2 (if Nat.eqb src D then S else src)). {
        destruct (Nat.eqb src D) eqn:EsD.
        - apply Nat.eqb_eq in EsD. subst. exact HDS_val.
        - apply Nat.eqb_neq in EsD. apply Hagree. exact EsD.
      }
      rewrite Hsrc_eq.
      specialize (IH D S
        (reg_set rf1 dst (rf2 (if Nat.eqb src D then S else src)))
        (reg_set rf2 dst (rf2 (if Nat.eqb src D then S else src)))
        HDS Hwf_rest Hos_rest Hnw_D_rest Hnw_S_rest).
      destruct (exec rest
        (reg_set rf1 dst (rf2 (if Nat.eqb src D then S else src))))
        as [rf1' obs1] eqn:E1.
      destruct (exec (subst_src D S rest)
        (reg_set rf2 dst (rf2 (if Nat.eqb src D then S else src))))
        as [rf2' obs2] eqn:E2.
      apply IH.
      * intros r Hr. unfold reg_set.
        destruct (Nat.eqb dst r) eqn:Ed; auto.
      * unfold reg_set.
        assert (Nat.eqb dst D = false) as ->
          by (apply Nat.eqb_neq; intro; subst; apply Hd_D; reflexivity).
        assert (Nat.eqb dst S = false) as ->
          by (apply Nat.eqb_neq; intro; subst; apply Hd_S; reflexivity).
        exact HDS_val.

    + (* Nop *)
      specialize (IH D S rf1 rf2 HDS Hwf_rest Hos_rest
                     Hnw_D_rest Hnw_S_rest Hagree HDS_val).
      destruct (exec rest rf1) as [rf1' obs1] eqn:E1.
      destruct (exec (subst_src D S rest) rf2) as [rf2' obs2] eqn:E2.
      exact IH.

    + (* Print src — substitutable *)
      assert (Hsrc_eq : rf1 src = rf2 (if Nat.eqb src D then S else src)). {
        destruct (Nat.eqb src D) eqn:EsD.
        - apply Nat.eqb_eq in EsD. subst. exact HDS_val.
        - apply Nat.eqb_neq in EsD. apply Hagree. exact EsD.
      }
      rewrite Hsrc_eq.
      specialize (IH D S rf1 rf2 HDS Hwf_rest Hos_rest
                     Hnw_D_rest Hnw_S_rest Hagree HDS_val).
      destruct (exec rest rf1) as [rf1' obs1] eqn:E1.
      destruct (exec (subst_src D S rest) rf2) as [rf2' obs2] eqn:E2.
      destruct IH as [Hobs Hregs].
      split; [f_equal; exact Hobs | exact Hregs].

    + (* Opaque dst reads f — NOT substitutable *)
      destruct Hwf as [Hrc Hwf'].
      destruct Hos as [Hns Hos'].
      (* f is independent of D because D ∉ reads and reads_correct *)
      assert (Hf_eq : f rf1 = f rf2). {
        apply (f_independent_agrees f D rf1 rf2).
        - apply Hrc. exact Hns.
        - exact Hagree.
      }
      rewrite Hf_eq.
      specialize (IH D S
        (reg_set rf1 dst (f rf2))
        (reg_set rf2 dst (f rf2))
        HDS Hwf' Hos' Hnw_D_rest Hnw_S_rest).
      destruct (exec rest (reg_set rf1 dst (f rf2))) as [rf1' obs1] eqn:E1.
      destruct (exec (subst_src D S rest) (reg_set rf2 dst (f rf2)))
        as [rf2' obs2] eqn:E2.
      apply IH.
      * intros r Hr. unfold reg_set.
        destruct (Nat.eqb dst r) eqn:Ed; auto.
      * unfold reg_set.
        assert (Nat.eqb dst D = false) as ->
          by (apply Nat.eqb_neq; intro; subst; apply Hd_D; reflexivity).
        assert (Nat.eqb dst S = false) as ->
          by (apply Nat.eqb_neq; intro; subst; apply Hd_S; reflexivity).
        exact HDS_val.
Qed.

(** ** 7. Main Theorem — Copy Propagation Correctness

    Directly specifies the optimization pass. Each precondition maps
    to an explicit runtime check in the Rust implementation:

    Rust transcription:
      match code[i] {
        Move { dst: d, src: s } => {
          if d == s { continue; }           // (P1) D <> S
          if d == 0 { continue; }           // (P2) D <> 0
          if defs[d] != 1 { continue; }     // (P3) not_written_in D
          for j in (i+1).. {
            match code[j] {                 // (P5) opaque_safe D
              Call | SaveRegs | .. => break,
            }
            code[j].replace_src_reg(d, s);  // [subst_src_instr]
            if dst_of(code[j]) == s break;  // (P4) not_written_in S
          }
          if propagated == uses[d] {
            code[i] = Nop;                  // [Nop]
          }
        }
      } *)

Theorem copy_propagation_correct :
  forall (D S : Reg) (rest : Program) (rf : RegFile),
    D <> S ->                     (* P1: not a self-move *)
    D <> 0 ->                     (* P2: don't clobber return register *)
    wf_program rest ->            (* Opaque instructions declare reads *)
    opaque_safe D rest ->         (* P5: D not read by Opaque *)
    not_written_in D rest ->      (* P3: D has exactly one def (the Move) *)
    not_written_in S rest ->      (* P4: S is not redefined *)
    let (rf1, obs1) := exec (Move D S :: rest) rf in
    let (rf2, obs2) := exec (Nop :: subst_src D S rest) rf in
    obs1 = obs2 /\ rf1 0 = rf2 0.
Proof.
  intros D S rest rf HDS HD0 Hwf Hos Hnw_D Hnw_S.
  simpl.
  pose proof (copy_prop_simulation rest D S
    (reg_set rf D (rf S)) rf
    HDS Hwf Hos Hnw_D Hnw_S) as H.
  destruct (exec rest (reg_set rf D (rf S))) as [rf1 obs1] eqn:E1.
  destruct (exec (subst_src D S rest) rf) as [rf2 obs2] eqn:E2.
  destruct H as [Hobs Hregs].
  - intros r Hr. apply reg_set_neq. exact Hr.
  - apply reg_set_eq.
  - split.
    + exact Hobs.
    + apply Hregs. exact HD0.
Qed.

(** ** Summary

    [copy_propagation_correct] proves that replacing

      [Move D S] ; rest

    with

      [Nop] ; [subst_src D S rest]

    preserves all observable outputs and the return value (register 0).

    Preconditions (mapping to Rust runtime checks):
    - D <> S             =>  [if d == s { continue; }]
    - D <> 0             =>  [if d == 0 { continue; }]
    - not_written_in D   =>  [if defs[d] != 1 { continue; }]
    - not_written_in S   =>  [stop propagation when dst == s]
    - opaque_safe D      =>  [stop propagation before Call/SaveRegs/RestoreRegs]
    - wf_program         =>  (structural: Opaque instructions honestly
                              declare their register dependencies)

    Transformation (mapping to Rust rewrite):
    - [subst_src D S]    =>  [code[j].replace_src_reg(d, s)] for each j
    - [Opaque] unchanged =>  [break] before Call/SaveRegs/RestoreRegs
    - [Nop]              =>  [code[i] = Opcode::Nop]

    The [Opaque] instruction models Call, CallIndirect, CallClosure,
    SaveRegs, and RestoreRegs — instructions whose source operands form
    a contiguous register range. Substituting a single register in such
    a range would shift the entire range, corrupting the call arguments.
    The proof requires [opaque_safe D rest] (D is not read by any Opaque
    instruction), which the Rust code enforces by breaking out of the
    propagation loop before reaching any such instruction.

    This is the bug that the first version of this proof missed:
    the original model had no [Opaque] instruction, so the theorem
    claimed substitution was always safe. The Rust code broke on Call
    instructions because [replace_src_reg] on Call shifts args_start,
    corrupting the argument range. Adding [Opaque] to the model forces
    the proof (and the generated Rust code) to stop before Calls.

    Improvements over MoveForwarding.v:
    - AXIOM-FREE: no [srcs_correct_agrees] axiom.
    - BISIMULATION: proves equivalence of two different programs.
    - NO [srcs_correct] FOR COMPUTE: [regfile_reconstruct] makes
      the [f] argument identical by extensionality.
    - OPAQUE INSTRUCTIONS: models non-substitutable instructions,
      preventing the Call-corruption bug at the proof level.
*)
