Theorem my_first_proof : (forall A : Prop, A -> A).
Proof.
  intros A.
  intros proof_of_A.
  exact proof_of_A.
Qed.

∀ A B : Prop, A /\ B -> B \/ B
∀ x y : Z, x * y = 0 -> x = 0 \/ y = 0

Inductive even : N -> Prop :=
| even_0 : even 0
| even_S n : odd n -> even (n + 1)
with odd : N -> Prop :=
| odd_S n : even n -> odd (n + 1).

Inductive nat : Set :=
| 0 : nat
| S : nat -> nat.

Inductive list (A:Type) : Type :=
| nil : list A
| cons : A -> list A -> list A.