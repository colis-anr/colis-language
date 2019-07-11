Efficient Implementation of Feature Tree Constraints
====================================================

The module Core contains the type definition of the whole structure.

The IVar module contains the definition of internal variables, as well as
efficient maps from internal variables and maps from Var.t to internal
variables.

Feature Trees
-------------

### Model

Let `F` be an infinite set of features. Let `V` be an infinite set of variables.
Let `K` be a finite set of kinds containing at least `dir` and `reg`.

The feature trees are defined by the following inductive definition:

    FT ::= dir(F ~> FT) | reg | ...

where `F ~> FT` denotes a partial function from `F` to `FT`.

### Syntax & Semantics

We say that `FT, ρ` is a model of a formula `ϕ`, with `ρ : V -> FT` written
`FT, ρ ⊧ ϕ` if:

|   `x = y`  | `ρ(x) = ρ(y)`                                             |
|   `x[f]y`  | `ρ(x) = dir(m)` with `m(f) = ρ(y)`                        |
|   `x[f]↑`  | equivalent to `¬(∃y⋅ x[f]y)` by definition                |
| `x[f]yz…?` | equivalent to `x[f]↑ ∨ (x[f]y ∧ z=y ∧ ...)` by definition |
|   `x[F]`   | `dom(ρ(x)) ⊆ F`                                           |
|  `x ~F y`  | for all `f ∉ F`, `ρ(x)(f) = ρ(y)(f)`                      |
|  `dir(x)`  | `ρ(x) = dir(_)`                                           |
|  `reg(x)`  | `ρ(x) = reg`                                              |
| …          | …                                                         |

### Remarks

If `FT ⊧ ∀⋅∃y⋅ϕ` then

    x[f]↑ ∨ ∃y⋅(x[f]y ∧ ϕ)

and

    ∃y⋅(x[f]y? ∧ ϕ)

are equivalent.

Rewriting System
----------------

### Clash Rules

    {C-Abs-Feat}  x[f]↑ ∧ x[f]y   => ⊥

### Simplification Rules

    {S-Abs-NDir}  x[f]↑ ∧ ¬dir(x) => ¬dir(x)
    {S-Abs-Kind}  x[f]↑ ∧ K(x)    => K(x)     (when K /= dir)
    {S-Abs-Fen}   x[f]↑ ∧ x[F]    => x[F\{f}]

    {S-Maybe-Abs} x[f]y? . x[f]↑  => x[f]↑

### Propagation Rules

    {P-Abs}       x[f]↑ ∧ x ~F y  => x[f]↑ ∧ y[f]↑ ∧ x ~F y  (when f ∉ F)

Notes on the different literals
-------------------------------

### Similarity

When we add a similarity `x ~F y`, we have to see for each pair of variables
similar to `x` or `y` whether using this new similarity is an improvement.

For instance, if we have `x ~F y ∧ x ~G z ∧ y ~H z` and we add `x ~F' y`, we
have to check whether:

- `x ~F' y` improves `x ~F y`,
- `x ~(H∪F') z` improves `x ~G z`,
- `y ~(G∪F') z` improves `y ~H z`.

**Definition.** `x ~F' y` is said to *improve* `x ~F y` if `F∩F'` is strictly
included in `F`.

In the special case where `x` and `y` did not have a similarity before, we know
that:

- There cannot be an improvement of variables that were similar to `x`
  (including `x`) between themselves.
- Same thing for variables similar to `y` (including `y`).
- There is an improvement between a variable similar to `x` (including `x`) and
  a variable similar to `y` (including `y`).

**Invariant.** For each triangle of similarities `x ~F y ∧ x ~G z ∧ y ~H z`, we
have that `F ⊂ G∪H`, `G ⊂ F∪H` and `H ⊂ F∪G`.

**Corollary.** In particular, using the long path in a triangle cannot improve
the small path.

**Proof.** If the invariant holds, `x ~(G∪H) y` cannot improve `x ~F y` because
`F∩(G∪H) = F` (since `F ⊂ G∪H`) which is not *strictly* included in `F`.

In fact, in our use case, handling the general case is not necessary. Only the
special case where there was no similarity before is important.
