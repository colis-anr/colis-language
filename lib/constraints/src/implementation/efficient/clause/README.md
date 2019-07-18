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

|  `x = y` | `ρ(x) = ρ(y)`                               |
|  `x[f]y` | `ρ(x) = dir(m)` with `m(f) = ρ(y)`          |
|  `x[f]↑` | equivalent to `¬(∃y⋅ x[f]y)` by definition  |
| `x[f]y?` | equivalent to `x[f]↑ ∨ x[f]y` by definition |
|  `x[F]`  | `dom(ρ(x)) ⊆ F`                             |
| `x ~F y` | for all `f ∉ F`, `ρ(x)(f) = ρ(y)(f)`        |
| `dir(x)` | `ρ(x) = dir(_)`                             |
| `reg(x)` | `ρ(x) = reg`                                |
| …        | …                                           |

### Remarks

If `FT ⊧ ∀⋅∃y⋅ϕ` then

    x[f]↑ ∨ ∃y⋅(x[f]y ∧ ϕ)

and

    ∃y⋅(x[f]y? ∧ ϕ)

are equivalent.

### Macros

#### Resolve

Defined using an auxiliary macro:

    resolve_s(x,   π,    ε, z) = x = z
    resolve_s(x,   π,  ./q, z) = resolve_s(x, π, q, z)
    resolve_s(x,   ε, ../q, z) = resolve_s(x, ε, q, z)
    resolve_s(x, y⋅π, ../q, z) = resolve_s(y, π, q, z)
    resolve_s(x,   π,  f/q, z) = ∃y⋅(x[f]y ∧ resolve_s(y, x⋅π, q, z))

And then `resolve` is only a small wrapper:

    resolve(x, cwd, abs(q), z) = resolve_s(x, [],     q, z)
    resolve(x, cwd, rel(q), z) = resolve_s(x, [], cwd/q, z)

#### Noresolve

Defined using an auxiliary macro:

    noresolve_s(x,   π,    ε) = ⊥
    noresolve_s(x,   π,  f  ) = x[f]↑
    noresolve_s(x,   π,  ./q) = noresolve_s(x, π, q)
    noresolve_s(x,   ε, ../q) = noresolve_s(x, ε, q)
    noresolve_s(x, y⋅π, ../q) = ¬dir(x) ∨ noresolve_s(y, π, q)
    noresolve_s(x,   π,  f/q) = ∃y⋅(x[f]y? ∧ noresolve_s(x, y⋅π, q))

We can do something more clever with the `..`. For instance, we could have:

    noresolve_s(x, π, f/../q) =

Rewriting System
----------------

### Clash Rules

    {C-Feat-Fen}    x[f]y ∧ x[F]     => ⊥       (when f ∉ F)
    {C-Abs-Feat}    x[f]↑ ∧ x[f]y    => ⊥
    {C-Kind-NKind}  K(x) ∧ ¬K(x)     => ⊥
    {C-Kind-Kind}   K(x) ∧ L(x)      => ⊥                 (when K ≠ L)

    {C-NEq-Refl}    x ≠ x            => ⊥
    {C-NSim-Refl}   x ≁F x           => ⊥

### Directory Rules

    {D-Feat}        x[f]y            => x[f]y     ∧ dir(x)
    {D-Fen}         x[F]             => x[F]      ∧ dir(x)
    {D-Sim}         x ~F y           => x ~F y    ∧ dir(x)

### Simplification Rules

    {S-Eq-Refl}                x = x   => ⊤

    {S-Feats}          x[f]y ∧ x[f]z   => x[f]y ∧ y = z

    {S-Abs-NDir}       x[f]↑ ∧ ¬dir(x) => ¬dir(x)
    {S-Abs-Kind}       x[f]↑ ∧ K(x)    => K(x)          (when K ≠ dir)
    {S-Abs-Fen}        x[f]↑ ∧ x[F]    => x[F\{f}]

    {S-Maybe-Feat}    x[f]y? ∧ x[f]z   => x[f]z ∧ y = z
    {S-Maybe-Abs}     x[f]y? ∧ x[f]↑   => x[f]↑
    {S-Maybe-NDir}    x[f]y? ∧ ¬dir(x) => ¬dir(x)
    {S-Maybe-Kind}    x[f]y? ∧ K(x)    => K(x)          (when K ≠ dir)
    {S-Maybe-Fen}     x[f]y? ∧ x[F]    => x[F]          (when f ∉ F)

    {S-Sim-Refl}               x ~F x  => ⊤

    {S-NEq-Kind}        K(x) ∧ x ≠ y   => K(x) ∧ ¬K(y)  (when K ≠ dir)
    {S-NEq-Dir}       dir(x) ∧ x ≠ y   => dir(x) ∧ (¬dir(y) ∨ x ≁∅ y)

    {S-NKind-Kind}     ¬K(x) ∧ L(x)    => L(x)          (when K ≠ L)

    {S-NFen-NDir}      ¬x[F] ∧ ¬dir(x) => ¬dir(x)
    {S-NFen-Kind}      ¬x[F] ∧ K(x)    => K(x)          (when K ≠ dir)

    {S-NSim-NDir}     x ≁F y ∧ ¬dir(x) => ¬dir(x)
    {S-NSim-Kind}     x ≁F y ∧ K(x)    => K(x)          (when K ≠ dir)

### Propagation Rules

    {P-Feat}           x[f]y ∧ x ~F x' =>    x[f]y ∧    x'[f]y   ∧ x ~F x'   (when f ∉ F)
    {P-Abs}            x[f]↑ ∧ x ~F x' =>    x[f]↑ ∧    x'[f]↑   ∧ x ~F x'   (when f ∉ F)
    {P-Maybe}       x[f]yz…? ∧ x ~F x' => x[f]yz…? ∧  x'[f]yz…?  ∧ x ~F x'   (when f ∉ F)
    {P-Fen}             x[G] ∧ x ~F x' =>     x[G] ∧   x'[F∪G]   ∧ x ~F x'
    {P-Sim}           x ~G y ∧ x ~F x' =>   x ~G y ∧ x' ~(F∪G) y ∧ x ~F x'

### Replacement Rules

    {R-NFeat}       ¬x[f]y => x[f]↑ ∨ ∃z⋅(x[f]z ∧ y ≠ z)
    {R-NFeat'}      ¬x[f]y => ∃z⋅(x[f]z? ∧ y ≠ z)
    {R-NAbs}        ¬x[f]↑ => ∃z⋅x[f]z

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
