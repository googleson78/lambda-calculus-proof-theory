module Types where

-- empty type (hopefully)
data ⊥ : Set where

efq : ∀{A : Set} -> ⊥ -> A
efq ()

postulate
    stab : ∀{A : Set} -> ((A -> ⊥) -> ⊥) -> A

-- constructors for (∧)
data Π (A : Set) (B : Set) : Set where
    _,_ : A -> B -> Π A B

-- destructors for (∧)
fst : ∀{A B : Set} -> Π A B -> A
fst (x , _) = x

snd : ∀{A B : Set} -> Π A B -> B
snd (_ , y) = y

-- constructor for (∨)
data Σ (A : Set) (B : Set) : Set where
    inj₁ : A -> Σ A B
    inj₂ : B -> Σ A B

-- destructor for (∨)
case_of_!_ : ∀{A B C : Set} -> Σ A B -> (A -> C) -> (B -> C) -> C
case (inj₁ x) of h ! _ = h x
case (inj₂ y) of _ ! g = g y

data ∃ {X : Set} (P : X -> Set) : Set where
    intro-∃ : ∀(x : X) -> P x -> ∃ P

