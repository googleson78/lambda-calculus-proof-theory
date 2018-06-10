module Types where

-- empty type (hopefully)
data ⊥ : Set where

efq : ∀{A : Set} -> ⊥ -> A
efq ()

postulate
    stab : ∀{A : Set} -> ((A -> ⊥) -> ⊥) -> A

-- constructors for (∧)
data _×_ (A : Set) (B : Set) : Set where
    _,_ : A -> B -> A × B

-- destructors for (∧)
fst : ∀{A B : Set} -> A × B -> A
fst (x , _) = x

snd : ∀{A B : Set} -> A × B -> B
snd (_ , y) = y

-- constructor for (∨)
data _+_ (A : Set) (B : Set) : Set where
    inj₁ : A -> A + B
    inj₂ : B -> A + B

-- destructor for (∨)
case_of_!_ : ∀{A B C : Set} -> A + B -> (A -> C) -> (B -> C) -> C
case (inj₁ x) of h ! _ = h x
case (inj₂ y) of _ ! g = g y

data ∃ {X : Set} (P : X -> Set) : Set where
    intro-∃ : ∀(x : X) -> P x -> ∃ P

