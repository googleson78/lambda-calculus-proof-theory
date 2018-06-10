module DeMorgan where

open import Types

-- deMorgan1 is̷ ¬ (A ∧ B) ⇔ ¬A ∨ ¬B
-- stab for -> direction

-- what a human (I atleast) does
deMorgan1->1 : ∀{A B : Set} -> (Π A B -> ⊥) -> Σ (A -> ⊥) (B -> ⊥)
deMorgan1->1 {A} {B} f = stab g
    where g : (Σ (A -> ⊥) (B -> ⊥) -> ⊥) -> ⊥ 
          g h = h s₁
            where s₁ : Σ (A -> ⊥) (B -> ⊥)
                  s₁ = inj₁ x
                    where x : A -> ⊥
                          x a = h s₂
                            where s₂ : Σ (A -> ⊥) (B -> ⊥)
                                  s₂ = inj₂ y
                                    where   y : B -> ⊥
                                            y b = f (a , b)
-- lambda syntax
deMorgan1->1' : ∀{A B : Set} -> (Π A B -> ⊥) -> Σ (A -> ⊥) (B -> ⊥)
deMorgan1->1' = λ f -> stab (λ h -> h (inj₁ (λ a -> h (inj₂ (λ b -> f (a , b))))))

-- autocompleted by agda from stab ?
deMorgan1->1auto : ∀{A B : Set} -> (Π A B -> ⊥) -> Σ (A -> ⊥) (B -> ⊥)
deMorgan1->1auto = stab (λ z → z (λ _ → inj₁ (λ x → z (λ z₁ → inj₂ (λ x₁ → z₁ (x , x₁))))))

-- adapted from the natural deduction proof, looks cleaner, but 2 more stabs
deMorgan1->2 : ∀{A B : Set} -> (Π A B -> ⊥) -> Σ (A -> ⊥) (B -> ⊥)
deMorgan1->2 {A} {B} f = stab g
    where g : (Σ (A -> ⊥) (B -> ⊥) -> ⊥) -> ⊥ 
          g h = f x
            where x : Π A B
                  x = (stab nna) , (stab nnb)
                    where nna : (A -> ⊥) -> ⊥
                          nna na = h (inj₁ na)
                          nnb : (B -> ⊥) -> ⊥
                          nnb nb = h (inj₂ nb)


-- works for random C, and not only ⊥
deMorgan1<- : ∀{A B ⊥ : Set} -> Σ (A -> ⊥) (B -> ⊥) -> (Π A B -> ⊥)
deMorgan1<- (inj₁ f) (x , _) = f x
deMorgan1<- (inj₂ g) (_ , y) = g y

-- deMorgan2 is ¬ (A ∨ B) ⇔ ¬A ∧ ¬B
-- doesn't require stab

deMorgan2-> : ∀{A B ⊥ : Set} -> (Σ A B -> ⊥) -> Π (A -> ⊥) (B -> ⊥)
deMorgan2-> f = (λ a → f (inj₁ a)) , (λ b → f (inj₂ b))

deMorgan2<- : ∀{A B ⊥ : Set} -> Π (A -> ⊥) (B -> ⊥) -> (Σ A B -> ⊥)
deMorgan2<- (x , _) (inj₁ f) = x f
deMorgan2<- (_ , y) (inj₂ g) = y g

-- deMorgan3 is ¬∃x∈U(P(x)) ⇔ ∀x∈U(¬P(x))
-- doesn't require stab

deMorgan3-> : ∀{U : Set} {P : U -> Set} -> (∃ P -> ⊥) -> ∀(x : U) -> P x -> ⊥
deMorgan3-> f x Px = f (intro-∃ x Px)

deMorgan3<- : ∀{U : Set} {P : U -> Set} -> (∀(x : U) -> P x -> ⊥) -> (∃ P -> ⊥)
deMorgan3<- f (intro-∃ x p) = f x p

-- deMorgan4 is ∃x∈U(¬P(x)) ⇔ ¬∀x∈U(P(x))

deMorgan4-> : ∀{U : Set} {P : U -> Set} -> ∃ (λ x -> P x -> ⊥) -> (∀(x : U) -> P x) -> ⊥
deMorgan4-> (intro-∃ x p) f = p (f x)

-- stabby stab
deMorgan4<- : ∀{U : Set} {P : U -> Set} -> ((∀(x : U) -> P x) -> ⊥) -> ∃ (λ x -> P x -> ⊥)
deMorgan4<- {U} {P} f = stab x
    where x : (∃ (λ x → P x → ⊥) → ⊥) → ⊥
          x h = f y
            where y : ∀(x : U) -> P x
                  y z = stab u
                    where u : (P z -> ⊥) -> ⊥
                          u g = h (intro-∃ z g)
