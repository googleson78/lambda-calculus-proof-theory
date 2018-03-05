data Term = Tru
          | Falsch
          | Cond Term Term Term
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
            deriving (Show, Eq)
