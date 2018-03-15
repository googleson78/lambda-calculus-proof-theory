data Term = Tru
          | Fls
          | Cond Term Term Term
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
            deriving (Show, Eq)

consts :: Term -> [Term]
consts Tru          = [Tru]
consts Fls          = [Fls]
consts (Cond x y z) = concatMap consts [x, y, z]
consts Zero         = [Zero]
consts (Succ x)     = consts x
consts (Pred x)     = consts x
consts (IsZero x)   = consts x

size :: Term -> Int
size Tru          = 1
size Fls          = 1
size (Cond x y z) = succ $ sum $ map size [x, y, z]
size Zero         = 1
size (Succ x)     = succ $ size x
size (Pred x)     = succ $ size x
size (IsZero x)   = succ $ size x

depth :: Term -> Int
depth Tru          = 1
depth Fls          = 1
depth (Cond x y z) = succ $ maximum $ map depth [x, y, z]
depth Zero         = 1
depth (Succ x)     = succ $ depth x
depth (Pred x)     = succ $ depth x
depth (IsZero x)   = succ $ depth x

reduce :: Term -> Either String Term
reduce (Cond Tru y z) = Right y
reduce (Cond Fls y z) = Right z
reduce (Cond x y z)   = let xval = reduce x in
                          (\t -> Cond t y z) <$> xval
reduce _              = Left "Does not reduce"

eval :: Term -> Term
eval = either error id . eval' . pure

eval' :: Either String Term -> Either String Term
eval' t = let next = reduce =<< t in
            either (const t) (eval' . pure) next
