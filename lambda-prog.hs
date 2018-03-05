-- these are simply projections
tru = \x -> \y -> x

fls = \x -> \y -> y

-- and by applying this to them we "choose"
-- which argument to project, i.e.
-- if-then-else
cond = \c -> \t -> \e -> c t e

-- we want logical and to be a function which
-- only evals to true when both arguments are true
-- so we hack it in - if the first argument is fls
-- we will project the second of (s fls), i.e. fls
-- and if our first argument is tru, but our second is fls
-- we will have projected fls still, but this time because
-- s = fls
(a) = \f -> \s -> f s fls

-- we want logical or to be fls only when both are fls
-- so we hack it in similiarly
(o) = \f -> \s -> f tru s 

-- we negate by flipping what would be projected
(n) = \f -> f fls tru

-- more basic version
-- realized it while writing the description for the
-- previous one and looking at cond
(n') = \c -> \x -> \y -> c y x


