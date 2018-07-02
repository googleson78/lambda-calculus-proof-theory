; why is this language not curried, kill me please

(define succ (lambda (x) (+ x 1)))

(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define ccons (curry2 cons))

(define (cfoldr f v xs)
  (if (null? xs)
        v
        ((f (car xs)) (foldr f v (cdr xs)))))

; easy list creation/reading
(define (toScmList xs) ((xs ccons) '()))
(define (fromScmList xs) (cfoldr : e xs))

; testing
(define l123 (fromScmList '(1 2 3)))
(define l1 (fromScmList '(1)))

; bools
(define tru (lambda (fst) (lambda (snd) fst)))
(define fls (lambda (fst) (lambda (snd) snd)))

; composition
(define dot (lambda (f) (lambda (g) (lambda (x) (f (g x))))))

; empty list
(define e (lambda (f) (lambda (v) v)))

; cons
(define : (lambda (x) 
            (lambda (xs)
              (lambda (f) 
                (lambda (v) 
                  ((f x) ((xs f) v)))))))

; list head, with error message, would be some constant in lambda calc
(define head (lambda (xs)
               ((xs tru) "Can't get head from empty list.")))


; foldr
(define lfoldr
  (lambda (f)
    (lambda (v)
      (lambda (xs)
        ((xs f) v)))))

; testing
(define (lsum xs) (((lfoldr (curry2 +)) 0) xs))

(define lmap
  (lambda (g)
    (lambda (xs)
      (lambda (f)
        (lambda (v)
          ((xs ((dot f) g)) v))))))

; alternative, aka "cheating"
(define lmap1
  (lambda (g)
    (lambda (xs)
      (((lfoldr (lambda (x) (lambda (xs) ((: (g x)) xs)))) e) xs))))

(define l234 ((lmap1 succ) l123))

(define lfilter
  (lambda (p)
    (lambda (xs)
      (lambda (f)
        (lambda (v)
          ((xs (lambda (y)
                 (lambda (ys)
                   (((p y) ((f y) ys))
                           ys)))) v))))))

; alternative; aka "cheating"
(define lfilter1
  (lambda (p)
    (lambda (xs)
      (((lfoldr (lambda (y)
                  (lambda (ys)
                    (((p y) ((: y) ys)) ys)))) e) xs))))

(define gt1
  (lambda (x)
    (if (< 1 x)
        tru
        fls)))

(define l23 ((lfilter gt1) l123))
