#lang typed/racket

(require typed/racket/random
         "core.rkt")

(provide
 (all-from-out "core.rkt")

 gen:natural
 gen:integer-in
 gen:real
 gen:one-of
 gen:boolean
 gen:char
 gen:char-in
 gen:char-letter
 gen:char-digit
 gen:char-alphanumeric
 gen:tuple
 gen:list
 gen:exact-list
 gen:vector
 gen:bytes
 gen:string
 gen:symbol
 ;gen:hash
 ;gen:hasheq
 ;gen:hasheqv
 gen:frequency)

(: halves (case->
           (-> Natural (Listof Natural))
           (-> Integer (Listof Integer))))
(define (halves n)
  (let loop ([n (quotient n 2)])
    (if (>= 0 n)
        '()
        (cons n (loop (quotient n 2))))))

(: shrink-natural (-> Natural (Listof Natural)))
(define (shrink-natural n)
  (if (zero? n)
      '()
      (cons 0 (map (λ ([m : Natural]) (cast (- n m) Natural)) (halves n)))))

(: shrink-integer (-> Integer (Listof Integer)))
(define (shrink-integer n)
  (if (zero? n)
      '()
      (append (if (negative? n) (list (abs n)) '())
              (cons 0 (map (λ ([m : Integer]) (- n m)) (halves n))))))

(: gen:natural (Gen Natural))
(define gen:natural
  (lambda (rng size)
    (let ([n (random 0 (add1 size) rng)])
      (build-shrink-tree n shrink-natural))))

(: gen:integer (Gen Integer))
(define gen:integer
  (lambda (rng size)
    (let ([n (random (- size) (add1 size) rng)])
      (build-shrink-tree n shrink-integer))))

#;(module+ test
  (require rackunit)

  (define-syntax-rule (tc name body ...)
    (test-case name
      (random-seed 1337)
      body ...))

  (define-syntax-rule (check-values (v ...) body ...)
    (call-with-values
     (lambda ()
       body ...)
     (lambda vs
       (check-equal? vs (list v ...)))))

  (tc "naturals"
    (check-equal? (sample gen:natural 5) '(0 0 4 5 8))
    (check-equal? (sample gen:natural 5) '(0 1 3 6 4)))

  (tc "shrinking naturals"
    (check-values (9 '((0) (5 3 0) (7 4 0) (8 6 5 3 2 1 0)))
      (sample-shrink gen:natural))

    (check-values (396 '((393 387 291 273 271 267 263 0)
                         (198 149 148 130 98 49 25 13 ...)
                         (372 361 350 348 347 337 327 322 ...)
                         (384 0)))
      (sample-shrink gen:natural 500))

    (check-values (74 '((0)
                        (73 69 52 51 39 35 33 29 ...)
                        (56 28 0)
                        (65 64 56 0)))
      (sample-shrink gen:natural 300))))

(: gen:integer-in (-> ([lo : Integer]
                       [hi : Integer])
                      #:pre (lo hi) (<= lo hi)
                      (Gen Integer)))
(define (gen:integer-in lo hi)
  (lambda (rng _size)
    (make-shrink-tree
     (random lo (add1 hi) rng))))

#;(module+ test
  (tc "integer-in"
    (check-equal? (sample (gen:integer-in 0 20) 5) '(6 3 19 12 10))
    (check-equal? (sample (gen:integer-in -5 5) 5) '(3 1 3 2 -3)))

  (tc "shrinking integer-in"
    (check-values (6 '((0) (3 2 0) (5 0)))
      (sample-shrink (gen:integer-in 0 20)))

    (check-values (-28 '((-200)
                         (-114 -116 -117 -137 -140 -155 -156 -167 ...)
                         (-30 -32 -33 -43 -200)
                         (-38 -78 -93 -146 -149 -150 -175 -187 ...)))
      (sample-shrink (gen:integer-in -200 20)))))

(: gen:real (Gen Real))
(define gen:real
  (lambda (rng _size)
    (make-shrink-tree (random rng))))

(: gen:one-of (All (a) (-> (Pairof a (Listof a)) (Gen a))))
(define (gen:one-of xs)
  (lambda (rng _size)
    (make-shrink-tree (random-ref xs rng))))

(: gen:boolean (Gen Boolean))
(define gen:boolean
  (lambda (rng _size)
    (if (zero? (random 0 2 rng))
        (make-shrink-tree #f)
        (make-shrink-tree #t (delay (list (make-shrink-tree #f)))))))

#;(module+ test
  (tc "boolean"
    (check-equal? (sample gen:boolean 5)
                  '(#f #f #t #t #f)))

  (tc "shrinking boolean"
    (check-values (#f '())
      (sample-shrink gen:boolean))

    (check-values (#f '())
      (sample-shrink gen:boolean))

    (check-values (#t '((#f)))
      (sample-shrink gen:boolean))))

(define-type Char-Integer (Refine [c : Integer]
                                  (or (and (<= 0 c) (<= c #xD7FF))
                                      (and (<= #xE000 c) (<= c #x10FFFF)))))

(: gen:char-in (-> ([lo : Char-Integer]
                    [hi : Char-Integer])
                   #:pre (lo hi) (<= lo hi)
                   (Gen Char)))
(define (gen:char-in lo hi)
  (define g
    (if (or (and (< lo #xD800)
                 (< hi #xD800))
            (and (> lo #xDFFF)
                 (> hi #xDFFF)))
        (gen:integer-in lo hi)
        (gen:choice
         (gen:integer-in lo #xD7FF)
         (gen:integer-in #xE000 hi))))

  (gen:map g integer->char))

(: gen:char (Gen Char))
(define gen:char
  (gen:char-in 0 255))

(: gen:char-letter (Gen Char))
(define gen:char-letter
  (gen:choice
   (gen:char-in 65 90)
   (gen:char-in 97 122)))

(: gen:char-digit (Gen Char))
(define gen:char-digit
  (gen:char-in 48 57))

(: gen:char-alphanumeric (Gen Char))
(define gen:char-alphanumeric
  (gen:choice
   gen:char-letter
   gen:char-digit))

(: gen:tuple (All (a ...) (-> (Gen a) ... a (Gen (List a ... a)))))
(define (gen:tuple . gs)
  (lambda (rng size)
    (let ([xs : (List (Shrink-Tree a) ... a)
              (map (lambda #:forall (b) ([g : (Gen b)]) (g rng size)) gs)])
      #;(shrink-tree-map
       (curry map value)
       (build-shrink-tree xs shrink-one))
      (shrink-tree-map
       (lambda ([x : (List (Shrink-Tree a) ... a)]) : (List a ... a)
         (map value x))
       (make-shrink-tree xs)))))

#;(module+ test
  (tc "tuple"
    (check-equal? (sample (gen:tuple gen:natural gen:char-digit) 4)
                  '((0 #\1) (1 #\5) (2 #\7) (6 #\7))))

  (tc "shrinking tuple"
    (check-values ('(6 |954|)
                   '(((6 |904|)
                      (6 |900|)
                      (5 |900|)
                      (4 |900|)
                      (4 |800|)
                      (4 |00|)
                      (0 |00|)
                      (0 |0|)
                      ...)
                     ((6 |95|)
                      (6 |93|)
                      (6 ||)
                      (3 ||)
                      (0 ||))))
      (sample-shrink (gen:tuple gen:natural (gen:symbol gen:char-digit)) 20 2 8))))

(: shrink-one (All (a) (-> (Listof (Shrink-Tree a)) (Listof (Listof (Shrink-Tree a))))))
(define (shrink-one xs)
  (match xs
    ['() '()]
    [(cons x xs)
     (append (for/list : (Listof (Listof (Shrink-Tree a)))
               ([shrunk-x (shrink x)])
               (cons shrunk-x xs))
             (for/list : (Listof (Listof (Shrink-Tree a)))
               ([shrunk-xs (shrink-one xs)])
               (cons x shrunk-xs)))]))

(: removes (All (a) (-> Natural Natural (Listof a) (Listof (Listof a)))))
(define (removes k n xs)
  (let ([c (- n k)])
    (cond
      [(> 0 c) '()]
      [(= 0 c) '(())]
      [else (let-values ([(xs-l xs-r) (split-at xs k)])
              (cons xs-r (for/list : (Listof (Listof a))
                           ([r-xs (removes k c xs-r)])
                           (append xs-l r-xs))))])))

(: shrink-list (All (a) (-> (Listof (Shrink-Tree a)) (Listof (Listof (Shrink-Tree a))))))
(define (shrink-list xs)
  (let ([n (length xs)])
    (if (= n 0)
        '()
        (append
         (append* (for/list : (Listof (Listof (Listof (Shrink-Tree a))))
                    ([k : Natural (cons n (halves n))])
                    (removes k n xs)))
         (shrink-one xs)))))

(: gen:list (All (a) (->* ((Gen a)) (#:max-length Natural) (Gen (Listof a)))))
(define (gen:list g #:max-length [max-len 128])
  (lambda (rng size)
    (let* ([len (min (random 0 (add1 size) rng) max-len)]
           [xs (for/list : (Listof (Shrink-Tree a)) ([_ (in-range len)])
                 (g rng size))])
      ((inst shrink-tree-map (Listof (Shrink-Tree a)) (Listof a))
       (curry (inst map a (Shrink-Tree a)) value)
       (build-shrink-tree xs (inst shrink-list a))))))

(: gen:exact-list (All (a) (-> (Gen a) Natural (Gen (Listof a)))))
(define (gen:exact-list g len)
  (lambda (rng size)
    (let ([xs (for/list : (Listof (Shrink-Tree a)) ([_ (in-range len)])
                (g rng size))])
      ((inst shrink-tree-map (Listof (Shrink-Tree a)) (Listof a))
       (curry (inst map a (Shrink-Tree a)) value)
       (build-shrink-tree xs (inst shrink-one a))))))

#;(module+ test
  (tc "list"
    (check-equal? (sample (gen:list gen:natural) 4)
                  '(()
                    ()
                    (2 2 3 3)
                    (6 2 6 5 2 2 9))))

  (tc "shrinking list"
    (check-values ('(3 19 12 10 16 12)
                   '(((3 19 12 10 12 12)
                      (3 19 12 10 6 12)
                      (3 19 12 8 6 12)
                      (8 6 12)
                      (8 6 6)
                      (8 6 3)
                      (8 6 0)
                      (8 3 0)
                      ...)
                     ((3 12 10 16 12)
                      (3 0 10 16 12)
                      (0 0 10 16 12)
                      (0 0 10 16)
                      (0 0 10)
                      (0 10)
                      ())))
      (sample-shrink (gen:list gen:natural) 20 2 8))))

(: gen:vector (All (a) (->* ((Gen a)) (#:max-length Natural) (Gen (Vectorof a)))))
(define (gen:vector g #:max-length [max-len 128])
  (gen:map (gen:list g #:max-length max-len)
           (inst list->vector a)))

(define-type Byte-Integer (Refine [i : Integer] (and (<= 0 i) (<= i 255))))

(: gen:bytes (->* () ((Gen Byte-Integer) #:max-length Natural) (Gen Bytes)))
(define (gen:bytes [g (gen:integer-in 0 255)] #:max-length [max-len 128])
  (gen:map (gen:list g #:max-length max-len)
           list->bytes))

(: gen:string (->* () ((Gen Char) #:max-length Natural) (Gen String)))
(define (gen:string [g gen:char] #:max-length [max-len 128])
  (gen:map (gen:list g #:max-length max-len)
           list->string))

(: gen:symbol (->* () ((Gen Char) #:max-length Natural) (Gen Symbol)))
(define (gen:symbol [g gen:char] #:max-length [max-len 128])
  (gen:map (gen:string g #:max-length max-len)
           string->symbol))

#;(define (make-gen:hash who constructor)
  (lambda pairs
    (unless (even? (length pairs))
      (raise-argument-error who "an even number of arguments" pairs))

    (gen
     (lambda (rng size)
       (define-values (keys streams)
         (for/fold ([keys    null]
                    [streams null])
                   ([(v i) (in-indexed pairs)])
           (if (even? i)
               (values (cons v keys) streams)
               (values keys (cons (v rng size) streams)))))

       (define streams-for-keys
         (for/list ([k (in-list keys)]
                    [s (in-list streams)])
           (for/stream ([v (in-stream s)])
             (cons k v))))

       (stream-dedupe
        (for*/stream ([s (in-list streams-for-keys)]
                      [p (in-stream s)]
                      [h (sequence-map
                          (lambda vals
                            (constructor (append vals (list p))))
                          (apply in-parallel streams-for-keys))])
          h))))))

#;(define-syntax-rule (define-gen:hash id f)
  (define id (make-gen:hash 'id f)))

;(define-gen:hash gen:hash make-immutable-hash)
;(define-gen:hash gen:hasheq make-immutable-hasheq)
;(define-gen:hash gen:hasheqv make-immutable-hasheqv)

(define-type (Freq a) (Pairof Exact-Positive-Integer (Gen a)))

(: freq-sum (All (a) (-> (Pairof (Freq a) (Listof (Freq a))) Exact-Positive-Integer)))
(define (freq-sum freqs)
  (foldl (λ ([freq : (Freq a)] [acc : Exact-Positive-Integer])
           (+ (car freq) acc))
         (car (first freqs))
         (rest freqs)))

(: gen:frequency (All (a) (-> (Pairof (Freq a) (Listof (Freq a))) (Gen a))))
(define (gen:frequency freqs)
  (let ([total : Exact-Positive-Integer (freq-sum freqs)])
    (gen:bind
     (gen:no-shrink
      (gen:integer-in 0 (sub1 total)))
     (lambda ([n : Integer])
       (let loop : (Gen a)
         ([sum 0]
          [freqs freqs])
         (let* ([pair (car freqs)]
                [sum* (+ (car pair) sum)])
           (if (> sum* n)
               (cdr pair)
               (loop sum* (cdr freqs)))))))))

#;(module+ test
  (tc "frequency"
    (check-equal?
     (sample (gen:frequency `((7 . ,gen:natural)
                              (5 . ,gen:char-letter)
                              (2 . ,(gen:string gen:char-letter))))
             10)
     '(0 1 "uU" #\u 13 #\U #\R #\G #\q 51))))

;; Local Variables:
;; eval: (put 'check-values 'racket-indent-function #'defun)
;; eval: (put 'tc 'racket-indent-function #'defun)
;; End:
