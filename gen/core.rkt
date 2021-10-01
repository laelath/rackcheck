#lang typed/racket

(require typed/racket/random)

(provide
 exn:fail:gen?
 exn:fail:gen:exhausted?

 shrink-tree
 shrink-tree?
 build-shrink-tree
 shrink-tree-map
 value
 shrink
 sample
 ;sample-shrink
 ;eager-shrink
 ;full-shrink

 gen:const
 gen:map
 gen:bind
 gen:filter
 gen:choice
 gen:sized
 gen:resize
 gen:scale
 gen:no-shrink
 gen:with-shrink)

(struct exn:fail:gen exn:fail ())
(struct exn:fail:gen:exhausted exn:fail:gen ())

(struct (a) shrink-tree ([val : a] [shrinks : (Promise (Listof (shrink-tree a)))]))

(: value (All (a) (-> (shrink-tree a) a)))
(define value shrink-tree-val)

(: shrink (All (a) (-> (shrink-tree a) (Listof (shrink-tree a)))))
(define (shrink st)
  (force (shrink-tree-shrinks st)))

(: build-shrink-tree (All (a) (-> a (-> a (Listof a)) (shrink-tree a))))
(define (build-shrink-tree val shr)
  (shrink-tree
   val
   (delay (map (lambda ([v : a]) (build-shrink-tree v shr))
              (shr val)))))

(: shrink-tree-map (All (a b) (-> (-> a b) (shrink-tree a) (shrink-tree b))))
(define (shrink-tree-map f st)
  (shrink-tree
   (f (value st))
   (delay (map (lambda ([st : (shrink-tree a)]) (shrink-tree-map f st))
               (shrink st)))))

(: shrink-tree-join (All (a) (-> (shrink-tree (shrink-tree a)) (shrink-tree a))))
(define (shrink-tree-join st)
  (shrink-tree
   (value (value st))
   (delay (append (map (lambda ([st : (shrink-tree (shrink-tree a))])
                         (shrink-tree-join st))
                       (shrink st))
                  (shrink (value st))))))

(define-type (Gen a) (-> Pseudo-Random-Generator Natural (shrink-tree a)))

(: sample (All (a) (->* ((Gen a)) (Natural Pseudo-Random-Generator) (Listof a))))
(define (sample g [n 10] [rng (current-pseudo-random-generator)])
  (for/list ([s : Natural (in-range n)])
    (value (g rng (expt s 2)))))

; it would be pretty neat to have a visual program for exploring shrink trees
(: sample-shrink (All (a) (->* ((Gen a)) (Natural Natural Natural Pseudo-Random-Generator)
                               (Values a (Listof (U (Listof (U a '...)) '...))))))
(define (sample-shrink g [size 30] [n 4] [depth 8] [rng (current-pseudo-random-generator)])
  (let ([st (g rng size)])
    (values
     (shrink-tree-val st)
     (let* ([shrinks (shrink st)]
            [starts ((inst random-sample (shrink-tree a))
                     shrinks (min n (length shrinks)) rng #:replacement? #f)])
       (for/list ([st starts])
         (let loop : (Listof (U a '...))
           ([st st]
            [depth depth])
           (if (zero? depth)
               '(...)
               (match-let ([(shrink-tree val shrinks) st])
                 (let ([shrinks (force shrinks)])
                   (if (null? shrinks)
                       (list val)
                       (cons val (loop (random-ref shrinks rng) (sub1 depth)))))))))))))

(: eager-shrink (All (a) (->* ((Gen a)) (Natural Pseudo-Random-Generator)
                              (Values a (Listof a)))))
(define (eager-shrink g [size 30] [rng (current-pseudo-random-generator)])
  (let ([st (g rng size)])
    (values
     (shrink-tree-val st)
     (let loop ([st st])
       (let ([shrinks (shrink st)])
         (if (null? shrinks)
             '()
             (cons (shrink-tree-val (car shrinks))
                   (loop (car shrinks)))))))))

(: full-shrink (All (a) (->* ((Gen a)) (Natural (Option Natural) (Option Natural) Pseudo-Random-Generator)
                             (Rec Tree (Pairof a (Listof (U Tree '...)))))))
(define (full-shrink g [size 30] [first-n? #f] [max-depth? #f]
                     [rng (current-pseudo-random-generator)])
  (let ([st (g rng size)])
    (let loop ([st st]
               [depth 0])
      (cons (value st)
            (if (and max-depth? (= max-depth? depth))
                (list '...)
                (let across : (Listof (U (Rec Tree (Pairof a (Listof (U Tree '...)))) '...))
                  ([shrinks (shrink st)]
                   [n 0])
                  (cond
                    [(null? shrinks) '()]
                    [(and first-n? (= first-n? n)) (list '...)]
                    [else (cons (loop (car shrinks) (add1 depth))
                                (across (cdr shrinks) (add1 n)))])))))))

(: gen:const (All (a) (-> a (Gen a))))
(define (gen:const v)
  (lambda (_rng _size)
    (shrink-tree v (delay '()))))

(: gen:map (All (a b) (-> (Gen a) (-> a b) (Gen b))))
(define (gen:map g f)
  (lambda (rng size)
    (shrink-tree-map f (g rng size))))

(: gen:bind (All (a b) (-> (Gen a) (-> a (Gen b)) (Gen b))))
(define (gen:bind g h)
  (lambda (rng size)
    (let ([g-st (g rng size)]
          [rng-state (pseudo-random-generator->vector rng)])
      (shrink-tree-join
       (shrink-tree-map
        (λ ([val : a]) ((h val) (vector->pseudo-random-generator rng-state) size))
        g-st)))))

(: shrink-tree-filter (All (a) (-> (-> a Boolean) (shrink-tree a) (Option (shrink-tree a)))))
(define (shrink-tree-filter p st)
  (let ([v (value st)])
    (if (p v)
        (shrink-tree
         v
         (delay
           (filter-map (lambda ([st : (shrink-tree a)])
                         (shrink-tree-filter p st))
                       (shrink st))))
        #f)))

(: gen:filter (All (a) (->* ((Gen a) (-> a Boolean)) (Natural) (Gen a))))
(define (gen:filter g p [max-attempts 1000])
  (lambda (rng size)
    (let search : (shrink-tree a)
      ([attempts : Natural 0]
       [size : Natural size])
      (let ([st? (shrink-tree-filter p (g rng size))])
        (cond
          [st? st?]
          [(= attempts max-attempts)
           (raise (exn:fail:gen:exhausted (format "exhausted after ~a attempts" attempts)
                                          (current-continuation-marks)))]
          [else
           (search (add1 attempts) (add1 size))])))))

(: gen:choice (All (a) (-> (Gen a) (Gen a) * (Gen a))))
(define (gen:choice . gs)
  (lambda (rng size)
    ((random-ref gs rng) rng size)))

(: gen:sized (All (a) (-> (-> Natural (Gen a)) (Gen a))))
(define (gen:sized f)
  (lambda (rng size)
    ((f size) rng size)))

(: gen:resize (All (a) (-> (Gen a) Natural (Gen a))))
(define (gen:resize g size)
  (lambda (rng _size)
    (g rng size)))

(: gen:scale (All (a) (-> (Gen a) (-> Natural Natural) (Gen a))))
(define (gen:scale g f)
  (gen:sized
   (lambda ([size : Natural])
     (gen:resize g (f size)))))

(: gen:no-shrink (All (a) (-> (Gen a) (Gen a))))
(define (gen:no-shrink g)
  (lambda (rng size)
    (shrink-tree (value (g rng size)) (delay '()))))

(: gen:with-shrink (All (a) (-> (Gen a) (-> a (Listof a)) (Gen a))))
(define (gen:with-shrink g shr)
  (lambda (rng size)
    (build-shrink-tree (value (g rng size)) shr)))

#;(module+ private
  (provide shrink-tree))

(module+ test
  (require typed/rackunit)

  (check-equal? (sample (gen:const 1) 1)
                '(1))

  (check-equal? (sample (gen:const 1) 5)
                '(1 1 1 1 1))

  (check-equal? (sample (gen:map (gen:const 1) add1) 1)
                '(2))

  (check-equal? (sample (gen:bind (gen:const 1)
                                  (lambda ([v : Natural])
                                    (gen:const (add1 v))))
                        1)
                '(2))

  (check-equal? (sample (gen:filter (gen:const 1)
                                    number?)
                        1)
                '(1))

  (check-exn
   exn:fail:gen?
   (lambda ()
     (sample (gen:filter (gen:const 1)
                         (lambda ([v : Natural])
                           (eqv? v 2)))))))
