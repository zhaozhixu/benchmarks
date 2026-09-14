#lang racket/base

(require racket/os racket/fixnum racket/tcp racket/unsafe/ops racket/list data/queue
         (rename-in racket/unsafe/ops
                    [unsafe-bytes-ref bytes-ref]
                    [unsafe-bytes-set! bytes-set!]
                    [unsafe-fx+ +]
                    [unsafe-fx- -]
                    [unsafe-fxmodulo modulo]
                    [unsafe-fx* *]
                    [unsafe-fx< <]
                    [unsafe-fx> >]
                    [unsafe-fx>= >=]
                    [unsafe-fx<= <=]
                    [unsafe-fx= =]))
(#%declare #:unsafe)

(struct Node (children terminal)
  #:mutable
  #:authentic)

(struct Sieve (limit [prime #:mutable])
  #:authentic
  #:guard (lambda (limit prime type-name)
            (values limit (make-bytes (+ limit 1) 0))))

(define (calc sieve)
  (define limit (+ (Sieve-limit sieve)))
  (define prime (Sieve-prime sieve))

  (define (to-list)
    (bytes-set! prime 2 1)
    (bytes-set! prime 3 1)
    (for/list ([p (in-range 2 (+ limit 1))]
               #:when (= 1 (bytes-ref prime p)))
      p))

  (define (omit-squares)
    (let loop ([r 5])
      (define sq (* r r))
      (cond
        [(>= sq limit) r]
        [else (when (= 1 (bytes-ref prime r))
                (let loop ([i sq])
                  (cond
                    [(>= i limit) i]
                    [else (bytes-set! prime i 0)
                          (loop (+ i sq))])))
              (loop (+ r 1))])))

  (define (step1 x y)
    (let ([n (+ (* 4 x x) (* y y))])
      (when (and (<= n limit) (or (= (modulo n 12) 1) (= (modulo n 12) 5)))
        (bytes-set! prime n (- 1 (bytes-ref prime n))))))

  (define (step2 x y)
    (let ([n (+ (* 3 x x) (* y y))])
      (when (and (<= n limit) (= (modulo n 12) 7 ))
        (bytes-set! prime n (- 1 (bytes-ref prime n))))))

  (define (step3 x y)
    (let ([n (- (* 3 x x) (* y y))])
      (when (and (> x y) (<= n limit) (= (modulo n 12) 11))
        (bytes-set! prime n (- 1 (bytes-ref prime n))))))

  (define (loop-y x)
    (let loop ([y 1])
      (cond
        [(< (* y y) limit)
         (step1 x y)
         (step2 x y)
         (step3 x y)
         (loop (+ y 1))]
        [else y])))

  (define (loop-x)
    (let loop ([x 1])
      (cond
        [(< (* x x) limit)
         (loop-y x)
         (loop (+ x 1))]
        [else x])))

  (loop-x)
  (omit-squares)
  (to-list))

;; Children are created on first use: most trie nodes are leaves, and an
;; empty mutable hash is not free in Racket the way an empty
;; pmr::unordered_map is in the C++ reference.
(define (Node-children! node)
  (or (Node-children node)
      (let ([children (make-hasheq)])
        (set-Node-children! node children)
        children)))

(define (generate-trie primes)
  (define root (Node #f #f))
  (for ([el (in-list primes)])
    (define head root)
    (for ([ch (in-string (number->string el))])
      (set! head (hash-ref! (Node-children! head) ch (lambda () (Node #f #f)))))
    (set-Node-terminal! head #t))
  root)

(define (find upper-bound prefix)
  (let/cc return
    (define head (generate-trie (calc (Sieve upper-bound #f))))
    (define str-prefix (number->string prefix))
    (for ([ch (in-string str-prefix)])
      (set! head (let ([children (Node-children head)])
                   (and children (hash-ref children ch #f))))
      (when (not head)
        (return #f)))

    (define queue (make-queue))
    (enqueue! queue (cons head str-prefix))
    (let loop ([queue queue]
               [result '()])
      (cond
        [(queue-empty? queue) (sort result <)]
        [else
         (define top-prefix (dequeue! queue))
         (define-values (top prefix) (values (car top-prefix) (cdr top-prefix)))
         (for ([(ch v) (in-hash (or (Node-children top) #hasheq()))])
           (enqueue! queue (cons v (string-append prefix (string ch)))))
         (if (Node-terminal top)
           (loop queue (cons (string->number prefix) result))
           (loop queue result))]))))

(define (verify)
  (define left '(2 23 29))
  (define right (find 100 2))
  (when (not (equal? left right))
    (error 'verify "~s != ~s" left right)))

(define (notify msg)
  (with-handlers ([exn:fail:network? void])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(define UPPER-BOUND 5000000)
(define PREFIX 32338)

(module+ test
  (verify))

(module+ main
  (verify)
  (notify (format "Racket\t~s" (getpid)))
  (define results (find UPPER-BOUND PREFIX))
  (notify "stop")

  (displayln results))
