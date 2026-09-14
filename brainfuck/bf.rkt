#lang racket/base
(require racket/file racket/tcp racket/cmdline racket/os
         (only-in racket/fixnum make-fxvector fxvector-length fxremainder fx< fx>= fx* fx=)
         (rename-in racket/unsafe/ops
                    [unsafe-vector*-ref vector-ref]
                    [unsafe-vector*-length vector-length]
                    [unsafe-fxvector-ref fxvector-ref]
                    [unsafe-fxvector-set! fxvector-set!]
                    [unsafe-fx+ +]))
(#%declare #:unsafe)
(define OP-INC 0) (define OP-MOVE 1) (define OP-PRINT 2) (define OP-LOOP 3)
(struct op (op val) #:authentic)
(struct tape ([data #:mutable] [pos #:mutable]) #:authentic)

;;; Printer.

(struct printer ([sum1 #:mutable] [sum2 #:mutable] quiet) #:authentic)

(define (print p n)
  (if (printer-quiet p)
      (begin
          (set-printer-sum1! p (fxremainder
                                (+ (printer-sum1 p) n)
                                255))
          (set-printer-sum2! p (fxremainder
                                (+ (printer-sum2 p) (printer-sum1 p))
                                255)))
      (begin
          (display (integer->char n))
          (flush-output))))

(define (get-checksum p)
  (bitwise-ior (arithmetic-shift (printer-sum2 p) 8) (printer-sum1 p)))

;;; Tape ops.

(define (grow-if-needed! t len)
  (define data (tape-data t))
  (define old-len (fxvector-length data))
  (when (fx>= len old-len)
    (let loop ([new-len (fx* 2 old-len)])
      (cond [(fx>= len new-len) (loop (fx* 2 new-len))]
            [else (define new-data (make-fxvector new-len 0))
                  (let copy ([i 0])
                    (when (fx< i old-len)
                      (fxvector-set! new-data i (fxvector-ref data i))
                      (copy (+ i 1))))
                  (set-tape-data! t new-data)]))))

(define (tape-get t)
  (fxvector-ref (tape-data t) (tape-pos t)))

(define (tape-move! t n)
  (define new-pos (+ n (tape-pos t)))
  (grow-if-needed! t new-pos)
  (set-tape-pos! t new-pos))

(define (tape-inc! t n)
  (let ((data (tape-data t)) (pos (tape-pos t)))
    (fxvector-set! data pos (+ n (fxvector-ref data pos)))))

;;; Parser.

(define (parse-helper lst acc)
  (if (null? lst)
      (cons '() (list->vector (reverse acc)))
      (let ([rst (cdr lst)])
        (case (car lst)
          [(#\+) (parse-helper rst (cons (op OP-INC 1) acc))]
          [(#\-) (parse-helper rst (cons (op OP-INC -1) acc))]
          [(#\>) (parse-helper rst (cons (op OP-MOVE 1) acc))]
          [(#\<) (parse-helper rst (cons (op OP-MOVE -1) acc))]
          [(#\.) (parse-helper rst (cons (op OP-PRINT -1) acc))]
          [(#\[) (let ([subparsed (parse-helper rst '())])
                 (parse-helper (car subparsed)
                               (cons (op OP-LOOP (cdr subparsed)) acc)))]
          [(#\]) (cons rst (list->vector (reverse acc)))]
          [else (parse-helper rst acc)]))))

(define (parse bf-code) (cdr (parse-helper (string->list bf-code) '())))

;;; Interpreter.

(define (run parsed t p)
  (define len (vector-length parsed))
  (let next ([i 0])
    (when (fx< i len)
      (define cur (vector-ref parsed i))
      (define op (op-op cur))
      (define val (op-val cur))
      (cond
        [(fx= op OP-INC) (tape-inc! t val)]
        [(fx= op OP-MOVE) (tape-move! t val)]
        [(fx= op OP-PRINT) (print p (tape-get t))]
        [else
         (let again ()
           (when (> (tape-get t) 0)
             (run val t p)
             (again)))])
      (next (+ i 1)))))

(define (notify msg)
  (with-handlers ([exn:fail:network? (lambda (_) (void))])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(define (read-c path)
  (parameterize ([current-locale "C"])
    (file->string path)))

(define (verify)
  (define text "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  (define p-left (printer 0 0 #t))
  (define p-right (printer 0 0 #t))

  (run (parse text) (tape (make-fxvector 1 0) 0) p-left)
  (for-each
   (lambda (c) (print p-right (char->integer c)))
   (string->list "Hello World!\n"))

  (let ((left (get-checksum p-left))
        (right (get-checksum p-right)))
    (when (not (eq? left right))
        (error 'verify "~s != ~s" left right))))

(module+ main
  (verify)
  (define text (read-c (command-line #:args (filename) filename)))
  (define p (printer 0 0 (getenv "QUIET")))

  (notify (format "Racket\t~s" (getpid)))
  (run (parse text) (tape (make-fxvector 1 0) 0) p)
  (notify "stop")

  (when (printer-quiet p) (printf "Output checksum: ~s\n" (get-checksum p))))
