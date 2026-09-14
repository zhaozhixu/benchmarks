#lang racket/base

;; A hand-written parser that walks the input byte string directly and
;; materialises only the three numbers the benchmark asks for, in the spirit
;; of the "Rust (Serde Custom)" and "Go (rjson custom)" entries.
;;
;; The stock `json` collection reads through an input port, so every byte of
;; the document costs a `peek-byte` plus a `read-byte` call. Walking the same
;; 115 MB file that way takes ~1.4 s before any parsing happens at all, versus
;; ~44 ms with `unsafe-bytes-ref`.

(require racket/flonum racket/tcp racket/file racket/os racket/unsafe/ops)

(#%declare #:unsafe)
(struct Coordinates (x y z) #:transparent)

(define SPACE 32)      (define TAB 9)         (define LF 10)     (define CR 13)
(define QUOTE 34)      (define COMMA 44)      (define COLON 58)
(define OPEN-BRACE 123)(define CLOSE-BRACE 125)
(define OPEN-BRACKET 91)(define CLOSE-BRACKET 93)
(define BACKSLASH 92)  (define PLUS 43)       (define MINUS 45)  (define DOT 46)
(define ZERO 48)       (define NINE 57)
(define LOWER-E 101)   (define UPPER-E 69)
(define X 120)         (define Y 121)         (define Z 122)

(define COORDINATES #"coordinates")

(define (calc bs)
  (define len (bytes-length bs))
  (define-syntax-rule (u8 i) (unsafe-bytes-ref bs i))

  (define (space? c)
    (or (unsafe-fx= c SPACE) (unsafe-fx= c LF)
        (unsafe-fx= c CR) (unsafe-fx= c TAB)))

  (define (skip-space i)
    (let loop ([i i])
      (if (and (unsafe-fx< i len) (space? (u8 i))) (loop (unsafe-fx+ i 1)) i)))

  ;; Returns the index just past the closing quote.
  (define (skip-string i) ; i points just past the opening quote
    (let loop ([i i])
      (define c (u8 i))
      (cond
        [(unsafe-fx= c QUOTE) (unsafe-fx+ i 1)]
        [(unsafe-fx= c BACKSLASH) (loop (unsafe-fx+ i 2))]
        [else (loop (unsafe-fx+ i 1))])))

  (define (number-byte? c)
    (or (and (unsafe-fx>= c ZERO) (unsafe-fx<= c NINE))
        (unsafe-fx= c MINUS) (unsafe-fx= c PLUS) (unsafe-fx= c DOT)
        (unsafe-fx= c LOWER-E) (unsafe-fx= c UPPER-E)))

  (define (read-number i)
    (let loop ([j i])
      (if (and (unsafe-fx< j len) (number-byte? (u8 j)))
          (loop (unsafe-fx+ j 1))
          (values (real->double-flonum
                   (string->number (bytes->string/latin-1 bs #f i j)))
                  j))))

  ;; Skips any JSON value; returns the index just past it.
  (define (skip-value i0)
    (define i (skip-space i0))
    (define c (u8 i))
    (cond
      [(unsafe-fx= c QUOTE) (skip-string (unsafe-fx+ i 1))]
      [(or (unsafe-fx= c OPEN-BRACE) (unsafe-fx= c OPEN-BRACKET))
       (let loop ([j (unsafe-fx+ i 1)] [depth 1])
         (cond
           [(unsafe-fx= depth 0) j]
           [else
            (define d (u8 j))
            (cond
              [(unsafe-fx= d QUOTE) (loop (skip-string (unsafe-fx+ j 1)) depth)]
              [(or (unsafe-fx= d OPEN-BRACE) (unsafe-fx= d OPEN-BRACKET))
               (loop (unsafe-fx+ j 1) (unsafe-fx+ depth 1))]
              [(or (unsafe-fx= d CLOSE-BRACE) (unsafe-fx= d CLOSE-BRACKET))
               (loop (unsafe-fx+ j 1) (unsafe-fx- depth 1))]
              [else (loop (unsafe-fx+ j 1) depth)])]))]
      [else
       (let loop ([j i])
         (define d (u8 j))
         (if (or (unsafe-fx= d COMMA) (unsafe-fx= d CLOSE-BRACE)
                 (unsafe-fx= d CLOSE-BRACKET) (space? d))
             j
             (loop (unsafe-fx+ j 1))))]))

  ;; Index just past the '[' that opens the "coordinates" array.
  (define (find-coordinates)
    (define key-len (bytes-length COORDINATES))
    (let loop ([i 0])
      (cond
        [(and (unsafe-fx= (u8 i) QUOTE)
              (equal? COORDINATES
                      (subbytes bs (unsafe-fx+ i 1) (unsafe-fx+ i 1 key-len))))
         (define colon (skip-space (unsafe-fx+ i 2 key-len)))
         (unsafe-fx+ (skip-space (unsafe-fx+ colon 1)) 1)]
        [else (loop (unsafe-fx+ i 1))])))

  ;; Steps past a ',' separator when there is one.
  (define (next-item i)
    (define j (skip-space i))
    (if (unsafe-fx= (u8 j) COMMA) (skip-space (unsafe-fx+ j 1)) j))

  (let object-loop ([i (skip-space (find-coordinates))]
                    [x 0.0] [y 0.0] [z 0.0] [n 0])
    (cond
      [(unsafe-fx= (u8 i) CLOSE-BRACKET)
       (define n-fl (->fl n))
       (Coordinates (unsafe-fl/ x n-fl) (unsafe-fl/ y n-fl) (unsafe-fl/ z n-fl))]
      [else
       ;; i points at the '{' of a coordinate object
       (let key-loop ([j (skip-space (unsafe-fx+ i 1))] [x x] [y y] [z z])
         (cond
           [(unsafe-fx= (u8 j) CLOSE-BRACE)
            (object-loop (next-item (unsafe-fx+ j 1))
                         x y z (unsafe-fx+ n 1))]
           [else
            (define key-end (skip-string (unsafe-fx+ j 1)))
            (define key-len (unsafe-fx- key-end (unsafe-fx+ j 2)))
            (define key (u8 (unsafe-fx+ j 1)))
            (define value (unsafe-fx+ (skip-space key-end) 1)) ; past the ':'
            (cond
              [(and (unsafe-fx= key-len 1)
                    (or (unsafe-fx= key X) (unsafe-fx= key Y) (unsafe-fx= key Z)))
               (define-values (v v-end) (read-number (skip-space value)))
               (define next (next-item v-end))
               (cond
                 [(unsafe-fx= key X) (key-loop next (unsafe-fl+ x v) y z)]
                 [(unsafe-fx= key Y) (key-loop next x (unsafe-fl+ y v) z)]
                 [else (key-loop next x y (unsafe-fl+ z v))])]
              [else
               (key-loop (next-item (skip-value value)) x y z)])]))])))

(define (notify msg)
  (with-handlers ([exn:fail:network? void])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(define (verify)
  (define right (Coordinates 2.0 0.5 0.25))
  (for ([v '(#"{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}"
             #"{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"
             #"{\"coordinates\":[{\"x\":2.0,\"name\":\"a,b}]\",\"y\":0.5,\"opts\":{\"1\":[1,true]},\"z\":0.25}]}")])
    (define left (calc v))
    (when (not (equal? left right))
      (error 'verify "~s != ~s" left right))))

(define (read-c path)
  (parameterize ([current-locale "C"])
    (file->bytes path)))

(module+ test
  (verify))

(module+ main
  (verify)
  (define text (read-c "/tmp/1.json"))

  (notify (format "Racket (custom)\t~s" (getpid)))
  (define results (calc text))
  (notify "stop")

  (displayln results))
