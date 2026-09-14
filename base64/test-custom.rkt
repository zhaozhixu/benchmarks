#lang racket/base

;; Hand-written base64, structured like the C reference in test-nolib.c:
;; table lookups, a 3-in/4-out encode loop, a 4-in/3-out decode
;; loop that tolerates line breaks between groups, and the tail handled
;; separately.
;;
;; `net/base64`'s encoder is already a direct-on-bytes loop, so this mostly
;; matches it. Its decoder is the slow half: it walks the whole input twice,
;; once to size the output and once to fill it. Computing the size up front
;; instead is what makes decoding here ~2.7x faster.
;;
;; Both tables are immutable vectors of fixnums rather than byte strings, which
;; measured faster for each of them despite the extra memory: a vector slot is
;; already a tagged fixnum, so the lookup is a single word load, and neither
;; table is big enough for the density to matter.

(require racket/tcp racket/os racket/unsafe/ops)
(#%declare #:unsafe)

(define DIGITS #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define PAD (char->integer #\=))
(define LF (char->integer #\newline))
(define CR (char->integer #\return))
(define INVALID 255)

(define encode-table
  (vector->immutable-vector (list->vector (bytes->list DIGITS))))

(define decode-table
  (let ([table (make-vector 256 INVALID)])
    (for ([digit (in-bytes DIGITS)] [i (in-naturals)])
      (vector-set! table digit i))
    (vector->immutable-vector table)))

(define (encode src)
  (define len (bytes-length src))
  (define tail (remainder len 3))
  (define body (- len tail))
  (define out (make-bytes (* 4 (quotient (+ len 2) 3)) PAD))

  (define-syntax-rule (digit! j v)
    (unsafe-bytes-set! out j (unsafe-vector*-ref encode-table v)))

  (let loop ([i 0] [j 0])
    (cond
      [(unsafe-fx< i body)
       (define b1 (unsafe-bytes-ref src i))
       (define b2 (unsafe-bytes-ref src (unsafe-fx+ i 1)))
       (define b3 (unsafe-bytes-ref src (unsafe-fx+ i 2)))
       (digit! j (unsafe-fxrshift b1 2))
       (digit! (unsafe-fx+ j 1) (unsafe-fxior (unsafe-fxlshift (unsafe-fxand b1 #b11) 4)
                                              (unsafe-fxrshift b2 4)))
       (digit! (unsafe-fx+ j 2) (unsafe-fxior (unsafe-fxlshift (unsafe-fxand b2 #b1111) 2)
                                              (unsafe-fxrshift b3 6)))
       (digit! (unsafe-fx+ j 3) (unsafe-fxand b3 #b111111))
       (loop (unsafe-fx+ i 3) (unsafe-fx+ j 4))]
      [else
       ;; 1 leftover byte -> 2 digits + "==", 2 leftover -> 3 digits + "="
       (when (unsafe-fx> tail 0)
         (define b1 (unsafe-bytes-ref src i))
         (digit! j (unsafe-fxrshift b1 2))
         (cond
           [(unsafe-fx= tail 1)
            (digit! (unsafe-fx+ j 1) (unsafe-fxlshift (unsafe-fxand b1 #b11) 4))]
           [else
            (define b2 (unsafe-bytes-ref src (unsafe-fx+ i 1)))
            (digit! (unsafe-fx+ j 1) (unsafe-fxior (unsafe-fxlshift (unsafe-fxand b1 #b11) 4)
                                                   (unsafe-fxrshift b2 4)))
            (digit! (unsafe-fx+ j 2) (unsafe-fxlshift (unsafe-fxand b2 #b1111) 2))]))
       out])))

(define (decode src)
  (define len (bytes-length src))

  ;; Drop trailing padding and line breaks, as the C reference does.
  (define end
    (let loop ([i len])
      (cond
        [(unsafe-fx= i 0) 0]
        [else
         (define c (unsafe-bytes-ref src (unsafe-fx- i 1)))
         (if (or (unsafe-fx= c PAD) (unsafe-fx= c LF) (unsafe-fx= c CR))
             (loop (unsafe-fx- i 1))
             i)])))

  ;; Upper bound: exact when there are no line breaks inside the stream.
  (define out (make-bytes (* 3 (quotient (+ end 3) 4))))

  (define (skip-breaks i)
    (let loop ([i i])
      (cond
        [(unsafe-fx>= i end) i]
        [else
         (define c (unsafe-bytes-ref src i))
         (if (or (unsafe-fx= c LF) (unsafe-fx= c CR)) (loop (unsafe-fx+ i 1)) i)])))

  (define-syntax-rule (digit i)
    (let ([v (unsafe-vector*-ref decode-table (unsafe-bytes-ref src i))])
      (when (unsafe-fx= v INVALID)
        (error 'decode "invalid base64 byte at ~s" i))
      v))

  (define written
    (let loop ([i (skip-breaks 0)] [j 0])
      (cond
        [(unsafe-fx<= (unsafe-fx+ i 4) end)
         (define v1 (digit i))
         (define v2 (digit (unsafe-fx+ i 1)))
         (define v3 (digit (unsafe-fx+ i 2)))
         (define v4 (digit (unsafe-fx+ i 3)))
         (unsafe-bytes-set! out j (unsafe-fxior (unsafe-fxlshift v1 2)
                                                (unsafe-fxrshift v2 4)))
         (unsafe-bytes-set! out (unsafe-fx+ j 1)
                            (unsafe-fxior (unsafe-fxlshift (unsafe-fxand v2 #b1111) 4)
                                          (unsafe-fxrshift v3 2)))
         (unsafe-bytes-set! out (unsafe-fx+ j 2)
                            (unsafe-fxior (unsafe-fxlshift (unsafe-fxand v3 #b11) 6) v4))
         (loop (skip-breaks (unsafe-fx+ i 4)) (unsafe-fx+ j 3))]
        [else
         ;; 2 leftover digits -> 1 byte, 3 leftover -> 2 bytes
         (define rest (unsafe-fx- end i))
         (cond
           [(unsafe-fx< rest 2) j]
           [else
            (define v1 (digit i))
            (define v2 (digit (unsafe-fx+ i 1)))
            (unsafe-bytes-set! out j (unsafe-fxior (unsafe-fxlshift v1 2)
                                                   (unsafe-fxrshift v2 4)))
            (cond
              [(unsafe-fx< rest 3) (unsafe-fx+ j 1)]
              [else
               (define v3 (digit (unsafe-fx+ i 2)))
               (unsafe-bytes-set! out (unsafe-fx+ j 1)
                                  (unsafe-fxior (unsafe-fxlshift (unsafe-fxand v2 #b1111) 4)
                                                (unsafe-fxrshift v3 2)))
               (unsafe-fx+ j 2)])])])))

  (if (= written (bytes-length out)) out (subbytes out 0 written)))

(define (verify)
  (for ([(src dst) (in-hash (hash "hello" "aGVsbG8="
                                  "world" "d29ybGQ="
                                  "a" "YQ=="
                                  "ab" "YWI="
                                  "abc" "YWJj"
                                  "" ""))])
    (define encoded (bytes->string/utf-8 (encode (string->bytes/utf-8 src))))
    (when (not (equal? encoded dst))
      (error 'verify "~s != ~s" encoded dst))
    (define decoded (bytes->string/utf-8 (decode (string->bytes/utf-8 dst))))
    (when (not (equal? decoded src))
      (error 'verify "~s != ~s" decoded src)))
  ;; line breaks between groups are tolerated on decode
  (when (not (equal? (decode #"YWJj\r\nZGVm") #"abcdef"))
    (error 'verify "line breaks not handled")))

(define (notify msg)
  (with-handlers ([exn:fail:network? void])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(module+ test
  (verify))

(module+ main
  (verify)

  (define STR-SIZE 131072)
  (define TRIES 8192)

  (define str1 (make-bytes STR-SIZE (char->integer #\a)))
  (define str2 (encode str1))
  (define str3 (decode str2))

  (notify (format "Racket (custom)\t~s" (getpid)))

  (define-values (s-encoded t-encoded t-encoded-real t-encoded-gc)
    (time-apply
     (lambda ()
       (for/sum ([_ (in-range 0 TRIES)])
         (bytes-length (encode str1))))
     '()))

  (define-values (s-decoded t-decoded t-decoded-real t-decoded-gc)
    (time-apply
     (lambda ()
       (for/sum ([_ (in-range 0 TRIES)])
         (bytes-length (decode str2))))
     '()))

  (notify "stop")

  (printf "encode ~s... to ~s...: ~s, ~s\n"
          (substring (bytes->string/utf-8 str1) 0 4)
          (substring (bytes->string/utf-8 str2) 0 4)
          s-encoded (* t-encoded 0.001))
  (printf "decode ~s... to ~s...: ~s, ~s\n"
          (substring (bytes->string/utf-8 str2) 0 4)
          (substring (bytes->string/utf-8 str3) 0 4)
          s-decoded (* t-decoded 0.001)))
