(import (chezscheme))

(define-record-type op (fields op val))
(define-record-type tape (fields (mutable data) (mutable pos)))

;;; Printer.

(define-record-type printer (fields (mutable sum1) (mutable sum2) quiet))

(define (print p n)
  (if (printer-quiet p)
      (begin
          (printer-sum1-set! p (fxremainder
                                (fx+ (printer-sum1 p) n)
                                255))
          (printer-sum2-set! p (fxremainder
                                (fx+ (printer-sum2 p) (printer-sum1 p))
                                255)))
      (begin
          (display (integer->char n))
          (flush-output-port))))

(define (get-checksum p)
  (bitwise-ior (ash (printer-sum2 p) 8) (printer-sum1 p)))

;;; Tape ops.

(define (grow-if-needed! t len)
  (let* ((data (tape-data t)) (old-len (fxvector-length data)))
    (when (fx>= len old-len)
      (let loop ((new-len (fx* 2 old-len)))
        (if (fx>= len new-len)
            (loop (fx* 2 new-len))
            (let ((new-data (make-fxvector new-len 0)))
              (let copy ((i 0))
                (when (fx< i old-len)
                  (fxvector-set! new-data i (fxvector-ref data i))
                  (copy (fx+ i 1))))
              (tape-data-set! t new-data)))))))

(define (tape-get t)
  (fxvector-ref (tape-data t) (tape-pos t)))

(define (tape-move! t n)
  (let ((new-pos (fx+ n (tape-pos t))))
    (grow-if-needed! t new-pos)
    (tape-pos-set! t new-pos)))

(define (tape-inc! t n)
  (let ((data (tape-data t)) (pos (tape-pos t)))
    (fxvector-set! data pos (fx+ n (fxvector-ref data pos)))))

;;; Parser.

(define (parse-helper lst acc)
  (if (null? lst)
      (cons '() (list->vector (reverse acc)))
      (let ((rst (cdr lst)))
        (case (car lst)
          ((#\+) (parse-helper rst (cons (make-op 'inc 1) acc)))
          ((#\-) (parse-helper rst (cons (make-op 'inc -1) acc)))
          ((#\>) (parse-helper rst (cons (make-op 'move 1) acc)))
          ((#\<) (parse-helper rst (cons (make-op 'move -1) acc)))
          ((#\.) (parse-helper rst (cons (make-op 'print '()) acc)))
          ((#\[) (let ((subparsed (parse-helper rst '())))
                      (parse-helper
                        (car subparsed)
                        (cons
                          (make-op 'loop (cdr subparsed))
                          acc))))
          ((#\]) (cons rst (list->vector (reverse acc))))
          (else (parse-helper rst acc))))))

(define (parse bf-code) (cdr (parse-helper (string->list bf-code) '())))

;;; Interpreter.

(define (run parsed t p)
  (let ((len (vector-length parsed)))
    (let next ((i 0))
      (when (fx< i len)
        (let* ((cur (vector-ref parsed i))
               (op (op-op cur))
               (val (op-val cur)))
          (case op
            ((inc) (tape-inc! t val))
            ((move) (tape-move! t val))
            ((print) (print p (tape-get t)))
            ((loop)
             (let again ()
               (when (fx> (tape-get t) 0)
                 (run val t p)
                 (again))))
            (else (void))))
        (next (fx+ i 1))))))

;;; I/O.
(load-shared-object "../common/libnotify/target/libnotify.so")

(define (notify msg)
  (let ([bv (string->utf8 msg)]
        [func (foreign-procedure "notify" (u8*) void)])
    (func bv)))

(define (notify-with-pid msg)
  (let ([bv (string->utf8 msg)]
        [func (foreign-procedure "notify_with_pid" (u8*) void)])
    (func bv)))

(define (get-file-arg-or-exit)
  (let ((cl (command-line)))
    (cond
      ((= (length cl) 2) (list-ref cl 1))
      (else
        (printf "usage: ~a filename" (list-ref (command-line) 0))
        (exit)))))

(define (file->string path)
  (call-with-input-file path
    (lambda (port) (get-string-all port))))

(define (verify)
  (define text "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  (define p-left (make-printer 0 0 #t))
  (define p-right (make-printer 0 0 #t))

  (run (parse text) (make-tape (make-fxvector 1 0) 0) p-left)
  (for-each
   (lambda (c) (print p-right (char->integer c)))
   (string->list "Hello World!\n"))

  (let ((left (get-checksum p-left))
        (right (get-checksum p-right)))
    (if (not (eq? left right))
        (errorf 'verify "~s != ~s" left right))))

(define (main)
  (define text (file->string (get-file-arg-or-exit)))
  (define p (make-printer 0 0 (getenv "QUIET")))

  (notify-with-pid "Chez Scheme")
  (run (parse text) (make-tape (make-fxvector 1 0) 0) p)
  (notify "stop")

  (if (printer-quiet p) (printf "Output checksum: ~s\n" (get-checksum p))))

((lambda ()
   (verify)
   (main)
))
