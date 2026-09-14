(import (chezscheme))

;;; The brainfuck source is translated into a Scheme expression, which is then
;;; handed to Chez's own compiler at run time, so the interpreter loop
;;; disappears entirely. Same approach as bf-staged.rkt.

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

;;; Parser.

(define (parse bf-code)
  (define in (open-input-string bf-code))
  (let go ((e 'pos))
    (let ((c (read-char in)))
      (cond
        ((eof-object? c) e)
        ((char=? c #\+) (go `(tape-inc! ,e 1)))
        ((char=? c #\-) (go `(tape-inc! ,e -1)))
        ((char=? c #\>) (go `(tape-move ,e 1)))
        ((char=? c #\<) (go `(tape-move ,e -1)))
        ((char=? c #\.) (go `(tape-print ,e)))
        ((char=? c #\[)
         (let* ((body (go 'pos))
                (end  (go 'pos)))
           `(let loop ((pos ,e))
              (if (fx> (tape-get pos) 0) (loop ,body) ,end))))
        ((char=? c #\]) e)
        (else (go e))))))

;;; Compiler.

(define (run parsed p)
  ((eval `(lambda (p print)
            (let ((data (make-fxvector 1 0)))
              (define (tape-get pos) (fxvector-ref data pos))
              (define (grow-if-needed! len)
                (let ((old-len (fxvector-length data)))
                  (when (fx>= len old-len)
                    (let loop ((new-len (fx* 2 old-len)))
                      (if (fx>= len new-len)
                          (loop (fx* 2 new-len))
                          (let ((new-data (make-fxvector new-len 0)))
                            (let copy ((i 0))
                              (when (fx< i old-len)
                                (fxvector-set! new-data i (fxvector-ref data i))
                                (copy (fx+ i 1))))
                            (set! data new-data)))))))
              (define (tape-move pos n)
                (let ((new-pos (fx+ n pos)))
                  (grow-if-needed! new-pos)
                  new-pos))
              (define (tape-inc! pos n)
                (fxvector-set! data pos (fx+ n (fxvector-ref data pos)))
                pos)
              (define (tape-print pos) (print p (tape-get pos)) pos)
              (let ((pos 0)) ,parsed)
              (void)))
         (environment '(chezscheme)))
   p print))

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

  (run (parse text) p-left)
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

  (notify-with-pid "Chez Scheme (Staged)")
  (run (parse text) p)
  (notify "stop")

  (if (printer-quiet p) (printf "Output checksum: ~s\n" (get-checksum p))))

((lambda ()
   (verify)
   (main)
))
