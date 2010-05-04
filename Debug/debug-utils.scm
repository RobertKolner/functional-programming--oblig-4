;; Asbjørn Brændeland April  2006
;; HUMIT 2710 -- Obligatorisk oppgave 4
;; n-grammer og vektede randomsignaler

(define ready (if (namespace-defined? 'ready) ready '()))

(define (displayline line) (display line) (newline))
(define (displaylineline line) (display line) (newline) (newline))

(define (do-if-ready proc quoted-expression)
  (if (member proc ready)
      (eval quoted-expression)
      (display (string-append "not ready : " (symbol->string proc))))
  (newline)
  (displayline "----------------------------------------------------------------------"))

(define (display-list list line-max display-width)
  (define cur-line 1)
  (define line-length 0)
  (define (iter list)
    (if (or (null? list) (> cur-line line-max))
        (newline)
        (let* ((item (car list))
               (str (cond ((list? item) (set! line-length (display-list item)) "")
                          ((symbol? item) (symbol->string item))
                          ((number? item) (number->string item))
                          (else "-")))
               (str-length (string-length str)))
          (if (> (+ line-length str-length) display-width)
              (begin (newline) (set! line-length 0) (set! cur-line (+ cur-line 1))))
          (if (<= cur-line line-max)
              (begin (display str) (display " ")))
          (set! line-length (+ line-length str-length 1))
          (iter (cdr list)))))
  (iter list))
