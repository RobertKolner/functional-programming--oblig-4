;; Asbjørn Brændeland April  2008
;; INF2810 -- Mandatory Assignment 4
;; n-grams and weighted random signals

;; Utilities for printing tables and long lists
;; and for reading and checking user input.

(define (make-white w) (if (<= w 0) "" (make-string w #\space)))
(define (show-white w) (make-white w))

(define (pad-and-string item width)
  (let* ((str (cond ((symbol? item) (symbol->string item))
                    ((number? item) (number->string item))
                    (else item)))
         (str-lng (string-length str))
         (pad-lng (max 0 (- width str-lng))))
    (cons (if (< pad-lng 0) "" (make-string pad-lng #\space)) str)))

(define (front-pad item width)
  (let ((p&s (pad-and-string item width)))
    (string-append (car p&s) (cdr p&s))))

(define (rear-pad item width)
  (let ((p&s (pad-and-string item width)))
    (string-append (cdr p&s) (car p&s))))

(define (item->string item)
  (cond ((symbol? item) (symbol->string item))
        ((number? item) (number->string item))
        ((string? item) item)
        ((char? item) (string item))
        ((boolean? item) (if item "true" "false"))
        ((pair? item) (string-append (item->string (car item)) (item->string (cdr item))))
        (else "")))

(define (downcase s)
  (define (string-downcase s) (list->string (map char-downcase (string->list s))))
  (cond ((string? s) (string-downcase s))
        ((symbol? s) (string->symbol (string-downcase (symbol->string s))))
        (else s)))

(define (good-number? n . constraints)
  (and (number? n)
       (or (null? constraints)
           (and (list? (car constraints)) (member n (car constraints)))
           (and (number? (car constraints)) (number? (cadr constraints))
                (>= n (car constraints)) (<= n (cadr constraints))))))

(define (user-input . prompt)
  (define (read-loop input)
    (if (char=? (peek-char) #\newline)
        (begin (read-char)
               (map downcase input))
        (read-loop (append input (list (read))))))
  (if (not (null? prompt)) (display (car prompt)))
  (read-loop '()))

(define (show . items) (for-each display items))

(define (show-separ . items)
  (if (not (null? items))
      (begin (display (car items))
             (for-each (lambda (x) (display " ") (display x))
                       (cdr items)))))

(define (showln . items) (apply show items) (newline))

(define (showln-separ . items) (apply show-separ items) (newline))

(define (err-msg . msg) (apply showln msg))

(define (display-list L curr-line-length dspl-wdth max-lines)
  (define cur-line 1)
  (define (iter L)
    (if (and (not (null? L)) (<= cur-line max-lines))
        (let* ((item (car L))
               (str (cond ((list? item)
                           (newline)
                           (set! cur-line (+ cur-line 1))
                           (display "(")
                           (set! curr-line-length
                                 (+ (display-list item 0 dspl-wdth max-lines) 2))
                           (display ")")
                           "")
                          ((symbol? item) (symbol->string item))
                          ((number? item) (number->string item))
                          ((string? item) item)
                          ((char? item)   (string item))
                          (else           "")))
               (str-length (string-length str)))
          (if (> (+ curr-line-length str-length) dspl-wdth)
              (begin (newline) (set! curr-line-length 0) (set! cur-line (+ cur-line 1))))
          (if (<= cur-line max-lines)
              (begin (display str) (display " ")))
          (set! curr-line-length (+ curr-line-length str-length 1))
          (iter (cdr L)))))
  (iter L)
  curr-line-length)
