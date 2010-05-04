;; Asbjørn Brændeland April  2008
;; INF2810 -- Mandatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     STREAM API                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stream-nil '())

(define stream-null? null?)

(define stream-car car)                 ; første i strøm

(define (stream-cdr s) (force (cdr s))) ; resten av strøm

(define-syntax cons-stream              ; konstruktor for strøm
  (syntax-rules ()
    ((cons-stream obj stream)
     (cons obj (delay stream)))))

(define (stream-ref str ref)
  (cond ((null? str) #f)
        ((= ref 0) (stream-car str))
        (else (stream-ref (stream-cdr str) (- ref 1)))))

(define (stream-map proc . streams)
  (if (null? (car streams))        ; tester én argstream for alle
      stream-nil
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-segment s n)
  (if (= n 0)
      stream-nil
      (cons-stream (stream-car s)
                   (stream-segment (stream-cdr s) (- n 1)))))

(define (stream-pair-ref stream ref)
  (if (= ref 0) stream (stream-pair-ref (stream-cdr stream) (- ref 1))))

(define (list->stream lst)
  (if (null? lst) '() (cons-stream (car lst) (list->stream (cdr lst)))))

(define (stream->list s n)
  (if (or (= n 0) (stream-null? s))
      '()
      (cons (car s) (stream->list (stream-cdr s) (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (stream-enum n) (cons-stream n (stream-enum (+ n 1))))

;(define integers (stream-enum 0))
