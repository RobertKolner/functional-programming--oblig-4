;;; Asbjørn Brændeland april 2005

;;; Denne fila inneholder en strøm-abstraksjon med muligheten for å slå
;;; memoiseringen av og på.

;;; I tillegg inneholder den en del utskriftsrutiner med større variasjon
;;; enn de SICP tilbyr.

;;; Endelig inneholder den et part-tre standard strømoperasjoner, men
;;; ikke les disse før de første oppgaven er løst.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memoiseringsrelaterte ting

(define *memoize* #t)
(define (set-memoize! . on/off)
  (set! *memoize* (or (null? on/off) (car on/off))))

;; Kall (set-memoize! #f) for ? teste delay uten memoisering.
;; Kall (set-memoize!) for ? gjeninnf?re memoisering.

(define (memo-proc proc)              ; SICPs memo for prosedyrer 
  (let ((already-run? #f)             ; uten argumenter
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)))
      result)))

(define-syntax delay                  
  (syntax-rules ()                    
                ((delay expression)
                 (if *memoize*
                     (memo-proc (lambda () expression))
                     (lambda () expression)))))

(define (force obj) (obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; De grunnleggende standardmekansimene og -begrepene for strømmer

(define-syntax cons-stream            
  (syntax-rules ()
                ((cons-stream obj stream)
                 (cons obj (delay stream)))))

(define stream-car car)                 ; f?rste element i str?mmen
(define (stream-cdr s) (force (cdr s))) ; resten av str?mmen
(define stream-null? null?)             ; er str?mmen tom?
(define stream-nil '())                 ; den tomme str?mmen (konstant)


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

