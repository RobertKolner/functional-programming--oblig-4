;; Asbjørn Brændeland April  2009
;; INF2810 -- Mandatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    RANDOM STREAM                                   ;;
;;                                   Unfinished Shell                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "Utilities/stream-API.scm")
(load "Utilities/mersenne-twister.scm")

(define ---BODY--- #f) ; temporary body for unfinished procedures.
(define ---ARG--- #f)  ; temporary argument for unfinished procedures.
(define ---ARGS--- #f) ; temporary arguments for unfinished procedures.

;; Some reportedly well behaved values for a, c, m, j and k
(define a 16807)
(define c 0)
(define m (- (expt 2 32) 1))
(define j 37)
(define k 100)

(define (make-MTS seed)
  (let ((MT (make-mersenne-twister seed)))
    (define (mersenne)
      (cons-stream (remainder (MT) m) ; keep the MT-sequence within bounds
                   (mersenne)))       ; for uniform down-scaling.
    (mersenne)))

(define (sum-mod-m x y) (modulo (+ x y) m))

(define (congruent-m x) (sum-mod-m (* a x) c))

(define (make-LCS x0)
  ;; define an implisit stream starting with x0,
  ;; that maps itself to an LCS-stream>
  ;; return the stream
  (cons-stream x0
               (make-LCS (congruent-m x0))))

(define (make-LFS base-stream combiner)
  ;; define L as the concatenation of
  ;; - (a) the first k elements in the given base stream,
  ;; - (b) a single element linking (a) to (c)
  ;; - (c) the lagged fibonacci-sequence based on L itself
  ;; Every element from (b) on must be made by means of the given combiner.
  ;; return L
  
  ; Algoritme:
  ; 1. lag en tom liste som skal inneholde historie av k siste resultater
  ; 2. lag en strøm med første element som neste verdi, og 
  ;    andre element som måten å få neste element på.
  ; 3. Oppdater listen med historie:
  ;    3.1 Hvis n < k, legg til et element på slutten
  ;    3.2 Hvis n >= k, legg til et element på slutten og fjern den føste.
  ;    4. Returner strømmen
  (let ((history (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm))
        (last-element 'm))
    (define (get-next-LFS last-stream n history last-element)
      (let* ((current-number 
             (cond ((< n k) (stream-car last-stream))
                   (else (combiner (car history) last-element))))

            (new-history
             (cond ((< n k) (append history current-number))
                   (else ((append (cdr history) current-number))))))
        
        (cons-stream current-number
                     (get-next-LFS (stream-cdr last-stream) (+ n 1) new-history current-number))))
    
    (get-next-LFS base-stream 13 history last-element)))
  ;---BODY---)

(define MTS (make-MTS (abs (current-milliseconds)))) ; Mersenne Twister Stream
  
(define LCS (make-LCS (abs (current-milliseconds)))) ; Linear Congurent Stream

(define LFS (make-LFS LCS sum-mod-m))                ; Lagged Fibonacci Stream

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All of the above is internal code.
;; Only SCALE-RAND and RANDOM-NUMBER-GENERATOR are used by the text generator

(define (scale-rand rand range)
  (inexact->exact (floor (/ (* rand range 1.0) m))))

(define (random-number-generator)
  (let ((rand-stream MTS)) ; subst. MTS with LCS and LFS in turn when they are ready
    (lambda ()
      (let ((rand (stream-car rand-stream)))
        (set! rand-stream (stream-cdr rand-stream))
        rand))))

;-------------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; DEBUGGING DEBUGGING DEBUGGING DEBUGGING DEBUGGING DEBUGGING DEBUGGING DEBUGGING ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (namespace-defined? 'TEXT-GENERATOR))
    (begin
      (load "Debug/debug-utils.scm")

      ;; Add make-LCS and make-LFS to the ready list in turn, or
      ;; substitute make-MTS with one or the other, when they are ready
      (set! ready '(make-MTS make-LCS make-LFS))
      

      (load "Debug/debug-random-stream.scm")))

