;; Asbjørn Brændeland April  2009
;; INF2810 -- Obligatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    N-GRAM TABLES                                   ;;
;;                                    Debugging Code                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code show the result of n-gram learning (frequencey counts).

;; The code must be run from "n-gram-tables.scm".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(newline)
(displaylineline "######## DEBUGGING PROCEDURE CALL FROM debug-n-gram-tables.scm: ########")

(define (but-last-and-last L)
  (define (iter L M)
    (cond ((null? L) (error "BUT-LAST-AND-LAST requires non-empty list"))
          ((null? (mcdr L)) (mcons (mreverse M) (mcar L)))
          (else (iter (mcdr L) (mcons (mcar L) M)))))
  (iter L ()))

(define rowboat-words
  '(\. row row row your boat gently down the stream \.
      merrily merrily merrily merrily life is but a dream \.))

(define rowboat-chars
  '(\. + r o w + r o w + r o w + y o u r + b o a t + g e n t l y + d o w n + t h e +
      s t r e a m + \. + m e r r i l y + m e r r i l y + m e r r i l y + m e r r i l
      y + l i f e + i s + b u t + a + d r e a m + \.))

(define denrt-words
  '(de derre \. etne \. tre tretten \.
    trette netter \. tretten erter \. tre terner tre ender \.
    der nede erter tre nerdete terner tre erteetende ender \.
    dette tenner de tre endene \.
    endene er redde de tre ternene er etter de tretten ertene \.
    tre netter etter eter de tre endene de tre ternene en etter en \.))

(define denrt-chars
  '(\. + d e + d e r r e + \. e t n e + \. + t r e + t r e t t e n + \.
      + t r e t t e + n e t t e r + \. + t r e t t e n + e r t e r + \.
      + t r e + t e r n e r + t r e + e n d e r + \. 
      + d e r + n e d e + e r t e r + t r e + n e r d e t e + t e r n e r
      + t r e + e r t e e t e n d e + e n d e r + \.
      + d e t t e + t e n n e r + d e + t r e +  e n d e n e + \.
      + e n d e n e + e r + r e d d e + d e + t r e + t e r n e n e
      + e r + e t t e r + d e + t r e t t e n + e r t e n e + \.
      + t r e + n e t t e r + e t t e r + e t e r + d e + t r e
      + e n d e n e + d e + t r e + t e r n e n e + e n + e t t e r + e n + \.))

(define (show-bigrams table indent)
  (show (mcar table))
  (if (not (null? (mcdr table)))
      (let ((T (but-last-and-last (mcdr table))))
        (newline)
        (mfor-each (lambda (R) (showln (make-white indent) R))(mcar T))
        (show (make-white indent) (mcdr T))))
  (if (zero? indent) (newline)))
  
(define (show-trigrams table w indent)
  (show "(" (rear-pad (mcaar table) w))
  (show-bigrams ((mcdar table) 'table) (+ indent w 1))
  (show ")")
  (if (not (null? (mcdr table)))
      (let ((T (but-last-and-last (mcdr table))))
        (newline)
        (mfor-each (lambda (P)
                    (show (make-white indent) "(" (rear-pad (mcar P) w))
                    (show-bigrams ((mcdr P) 'table) (+ indent w 1))
                    (showln ")"))
                  (mcar T))
        (show (make-white indent) "(" (rear-pad (mcadr T) w))
        (show-bigrams ((mcddr T) 'table) (+ indent w 1))
        (show ")")))
  (if (zero? indent) (newline)))

(define (show-quadragrams table w)
  (show "(" (rear-pad (mcaar table) w))
  (show-trigrams ((mcdar table) 'table) w (+ w 1))
  (showln ")")
  (mfor-each (lambda (V)
              (show "(" (rear-pad (mcar V) w))
              (show-trigrams ((mcdr V) 'table) w (+ w 1))
              (showln ")"))
            (mcdr table)))

(do-if-ready
 'learn-bigrams
 '(begin
    (show-bigrams     ((learn-bigrams     denrt-words) 'table) 0)(newline)
    (newline)
    (show-bigrams     ((learn-bigrams     denrt-chars) 'table) 0) (newline)
    (newline)))

(do-if-ready
 'learn-trigrams
 '(begin
    (show-trigrams    ((learn-trigrams    denrt-words) 'table) 12 0) (newline)
    (newline)
    (show-trigrams    ((learn-trigrams    denrt-chars) 'table) 3 0) (newline)
    (newline)))
(do-if-ready
 'learn-quadragrams
 '(begin
    (show-quadragrams ((learn-quadragrams denrt-words) 'table) 12) (newline)
    (newline)
    (show-quadragrams ((learn-quadragrams denrt-chars) 'table) 3) (newline)
    (newline)))

