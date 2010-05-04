;; Asbjørn Brændeland April  2009
;; INF2810 -- Obligatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    RANDOM STREAM                                   ;;
;;                                    Debugging Code                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code show the result of random number generation.

;; The code must be run from "random-stream.scm".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(newline)
(display "####### DEBUGGING PROCEDURE CALL FROM debug-debug-random-stream.scm:#######")
(newline)

; Some debugging firendly values for a, c, m, j og k
; NB!!! Uncomment these after debugging
(define a 7)
(define c 0)
(define m 11)
(define j 5)
(define k 8)

(newline)
(displayline     "(map (lambda (x) (remainder x 10))")
(displaylineline "     (stream->list (stream-pair-ref (make-MTS 69867674635) 34) 30))")
(displaylineline " should give")
(displaylineline '(8 0 2 1 0 9 3 4 0 8 0 4 9 1 7 3 8 2 4 0 1 7 2 4 3 6 5 7 0 2))
(displaylineline "and this is what we got:")
(do-if-ready
 'make-MTS
 '(displayline (map (lambda (x) (remainder x 10))
                  (stream->list (stream-pair-ref (make-MTS 69867674635) 34) 30))))

(newline)
(displaylineline "(stream->list (stream-pair-ref (make-LCS 111) 34) 30)")
(displaylineline "should give")
(displaylineline '(3 10 4 6 9 8 1 7 5 2 3 10 4 6 9 8 1 7 5 2 3 10 4 6 9 8 1 7 5 2))
(displaylineline "and this is what we got:")
(do-if-ready
 'make-LCS
 '(displayline (stream->list (stream-pair-ref (make-LCS 111) 34) 30)))

(define alph-stream
  (list->stream '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
(define (combine-letters x y)
  (string->symbol (string-append (symbol->string x) (symbol->string y))))
(newline)
(displayline    "given")
(displayline    "(define alph-stream")
(displayline    "  (list->stream '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))")
(displayline    "(define (combine-letters x y)")
(displaylineline"  (string->symbol (string-append (symbol->string x) (symbol->string y))))")
(displaylineline "(stream->list (make-LFS alph-stream combine-letters) 21)")
(displaylineline "should give")
(displaylineline '(a b c d e f g h ad be cf dg eh fad gbe hcf addg beeh cffad dggbe ehhcf))
(displaylineline "and this is what we got:")
(do-if-ready
 'make-LFS
 '(displayline (stream->list (make-LFS alph-stream combine-letters) 21)))

(newline)
(displaylineline "(stream->list (stream-pair-ref (make-LFS (make-LCS 111) sum-mod-m) 34) 30)")
(displaylineline "should give")
(displaylineline '(2 3 2 1 1 2 8 9 3 4 4 9 10 5 1 2 1 3 9 10 1 6 4 0 0 4 4 3 1 6))
(displaylineline "and this is what we got:")
(do-if-ready
 'make-LFS
 '(displayline (stream->list (stream-pair-ref (make-LFS (make-LCS 111) sum-mod-m) 34) 30)))
