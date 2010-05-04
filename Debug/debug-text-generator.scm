;; Asbjørn Brændeland April  2009
;; INF2810 -- Obligatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    TEXT GENERATOR                                  ;;
;;                                    Debugging Code                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code show the result of weighted random text generation.

;; The code must be run from "text-generator.scm".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Temporary random number generator

(define m (- (expt 2 32) 1))
(define (scale-rand rand range) (inexact->exact (floor (/ (* rand range 1.0) m))))
(define (make-MTS seed)
  (let ((MT (make-mersenne-twister seed)))
    (define (mersenne) (cons-stream (remainder (MT) m) (mersenne)))
    (mersenne)))
(define (random-number-generator)
  (let ((rs (make-MTS (abs (current-milliseconds)))))
    (lambda ()
      (let ((r (stream-car rs)))
        (set! rs (stream-cdr rs))
        r))))

(newline)
(display "######## DEBUGGING PROCEDURE CALL FROM debug-text-generator.scm: ########")
(newline)(newline)


(define bigrams
  (my-list->mlist
   '((\. (+ . 9) (e . 1))
     (+ (d . 9) (\. . 10) (t . 19) (n . 4) (e . 17) (r . 1))
     (d (e . 18) (d . 1))
     (e (+ . 30) (r . 26) (t . 14) (n . 18) (d . 2) (e . 1))
     (r (r . 1) (e . 16) (+ . 16) (t . 4) (n . 4) (d . 1))
     (t (n . 1) (r . 14) (t . 10) (e . 22))
     (n (e . 16) (+ . 5) (d . 6) (n . 1)))))

(define trigrams
  (my-list->mlist
   '((\.  (+ (d . 3) (t . 5) (e . 1))
          (e (t . 1)))
     (+  (d (e . 9))
         (\. (e . 1) (+ . 8))
         (t (r . 14) (e . 5))
         (n (e . 4))
         (e (r . 6) (n . 7) (t . 4))
         (r (e . 1)))
     (d  (e (+ . 9) (r . 4) (t . 2) (n . 3))
         (d (e . 1)))
     (e  (+ (d . 3) (\. . 4) (t . 11) (n . 3) (e . 9))
         (r (r . 1) (+ . 16) (t . 4) (n . 4) (d . 1))
         (t (n . 1) (t . 10) (e . 3))
         (n (+ . 5) (d . 6) (n . 1) (e . 6))
         (d (e . 1) (d . 1))
         (e (t . 1)))
     (r  (r (e . 1))
         (e (+ . 11) (t . 4) (d . 1))
         (+ (\. . 4) (t . 3) (n . 1) (d . 3) (r . 1) (e . 4))
         (t (e . 4))
         (n (e . 4))
         (d (e . 1)))
     (t  (n (e . 1))
         (r (e . 14))
         (t (e . 10))
         (e (n . 6) (+ . 3) (r . 12) (e . 1)))
     (n  (e (+ . 7) (t . 2) (r . 4) (d . 1) (n . 2))
         (+ (\. . 2) (e . 3))
         (d (e . 6))
         (n (e . 1))))))
  
(define quadragrams
  (my-list->mlist
   '((\.  (+  (d (e . 3))
              (t (r . 5))
              (e (n . 1)))
          (e  (t (n . 1))))
     (+  (d  (e (+ . 6) (r . 2) (t . 1)))
         (\.  (e (t . 1))
              (+ (t . 5) (d . 2) (e . 1)))
         (t  (r (e . 14))
             (e (r . 4) (n . 1)))
         (n  (e (t . 2) (d . 1) (r . 1)))
         (e  (r (t . 4) (+ . 2))
             (n (d . 5) (+ . 2))
             (t (t . 3) (e . 1)))
         (r  (e (d . 1))))
     (d  (e  (+ (d . 2) (e . 2) (t . 5))
             (r (r . 1) (+ . 3))
             (t (e . 1) (t . 1))
             (n (e . 3)))
         (d  (e (+ . 1))))
     (e  (+  (d (e . 3))
             (\. (e . 1) (+ . 3))
             (t (r . 6) (e . 5))
             (n (e . 3))
             (e (n . 5) (r . 4)))
         (r  (r (e . 1))
             (+ (\. . 4) (t . 3) (n . 1) (d . 3) (r . 1) (e . 4))
             (t (e . 4))
             (n (e . 4))
             (d (e . 1)))
         (t  (n (e . 1))
             (t (e . 10))
             (e (+ . 1) (n . 1) (r . 1)))
         (n  (+ (\. . 2) (e . 3))
             (d (e . 6))
             (n (e . 1))
             (e (+ . 6)))
         (d  (e (+ . 1))
             (d (e . 1)))
         (e  (t (e . 1))))
     (r  (r  (e (+ . 1)))
         (e  (+ (\. . 1) (t . 4) (e . 4) (n . 2))
             (t (t . 4))
             (d (d . 1)))
         (+  (\. (+ . 4))
             (t (r . 3))
             (n (e . 1))
             (d (e . 3))
             (r (e . 1))
             (e (t . 3) (n . 1)))
         (t  (e (r . 2) (e . 1) (n . 1)))
         (n  (e (r . 2) (n . 2)))
         (d  (e (t . 1))))
     (t  (n  (e (+ . 1)))
         (r  (e (+ . 10) (t . 4)))
         (t  (e (n . 3) (+ . 2) (r . 5)))
         (e  (n (+ . 3) (d . 1) (n . 1) (e . 1))
             (+ (n . 1) (t . 2))
             (r (+ . 8) (n . 4))
             (e (t . 1))))
     (n  (e  (+ (\. . 3) (e . 3) (d . 1))
             (t (t . 2))
             (r (+ . 3) (d . 1))
             (d (e . 1))
             (n (e . 2)))
         (+  (\. (+ . 1))
             (e (r . 2) (t . 1)))
         (d  (e (r . 2) (+ . 1) (n . 3)))
         (n  (e (r . 1)))))))
  
(define (make-bigrams-table table)
  (lambda (m)
    (cond ((eq? m 'lookup-row) (lambda (k) (massoc k table)))
          ((eq? m 'first-gram) (mlist '\. '+))
          (else (for-each println table)))))

(define (make-trigrams-table tab)
  (define table (mmap (lambda (row) (mcons (mcar row) (make-bigrams-table (mcdr row)))) tab))
  (lambda (m)
    (cond ((eq? m 'lookup-row)
           (lambda (pk rk)
             (let ((p (massoc pk table))) (and p (((mcdr p) 'lookup-row) rk)))))
          ((eq? m 'first-gram) (mlist '\. '+ 'd))
          (else (for-each (lambda (row) (print (mcar row) " ") ((mcdr row) 'print)) table)))))

(define (make-quadragrams-table tab)
  (define table (mmap (lambda (page) (mcons (mcar page) (make-trigrams-table (mcdr page)))) tab))
  (lambda (m)
    (cond ((eq? m 'lookup-row)
           (lambda (vk pk rk)
             (let ((v (massoc vk table))) (and v (((mcdr v) 'lookup-row) pk rk)))))
          ((eq? m 'first-gram) (mlist '\. '+ 'd 'e))
          (else (for-each (lambda (page) (print (mcar page) " ") ((mcdr page) 'print)) table)))))

(define bigrams-table (make-bigrams-table bigrams))
(define trigrams-table (make-trigrams-table trigrams))
(define quadragrams-table (make-quadragrams-table quadragrams))

(do-if-ready
 'random-text-signal
 '(display
   (map (lambda (r)
          (random-text-signal r (my-list->mlist '((a . 9) (b . 6) (c . 2) (d . 11) (e . 18) (f . 5)))))
        '(336860187 1010580543 1347440729 1852730991 3368601807 4210752254))))

; Should give (a b c d e f)

(do-if-ready 'random-bi-text
             '(for-each
               (lambda (x) (display (if (memq x '(\. +)) " " x)))
               (stream->list (random-bi-text bigrams-table) 50)))

(do-if-ready 'random-tri-text
             '(for-each
               (lambda (x) (display (if (memq x '(\. +)) " " x)))
               (stream->list (random-tri-text trigrams-table) 50)))

(do-if-ready 'random-quadra-text
             '(for-each
               (lambda (x) (display (if (memq x '(\. +)) " " x)))
               (stream->list (random-quadra-text quadragrams-table) 50)))
