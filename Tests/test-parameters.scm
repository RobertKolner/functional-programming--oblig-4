;; Asbjørn Brændeland April  2009
;; INF2810 -- Mandatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test Text Processing Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;

(load "up-directory.scm")
(load "texts.scm")
(load "Utilities/user-IO.scm")

;; A case unsensitive learning gives the tightest n-gram tables, and thus,
;; supposedly, best statistics, if case information is not relevant, but in the 
;; texts supplied with oblig 2, this parameter does not seem to make much differencs.
(define CASE-SENSITIVE #t)
(define CASE-UNSENSITIVE #f)
(define LETTERS 'c)
(define WORDS 'w)
(define BIGRAMS 2)
(define TRIGRAMS 3)
(define QUADRAGRAMS 4)

(define (test-parameters text-id sense-case text-unit gram-length)
  (set-cur-text! text-id)
  (set! *sense-case* sense-case)
  (set-text-units! text-unit)
  (set-gram-length! gram-length)
  (apply showln (get-status))
  (process-text))

(define (test-one-or-more-texts-1 text-ids) ; with regard to CASE, UNIT and gram length
  (for-each (lambda (id)
              (set-cur-text! id)
              (for-each (lambda (s-case)
                          (set! *sense-case* s-case)
                          (for-each (lambda (t-unit)
                                      (set-text-units! t-unit)
                                      (for-each (lambda (grams)
                                                  (set-gram-length! grams)
                                                  (apply showln (get-status))
                                                  (process-text))
                                                (list BIGRAMS TRIGRAMS QUADRAGRAMS)))
                                    (list LETTERS WORDS)))
                        (list CASE-SENSITIVE CASE-UNSENSITIVE)))
            text-ids))

(define (test-one-or-more-texts-2 text-ids) ; with regard to UNIT and gram length
  (for-each (lambda (id)
              (set-cur-text! id)
              (for-each (lambda (t-unit)
                          (set-text-units! t-unit)
                          (for-each (lambda (grams)
                                      (set-gram-length! grams)
                                      (apply showln (get-status))
                                      (process-text))
                                    (list BIGRAMS TRIGRAMS QUADRAGRAMS)))
                        (list LETTERS WORDS)))
            text-ids))

(set! *n-display-lines* 4)

(test-one-or-more-texts-2 '(2))

;(test-parameters 'Seanaid-na-nGaedheal CASE-UNSENSITIVE LETTERS QUADRAGRAMS)
