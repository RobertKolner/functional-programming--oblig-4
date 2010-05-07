;; Asbjørn Brændeland April  2009
;; INF 2810 -- Obligatory assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    N-GRAM TABLES                                   ;;
;;                                   Unfinished Shell                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "Utilities/mutable-pairs.scm")

;; Every table has a table head comprising a single item list with the table name.
;; A table type may be instantiated more than once during one run, and in order to
;; get a unique table head for each instance we create the head by means of the
;; list constructor, rather using a literal value. (eq? (list 'a) (list 'a)) ==> #f,
;; whereas (eq? '(a) '(a)) ==> #t.

;---------------------------------------------------------------------------------------
;; bigrams table

;; The bigrams table is a two dimensional associations list where, given a bigram B, the
;; first signal in B is the row key and the second signal is the entry key, and the entry
;; value is the number of occurences of B in the current text.

(define (make-bigrams-table)
  (let ((table (mlist (mlist 'bigrams))))
    
    ;; lookup the row with the given key
    (define (lookup-row row-key)
      (massoc row-key (mcdr table)))
    
    ;; make entry with key and initial count
    (define (make-entry entry-key)
      (mcons entry-key 1)) ;> {e . 1}
    
    ;; make new row with new entry
    (define (make-row entry row-key)
      (mlist row-key entry)) ;> {r {e . 1}}
    
    ;; add new entry to existing row
    (define (add-entry! entry row)
      (mappend! (mcdr row) (mlist entry))) ;> {r {e . 1} {n . 1}}
    
    ;; add new row to table
    (define (add-row! row)
      (mappend! table (mlist row))) ;> {{bigrams} {r {e . 1} {n . 1}}}
    
    ;; increase count of existing entry
    (define (update-entry! entry)
      (set-mcdr! entry (+ (mcdr entry) 1)))
    
    ;; insert or update bigram given by the keys
    (define (add-or-update! row-key entry-key)
      (let ((row (lookup-row row-key)))
        (if row ; Om row eksisterer
            (let ((entry (massoc entry-key (mcdr row))))
              (if entry ; Om entry eksisterer
                  (update-entry! entry) ; Oppdaterer eksisterende entry
                  (add-entry! (make-entry entry-key) row))) ; Ellers lages det ny entry
            (add-row! (make-row (make-entry entry-key) row-key))))) ; Ellers lages det ny row og entry
        
    
    ;; first bigram in text = key of first row + key of first entry in first row
    (define (first-gram)
      (let ((first-row (mcadr table)))
        (mcons (mcar first-row) (mcaadr first-row)))) ;> {first-row-key . first-row-first-entry-key}
    
    ; The procedure returned by MAKE_BIGRAMS-TABLE
    (lambda (m)
      (cond ((eq? m 'add-or-update!) add-or-update!)
            ((eq? m 'lookup-row) lookup-row)
            ((eq? m 'first-gram) (first-gram))
            ((eq? m 'table) (mcdr table))
            (else (error "Unknown operation -- BIGRAMS" m))))))

;; LEARN-BIGRAMS runs throug a signals list, one signal at the time, and, for each round,
;; sends two consecutive signals to the table. (Notice that, since the procedure must keep
;; track of two consecutive signals this isn't a straightforard FOR-EACH operation.)
(define (learn-bigrams signals)
  (let ((table (make-bigrams-table)))
    (define (iter signal-1 signals)
      (if (null? signals)
          ((table 'add-or-update!) signal-1 '+) ; ingen flere signaler å hente, men legg til siste med en +
          (begin ((table 'add-or-update!) signal-1 (car signals)) (iter (car signals) (cdr signals)))))
    (iter '+ signals)
    table))
;; NOTE: In order to get hold of the last bigram, one has to do
;; some special final operations.

;---------------------------------------------------------------------------------------
;; trigrams table

;; The trigrams table is a simple association list of pages where, given a trigram T
;; received by the table, the first signal in T is a page key, and the associated page
;; is a bigrams table, to which the second and the third signal in T are sent.

(define (make-trigrams-table)
  (let* ((table (mlist (mlist 'trigrams))))
    
    ;; Lookup the page and, if it was there, ask the bigrams table,
    ;; i.e. the value associated with the page key, to lookup the row.
    (define (lookup-row ---ARGS---)
      ---BODY---)
    
    ; Let the page-key be associated with a new bigrams table
    (define (make-page ---ARG---)
      ---BODY---)
    
    ;; Look up the page, and after the page is located, or a new one added, send
    ;; the 'add-or-update message to the associated existing or newly created
    ;; bigrams table, and pass the row and entry keys to the procedure returned.
    (define (add-or-update! ---ARGS---)
      ---BODY---)
    
    ;; first trigram in text = key of first page + key of first row
    ;; + key of first entry in first row
    (define (first-gram-in-table)
      ---BODY---)
    
    ; The procedure returned by MAKE_TRIGRAMS-TABLE
    (lambda (m)
      (cond ((eq? m 'add-or-update!) add-or-update!)
            ((eq? m 'lookup-row) lookup-row)
            ((eq? m 'first-gram) (first-gram-in-table))
            ((eq? m 'table) (mcdr table))
            (else (error "Unknown operation -- TRIGRAMS" m))))))

;; LEARN-TRIGRAMS runs throug a signals list, one signal at the time,
;; and, for each round, sends three consecutive signals to the table.
(define (learn-trigrams signals)
  (let ((table (make-trigrams-table)))
    (define (iter signal-1 signal-2 signals)
      ---BODY---)
    (iter ---ARGS ---)
    table))
;; NOTE: In order to get hold of the last two trigrams, one has to do
;; some special final operations.

;---------------------------------------------------------------------------------------
;; quadragrams table

;---------------------------------------------------------------------------------------
;; quadragrams table

;; The quadragrams table is a simple association list of pages where, given a quadragram
;; Q received by the table, the first signal in Q is a volume key, and the associated
;; volume is a trigrams table, to which the thre subsequent signals in Q are sent.

(define (make-quadragrams-table)
  (let* ((table (mlist (mlist 'quadragrams))))
    
    ;; lookup the volume and, if it was there, ask the trigrams table,
    ;; i.e. the value associated with the volume key, to lookup the row
    (define (lookup-row ---ARGS---)
      ---BODY---)
    
    ;; connect the page-key to a new trigrams table
    (define (make-volume ---ARG---) ---BODY---)
    
    ;; Look up the volume, and after the volume is located, or a new one added, send
    ;; the 'insert/update message to the associated existing or newly created tri-
    ;; grams table, and pass the page, row and entry keys to the procedure returned.
    (define (add-or-update! ---ARGS---)
      ---BODY---)
    
    ;; first quadragram in text = key of first volume + key of first page
    ;; + key of first row + key of first entry in first row
    (define (first-gram-in-table)
      ---BODY---)
    
    ; The procedure returned by MAKE_QUADRAGRAMS-TABLE
    (lambda (m)
      (cond ((eq? m 'add-or-update!) add-or-update!)
            ((eq? m 'lookup-row) lookup-row)
            ((eq? m 'first-gram) (first-gram-in-table))
            ((eq? m 'table) (mcdr table))
            (else (error "Unknown operation -- QUADRAGRAMS" m))))))

;; LEARN-QUADRAGRAMS runs throug a signals list, one signal at the time,
;; and, for each round, sends three consecutive signals to the table.
(define (learn-quadragrams signals)
  (let ((table (make-quadragrams-table)))
    (define (iter s1 s2 s3 signals)
      ---BODY---)
    (iter ---ARGS---)
    table))
;; NOTE: In order to get hold of the last three quadragrams, one has to do
;; som special final operations.

;-------------------------------------------------------------------------------------

;; We do not want to run the debuggin code, if "n-gram-tables.scm"
;; was loaded by another code file.

(if (not (namespace-defined? 'TEXT-GENERATOR))
    (begin
      (load "Utilities/user-IO.scm")
      (load "Debug/debug-utils.scm")

      ;; Add learn-bigrams, learn-trigrams and learn-quadragrams to the ready list
      ;; in turn, when they are ready. Notice that learn-trigrams uses learn-bigrams
      ;; and learn-quadragrams uses learn-trigrams.
      (set! ready '(learn-bigrams))

      (load "Debug/debug-n-gram-tables.scm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
