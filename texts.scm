;; Asbjørn Brændeland April  2009
;; INF2810 -- Obligatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        TEXTS                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TEXTS 'OK) ; Tells the text generator not to run its debugging code.
(load "text-generator.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; DATA TYPES FOR TEXT, AND TEXT PROCESSING PARAMETERS ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    THE TEXT OBJECT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-text-info name id langue text)
  (mlist name id langue text))
(define (set-text! text-info text)          ; Set recently loaded text, if
  (set-mcar! (mcdddr text-info) text) text) ; text was not already loaded.
(define (text-name info)     (mcar info))
(define (text-id info)       (mcadr info))
(define (text-language info) (mcaddr info))
(define (text info)          (mcadddr info))
(define (text-path info)     (string-append "Texts/texts/"
                                            (symbol->string (text-name info))
                                            ".txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                      THE TEXT LIST                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *text-list*
  (mmap (lambda (txt) (apply make-text-info (append txt '(#f)))) (load "Texts/text-list.scm")))

(define *cur-text-id*  0)

(define (text-number? n)         (and (integer? n) (>= n 0) (< n (text-count))))
(define (text-name? name)        (and (massoc name *text-list*) #t))

(define (cur-text)               (text (mlist-ref *text-list* *cur-text-id*)))
(define (text-count)             (mlength *text-list*))
(define (text-info-by-number n)  (mlist-ref *text-list* n))
(define (text-info-by-name name) (massoc name *text-list*))
(define (text-ID-interval)       (string-append
                                  "1 .. " (number->string (text-count))))
(define (get-status)
  (let ((text-info (mlist-ref *text-list* *cur-text-id*)))
    (list "units = "      *text-units*
          ", case = "     (if *sense-case* "sense" "unsense")
          ", grams = "    *gram-length*
          ", text = "     (text-name text-info)
          ", language = " (text-language text-info)
          (if (text text-info) "" "*"))))

(define (set-cur-text! id)
  (let ((info (if (number? id)
                  (and (text-number? (- id 1)) (text-info-by-number (- id 1)))
                  (text-info-by-name id))))
    (if (not info)                          ; if SET-CUR-TEXT! was called from MAIN the
        (error "SET-CUR-TEXT! bad ID: " id) ; validity of ID should have been cleared
        (begin
          (if (not (= (text-id info) *cur-text-id*))
              (set! *cur-text-id* (text-id info)))
          (if (not (text info))
              (set-text! info (load (text-path info))))
          (showln "current text was set to " (text-name info))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       TEXT PROCESSING AND DISPLAY PARAMETERS                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The current paramater values

(define *gram-length* 3)           ; bigrams: 2, trigrams: 3 or tetragrams: 4
(define *text-units* 'letters)     ; letters or characters
(define *sense-case* #t)           ; case sensitive or canse insensitive
(define *n-words-out* 200)         ; # of words to be output -- x 5 for letters
(define (n-units-out)              ; size of output, independent of display
  (if (eq? *text-units* 'letters)
      (* *n-words-out* 5)
      *n-words-out*))
(define *n-display-lines* 6)      ; # of lines in text display
(define *display-width* 80)        ; line width in text display

;; Setting the parameter values

(define (set-text-units! m)      (set! *text-units* (if (eq? m 'c) 'letters 'words)))
(define (set-sense-case! sense)  (set! *sense-case* (eq? sense 's)))
(define (set-gram-length! n)     (set! *gram-length* n))
(define (set-n-words-out! n)     (set! *n-words-out* n))
(define (set-display-width! w)   (set! *display-width* w))
(define (set-n-display-lines! n) (set! *n-display-lines* n))

(define (get-display-width) *display-width*)
(define (get-text-units) (if (eq? *text-units* 'letters) *text-units* 'words))

(define (show-text-list)
  (mfor-each (lambda (info) (showln (front-pad (+ (mcadr info) 1) 3) " " (mcar info)))
             *text-list*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  PROCESSING THE TEXT                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (process-text)
  (let* ((letters?  (eq? *text-units* 'letters))
         (text      (cur-text))
         (prepped   (cond (letters? (words->letters text))
                          ((not *sense-case*) (words->downcase-words text))
                          (else text)))
         (learner   (list-ref (list learn-bigrams learn-trigrams learn-quadragrams)
                              (- *gram-length* 2)))
         (learned   (learner prepped))
         (generator (list-ref (list random-bi-text random-tri-text random-quadra-text)
                              (- *gram-length* 2)))
         (generated (generator learned))
         (listed    (stream->list generated (n-units-out)))
         (finished  (if letters? (letters->words listed) listed)))
    (display-processed-text finished *display-width* *n-display-lines*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion from words to groups of single character symbols and vice versa.

;; WORDS->LETTERS converts a list of numbers and words, i.e. symbols of one or more
;; characters, to a list of numbers and character groups, separated by plus signs.

;; Ex: (words->letters (a list of 6 words and 2 numbers))
;;     ===> (+ a + l i s t + o f + 6 + w o r d s + a n d + 2 + n u m b e r s)

(define (words->letters tekst)
  (define (item->group item)
    (cond ((symbol? item)
           (cons '+ (map
                     (lambda (c)
                       (string->symbol (string (if *sense-case* c (char-downcase c)))))
                     (string->list (symbol->string item)))))
          ((number? item) (list '+ item))
          (else ())))
  (apply append (map item->group tekst)))

;; WORDS->DOWNCASE-WORDS is used for case insensitiv frequency counts.
(define (words->downcase-words text)
  (define (iter intext outtext)
    (cond ((null? intext) (reverse outtext))
          ((symbol? (car intext))
           (iter (cdr intext)
                 (cons (string->symbol (string-downcase (symbol->string (car intext))))
                       outtext)))
          (else (iter (cdr intext) (cons (car intext) outtext)))))
  (iter text '()))

;; LETTERS->WORDS converts a list of numbers and characters, in the format returned by
;; WORDS->LETTERS, to a list of numbers and words.

;; Ex: (letters->words (+ a + l i s t + o f + 6 + w o r d s + a n d + 2 + n u m b e r s)
;;     ===> (a list of 6 words and 2 numbers)

(define (letters->words letters)
  (define (compile group)
    (if (and (= (length group) 1) (number? (car group)))
        (car group)
        (string->symbol (apply string-append (map symbol->string group)))))
  (define (iter letters current-group words)
    (cond ((null? letters) (reverse (cons 'end-of-list words)))
          ((eq? (car letters) '+)
           (iter (cdr letters) '() (cons (compile (reverse current-group)) words)))
          (else (iter (cdr letters) (cons (car letters) current-group) words))))
  (iter letters () ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            DISPLAYING PROCESSED TEXT                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPLAY-PROCESSED-TEXT takes as input a Scheme list of words, numbers, punctuation 
;; marks and other delimeters and displays the list in normal text format. In Scheme the
;; following characters are reserved and must be escaped  ' " . , ; ( ) [ ] { } \. The
;; escape character is \ (backslash).
;; With regard to frequency counts, space is supposed to have been inserted before every
;; occurrence of the the above mentioned characters. The procedure removes space between
;; a left parenthesis, (, {, [ and the succeeding item, and after an item followed by
;; a right parenthesis or a punctuation mark, ), }, ] . , ; :  ! ?.
;; Hard line shifts are indicated by the percentage character %. To include a % in the
;; text it must be doubly escaped. (Notice that PLT allows non-standard symbols such as
;; numbers and reserved characters. These are rendered between two pipes, e.g. |42|, as
;; literals in the code as well as in the output from REPL.

(define (display-processed-text text display-width line-max)
  (define left-parentheses '(\( \{ \[))
  (define right-parentheses '(\) \} \]))
  (define right-delimeters (append right-parentheses '(\. \, \; : ! ?)))
  (define cur-line 1)
  (define line-length 0)
  (define (grow-line-length add)
    (set! line-length (+ line-length add)))
  (define (line-shift) (newline)
    (set! line-length 0)
    (set! cur-line (+ cur-line 1)))
  (define (item->string item)
    (cond ((symbol? item) (symbol->string item))
          ((number? item) (number->string item))
          (else (error "DISPLAY-PROCESSED-TEXT: not a symbol or a number" item))))
  
  (define (iter text item-1 item-2)
    (if (or (null? text) (> cur-line line-max))
        (begin (if (> line-length 0) (newline))
               (display (make-string display-width #\-))  ; Finish with a horizontal bar
               (newline))
        (let* ((do-shift (eq? item-1 '%))                 ; % indicates a hard line shift,
               (str-1 (cond (do-shift "")                 ; so leave it out of the text.
                            ((eq? item-1 '|\%|) "&")      ; Keep % if it is doubly escaped.
                            (else (item->string item-1)))))
          (if (or (memq item-1 left-parentheses)          ; If we have a left or a right 
                  (memq item-2 right-delimeters))         ; parenthesis or a delimeter,
              (iter (cdr text)                            ; immediately start a new lap,
                    (string->symbol (string-append                ; with the concatenation
                                     str-1 (item->string item-2))); of item-1 and item-2,
                    (car text))                                   ; and the next text item.
              (let ((str-length (string-length str-1)))
                (if (or do-shift (> (+ line-length str-length) display-width))
                    (line-shift))
                (if (<= cur-line line-max)               ; An added line may have filled
                    (begin                               ; the alloted number of output lines
                      (if (> line-length 0)
                          (begin (display " ")           ; separate current output item
                                 (grow-line-length 1)))  ; from previous
                      (display str-1)
                      (grow-line-length str-length)))
                (iter (cdr text) item-2 (car text)))))))

  (display (make-string display-width #\-))               ; Start with a horizontal bar
  (newline)
  (iter (cddr text) (car text) (cadr text)))
