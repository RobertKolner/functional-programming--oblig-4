(load "up-directory.scm")
(load "solut-texts.scm")
(load "Utilities/user-IO.scm")

(define (the-last-two-pairs L)
  (define (iter L M)
    (cond ((null? L) (error "BUT-LAST-AND-LAST requires non-empty list"))
          ((null? (mcdr L)) (mcons (mreverse M) (mcar L)))
          (else (iter (mcdr L) (mcons (mcar L) M)))))
  (iter L ()))

(define (show-bigrams table
                      indent)
  (show " " (mcar table))
  (if (not (null? (mcdr table)))
      (let ((tbl (the-last-two-pairs (mcdr table))))
        (mfor-each (lambda (R) (show" " R) (newline))(mcar tbl))
        (show " " (mcdr tbl)))))

(define (show-trigrams table
                       cur-line-length
                       indent)
  (show "  (" (rear-pad (mcaar table) cur-line-length))
  (show-bigrams ((mcdar table) 'table) (+ indent cur-line-length 1))
  (show ")")
  (if (not (null? (mcdr table)))
      (let ((tbl (the-last-two-pairs (mcdr table))))
        (showln)
        (mfor-each (lambda (page)
                     (show (make-white 2) "(" (rear-pad (mcar page) cur-line-length))
                     (show-bigrams ((mcdr page) 'table) (+ indent cur-line-length 1))
                     (showln ")"))
                   (mcar tbl))
        (show (make-white 2) "(" (rear-pad (mcadr tbl) cur-line-length))
        (show-bigrams ((mcddr tbl) 'table) (+ indent cur-line-length 1))
        (show ")")))
  (if (zero? indent) (newline)))

(define (show-quadragrams table
                          cur-line-length)
  (show "(" (rear-pad (mcaar table) (+ cur-line-length 1)))
  (showln)
  (show-trigrams ((mcdar table) 'table) 0 2)
  (showln ")")
  (mfor-each (lambda (volume)
               (show "(" (rear-pad (mcar volume) cur-line-length))
               (showln)
               (show-trigrams ((mcdr volume) 'table) cur-line-length (+ cur-line-length 1))
               (showln ")"))
             (mcdr table)))

(define intext      'undefined)
(define prepped     'undefined) ; words or letters, maybe all downcase for unsensitive
(define learner     'undefined) ; the n-grams learning procedure
(define learned     'undefined) ; the n-grams table
(define generator   'undefined) ; the text generator procedure
(define generated   'undefined) ; the output text stream
(define listed      'undefined) ; the desired number of items from the text stream
(define almost-done 'undefined) ; input to the display procedure

(define (test id unt grams)
  (set-cur-text! id)
  (set-text-units! unt)
  (set! intext      (cur-text))
  (set! prepped     (if (eq? unt 'c) (words->letters intext)  intext))
  (set! learner     (list-ref (list learn-bigrams learn-trigrams learn-quadragrams) (- grams 2)))
  (set! learned     (learner prepped))
  (set! generator   (list-ref (list random-bi-text random-tri-text random-quadra-text) (- grams 2)))
  (set! generated   (generator learned))
  (set! listed      (stream->list generated (n-units-out)))
  (set! almost-done (if (eq? unt 'c) (letters->words listed) listed))
  (case grams
    ((2) (show-bigrams (learned 'table) 0))
    ((3) (show-trigrams (learned 'table) 0 0))
    (else (show-trigrams (learned 'table) 0)))
  (newline)
  )

(set! *n-words-out* 50)

(set! *n-display-lines* 5)

(test 'en-setning 'c 3)

(display prepped) (newline)

(display listed) (newline)

(display almost-done) (newline)

(display-processed-text almost-done *display-width* *n-display-lines*)
