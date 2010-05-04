;; Asbjørn Brændeland March  2008
;; INF2810 -- Mandatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         MAIN                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "texts.scm")
(load "Utilities/user-IO.scm")

(define (help)
  (display"\
    t  <file name> | <file #> : set as current text
    g  2 | 3 | 4              : count bigrams | trigrams | quadragrams
    u    c | w                : text units: characters | words
    c    s | u                : case sensitive | unsensitive
    p                         : process current text
    wo <integer>              : # words in output list
    dl <integer>              : # display lines
    dw <integer>              : # display with
    tl                        : show text list
    tt                        : show top of current text
    i                         : show info: text, mode, gram
    h | ?                     : show this menu
    x                         : exit program\n"))

(define (read-command)
  (define (insist) (let ((cmd (user-input))) (if (null? cmd) (insist) cmd)))
  (let* ((err #f)
         (cmd-line (insist))
         (cmd (car cmd-line))
         (arg (and (not (null? (cdr cmd-line))) (cadr cmd-line))))
    (case cmd
      ((tl tt p i h ? x) 'whatever) ; none of these take arguments
      ((t)  (if (and (not (or (text-name? arg) (text-number? arg))))
                (set! err (string-append "SELECT TEXT: No such text: \""(item->string arg) "\""))))
      ((u)  (if (not (and arg (member arg '(c w))))
                (set! err "TEXT UNITS: expected one of \"c\" and \"w\"")))
      ((c)  (if (not (and arg (member arg '(s u))))
                (set! err "CASE SENSITIVITY: expected one of \"s\" and \"u\"")))
      ((g)  (if (not (and arg (good-number? arg (list 2 3 4))))
                (set! err "N GRAMS: expected one of 2, 3, 4")))
      ((wo) (if (not (number? arg))
                (set! err "NUMBER OF OUTPUT UNITS: expected an integer")))
      ((dw) (if (not (number? arg))
                (set! err "DISPLAY WIDTH: expected an integer")))
      ((dl) (if (not (number? arg))
                (set! err "DISPLAY LENGTH: expected an integer")))
      (else (set! err (string-append "Unknown command: " (item->string cmd)))))
    (if err (begin (err-msg err) (read-command)) cmd-line)))

(define (command-loop)
  (let ((cmd (read-command)) (done #f))
    (let ((arg (and (not (null? (cdr cmd))) (cadr cmd))))
      (case (car cmd)
        ((t)  (set-cur-text! arg))
        ((u)  (set-text-units! arg))
        ((c)  (set-sense-case! arg))
        ((g)  (set-gram-length! arg))
        ((wo) (set-n-words-out! arg))
        ((dl) (set-n-display-lines! arg))
        ((dw) (set-display-width! arg))
        ((tl) (show-text-list))
        ((tt) (if (cur-text)
                  (display-processed-text (cur-text) (get-display-width) 10)
                  (println "no current text")))
        ((p)  (process-text))
        ((i)  (apply showln (get-status)))
        ((h ?)  (help))
        ((x)  (set! done 'bye))
        (else (err-msg "Unknown command: " (car cmd)))))
    (or done (command-loop))))

(set-cur-text! 'trette-netter)

(if (not (namespace-defined? 'simulated-commands))
    (command-loop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

