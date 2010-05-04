;; Asbjørn Brændeland April  2009
;; INF2810 -- Mandatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simulate Command Loop ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define simulated-commands
  '( (tl) (tt) (p) (t little-red-house) (i) (tt) (g 2) (u w) (p) (x) ))

(define (test-main)
  (load "up-directory.scm")
  (load "main.scm")
                            ; USER-INPUT from "Utilities/user-IO.scm", which is
  (set! user-input          ; used by READ-COMMMAND in MAIN to read command lines,
                            ; is now replaced by this procedure
        (lambda ()
          (let ((cmd (car simulated-commands)))
            (display cmd) (newline)
            (set! simulated-commands (cdr simulated-commands))
            cmd)))          ; return the command, just as USER-INPUT would have.

  ; The command loop will now get its successive argument from SIMULATED-COMMANDS.
  ; Note that if this list does not end with the exit command (x), the program
  ; will crash when CAR is eventually applied to the empty command list.
  (command-loop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-main)
