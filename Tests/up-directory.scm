;; Asbjørn Brændeland March  2008
;; INF2810 -- Mandatory Assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ASCEND TO PARENT DIRECTORY                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A directory is a string of subdirectories and a sub directory in a path ends with
;; either a slahs or a bakcslash, thus in order to get the parent directory of a given
;; directory D, we take the reverse R of D and the substring S of R that starts with the
;; first occurence of R[0] after R[1] and then reverse S.

(define (string-reverse S) (list->string (reverse (string->list S))))
(define (string-member c S)
  (let ((L (member c (string->list S)))) (if L (list->string L) "")))
(let* ((R (string-reverse (path->string (current-directory))))
       (S (substring R 2 (string-length R))))
  (current-directory (string-reverse (string-member (string-ref R 0) S))))
