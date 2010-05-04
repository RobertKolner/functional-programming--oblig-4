;; Asbjørn Brændeland April  2009
;; INF 2810 -- Obligatory assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    MUTABLE PAIRS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For mutable pairs and lists, we use the PLT mutable pair data type rather than the
;; R5RS pair data type. Every R5RS procedure pertaining to pairs and lists has a PLT
;; counterpart prefixed with an m, except for set-car! and set-cdr whose counterparts
;; are set-mcar! and set-mcdr!.

;; mpair?, mcons, mcar, mcdr, set-mcar!, set-mcdr!, mlist?, mlist, mlength, mlist-ref,
;; mlist-tail, mappend, mappend!, mreverse, mreverse!, mmap, mfor-each, mmember, mmemv,
;; mmemq, massv, massq, massoc.

;; In addition there are the two conversion procedured list->mlist, mlist->list.

(require scheme/mpair)

(define (mcaar   pair) (mcar (mcar   pair)))
(define (mcadr   pair) (mcar (mcdr   pair)))
(define (mcdar   pair) (mcdr (mcar   pair)))
(define (mcddr   pair) (mcdr (mcdr   pair)))
(define (mcaaar  pair) (mcar (mcaar  pair)))
(define (mcaadr  pair) (mcar (mcadr  pair)))
(define (mcadar  pair) (mcar (mcdar  pair)))
(define (mcaddr  pair) (mcar (mcddr  pair)))
(define (mcdaar  pair) (mcdr (mcaar  pair)))
(define (mcdadr  pair) (mcdr (mcadr  pair)))
(define (mcddar  pair) (mcdr (mcdar  pair)))
(define (mcdddr  pair) (mcdr (mcddr  pair)))
(define (mcaaaar pair) (mcar (mcaaar pair)))
(define (mcaaadr pair) (mcar (mcaadr pair)))
(define (mcaadar pair) (mcar (mcadar pair)))
(define (mcaaddr pair) (mcar (mcaddr pair)))
(define (mcadaar pair) (mcar (mcdaar pair)))
(define (mcadadr pair) (mcar (mcdadr pair)))
(define (mcaddar pair) (mcar (mcddar pair)))
(define (mcadddr pair) (mcar (mcdddr pair)))
(define (mcdaaar pair) (mcdr (mcaaar pair)))
(define (mcdaadr pair) (mcdr (mcaadr pair)))
(define (mcdadar pair) (mcdr (mcadar pair)))
(define (mcdaddr pair) (mcdr (mcaddr pair)))
(define (mcddaar pair) (mcdr (mcdaar pair)))
(define (mcddadr pair) (mcdr (mcdadr pair)))
(define (mcdddar pair) (mcdr (mcddar pair)))
(define (mcddddr pair) (mcdr (mcdddr pair)))

;; Procedure required in "n-gram-tables.scm"
;; mlist, massoc, mappend!, mcadr, mcddr, mcdddr

;; Procedure required in "text-generator.scm"
;; mcadr, mcaddr, mcadddr

;; The following procedure operates on nested lists and transforms every layer and
;; innermost from a list to and mlist or vice versa (whereas PLT's list->mlist and
;; mlist->list only handles the outmost layer).
(define (my-list->mlist L)
  (cond ((null? L) (mlist))
        ((not (pair? L)) L)
        (else (mcons (my-list->mlist (car L)) (my-list->mlist (cdr L))))))

(define (my-mlist->list L)
  (cond ((null? L) '())
        ((not (mpair? L)) L)
        (else (cons (my-mlist->list (mcar L)) (my-mlist->list (mcdr L))))))
