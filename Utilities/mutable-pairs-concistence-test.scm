;; Asbjørn Brændeland April  2009
;; INF 2810 -- Obligatory assignment 4
;; n-grams and weighted random signals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          MUTABLE PAIRS CONSISTENCY TESTS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "mutable-pairs.scm")

(mcar    (mcons 'a 'd))
(mcdr    (mcons 'a 'd))
(mcaar   (mcons (mcons 'aa 'da) 'd))
(mcadr   (mcons 'a (mcons 'ad 'dd)))
(mcdar   (mcons (mcons 'aa 'da) 'd))
(mcddr   (mcons 'a (mcons 'ad 'dd)))
(mcaaar  (mcons (mcons (mcons 'aaa 'daa) 'da) 'd))
(mcaadr  (mcons 'a (mcons (mcons 'aad 'dad) 'dd)))
(mcadar  (mcons (mcons 'aa (mcons 'ada 'dda)) 'd))
(mcaddr  (mcons 'a (mcons 'ad (mcons 'add 'ddd))))
(mcdaar  (mcons (mcons (mcons 'aaa 'daa) 'da) 'd))
(mcdadr  (mcons 'a (mcons (mcons 'aad 'dad) 'dd)))
(mcddar  (mcons (mcons 'aa  (mcons 'ada 'dda)) 'd))
(mcdddr  (mcons 'a (mcons 'ad (mcons 'add 'ddd))))
(mcaaaar (mcons (mcons (mcons (mcons 'aaaa 'daaa) 'daa) 'da) 'd))
(mcaaadr (mcons 'a (mcons (mcons (mcons 'aaad 'daad) 'daad) 'dad)))
(mcaadar (mcons (mcons 'aa  (mcons (mcons 'aada 'dada) 'daa)) 'd))
(mcaaddr (mcons 'a (mcons 'ad (mcons (mcons 'aadd 'dadd) 'dad))))
(mcadaar (mcons (mcons (mcons 'aaa (mcons 'adaa 'ddaa)) 'da) 'd))
(mcadadr (mcons 'a (mcons (mcons 'ada (mcons 'adad 'ddad)) 'dd)))
(mcaddar (mcons (mcons 'aa (mcons 'ada (mcons 'adda 'ddda))) 'd))
(mcadddr (mcons 'a (mcons 'ad (mcons 'add (mcons 'addd 'dddd)))))
(mcdaaar (mcons (mcons (mcons (mcons 'aaaa 'daaa) 'daa) 'da) 'd))
(mcdaadr (mcons 'a (mcons (mcons (mcons 'aaad 'daad) 'dad) 'dd)))
(mcdadar (mcons (mcons 'aa (mcons (mcons 'aada 'dada) 'dad)) 'd))
(mcdaddr (mcons 'a (mcons 'ad (mcons (mcons 'aadd 'dadd) 'dd))))
(mcddaar (mcons (mcons (mcons 'aaa (mcons 'adaa 'ddaa)) 'dd) 'd))
(mcddadr (mcons 'a (mcons (mcons 'add (mcons 'adad 'ddad)) 'dd)))
(mcdddar (mcons (mcons 'aa (mcons 'ada (mcons 'adda 'ddda))) 'd))
(mcddddr (mcons 'a (mcons 'ad (mcons 'add (mcons 'addd 'dddd)))))
