;; Mersenne Twister from Wikipedia:
;; http://en.wikipedia.org/wiki/Mersenne_twister

;; Scheme code by Asbjørn Brændeland march 28. 2008

;-----------------------------------------------------------------------------------------

;; UTILITIES
(define BAND bitwise-and)          ; (BAND n m) ==> the bits that are set in both n and m
(define BXOR bitwise-xor)          ; (BXOR n m) ==> the bits that are different in n and m
(define (<< n m) (arithmetic-shift n m))     ; left-shift bits in n m places 
(define (>> n m) (arithmetic-shift n (- m))) ; right-shift bits in n m places 
(define (BITS-0:30 x) (BAND x #x7fffffff))   ; bits #  0 - 30 from right in x
(define (BITS-0:31 x) (BAND x #xffffffff))   ; bits #  0 - 31
(define (BIT-32 x)    (BAND x #x80000000))   ; bit  # 32

;-----------------------------------------------------------------------------------------

(define (make-mersenne-twister . seed)         ; use same seed to get identical sequences
  (define N 624) ; Size of vector
  (define M 397) ; ???
  (define MT (make-vector N))
  (define (MT@ i) (vector-ref MT i))           ; return the number at postion i in MT
  (define (MT! i x) (vector-set! MT i x))      ; set! the number at pos. i in MT to x
  
  (define (initialize-MT seed)
    (vector-set! MT 0 (BAND (BITS-0:31 seed))) ; make sure we have at most 32 bits
    (do ((ind 1 (+ ind 1))) ((= ind N))        ; run throug the 623 remaining positions
      (let* ((y (MT@ (- ind 1))))
        (MT! ind (BITS-0:31 (+ 1 (* 1812433253 ; See Knuth TAOCP Vol2. 3rd Ed. P.106
                                    (BXOR y (>> y 30)))))))))
                                
  (define (refill-MT)
    (define (MT-index i) (modulo i N))         ; restrain vector index to [0 .. 623]
    (do ((ind 0 (+ ind 1))) ((= ind N))        ; run throug all 625 positions
      (let* ((y (+ (BIT-32 (MT@ ind))
                   (BITS-0:30 (MT@ (MT-index (+ ind 1))))))
             (z (BXOR (MT@ (MT-index (+ ind M))) (>> y 1))))
        (MT! ind (if (even? y) z (BXOR z 2567483615))))))
  
  (define (extract-rand i)
    (let* ((a (MT@ i))
           (b (BXOR a (>> a 11)))
           (c (BXOR b (BAND (<< b  7) 2636928640)))  ; 0x9d2c5680
           (d (BXOR c (BAND (<< c 15) 4022730752)))) ; 0xefc60000
      (BXOR d (>> d 18))))
  
  (let ((seed (if (null? seed) (abs (current-milliseconds)) (car seed))))
    (initialize-MT seed))
  (refill-MT)

  (let ((ind 0))
    (lambda ()
      (if (= ind N) (begin (refill-MT) (set! ind 0)))
      (let ((rand (extract-rand ind))) (set! ind (+ ind 1)) rand))))
