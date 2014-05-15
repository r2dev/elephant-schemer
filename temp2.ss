;;;;;;add1;;;;;;;;
(define add1
  (lambda (x)
    (+ x 1)))

;;;;;;sub1;;;;;;;;
(define sub1
  (lambda (x)
    (- x 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;abstraction;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define +
  (lambda (a b)
    (cond
     [(zero? b) a]
     [else (add1 (+ a (sub1 b)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;substraction;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define -
  (lambda (a b)
    (cond
     [(zero? b) a]
     [else (sub1 (- a (sub1 b)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;addtup;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define addtup
  (lambda (lat)
    (cond
     [(null? lat) 0]
     [else (+ (car lat) (addtup (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;multiplaiton;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *
  (lambda (a b)
    (cond
     [(zero? b) 0]
     [else (+ a (* a (sub1 b)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;tup+;;;;;;;;;;;;;;;;;
;;[(and (null? lat1) (null? lat2)) (quote())]
;;not need
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tup+
  (lambda (lat1 lat2)
    (cond
     [(and (null? lat1) (null? lat2)) (quote())]
     [(null? lat1) lat2]
     [(null? lat2) lat1]
     [else (cons (+ (car lat1) (car lat2)) (tup+ (cdr lat1) (cdr lat2)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;great than;;;;;;;;;;
;;;;;;;;;;;;;>;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define >
  (lambda (a b)
    (cond
     [(zero? a) #f]
     [(zero? b) #t]
     [else (> (sub1 a) (sub1 b))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;less than;;;;;;;;;;
;;;;;;;;;;;;;;<;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <
  (lambda (a b)
    (cond
     [(zero? b) #f]
     [(zero? a) #t]
     [else (< (sub1 a) (sub1 b))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;equal;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define =
  (lambda (a b)
    (cond
    [(< a b) #f]
    [(> a b) #f]
    [else #t])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;exption;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define expt
  (lambda (a b)
    (cond
     [(zero? b) 1]
     [else (* a (expt a (sub1 b)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;division;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define /
  (lambda (a b)
    (cond
     [(< a b) 0]
     [else (add1 (/ (- a b) b))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;length;;;;;;;;;;;;;;
;;;;'''''''''''''''''''''''''

(define length
  (lambda (lat)
    (cond
     [(null? lat) 0]
     [else (add1 (length (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pick;;;;;;;;;;;;;;;;;;;;;;;
;;;;the second condition;;;;
;;(zero? (sub1 n)) because;;
;;the number begin from 1;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pick
  (lambda (n lat)
    (cond
     [(null? lat) 'nil]
     [(zero? (sub1 n)) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;rempick;;;;;;;;;;;;;;;;;;
;;;;;;;;n;;;;;;lat;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rempick
  (lambda (n lat)
    (cond
     [(zero? n) (cdr lat)]
     [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;no-nums;;;;;;;;;;;;;;;;;
;;;;lat;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define no-nums
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [(number? (car lat)) (no-nums (cdr lat))]
     [else (cons (car lat) (no-nums (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;all-nums;;;;;;;;;;;;;;;;
;;;;;lat;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define all-nums
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [(number? (car lat)) (cons (car lat) (all-nums lat))]
     [else (all-nums (cdr lat))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;eqan?;;;;;;;;;;;;;;;;
;;;;;;;;a1;;;;a2;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (= a1 a2)]
     [(and (atom? a1) (atom? a2)) (eq? a1 a2)]
     [else #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;occur;;;;;;;;;;;;;;;;;;
;;;;;;a;;;;;lat;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define occur
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [(eqan? (car lat) a) (add1 (occur a (cdr lat)))]
     [else (occur a (cdr lat))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;one?;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;a;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define one?
  (lambda (a)
    (= a 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;rempick;;;;;;;;;;;;;
;;one?;;;version;;;;;;;;;;;;;
;;;;;;;;;n;;;;;lat;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rempick  
  (lambda (n lat)
    (cond
     [(one? n) (car lat)]
     [else (rempick (sub1 n) (cdr lat))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;it's full of stars;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;rember*;;;;;;;;;;;;;;;
;;;;;a;;;;;;;;;lat;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rember*
  (lambda (a lat)
    (cond
     [(null? lat) (quote ())]
     [(atom? (car lat))
      (cond
       [(eqan? (car lat) a) (rember* a (cdr lat))]
       [else (cons (car lat) (rember* a (cdr lat)))])]
     [else (cons (rember* a (car lat)) (rember* a (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;insertR*;;;;;;;;;;;;;;
;;;;;new;;;old;;;;lat;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertR*
  (lambda (new old lat)
    (cond
     [(null? lat) (quote ())]
     [(atom (car lat))
      (cond
       [(eqan? (car lat) old) (cons old (cons new (insertR* new old (car lat))))]
       [else (cons (car lat) (insertR* new old (car lat)))])]
     [else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;occur*;;;;;;;;;;;;;;;
;;;;;;;;a;;;;;;lat;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define occur*
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [(atom? (car lat))
      (cond
       [(eqan? (car lat) a) (add1 (occur* a (cdr lat)))]
       [else (occur* a (cdr lat))])]
     [else (+ (occur* a (car lat)) (occur* a (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;subst*;;;;;;;;;;;;
;;;;;new;;;;old;;;;;;lat;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define subst*
  (lambda (new old lat)
    (cond
     [(null? lat) (quote ())]
     [(atom? (car lat))
      (cond
       [(eqan? (car lat) old) (cons new (subst* new old (cdr lat)))]
       [else (cons (car lat) (subst* new old (cdr lat)))])]
     [else (cons (subst* new old (car lat)) (subst* new old (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;insertL*;;;;;;;;;;
;;;;new;;;;old;;;;;;lat;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertL*
  (lambda (new old lat)
    (cond
     [(null? lat) (quote ())]
     [(atom? (car lat))
      (cond
       [(eqan? (car lat) old) (cons new (cons old (insertL* new old (cdr lat))))]
       [else (cons (car lat) (insertL* new old (cdr lat)))])]
     [else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;member*;;;;;;;;;;;;;
;;;;;;;;a;;;;;;lat;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define member*
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(atom? (car lat))
      (cond
       [(eqan? (car lat) a) #t]
       [else (member* a (cdr lat))])]
     [else (or (member* a (car lat)) (member* a (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;leftmost;;;;;;;;;;;;;;;
;;;;;;;;;lat;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define leftmost
  (lambda (lat)
    (cond
     [(atom? (car lat)) (car lat)]
     [else (leftmost (car lat))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;eqlist?;;;;;;;;;;;;;;;;;
;;;;;;;lat1;;;;lat2;;;;;;;;;;;
;;thinking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eqlist?
  (lambda (lat1 lat2)
    (cond
     [(and (null? lat1) (null? lat2)) #t]
     [(or (null? lat1) (null? lat2)) #f]
     [(and (atom? (car lat1)) (atom? (car lat2)))
      (cond
       [(eqan? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2))]
       [else #f])]
     [(or (atom? (car lat1)) (atom? (car lat2))) #f]
     [else (and (eqlist? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2)))])))

;;;;;    [(and (atom? (car lat1)) (atom? (car lat2)))
;;;;;     (cond
;;;;;      [(eqan? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2))]
;;;;;      [else #f])]
;;;;;
;;;;;
;;;;;    [(and (atom? (car lat1)) (atom? (car lat2)))
;;;;;     (and (eqan? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2))))]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;equal?;;;;;;;;;;;;;;;;;;
;;;;;s1;;;;;s2;;;;;;;;;;;;;;;
;;;;;compare S-expression;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define equal?
  (lambda (s1 s2)
    (cond
     [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
     [(or (atom? s1) (atom? s2)) #f]
     [else (eqlist? s1 s2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;eqlist? (S);;;;;;;;;;
;;;;;lat1;;;lat2;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eqlist?
  (lambda (lat1 lat2)
    (cond
     [(and (null? lat1) (null? lat2)) #t]
     [(or (null? lat1) (null? lat2)) #f]
     [else (and (equal? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;rember;;;;;;;;;;;;;;
;;;;;;;a;;;;;;lat;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember
  (lambda (a lat)
    (cond
     [(null? lat) (quote ())]
     [(equal? a lat) (cdr lat)]
     [else (cons (car lat) (rember a lat))])))