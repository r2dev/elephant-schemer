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

