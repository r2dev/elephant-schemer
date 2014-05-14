;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;lat? check a list of atom;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lat?
  (lambda (list)
    (cond
     [(null? list) #t]
     [(atom? (car list)) (lat? (cdr list))]
     [else #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;member? find atom in list;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(eq? (car lat) a) #t]
     [else (member? a (cdr lat))])))

;;   [(eq? (car lat) a) #t]
;;   [else (member? a (cdr lat))]
;;   ?  
;;   [else (or (eq? (car lat) a )
;;             (member? a (cdr lat)))]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rember - remove a member;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rember
  (lambda (a lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? (car lat) a) (cdr lat)]
     [else (cons (car lat) (rember a (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;firsts;;;;;;;;;;;;
;;;;;;;;;;;lat;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define firsts
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [else (cons (car (car lat)) (firsts (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;insertR;;;;;;;;;;;;
;;;new;;;old;;;;lat;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertR
  (lambda (new old lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
     [else (cons (car lat) (insertR new old (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;insertL;;;;;;;;;;;;
;;;new;;;old;;;;;lat;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertL
  (lambda (new old lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? (car lat) old) (cons new (cons old (cdr lat)))]
     [else (cons (car lat) (insertL new old (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;subst;;;;;;;;;;;;;;
;;;;new;;;old;;;;;;lat;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define subst
  (lambda (new old lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? (car lat) old) (cons new (cdr lat))]
     [else (cons (car lat) (subst new old (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;subst2;;;;;;;;;;;;;
;;;;new;;o1;;;o2;;;lat;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     [(null? lat) (quote ())]
     [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
     [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;multirember;;;;;;;;;;
;;;;a;;;;;;lat;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? (car lat) a) (multirember a (cdr lat))]
     [else (cons (car lat) (multirember a (cdr lat)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;multiinsertR;;;;;;;;;
;;;;;new;;;old;;;lat;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multiinsertR
  (lambda (new old lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat))))]
     [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;multiinsertL;;;;;;;;;
;;;;;new;;;old;;;lat;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







