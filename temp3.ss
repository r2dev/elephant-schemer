;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;numbered?;;;;;;;;;;;;;;
;;;;;;aexp;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define numbered?
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [(eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
     [(eq? (car (cdr aexp)) (quote *)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
     [(eq? (car (cdr aexp)) (quote expt)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
     [else #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;value;;;;;;;;;;;;;
;;;;;;;;;aexp;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value
  (lambda (aexp)
    (cond
     [(atom? aexp) aexp]
     [(eq? (car (cdr aexp)) (quote +) ) (+ (value (car aexp)) (value (car (cdr (cdr aexp)))))]
     [(eq? (car (cdr aexp)) (quote -) ) (- (value (car aexp)) (value (car (cdr (cdr aexp)))))]
     [(eq? (car (cdr aexp)) (quote *) ) (* (value (car aexp)) (value (car (cdr (cdr aexp)))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;value;;;;;;;;;;;;;;
;;;;;;;;sexp;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value
  (lambda (sexp)
    (cond
     [(atom? sexp) sexp]
     [(eq? (car sexp) (quote +)) (+ (value (car (cdr sexp))) (value (car (cdr (cdr sexp)))))]
     [(eq? (car sexp) (quote -)) (- (value (car (cdr sexp))) (value (car (cdr (cdr sexp)))))]
     [(eq? (car sexp) (quote *)) (* (value (car (cdr sexp))) (value (car (cdr (cdr sexp)))))]
     [(eq? (car sexp) (quote /)) (/ (value (car (cdr sexp))) (value (car (cdr (cdr sexp)))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;sero?;;;;;;;;;;;;;;
;;;;;;;s;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sero?
  (lambda (s)
    (null? s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;edd1;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;s;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define edd1
  (lambda (s)
    (cons s (quote ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;zub1;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;s;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zub1
  (lambda (s)
    (cdr s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;+;;;;;;;;;;;;;;;;;;
;;;;;;;;;;s1;;;;s2;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define +
  (lambda (s1 s2)
    (cond
     [(sero? s2) s1]
     [else (edd1 (+ s1 (zub1 s2)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;lat?;;;;;;;;;;;;;;;
;;;;;;;;;;slat;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lat?
  (lambda (slat)
    (cond
     [(null? slat) #t]
     [(atom? (car slat)) (lat? (cdr slat))]
     [else #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;friends and relations;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;member? find atom in list;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(eq? (car lat) a) #t]
     [else (member? a (cdr lat))])))


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
;;;;;set?;;;;;;;;;;;;;;;;;;;;
;;;;lat;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define set?
  (lambda (lat)
    (cond
     [(null? lat) #t]
     [(member? (car lat) (cdr lat)) #f]
     [else (set? (cdr lat))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;makeset;;;;;;;;;
;;;;;;;;lat;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define makeset
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
     [else (cons (car lat) (makeset (cdr lat)))])))
;#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;makeset;;;;;;;;;;
;;;;;;multirember;;;;;;;;;;;;
;;;;;;lat;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define makeset
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;subset?;;;;;;;;;;;
;;;;;;set1;;;;;;;set2;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define subset?
  (lambda (set1 set2)
    (cond
     [(null? set1) #t]
     [else (and (member? (car set1) set2) (subset? (cdr set1) set2))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;eqset?;;;;;;;;;;;;;;
;;;;;;;;set1;;;;;;set2;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;intersect?;;;;;;;;;;;
;;;;;set1;;;;;;;;set2;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define intersect?
  (lambda (set1 set2)
    (cond
     [(null? set1) #f]
     [(or (member? (car set1) set2) (intersect? (cdr set1) set2))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;interest;;;;;;;;;;;;;;;
;;;;;set1;;;;;;set2;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define interest
  (lambda (set1 set2)
    (cond
     [(null? set1) (quote ())]
     [(member? (car set1) set2) (cons (car set1) (interest (cdr set1) set2))]
     [else (interest (cdr set1) set2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;union;;;;;;;;;;;
;;;;;;ERROR;;;;;;;;;;;;;;;;;;
;;;;;set1;;;;;;set2;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define union
  (lambda (set1 set2)
    (cond
     [(null? set1) set2]
     [(member? (car set1) set2) (union (cdr set1) set2)]
     [else (cons (car set1) (union (cdr set1) set2))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;intersectall;;;;;;;;;;;;
;;;;;;;lat;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define intersectall
  (lambda (lat)
    (cond
    [(null? lat) (quote ())]
    [else (interest (car lat) (intersectall (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;a-pair?;;;;;;;;;;;
;;;;;;;;;;lat;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define a-pair?
  (lambda (lat)
    (null? (cdr (cdr lat)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;first;;;;;;;;;;;;;
;;;;;;;;;lat;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define first
  (lambda (lat)
    (car lat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;second;;;;;;;;;;;;;;
;;;;;;;lat;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define second
  (lambda (lat)
    (car (cdr lat))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;build;;;;;;;;;;;;;;;
;;;;;;;;;;a;;;;b;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define build
  (lambda (a b)
    (cons a (cons b (quote ())))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;third;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;lat;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define third
  (lambda (lat)
    (car (cdr (cdr lat)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;firsts;;;;;;;;;;;;;;;;;;
;;;;;;;;;lat;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (firsts)
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [else (cons (car (car lat)) (firsts (cdr lat)))])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;fun?;;;;;;;;;;;;;;
;;;;;;;;;;;lat;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fun?
  (lambda (lat)
    (set? (firsts lat))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;revrel;;;;;;;;;;;;;;;
;;;;;;;;lat;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define revrel
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [else (cons (build (second (car lat)) (first (car lat))) (revrel (cdr lat)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;fullfun?;;;;;;;;;;;
;;;;;;;;fun;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define seconds
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [else (cons (car (cdr (car lat))) (seconds (cdr lat)))])))

(define fullfun?
  (lambda (lat)
    (and (set? (firsts lat)) (set? (seconds lat)))))

(define fullfun?
  (lambda (lat)
    (and (set? (firsts lat)) (set? (firsts (revrel lat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;rember-f;;;;;;;;;;;;
;;;;;;;;;;test? a l;;;;;;;;;;
;;;;;;error;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember-f
  (lambda (test? a l)
    (cond
     [(null? l) (quote a)]
     [(test? (car l) a) (cdr l)]
     [else (cons (car l) (rember-f test? a (car l)))])))


(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) (quote ())]
       [(test? (car l) a) (cdr l)]
       [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

(define insertL-f
  (lambda (new)
    (lambda (old lat)
      (cond
       [(null? lat) (quote ())]
       [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
       [else (cons (car lat) ((insertL-f new) old (cdr lat)))]))))

(define insertR-f
  (lambda (new)
    (lambda (old lat)
      (cond
       [(null? lat) (quote ())]
       [(eq? (car lat) old) (cons new (cons old (cdr lat)))]
       [else (cons (car lat) ((insertR-f new) old (cdr lat)))]))))


(define insert-g
  (lambda (pos)
    (lambda (new)
      (lambda (old lat)
        ((pos (
         

