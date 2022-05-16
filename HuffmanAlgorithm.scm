(define empty-tree '())

(define (make-tree root left right) (list root left right))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (make-leaf count value) (list count value empty-tree empty-tree))

(define (leaf? x)
  (and (not(empty-tree? x)) (null? (caddr x)) (null? (cadddr x))))

(define (left? t tree)
  (equal? t (left-tree tree)))

(define (right? t tree)
  (equal? t (right-tree tree)))

(define (filter p l)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (count-equals equality l x)
  (cond ((null? l) 0)
        ((equality x (car l)) (+ 1 (count-equals equality (cdr l) x)))
        (else (count-equals equality (cdr l) x))))

(define (map-reduce equality f l)
  (if (null? l) '()
      (cons (f (car l))
            (map-reduce equality f (filter (lambda (x) (not (equality x (car l)))) (cdr l))))))

(define (make-initial-list equality l)
  (if (null? l) '()
       (map-reduce equality (lambda (x) (list x (count-equals equality l x))) l)))

(define (start-list equality l)
  (map (lambda (x) (make-leaf (cadr x) (car x))) (make-initial-list equality l)))

(define (min-tree l)
  (define (helper l res)
    (cond ((null? l) res)
          ((null? res) (helper (cdr l) (car l)))
          ((> (car res) (caar l)) (helper (cdr l) (car l)))
          (else (helper (cdr l) res))))
  (helper l '()))

(define (remove-tree x l)
  (cond ((null? l) '())
        ((equal? (root-tree x) (root-tree (car l))) (cdr l))
        (else (cons (car l) (remove-tree x (cdr l))))))

(define (remove-min-tree l)
  (remove-tree (min-tree l) l))

(define (twice f)
  (lambda (x) (f (f x))))

(define (remove-two-min l)
  ((twice remove-min-tree) l))

(define (combine-two-trees t1 t2)
  (make-tree (+ (root-tree t1) (root-tree t2)) t1 t2))

(define (make-huffman-tree equality l)
  (let* ((first (min-tree l))
         (second (min-tree (remove-tree first l)))
         (remaining (remove-two-min l)))
    (cond ((null? l) '())
          ((equality (length l) 2) (combine-two-trees first second))
          ((equality (length remaining) 1) (combine-two-trees (combine-two-trees first second) (car remaining)))
          (else (make-huffman-tree equality (cons (combine-two-trees first second) remaining))))))

(define find-symbol cadr)
  
(define (codes equality l)
  (define (helper res tree)
    (cond ((empty-tree? tree) '())
          ((leaf? tree) (list (cons (find-symbol tree) (reverse res))))
          (else (append (helper (append (list 0) res) (left-tree tree))
                      (helper (append (list 1) res) (right-tree tree))))))
  (helper '() (make-huffman-tree equality (start-list equality l))))       

(define (encoding equality l)
  (define (helper list codes)
    (cond ((null? list) '())
          (else (append (cdr (assoc (car list) codes)) (helper (cdr list) codes)))))
  (helper l (codes equality l)))

(define (encode equality l)
  (list (make-huffman-tree equality (start-list equality l)) (encoding equality l)))

(define (decode tree encoding)
  (define (helper t enc)
    (cond ((leaf? t) (append (list (find-symbol t)) (helper tree enc)))
          ((null? enc) '())
          ((= (car enc) 0) (helper (left-tree t) (cdr enc)))
          (else (helper (right-tree t) (cdr enc)))))
  (helper tree encoding))

(define list1 '(a b r a c a d a b r a))

(define tr1 '(11 (6 (2 r () ()) (4 (2 (1 c () ()) (1 d () ())) (2 b () ()))) (5 a () ())))
(define encoding1 '(1 0 1 1 0 0 1 0 1 0 0 1 0 1 0 1 1 0 1 1 0 0 1))

(define codes1 '((r 0 0) (c 0 1 0 0) (d 0 1 0 1) (b 0 1 1) (a 1)))

(define list2 '(8 2 3 0 3))

(define tr2 '(5 (3 (1 0 () ()) (2 (1 8 () ()) (1 2 () ()))) (2 3 () ())))
(define encoding2 '(0 1 0 0 1 1 1 0 0 1))