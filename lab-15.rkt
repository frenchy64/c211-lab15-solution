;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Here is the data definition for a ByTwo:
;
;;; A ByTwo is one of:
;;; - empty
;;; - (make-two Number Number ByTwo)
(define-struct two (first second rest))
;Exercise 1: Design a function that computes the length of a ByTwo.

; by-two-length
; ByTwo -> Number
; Returns the length of a ByTwo.
(define (by-two-length t)
  (cond
    [(empty? t) 0]
    [else (+ 2 (by-two-length (two-rest t)))]))

(check-expect (by-two-length empty) 0)
(check-expect (by-two-length (make-two 1 2 empty)) 2)
(check-expect (by-two-length (make-two 1 2 (make-two 3 4 empty))) 4)

;Exercise 2: Design a function that computes the sum of all the numbers in a ByTwo.

; by-two-sum
; ByTwo -> Number
; Returns the sum of all the members of a ByTwo.
(define (by-two-sum t)
  (cond
    [(empty? t) 0]
    [else (+ (two-first t)
             (two-second t)
             (by-two-sum (two-rest t)))]))

(check-expect (by-two-sum empty) 0)
(check-expect (by-two-sum (make-two 1 2 empty)) 3)
(check-expect (by-two-sum (make-two 1 2 (make-two 3 4 empty))) 10)

;Exercise 3: Design a function that computes the product of all the numbers in a ByTwo.

; by-two-prod
; ByTwo -> Number
; Returns the product of all the members of a ByTwo.
(define (by-two-prod t)
  (cond
    [(empty? t) 1]
    [else (* (two-first t)
             (two-second t)
             (by-two-prod (two-rest t)))]))

(check-expect (by-two-prod empty) 1)
(check-expect (by-two-prod (make-two 1 2 empty)) 2)
(check-expect (by-two-prod (make-two 1 2 (make-two 3 4 empty))) 24)

;Exercise 4: Re-design all of the functions from Exercises 1, 2, and 3 to use an accumulator. Don’t forget the accumulator invariant.

; by-two-length-acc
; Number ByTwo -> Number
; Returns the length of a ByTwo, with an initial accumulator of 0.
; Accumulator is the length of everything we have processed so far.
(define (by-two-length-acc acc t)
  (cond
    [(empty? t) acc]
    [else (by-two-length-acc (+ 2 acc) (two-rest t))]))

(check-expect (by-two-length-acc 0 empty) 0)
(check-expect (by-two-length-acc 0 (make-two 1 2 empty)) 2)
(check-expect (by-two-length-acc 0 (make-two 1 2 (make-two 3 4 empty))) 4)

; by-two-sum-acc
; Number ByTwo -> Number
; Returns the sum of all the members of a ByTwo, with an initial accumulator of 0.
; Accumulator is the sum of everything we have processed so far.
(define (by-two-sum-acc acc t)
  (cond
    [(empty? t) acc]
    [else (by-two-sum-acc (+ (two-first t)
                             (two-second t)
                             acc)
                          (two-rest t))]))

(check-expect (by-two-sum-acc 0 empty) 0)
(check-expect (by-two-sum-acc 0 (make-two 1 2 empty)) 3)
(check-expect (by-two-sum-acc 0 (make-two 1 2 (make-two 3 4 empty))) 10)

; by-two-prod-acc
; ByTwo -> Number
; Returns the product of all the members of a ByTwo, with the initial product of 1.
; Accumulator is the product of everything we have processed so far (or 1 if we haven't
; processed anything.
(define (by-two-prod-acc acc t)
  (cond
    [(empty? t) acc]
    [else (by-two-prod-acc (* (two-first t)
                              (two-second t)
                              acc)
                           (two-rest t))]))

(check-expect (by-two-prod-acc 1 empty) 1)
(check-expect (by-two-prod-acc 1 (make-two 1 2 empty)) 2)
(check-expect (by-two-prod-acc 1 (make-two 1 2 (make-two 3 4 empty))) 24)

; Exercise 5: Abstract your functions from Exercises 2 & 3 into a single function. Then redefine your original functions using local.

; by-two-aggregate
; Base [Number Number Number -> Number] ByTwo -> Number
; Returns the sum of all the members of a ByTwo.
(define (by-two-aggregate base f t)
  (cond
    [(empty? t) base]
    [else (f (two-first t)
             (two-second t)
             (by-two-aggregate base f (two-rest t)))]))

;; sum tests
(check-expect (by-two-aggregate 0 + empty) 0)
(check-expect (by-two-aggregate 0 + (make-two 1 2 empty)) 3)
(check-expect (by-two-aggregate 0 + (make-two 1 2 (make-two 3 4 empty))) 10)

;; prod tests
(check-expect (by-two-aggregate 1 * empty) 1)
(check-expect (by-two-aggregate 1 * (make-two 1 2 empty)) 2)
(check-expect (by-two-aggregate 1 * (make-two 1 2 (make-two 3 4 empty))) 24)

; by-two-sum-local
; ByTwo -> Number
; Returns the sum of all the members of a ByTwo.
(define (by-two-sum-local t)
  (cond
    [(empty? t) 0]
    [else (local [(define l (two-first t))
                  (define r (two-second t))
                  (define rec (by-two-sum-local (two-rest t)))]
            (+ l r rec))]))

(check-expect (by-two-sum-local empty) 0)
(check-expect (by-two-sum-local (make-two 1 2 empty)) 3)
(check-expect (by-two-sum-local (make-two 1 2 (make-two 3 4 empty))) 10)

; by-two-prod-local
; ByTwo -> Number
; Returns the product of all the members of a ByTwo.
(define (by-two-prod-local t)
  (cond
    [(empty? t) 1]
    [else (local [(define l (two-first t))
                  (define r (two-second t))
                  (define rec (by-two-prod-local (two-rest t)))]
            (* l r rec))]))

(check-expect (by-two-prod-local empty) 1)
(check-expect (by-two-prod-local (make-two 1 2 empty)) 2)
(check-expect (by-two-prod-local (make-two 1 2 (make-two 3 4 empty))) 24)

; Exercise 6: Re-do Exercise 5 using lambda.

; by-two-sum-local
; ByTwo -> Number
; Returns the sum of all the members of a ByTwo.
(define (by-two-sum-lambda t)
  (cond
    [(empty? t) 0]
    [else ((lambda (l r rec) (+ l r rec))
           (two-first t)
           (two-second t)
           (by-two-sum-lambda (two-rest t)))]))

(check-expect (by-two-sum-lambda empty) 0)
(check-expect (by-two-sum-lambda (make-two 1 2 empty)) 3)
(check-expect (by-two-sum-lambda (make-two 1 2 (make-two 3 4 empty))) 10)

; by-two-prod-local
; ByTwo -> Number
; Returns the product of all the members of a ByTwo.
(define (by-two-prod-lambda t)
  (cond
    [(empty? t) 1]
    [else ((lambda (l r rec) (* l r rec))
           (two-first t)
           (two-second t)
           (by-two-prod-lambda (two-rest t)))]))

(check-expect (by-two-prod-lambda empty) 1)
(check-expect (by-two-prod-lambda (make-two 1 2 empty)) 2)
(check-expect (by-two-prod-lambda (make-two 1 2 (make-two 3 4 empty))) 24)

; Exercise 7: Design a data definition ByTwoS that’s like ByTwo, but has strings as elements. Then design a function that computes the length of these.

;; A ByTwoS is one of:
;; - empty
;; - (make-two String String ByTwo)

; by-two-s-length
; ByTwoS -> Number
; Returns the length of a ByTwoS.
(define (by-two-s-length t)
  (cond
    [(empty? t) 0]
    [else (+ 2 (by-two-s-length (two-rest t)))]))

(check-expect (by-two-s-length empty) 0)
(check-expect (by-two-s-length (make-two "1" "2" empty)) 2)
(check-expect (by-two-s-length (make-two "1" "2" (make-two "3" "4" empty))) 4)


; Exercise 8: Design a function that consumes a ByTwo and a function from Numbers to Numbers, and applies the function to every element in the ByTwo, producing a new ByTwo. Write two tests for this function that use lambda.

; map-by-two
; ByTwo [Number -> Number] -> ByTwoS
; Returns a new ByTwoS that is the original but with f applied to each element of it.
(define (map-by-two t f)
  (cond
    [(empty? t) t]
    [else (make-two
           (f (two-first t))
           (f (two-second t))
           (map-by-two (two-rest t) f))]))

(check-expect (map-by-two empty add1) empty)
(check-expect (map-by-two (make-two 1 2 empty) add1)  (make-two 2 3 empty))
(check-expect (map-by-two (make-two 1 2 (make-two 3 4 empty)) add1)
              (make-two 2 3 (make-two 4 5 empty)))

; Exercise 9: Abstract the two data definitions. Then abstract the two length functions.

;; A [ByTwoA X] is one of:
;; - empty
;; - (make-two X X ByTwoA)

; by-two-length-x
; [ByTwoA X] -> Number
; Returns the length of a [ByTwoA X].
(define (by-two-length-x t)
  (cond
    [(empty? t) 0]
    [else (+ 2 (by-two-length-x (two-rest t)))]))

(check-expect (by-two-length-x empty) 0)
(check-expect (by-two-length-x (make-two 1 2 empty)) 2)
(check-expect (by-two-length-x (make-two "1" "2" empty)) 2)
(check-expect (by-two-length-x (make-two 1 2 (make-two 3 4 empty))) 4)
(check-expect (by-two-length-x (make-two "1" "2" (make-two "3" "4" empty))) 4)

; Exercise 10: Abstract the function you wrote in Exercise 8 so that it works over your abstracted data definition.

; map-by-two
; [ByTwo X] [X -> Y] -> [ByTwo Y]
; Returns a new [ByTwo X] that is the original but with f applied to each element of it.
(define (map-by-two-x t f)
  (cond
    [(empty? t) t]
    [else (make-two
           (f (two-first t))
           (f (two-second t))
           (map-by-two-x (two-rest t) f))]))

(check-expect (map-by-two-x empty add1) empty)
(check-expect (map-by-two-x (make-two 1 2 empty) add1)  (make-two 2 3 empty))
(check-expect (map-by-two-x (make-two "1" "22" empty) string-length)  (make-two 1 2 empty))
(check-expect (map-by-two-x (make-two 1 2 (make-two 3 4 empty)) add1)
              (make-two 2 3 (make-two 4 5 empty)))
(check-expect (map-by-two-x (make-two "1" "22" (make-two "333" "4444" empty)) string-length)
              (make-two 1 2 (make-two 3 4 empty)))

;Here is the data definition for a StringTree:
;
;; A StringTree is one of:
;; - (make-leaf String)
;; - (make-node String [List-of StringTree])

(define-struct leaf (label))
(define-struct node (label kids))

;Exercise 11 Write down the data definition for a [List-of StringTree]. Give two examples of [List-of StringTree] that are not empty.

;; A [List-of StringTree] is one of:
;; - empty
;; - (cons StringTree [List-of StringTree])

; tree-just-leaf : StringTree
(define tree-just-leaf (list (make-leaf "l1")))

; tree-two-leaves : StringTree
(define tree-two-leaves (list (make-node "l2" (list (make-leaf "l1") (make-leaf "l2")))
                              (make-leaf "l1")))

;Exercise 12 Design a function that takes a StringTree and returns a String consisting of all the labels of the leaves (ignoring the nodes)
; concatenated together.

; concat-string-tree
; String-Tree -> String
; Returns the concatenation of all the node labels, left to right.
(define (concat-string-tree t)
  (cond
    [(leaf? t) (leaf-label t)]
    [else (apply string-append (map concat-string-tree (node-kids t)))]))

(check-expect (concat-string-tree (make-leaf "l1")) "l1")
(check-expect (concat-string-tree (make-node "l2" (list (make-leaf "l1") (make-leaf "l2")))) "l1l2")

;Exercise 13 Design a function that takes a StringTree and returns the length of the longest label on any of the nodes or leaves.

; longest-label
; String-Tree -> Number
; Returns the length of the longest label on nodes in the tree.
(define (longest-label t)
  (cond
    [(leaf? t) (string-length (leaf-label t))]
    [else (apply max 
                 (string-length (node-label t))
                 (map longest-label (node-kids t)))]))

(check-expect (longest-label (make-leaf "l1")) 2)
(check-expect (longest-label (make-leaf "")) 0)
(check-expect (longest-label (make-node "l2" (list (make-leaf "1") (make-leaf "l23")))) 3)

;Exercise 14 Design a function that takes a StringTree and a String and determines if the given String is used as a label on any of the nodes or leaves.

; has-label?
; String-Tree String -> Number
; Returns true if the tree has a matching label l.
(define (has-label? t l)
  (cond
    [(leaf? t) (equal? (leaf-label t) l)]
    [else (or
           (equal? (node-label t) l)
           (ormap (lambda (k) (has-label? k l)) (node-kids t)))]))

(check-expect (has-label? (make-leaf "l1") "l1") #true)
(check-expect (has-label? (make-leaf "l1") "l2") #false)
(check-expect (has-label? (make-node "l2" (list (make-leaf "1") (make-leaf "l23"))) "1") #true)
(check-expect (has-label? (make-node "l2" (list (make-leaf "1") (make-leaf "l23"))) "l2") #true)
(check-expect (has-label? (make-node "l2" (list (make-leaf "1") (make-leaf "l23"))) "l23") #true)
(check-expect (has-label? (make-node "l2" (list (make-leaf "1") (make-leaf "l23"))) "l234") #false)

;Exercise 15 Design a function that takes a StringTree and returns a StringTree that shortens each label to just its first letter.
; You may find the function string-ith useful.

; shorten-labels
; String-Tree -> Number
; Returns a new tree with labels shortened to one letter.
(define (shorten-labels t)
  (cond
    [(leaf? t) (make-leaf (string-ith (leaf-label t) 0))]
    [else (make-node (string-ith (node-label t) 0)
                     (map shorten-labels (node-kids t)))]))

(check-expect (shorten-labels (make-leaf "l1")) (make-leaf "l"))
(check-expect (shorten-labels (make-node "l223" (list (make-leaf "1112332") (make-leaf "l23232"))))
              (make-node "l" (list (make-leaf "1") (make-leaf "l"))))

;Exercise 16 Design a function that takes a StringTree and returns a StringTree where each label has been modified to include the root label as well.
; Use the following check-expect to test your function.

; prefix-relabel
; String-Tree -> Number
; Returns a new tree where every label includes the root label.
(define (prefix-relabel t)
  (cond
    [(leaf? t) (prefix-relabel-to t (leaf-label t))]
    [else (prefix-relabel-to t (node-label t))]))

; prefix-relabel-to
; String-Tree -> Number
; Returns a new tree where every label includes the root label.
(define (prefix-relabel-to t s)
  (cond
    [(leaf? t) (make-leaf (string-append s ":" (leaf-label t) ))]
    [else (make-node (string-append s ":" (node-label t))
                     (map (lambda (k) (prefix-relabel-to k s))
                          (node-kids t)))]))

(check-expect (prefix-relabel
               (make-node "root"
                          (list
                           (make-leaf "leaf1")
                           (make-node "node1"
                                      (list
                                       (make-leaf "leaf2")
                                       (make-leaf "leaf3"))))))
              (make-node "root:root"
                         (list
                          (make-leaf "root:leaf1")
                          (make-node "root:node1"
                                     (list
                                      (make-leaf "root:leaf2")
                                      (make-leaf "root:leaf3"))))))

; Exercise 17 Design a function that takes a number (a positive integer) and a precision (another positive integer) and returns the 
; square root of the number with that precision. So if the number is 6 and precision is 3 the result should be the square root of 6
; within 0.001 (thus, with a precision of three decimals).

; sqrt-with-precision
; Number Number -> Number
; Returns the square root of n with precision p.
(define (sqrt-with-precision n p)
  )

(check-expect (<= (- (sqrt 5) 0.001)
                  (sqrt-with-precision 5 3)
                  (+ (sqrt 5) 0.001))
              #true)