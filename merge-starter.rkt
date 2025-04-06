;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname merge-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment bank/merge)

(@cwl ???) ;replace ??? with your cwl


(@problem 1)
#|
Design a function that consumes two lists of numbers. Each list
is already sorted in increasing order. The function should produce
the merged list sorted in increasing order.
Show the cross product of type comments table, the simplification,
and the correspondence between table cells and cond cases.

For example:

  (merge (list 2 3 5) (list 1 4 6)) --> (list 1 2 3 4 5 6) 

|#

;; Data Definitions:
;;
;; NOTE: IN 2-ONE-OF PROBLEMS ONLY, WE USE LON FOR ListOfNumber.
;;

(@htdd LON)
;; LON is one of:
;;  - empty
;;  - (cons Number LON)
;; interp. a list of numbers
(define LON1 empty)
(define LONA (list 2 3 5))
(define LONB (list 1 4 6))

;; Function:

(@htdf merge)
;;ListOfNumber ListOfNumber -> ListOfNumber
;;Merge two Shorted list and give new shorted List
(check-expect (merge empty empty) empty)
(check-expect (merge empty LONA)  LONA)
(check-expect (merge LONB empty)  LONB)
(check-expect (merge (list 3) (list 1))  (list 1 3))
(check-expect (merge (list 5 6) (list 8 10)) (list 5 6 8 10))
(check-expect (merge (list 3 9) (list 4 8))  (list 3 4 8 9))
(check-expect (merge (list 6) (list 4 5))  (list 4 5 6))

;(define (merge l1 l2) empty) ;stub


(define (merge l1 l2)
  (cond [(and (empty? l1)(empty? l2)) empty]
        [(empty? l1) l2]
        [(empty? l2) l1]
        [else
         (if(< (first l1)   (first l2))                   
            (cons   (first l1) (merge (rest l1) l2))      ;;ListOfNumber
            (cons   (first l2) (merge (rest l2) l1)))
         ]))