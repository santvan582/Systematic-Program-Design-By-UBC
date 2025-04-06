;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hp-family-tree-starter) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require spd/tags)

(@assignment bank/mutual-ref-p3)
(@cwl ???)

;; In this problem set you will represent information about descendant family 
;; trees from Harry Potter and design functions that operate on those trees.
;;
;; To make your task much easier we suggest two things:
;;   - you only need a DESCENDANT family tree
;;   - read through this entire problem set carefully to see what information 
;;     the functions below are going to need. Design your data definitions to
;;     only represent that information.
;;   - you can find all the information you need by looking at the individual 
;;     character pages like the one we point you to for Arthur Weasley.


(@problem 1)
;; Design a data definition that represents a family tree from the Harry Potter
;; wiki, which contains all necessary information for the other problems.  You 
;; will use this data definition throughout the rest of the homework.

(define-struct wiz(name wind patronus children))
;;Member is (make-mem String String ListOfMember)
;;Inter. memeber is represent name of persion with thier childeren
;;       name is name of person
;;       wind is material of wind
;;       patronus is member's patronus
;;       childern is list of child member have

#;
(define (fn-for-wiz w)
  (...  (wiz-name w)
        (wiz-wind w)
        (wiz-patronus w)
        (fn-for-low (wiz-children w)))) 

#;
(define (fn-for-low low)
  (cond [(empty? low)(...)]
        [else
         (.... (fn-for-wiz (first low))
               (fn-for-low (rest low)))]))

;;ListOfMember is one of:
;; - Empty
;; - (cons member ListOfMember)
;;Interp. List of child member 



(@problem 2)
;; Define a constant named ARTHUR that represents the descendant family tree for
;; Arthur Weasley. You can find all the infomation you need by starting 
;; at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
;;
;; You must include all of Arthur's children and these grandchildren: Lily, 
;; Victoire, Albus, James.
;;
;;
;; Note that on the Potter wiki you will find a lot of information. But for some
;; people some of the information may be missing. Enter that information with a 
;; special value of "" (the empty string) meaning it is not present. Don't
;; forget this special value when writing your interp.
(define ARTHUR
  (make-wiz "Arthur" "" "Weasel"
            (list (make-wiz "Bill" "" ""
                            (list (make-wiz "Victoire"  "" "" empty)
                                  (make-wiz "Dominique" "" "" empty)  ;optional
                                  (make-wiz "Louis"     "" "" empty)));optional
                  (make-wiz "Charlie" "ash" "" empty)
                  (make-wiz "Percy" "" ""
                            (list (make-wiz "Molly" "" "" empty)          
                                  (make-wiz "Lucy"  "" "" empty)))
                  (make-wiz "Fred"    ""    "" empty)
                  (make-wiz "George"  ""    ""
                            (list (make-wiz "Fred" "" "" empty)     
                                  (make-wiz "Roxanne"  "" "" empty)))
                  (make-wiz "Ron"     "ash" "Jack Russell Terrier"
                            (list (make-wiz "Rose" "" "" empty)
                                  (make-wiz "Hugo" "" "" empty)))
                  (make-wiz "Ginny"   ""    "horse" 
                            (list (make-wiz "James" "" "" empty)
                                  (make-wiz "Albus" "" "" empty)
                                  (make-wiz "Lily"  "" "" empty))))))

(@problem 3)
;;WIZ -> ListOfString
;;ListOfWizard -> ListOfString
;; Design a function that produces a pair list (i.e. list of two-element lists)
;; of every person in the tree and his or her patronus. For example, assuming 
;; that HARRY is a tree representing Harry Potter and that he has no children
;; (even though we know he does) the result would be:
;;
;; (list (list "Harry" "Stag")).
;;
;; You must use ARTHUR as one of your examples.
(check-expect (wiz-patronus-low   empty) empty)
(check-expect (wiz-patronus--pair (make-wiz "Charlie" "ash" "" empty)) (list (list "Charlie" "")))
(check-expect (wiz-patronus--pair (make-wiz "Ginny"   ""    "horse" 
                                  (list (make-wiz "James" "" "" empty)
                                        (make-wiz "Albus" "" "" empty)
                                        (make-wiz "Lily"  "" "" empty)))) (list (list "Ginny" "horse")
                                                                                (list "James" "")
                                                                                (list "Albus" "")
                                                                                (list "Lily"  "")))



;(define (wiz-patronus--pair w) empty) ;stub
;(define (wiz-patronus-low low) empty) ;stub


(define (wiz-patronus--pair w)
  (cons (list (wiz-name w)                          ;;ListOfString
        (wiz-patronus w))            
        (wiz-patronus-low (wiz-children w)))) ;;ListOfString 


(define (wiz-patronus-low low)
  (cond [(empty? low) empty]
        [else
         (append (wiz-patronus--pair (first low)) ;;ListOfSrtring
                 (wiz-patronus-low   (rest low)))]))

(@problem 4)
;; Design a function that produces the names of every person in a given tree 
;; whose wands are made of a given material. 
;;
;; You must use ARTHUR as one of your examples.


;;Member String -> ListOfmember
;;ListOfMember String -> ListOfMember
;;Produce list of Member who's Wind made of given material
(check-expect (wind--low empty "ash") empty)
(check-expect (wind--list (make-wiz "Ginny"   ""    "horse" 
                            (list (make-wiz "James" "" "" empty)
                                  (make-wiz "Albus" "" "" empty)
                                  (make-wiz "Lily"  "" "" empty))) "ash") empty)
(check-expect (wind--list (make-wiz "Ron" "ash" "Jack Russell Terrier"
                            (list (make-wiz "Rose" "" "" empty)
                                  (make-wiz "Hugo" "" "" empty))) "ash") (list "Ron"))
(check-expect (wind--list ARTHUR "ash") (list "Charlie" "Ron"))
(check-expect (wind--list (make-wiz "a" "b" "c" empty) "x") empty)
(check-expect (wind--list (make-wiz "a" "b" "c" empty) "b")
              (list "a"))
(check-expect (wind--low (list (make-wiz "a" "b" "c" empty)
                                           (make-wiz "d" "e" "f" empty))
                                     "e")
              (list "d"))
(check-expect (wind--list ARTHUR "ash") (list "Charlie" "Ron"))

;(define (wind--list w s) empty)
;(define (wind--low  w s) empty)




(define (wind--list w s )
    (if(string=? s (wiz-wind w))
     (list (wiz-name w))
     (wind--low (wiz-children w) s))) 

(define (wind--low low s)
  (cond [(empty? low) empty ]
        [else
         (append (wind--list (first low) s)     ;;ListOfString
                 (wind--low (rest low)  s))])) ;;ListOfString

