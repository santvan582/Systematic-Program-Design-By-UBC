;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname accounts-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment bank/accounts)
(@cwl ???)


;; =================
;; Data definitions:

(@htdd Accounts)
(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such
;;   that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD 
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42 
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40 
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))


(@problem 1)
;; Design an abstract function (including signature, purpose, and tests) 
;; to simplify the remove-debtors and remove-profs functions defined below.

; Boolean act act -> act
(define (backtracking-remover fn b act)
  (local [(define (fn-for-act act)
            (cond [(false? act) b]
                  [else
                   (if (fn act)
                       (join (fn-for-act (node-l act))
                             (fn-for-act (node-r act)))
                       (make-node (node-id act)
                                  (node-name act)
                                  (node-bal act)
                                  (fn-for-act (node-l act))
                                  (fn-for-act (node-r act))))]))]


    (fn-for-act act))) 

;; Now re-define the original remove-debtors and remove-profs functions 
;; to use your abstract function. Remember, the signature and tests should 
;; not change from the original functions.


(@htdf remove-debtors)
(@signature Accounts -> Accounts)
;; remove all accounts with a negative balance
(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false)) 
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              false)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors 
               (make-node 4 "Mrs. Doubtfire" -3
                          false 
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))

(@template-origin Accounts)


(define (remove-debtors act)
  (local [(define (bl-fn act) (negative? (node-bal act)))]
    (backtracking-remover bl-fn false act)))

(@htdf remove-profs)
(@signature Accounts -> Accounts)
;; Remove all professors' accounts.  
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false)) 
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false))
              false)
(check-expect (remove-profs
               (make-node 67 "Mrs. Dash" 3000
                          (make-node 9 "Prof. Booty" -60 false false)
                          false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))

(@template-origin Accounts)

(define (remove-profs act)
  (local [(define (bl-fn act) (has-prefix? "Prof." (node-name act)))]
    (backtracking-remover bl-fn false act)))
 


(@htdf has-prefix?)
(@signature String String -> Boolean)
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(@template-origin String)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))


(@htdf join)
(@signature Accounts Accounts -> Accounts)
;; Combine two Accounts's into one
;; CONSTRAINT: all ids in act1 are less than the ids in act2
(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4) 
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42) 
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))

(@template-origin Accounts)

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2) 
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))


(@problem 2)
;; Using your new abstract function, design a function that removes from a given
;; BST any account where the name of the account holder has an odd number of
;; characters.  Call it remove-odd-characters.
(check-expect (remove-odd-characters ACT4) (make-node 4 "Mrs. Doubtfire"  -3
                                                      false
                                                      false))
(check-expect (remove-odd-characters ACT42)  (make-node 42 "Mr. Mom" -79
                                                        (make-node 14 "Mr. Impossible" -9 false false)
                                                        (make-node 50 "Miss 604"  16 false false)))

(check-expect (remove-odd-characters ACT3) (remove-odd-characters ACT4))

;(define (remove-odd-characters act) false) ;stub

(define (remove-odd-characters act)
  (local [(define (bl-fn act) (odd? (node-id act)))]
    (backtracking-remover bl-fn false act)))




(@problem 3)
#|
Complete the design of the following abstract fold function for Account.
Note that we have already given you the actual function definition and the
template tag. You must complete the design with a signature, purpose,
function definition and the two following check-expects:

  - uses the fold function to produce a copy of ACT42 
  - uses the fold function to produce the sum of all the account balances
    in ACT10, which is (+ 22 -3 13 600 -79 40 -9 16 84)

Be VERY CAREFUL WRITING THE SIGNATURE. The autograder is very picky about
these problems. If you skip the type of one parameter then the types of all
following parameters will probably be marked wrong. On the other hand an
incorrect type typically does not affect anything after it. So work very
carefully to first setup the number of parameters the function has, and be
sure your final answer has types for that many parameters. HINT, there are 7.

This problem will be autograded.  NOTE that all of the following are required.
Violating one or more will cause your solution to receive 0 marks.

  - Files must not have any errors when the Check Syntax button is pressed.
    Press Check Syntax and Run often, and correct any errors early.

  - You MUST NOT edit the provided fold-accounts function definition or
    the template tag.

|#

(@template-origin Accounts)

(check-expect (fold-copy ACT42) ACT42)


(define (fold-copy act)
  (local [(define (cf id name bal act1 act2) (make-node id name bal act1 act2))]
         (fold-accounts cf false act)))


(check-expect (fold-sum ACT10) (+ 22 -3 13 600 -79 40 -9 16 84))

(define (fold-sum act)
  (local [(define (cf id name bal act1 act2) (+ bal act1 act2))]
         (fold-accounts cf 0 act)))

;;(Number string Integer X X -> X) X Accounts -> X  
(define (fold-accounts c1 b1 act)
  (local [(define (fold-act c1 b1 act)
            (cond [(false? act) b1]
                  [else
                   (c1 (node-id act)
                       (node-name act)
                       (node-bal act)
                       (fold-act c1 b1 (node-l act))
                       (fold-act c1 b1 (node-r act)))]))]
    (fold-act c1 b1 act)))


(@problem 4)
;; Use fold-accounts to design a function called charge-fee that decrements
;; the balance of every account in a given collection by the monthly fee of 3
;; CAD.
;;Account -> Account
;;for every acciunt in tree charge fee of 3
(check-expect (charge-fee ACT1) (make-node 1 "Mr. Rogers"  (- 3 22) false false))
(check-expect (charge-fee ACT4) (make-node 4 "Mrs. Doubtfire" (- 3 -3)
                        false
                        (make-node 7 "Mr. Natural" ( - 3 13) false false)))
(check-expect (charge-fee ACT3) (make-node 3 "Miss Marple"  ( - 3 600) (charge-fee ACT1) (charge-fee ACT4)))

;(define (charge-fee act) false) ; 

(define (charge-fee act)
  (local [(define (cf id name bal act1 act2) (make-node id name ( - 3 bal) act1 act2))]
           (fold-accounts cf false act)))

(@problem 5)
;; Suppose you needed to design a function to look up an account based on its
;; ID.
;; Would it be better to design the function using fold-act, or to design the
;; function using the fn-for-acts template?  Briefly justify your answer.

;;usinig fn-for-act due to less complex