;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fold-dir-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@assignment bank/abstraction-p8)
(@cwl ???)


;; In this problem you will need to remember the following DDs 
;; for an image organizer.


;; =================
;; Data definitions:

(@htdd Dir ListOfDir ListOfImage)
(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. a directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.


;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.


;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))



;; =================
;; Functions:


(@problem 1)
;; Design an abstract fold function for Dir called fold-dir.


;;(String X Y -> Z) ((Z X) -> X) ((Image Y) -> Y),X,Y,dir -> Z

(define (fn-for-dir fn1 fn2 fn3 2b 3b d)
  (local  [(define (fn-for-sub-dir lod)                     ;X  
             (cond [(empty? lod) 2b]                              
                   [else (fn2 (fn-for-dir (first lod))                     
                              (fn-for-sub-dir (rest lod)))])) 
           (define (fn-for-images loi)                         ;Y
             (cond [(empty? loi) 3b]
                   [else (fn3 (first loi)                     ;; Image  
                              (fn-for-images (rest loi)))]))  
           (define  (fn-for-dir d)                            
             (fn1 (dir-name d)                         ;String
                  (fn-for-sub-dir (dir-sub-dirs d) )    ;X     
                  (fn-for-images  (dir-images   d))))] ;Y                      
                              
    (fn-for-dir d)))



(@problem 2)
;; Design a function that consumes a Dir and produces the number of 
;; images in the directory and its sub-directories. 
;; Use the fold-dir abstract function.


;; Dir -> Number
;; Give total number of image from given directory(Count image from sub dir also)
(check-expect (no-image (make-dir "A" empty empty)) 0);
(check-expect (no-image D4) 2)
(check-expect (no-image D5) 1)
(check-expect (no-image D6) 3)

;(define (no-image dir) 0) ;stub

(define  (no-image dir)
  (local [(define (fn1 s lsd loi) (+ lsd loi))
          (define (fn2 lsd lod)   (+ lsd lod))
          (define (fn3 i1 loi)    (+ 1 loi))]
    (fn-for-dir fn1 fn2 fn3 0 0 dir)))
       


(@problem 3)
;; Design a function that consumes a Dir and a String. The function looks in
;; dir and all its sub-directories for a directory with the given name. If it
;; finds such a directory it should produce true, if not it should produce  
;; false. Use the fold-dir abstract function.

;;Dir String -> Boolean
;;Produce true if given Dir match string otherwise false
(check-expect (find-match-dir D4 "") false) 
(check-expect (find-match-dir D6 "D4") true)
(check-expect (find-match-dir D6 "D5") true)
(check-expect (find-match-dir D6 "D6") true)
(check-expect (find-match-dir D6 "D100") false)

;(define (find-match-dir D4 st) false)

(define (find-match-dir d st)
   (local [(define (fn1 s lsd loi) (or (string=? s st) lsd loi))
           (define (fn2 lsd lod)   (or lsd lod))
           (define (fn3 i1 loi)     false)]
    (fn-for-dir fn1 fn2 fn3 false false d)))  



(@problem 4)
;; Is fold-dir really the best way to code the function from part C? Why or 
;; why not?
;; Depend on function bacuse for some fnction it increase complexting due to lots of type need to pass  

