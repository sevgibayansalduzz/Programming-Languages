(load "csv-parser.lisp")
(in-package :csv-parser)

;; (read-from-string STRING)
;; This function converts the input STRING to a lisp object.
;; In this code, I use this function to convert lists (in string format) from csv file to real lists.

;; (nth INDEX LIST)
;; This function allows us to access value at INDEX of LIST.
;; Example: (nth 0 '(a b c)) => a

;; !!! VERY VERY VERY IMPORTANT NOTE !!!
;; FOR EACH ARGUMENT IN CSV FILE
;; USE THE CODE (read-from-string (nth ARGUMENT-INDEX line))
;; Example: (mypart1-funct (read-from-string (nth 0 line)) (read-from-string (nth 1 line)))

;; DEFINE YOUR FUNCTION(S) HERE
 ;Merge-list function takes two lists as inputs and merges the lists.

;1.cond.: if list1 or list2 is empty return nil.
;2.cond.: if list1 and list2 includes one or more element :
;     2.1 cond.:if the first element of the list1 or list2 is atom, use the function CONS to add the first element of these two lists
;               into the output list.After adding elements,Recall function merge-lists the rest of the these lists.
;     2.2 cond.:Otherwise use the function Append.
;3.cond.: if list1 includes one or more element and list2 is empty,add the all element of list1 into the output list using function add-rest.
;4.cond.: if list1 is empty and list2 has element(s),do the same thing for list2.
(defun merge-lists (list1 list2)
    (cond
        ( (and (null list1) (null  list2) ) nil)  ;1.cond.
        ((and (> (length list1) 0) (> (length list2) 0))  ;2.cond.
          (cond 
          
                ((or (atom (car list1)) (atom (car list2)) );2.1 cond.
                     (cons (car list1) (cons(car list2) (merge-lists (cdr list1) (cdr list2)))))
                 (t (list (car list1) (car list2))
                    (merge-lists (cdr list1) (cdr list2)))
           )
        )
        ( (> (length list1) 0)
            (list list1)
        )
        ( (> (length list2) 0)
            (list list2)
        )
 
    )
)


;; MAIN FUNCTION
(defun main ()
  (with-open-file (stream #p"input_part2.csv")
    (loop :for line := (read-csv-line stream) :while line :collect
      (format t "~a~%"
      ;; CALL YOUR (MAIN) FUNCTION HERE
      	(merge-lists (read-from-string (nth 0 line)) (read-from-string (nth 1 line)))
      )
    )
  )
)

;; CALL MAIN FUNCTION
(main)
