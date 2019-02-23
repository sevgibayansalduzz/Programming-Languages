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
;insert-n takes a list, a number and an index as inputs, then inserts the number to the specified index in the list.

;first  cond.: if the given index is out of bound,return list1.
;second cond.: if the given index is zero,add the given number into to the output list.
;third cond.:  if the first element of list1 is atom ,add this element into the output list by the function CONS and 
;              recall the function insert-n to add the given number and the rest of list1.if the first element of list1 is not atom
;              do same thing with the function APPEND.
(defun insert-n(list1 number index)
    (cond
        ((or (> index (- (length list1) 1)) (< index 0) ) list1);first cound.
        ((= index 0) (cons number list1));second cond.
        (t  (if (atom (car list1))       ;third cond  
                (cons (car list1) (insert-n (cdr list1) number (- index 1)));The size of list1 is decreased each time the function is called,
																		                ;so the index is reduced parallel to the size of list1 
                (append (list(car list1)) (insert-n (cdr list1) number (- index 1)))
            )
        )
    )
)


;; MAIN FUNCTION
(defun main ()
  (with-open-file (stream #p"input_part3.csv")
    (loop :for line := (read-csv-line stream) :while line :collect
      (format t "~a~%"
      ;; CALL YOUR (MAIN) FUNCTION HERE
      	(insert-n (read-from-string (nth 0 line)) (read-from-string (nth 1 line)) (read-from-string (nth 2 line)))
      )
    )
  )
)

;; CALL MAIN FUNCTION
(main)
