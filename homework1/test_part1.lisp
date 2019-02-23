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
;List-leveller function takes a (nested) list as an input and converts the input (nested) list 
;to a list which does not contain any list(s). 

;first cond.:if list is empty return nil
;second cond.:if the first element of nestedlist is QUOTE (') ,function does not add this symbol into the output list.
;            Then function calls itself.
;third cond.:if the first element of the nestedlist is Atom,use function CONS (this function adds a list and an atom) 
;            to add first element of the nestedlist and return of the list-leveller function for rest of nestedlist.
;fourth cond.:if the first element of the nestedlist is structure ,use function APPEND to add first element and rest of this structure and return 
;            of the list-leveller function for rest of nestedlist.(Function append merges two lists) 

(defun list-leveller(nestedlist)
    (cond
        ((null nestedlist) nil);first cond.
        ((and (atom (car nestedlist)) (equal  (car nestedlist) 'QUOTE )) (list-leveller(cdr nestedList))  );second cond.
        ((atom (car nestedlist)) (cons (car nestedlist) (list-leveller(cdr nestedlist)) ) );third cond.
        (t (append  (list (caar nestedlist)) (list-leveller (cdar nestedlist)) (list-leveller (cdr nestedlist)) ));fourth cond.
    )
)


;; MAIN FUNCTION
(defun main ()
  (with-open-file (stream #p"input_part1.csv")
    (loop :for line := (read-csv-line stream) :while line :collect
      (format t "~a~%"
      ;; CALL YOUR (MAIN) FUNCTION HERE
      	(list-leveller (read-from-string (nth 0 line)))
      )
    )
  )
)

;; CALL MAIN FUNCTION
(main)
