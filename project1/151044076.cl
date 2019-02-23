;keywords are added to map
(setq keywords (make-hash-table :test 'equalp)) 
(setf (gethash "and" keywords) "keyword")
(setf (gethash "or" keywords) "keyword")
(setf (gethash "not" keywords) "keyword")
(setf (gethash "equal" keywords) "keyword")
(setf (gethash "append" keywords) "keyword")
(setf (gethash "concat" keywords) "keyword")
(setf (gethash "set" keywords) "keyword")
(setf (gethash "deffun" keywords) "keyword")
(setf (gethash "for" keywords) "keyword")
(setf (gethash "while" keywords) "keyword")
(setf (gethash "exit" keywords) "keyword")
(setf (gethash "if" keywords) "keyword")
;operators are added into the map
(setq operators (make-hash-table :test 'equalp)) 
(setf (gethash "(" operators) "operator")
(setf (gethash ")"operators) "operator")
(setf (gethash "**" operators) "operator")
(setf (gethash "+" operators) "operator")
(setf (gethash "-" operators) "operator")
(setf (gethash "*" operators) "operator")
(setf (gethash "/" operators) "operator")

;operators are added into the map
(setq booleans (make-hash-table :test 'equalp)) 
(setf (gethash "false" booleans) "BinaryValue")
(setf (gethash "true" booleans) "BinaryValue")

;Numbers are added into the map
(setq numbers (make-hash-table :test 'equalp)) 
(setf (gethash "0" numbers) "number")
(setf (gethash "1" numbers) "number")
(setf (gethash "2" numbers) "number")
(setf (gethash "3" numbers) "number")
(setf (gethash "4" numbers) "number")
(setf (gethash "5" numbers) "number")
(setf (gethash "6" numbers) "number")
(setf (gethash "7" numbers) "number")
(setf (gethash "8" numbers) "number")
(setf (gethash "9" numbers) "number")



;this function is called by the user and it is taked a file name from the user.
;First sends the given file name to the fileTolist function,This functions read file,and copies the content of the file onto the list without newlines.
;After calling fileTolist function,Return list is sending to the lex function. 
(defun lexer (filename)
	(setq a (fileToList filename))
	(return-from lexer (lex a 0 a nil (cadr a) (caddr a)))
)

;This recursive funciton returns the tokens list.
;"liste" holds given list,it is used for traverse the elements.Its size is reduced when the recursive calls
;"liste2" holds the first given list.(This list is used by the DFA function.)
;count holds the index of the current element
;1. cond: if given 'liste' is null,returns nil
;2. cond: if given 'liste' type is list ,it makes recursive call.
;3. cond: if given 'liste' is the space character, retunrs nil.
;4. cond: if given 'liste' is equal to '(' or ')',it adds this operator into to return (token) list.
;5. cond: if given 'liste' is equal to '*' and next character is alsa equal to '*' and next of the next character 
;           is the space chracter,it adds '**' oprator into the token list.
;6. cond: this cond for then second star of the oparator "**".if the func traverse the second star ,it will return nil.
;7. cond: if 'liste' is operator and the next character is the space character,this operator will be added into the token list.
;8. cond: if previous is not an operator or an alphabet character , the function will call the Dfa function.(The liste2 and count will be sent to the DFA)
(defun lex (liste count liste2 &optional prev next next2 )
	(cond
        ((null liste) nil)
        ((listp liste) (append (lex (car liste ) count liste2 prev next next2) (lex (cdr liste) (+ count 1) liste2 (car liste) (caddr liste) (cadddr liste)) ) )
        ((equal liste #\Space) nil)
        ((or (equal ")" (string liste)) (equal "(" (string liste))) (list (list(gethash (string liste) operators)(string liste)))) 
        ((and (equal #\* liste) (equal next #\*) (or (equal next2 #\Space) (null next2))) (list (list(gethash "**" operators) "**" )))
        ((and (equal #\* liste) (equal prev #\*) (or (equal next #\Space) (null next))) nil)
        ((and (gethash (string liste) operators) (equal next #\Space)) (list (list(gethash (string liste) operators)(string liste))))
        ((or (equal prev #\Space) (equal (string prev) "(" ) (equal (string prev) ")" )) (list (DFA liste2 (- count 1))))
    )
)

;Dfa function is used for the identifier,integer listerals and reserved words.
;First it uses the cdr function count times,for goes back to where the current element. 
;1.cond: if 'liste' is an atom and a number,"integer" will be added to the token list.
;2.cond: if 'liste' is an atom and in the alphabet,"identifier" will be added to the token list.
;3.cond: if 'liste' is an atom ,"UnknownToken" will be added to the token list.
;4.cond: if 'liste' is a list and its firs element is a number,it will call DFAint for the integer vaalues.
;5.cond: otherwise calls the DAFident list for the identifier,keywords..
(defun DFA (liste count)
    (loop for a from 0 to count
         do (setq liste (cdr liste))
    )
    (cond
        ((null liste) nil)
        ((and (atom liste) (gethash (string liste ) numbers)) (list "integer" (string liste) ))
        ((and (atom liste) (alpha-char-p liste)) (list "identifier" (string liste)) )
        ((atom liste)(list "UnknownToken" (string liste)))
        ((gethash (string (car liste )) numbers) (DFAint liste))
        (t (DFAident liste))
    )
)
;It determines integer values accorsing to given list
;ıt uses grammar rules,if rules are matches returns "integer" ,otherwise returns "UnknownToken".
(defun DFAint (liste)
    (setq word "")
    (setq token 1)
    (dolist (n liste)
       (cond ((char-equal n #\Space) (return 1) )
   			((equal (string n) "(") (return 1) )
             ((equal (string n) ")") (return 1) )
       ) 
       (cond ((not (gethash (string n) numbers)) (setq token 0)))
       (setq word (concatenate 'string word (string n)))
    )
    (if (equal 1 token) (list "integer" word ) (list "UnknownToken" word ) )
)
;It determines string according to given list
;ıt uses grammar rules,if rules are matches returns "identifier" or "keywordS" or "BinaryValue" ,otherwise returns "UnknownToken".
(defun DFAident (liste)
    (setq word "")
    (setq token 1)
    (dolist (n liste)
       (cond ((char-equal n #\Space) (return 1) )
       		((equal (string n) "(") (return 1) )
             ((equal (string n) ")") (return 1) )
       ) 
       (cond ((not (alpha-char-p n)) (setq token 0)))
       (setq word (concatenate 'string word (string n)))
    )
    (if (equal 1 token) 
        (cond 
            ((gethash word keywords) (list (gethash word keywords) word))
            ((gethash word booleans) (list (gethash word booleans) word))
            (t (list "identifier" word))
        ) 
        (list "UnknownToken" word ) 
    )
)

;This function reads file and return it contents as list
(defun fileToList(filename)
	(setq x '())
	(with-open-file (stream filename)
		(do ((char (read-char stream nil) (read-char stream nil)))
	        ((null char) x)
	      	
	      		(cond
	      			((null char) nil)
	      			((char-equal char #\Newline) (setq x (append x (list #\Space))))
	      			(t (setq x (append x (list char))))
	      		)
	    )
	)	
)

