(setq grammar (make-hash-table :test 'equalp)) 
(setf (gethash "INPUT" grammar) "START")
(setf (gethash "EXPI" grammar) "INPUT")
(setf (gethash "EXPLISTI" grammar) "INPUT")
;EXPI
(setf (gethash "(setIDEXPI)" grammar) "EXPI")
(setf (gethash "(+EXPIEXPI)" grammar) "EXPI")
(setf (gethash "(-EXPIEXPI)" grammar) "EXPI")
(setf (gethash "(*EXPIEXPI)" grammar) "EXPI")
(setf (gethash "(/EXPIEXPI)" grammar) "EXPI")
(setf (gethash "ID" grammar) "EXPI")
(setf (gethash "(IDEXPLISTI)" grammar) "EXPI")
(setf (gethash "VALUES" grammar) "EXPI")
(setf (gethash "(deffunIDIDLISTEXPLISTI)" grammar) "EXPI")
(setf (gethash "(defvarIDEXPI)" grammar) "EXPI")
(setf (gethash "(ifEXPBEXPLISTI)" grammar) "EXPI")
(setf (gethash "(ifEXPBEXPLISTIEXPLISTI)" grammar) "EXPI")
(setf (gethash "(while(EXPB)EXPLISTI)" grammar) "EXPI")
(setf (gethash "(for(IDEXPIEXPI)EXPLISTI)" grammar) "EXPI")
;EXPB
(setf (gethash "(andEXPBEXPB)" grammar) "EXPB")
(setf (gethash "(orEXPBEXPB)" grammar) "EXPB")
(setf (gethash "(notEXPB)" grammar) "EXPB")
(setf (gethash "(equalEXPBEXPB)" grammar) "EXPB")
(setf (gethash "(equalEXPIEXPI)" grammar) "EXPB")
(setf (gethash "BinaryValue" grammar) "EXPB")
;EXPLISTI
(setf (gethash "(concatEXPLISTIEXPLISTI)" grammar) "EXPLISTI")
(setf (gethash "(appendEXPIEXPLISTI)" grammar) "EXPLISTI")
(setf (gethash "null" grammar) "EXPLISTI")
(setf (gethash "'(VALUES)" grammar) "EXPLISTI")
(setf (gethash "'()" grammar) "EXPLISTI")
(setf (gethash "EXPI" grammar) "EXPLISTI")
;values
(setf (gethash "VALUES IntegerValue" grammar) "VALUES")
(setf (gethash "IntegerValue" grammar) "VALUES")
;idlist
(setf (gethash "ID" grammar) "IDLIST")
(setf (gethash "IDIDLIST" grammar) "IDLIST")
(setf (gethash "(IDLIST)" grammar) "IDLIST")
(setf (gethash "(ID)" grammar) "IDLIST")
;change
(setq change (make-hash-table :test 'equalp)) 
(setf (gethash "identifier" change) "ID")
(setf (gethash "integer" change) "IntegerValue")
(setf (gethash "BinaryValue" change) "BinaryValue")
;operators
(setq operators (make-hash-table :test 'equalp)) 
(setf (gethash "(" operators) "operator")
(setf (gethash ")"operators) "operator")
(setf (gethash "**" operators) "operator")
(setf (gethash "+" operators) "operator")
(setf (gethash "-" operators) "operator")
(setf (gethash "*" operators) "operator")
(setf (gethash "/" operators) "operator")
;keywords
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

(defun parser (lexerlist)
	;Aldığım listenin lexerlarını bir listeye reverse order olacak şekilde attım.
	(setq lexerl (reverse (map 'list (lambda (x) (cond ((gethash (string (car x)) change) (gethash (string (car x)) change) )(t (cadr x))) ) lexerlist)))
	;Bu listeyi parserHelper metoduna gönderip derivationı tersten yaptım, parserHelper metodunun sonucunu reverse edip sonucu stack değişkenine atadım.
	(setq stack (append '(("; DIRECTIVE: derivation")) (reverse (parserHelper lexerl '())) (list(reverse lexerl))))
	;Stack değişkenini recursive bir şekildde dosyaya yazdırdım.
	(write_tree stack)
	(return-from parser t)
)

;Recursive bir şekilde derivation yapar.
;Listenin sonundan başlar, listenin ilk elamanı ")" kapalı parantezdir.
;Ne zaman reduce edilebilecek bir değer ya da "(" açık parantez yakalanırsa reduce edilir. Bu kontrol check metodu ile sağlanır.
;Check metodu true döndürürse reduceR metodu ile reduce işlemi yapılır.
;Eğer check metodu reduce edilecek bir şey yakalamdıysa 4. condition ile shift edilir.
;Bu metod stackte "START" kalana kadar veya stackte reduce edilemeyecek hatalı bir değer alınıncaya kadar devam eder. 
(defun parserHelper(lexerlist stack )
	(cond
		((equal (car stack) "START") nil)
		((and (equal (list-length stack ) 1) (not (gethash (string (car stack)) operators)) (not (gethash (string (car stack)) keywords)) (not (gethash (string (car stack)) grammar))) nil  )
    	((check lexerlist stack) 
    		(setq stack (reduceR lexerlist stack)) (append (list (append(reverse lexerlist) '("<") (list (car stack)) '(">")(cdr stack))) (parserHelper lexerlist stack )) 
    	) 
		( t (setq stack (append (list (car lexerlist)) stack ))(append (parserHelper (cdr lexerlist) stack)) )
	)
)


;check fonksiyonu stackteki ilk elemanın reduce edilip edilemeyeceğine veya stackteki ilk elemanın "(" okup olmadığına bakar.
;Gramerde conflict olduğundan bu durumları condition koyarak denetledim.
(defun check (lexerlist stack)
	(cond
	    ((gethash (string (car stack)) keywords) nil);ilk eleman keyword ise reduce yapma
        ((or (equal (car stack) "EXPB")) nil);ilk eleman EXPB ise reduce yapma
		((and (gethash (string (car stack)) operators) (not (equal (car stack) "("))) nil);ilk eleman "(" dışındaki bir operatör ise reduce yapma.
		((equal (car stack) "EXPI") ;EXPI ne zaman reduce olur.(Gramerde bazı yerlerde  EXPI -->EXPLISTI dönüşümü olurken bazı yerlerde bu dönüşüm olmuyordu.Aşağıdaki conditonlar bunu denetliyor.)
			(cond
				;Bir öncesi "(" ve ")"" dışındaki operatörlerden ise reduce yapma. (ÖNCESİ derken daha stack`e atılmayan lexerlist elemanlarından söz ediyorum)
				( (and (> (list-length lexerlist) 0)  (gethash (string (car lexerlist)) operators) (not (equal (car lexerlist) ")")) (not (equal (car lexerlist) "(")) ) nil )
				;Bir öncesi equal veya append ise reduce yapma.
				( (and (> (list-length lexerlist) 0) (or (equal (car lexerlist) "equal") (equal (car lexerlist) "append")) ) nil )
				;iki öncesi ( ) dışındaki operatörlerden ise reduce yapma.
				( (and (> (list-length lexerlist) 1) (gethash (string (cadr lexerlist)) operators) (not (equal (cadr lexerlist) ")")) (not (equal (cadr lexerlist) "(")) ) nil )
				;iki öncesi set,defvar,ya daequal ise reduce yapma.
				( (and (> (list-length lexerlist) 1) (or (equal (cadr lexerlist) "set") (equal (cadr lexerlist) "equal") (equal (cadr lexerlist) "defvar")) ) nil )
				;önesi id veya integer ve iki öncesi id ise reduce yapma.
				( (and (> (list-length lexerlist) 1) (equal (car lexerlist)"ID") (or (equal (cadr lexerlist)"ID") (equal(cadr lexerlist)"IntegerValue")) ) nil )
				(t t);reduce  yap
			)
		)
		((equal (car stack) "ID");ID ne zaman reduce olur.(Gramerde bazı yerlerde  ID -->EXPI veya ID -->IDLIST dönüşümleri olurken bazı yerlerde bu dönüşüm olmuyordu.Aşağıdaki conditonlar bunu denetliyor.)
			(cond
				;öncesi deffun  defvar ya da set ise reduce yapma.
				( (and (> (list-length lexerlist) 0) (or (equal (car lexerlist) "set") (equal (car lexerlist) "deffun") (equal (car lexerlist) "defvar")) ) nil )
				;sonrası IDLIST ise reduce yapma.(Bir sonrası stack`e en son atılan elemanı gösterir.)
				( (and (> (list-length stack) 1) (equal (cadr stack) "IDLIST") ) t)
				;öncesi "("reduce yapma. 
				( (and (> (list-length lexerlist) 1) (equal (car lexerlist) "(") ) nil)
				;reduce yap
				(t t)
			)
		)
		((and (equal (car stack) "EXPLISTI") (not (equal (list-length lexerlist) 0)))  nil )
		((and (not (gethash (string (car stack)) operators)) (not (gethash (string (car stack)) grammar))) nil)
		(t t)
	)
)
;eğer check metodu true döndürürse,reduceR metodu reduce işlemi için çağrılır.
;conflictler conditionlar ile sorgulanarak stackteki ilk elemanın hangi değere reduce edileceğini belirler ve reduce işlemini yapar.
;eğer stackteki ilk eleman açık parantez "(" ise  reduce_rule2 kaplaı paranteze ")" kadar olan elemanların gramerde represent ettiği 
;değer bulunur ve bu değer ilereduce işlemi yapılır
(defun reduceR (lexerlist stack)
	(cond
		((and (equal (car stack) "EXPI") (not (equal (list-length lexerlist) 0))) (append '("EXPLISTI")  (cdr stack)) )
		((equal (car stack) "EXPI") (append '("INPUT")  (cdr stack)))
		((and (equal (car stack) "ID") (> (list-length lexerlist) 0) (equal (car lexerlist) "ID") ) (append '("IDLIST")  (cdr stack)))
		((and (equal (car stack) "ID")(> (list-length stack) 0) (equal (cadr stack) "IDLIST") ) (append '("IDLIST") (cddr stack)))
		((equal (car stack) "ID") (append '("EXPI")  (cdr stack)))
		((equal (car stack) "(") (append (reduce_rule stack )))
		(t (append (list (gethash (string (car stack)) grammar))  (cdr stack)))		
	)
)

;bir listeyi concatanete eder
(defun my-concat( list )(format nil "~{~a~}" list)) 
;Açık parantez "(" saptandığında bu metod çağrılır.Bu metod reduce_rule2 metodunu çağırıp stackteki kapalı paranteze ")" kadar olan elemanlar bir liste
;şeklinde alr.Bu listeyi my-concat metoduna gönderip contanete eder.Daha sonra concatanete edilen veriyi alır ve gramer de olup olmadığını sorgular.
;Gramerde ise stackì  kapalı paranteze ")" kadar boşaltır ve yerine gramerdeki karşılığı eklenir.
(defun reduce_rule (stack)
    (setq rule '()) 
    (setq rule (reduce_rule2 stack rule)) 
    (setq rule (append (list (my-concat (car rule))) (cdr rule)))
    (cond
        ((gethash (string (car rule)) grammar) (append (list(gethash (string (car rule)) grammar)) (cdr rule)))
        (t rule)
    )
)
;Açık parantez "(" saptandığında bu metod çağrılır. Bu metod ile stackteki kapalı paranteze ")" kadar olan elemanlar bir listeye atılıp return edilir. 
(defun reduce_rule2 (stack &optional rule)
	(cond
        ((equal (car stack) ")") (append (list(append rule (list (car stack)))) (cdr stack)))
        (t (setq rule (append rule (list (car stack))) ) (reduce_rule2 (cdr stack) rule) )
    )
)

;Dosyaya format fonksiyonu ile recurive bir şekilde yazar.
(defun write_tree (tree)
  (with-open-file (str "151044076.tree"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (map 'list (lambda (x) (format str (format nil "~~{~~A~~^ ~~}~T") x) (format str "~%")) tree )
))
