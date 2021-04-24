;practice1

; 1.1
(defun last-name (str) "extract last name"
	(cdr str)
	)


;1.2
(defun power (x n) "n power of x"
	(if (eql n 0)
		1
		(if (eql n 1) x
			(* x (power x (- n 1))))
	))

;1.2 解答
(defun power-quick (x n) "n power of x in log(n) time because of check for even n"
	(cond 
		((= n 0) 1)
		((evenp n) (expt (power-quick x (/ n 2)) 2)) 
		(t (* x (power-quick x (- n 1))))
	))

;1.3
(defun count-atoms (lst)
	"count atoms in a list"
	(if lst
	(let ((res 0))
		(if (atom (car lst)) (+ res 1 (count-atoms (cdr lst)))
			(+ res (count-atoms (cdr lst)))
		)
	)
	0)

;1.3解答   (a (b) c ) ==> 3
(defun count-atoms2 (lst)
	"Return the total number of atoms in the expression"
	(cond 
		((null lst) 0)
		((atom lst) 1)
		(t (+ (count-atoms2 (car lst)) 
			  (count-atoms2 (cdr lst)) ))
	)
)


;1.3解答2

(defun count-atoms3 (lst &optional (if-null 1)) 
	"Return the total number of atoms in the expression, counting nil as an atom only in non-tail postion"
	(cond 
		((null lst) if-null)
		((atom lst) 1)
		(t (+ (count-atoms3 (car lst) 1) 
			  (count-atoms3 (cdr lst) 0) ))
	)
)










