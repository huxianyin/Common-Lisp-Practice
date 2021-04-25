;practice1

; 1.1
(defun last-name (str) "extract last name"

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


;1.4
(defun count-anywhere (item lst) "count how many items in the list"
	(cond
		((null lst) 0)
		((atom lst) (if (equal item lst) 1 0))
		(t (+ (count-anywhere item (car lst)) (count-anywhere item (cdr lst))))
	)
)


;1.4解答
(defun count-anywhere (item tree) "count the times item appears anywhere within the tree"
	(cond
		((eql item tree) 1)
		((atom tree) 0)
		(t (+ (count-anywhere item (car tree)) (count-anywhere item (cdr tree))))
	)
)

;1.5
(defun dot-product (vec1 vec2) "dot product of two vectors"
	(cond 
		((null vec1) 0)
		((atom vec1) (* vec1 vec2))
		(t (+ (dot-product (car vec1) (car vec2)) (dot-product (cdr vec1) (cdr vec2))))
		)
	)



;1.5解答 using recurrent
(defun dot-product (vec1 vec2) "dot product of two vectors using recurrent"
	(if (or (null vec1) (null vec2)) 0
		(+ (* (car vec1) (car vec2)) (dot-product (cdr vec1) (cdr vec2)))
	)
)

;1.5解答 using iterration
(defun dot-product (vec1 vec2) "dot product of two vectors using iterration"
	(let ((res 0))
		(dotimes (i (length vec1))
			(incf res (* (elt vec1 i) (elt vec2 i)))
		)
		res
	)
)

;1.5解答 using marcar
(defun  dot-product (vec1 vec2) "dot product of two vectors using marcar"
	(apply #'+ (mapcar #'* vec1 vec2))
)








