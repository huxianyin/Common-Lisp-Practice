(defun random-elt (choices)
	(elt choices (random (length choices)))
)

(defun one-of (set)
 (list (random-elt set))
 )


(defun Adj ()
	(one-of '(big little blue green adiabatic))
)

(defun Prep () 
	(one-of '(to in by with on))
)

(defun Noun ()
	(one-of '(man ball women table))
)

(defun Article ()
	(one-of '(the a) )
)

(defun Verb ()
	(one-of '(hit took saw link))
)

(defun PP() 
	(append (Prep) (noun-phrase)))


(defun Adj* ()
	(if (= (random 2) 0)
		nil
		(append (Adj) (Adj*))
	)
)

(defun PP* ()
	(if (random-elt '(nil t))
		(append (PP) (PP*))
		nil
	)
)

(defun noun-phrase ()
	(append (Article) (Adj*) (Noun) (PP*)))

(defun verb-phrase ()
	(append (Verb) (noun-phrase)))


(defparameter *simple-grammar*
	'(
		(sentence -> (noun-phrase verb-phrase))
		(noun-phrase -> (Article Noun))
		(verb-phrase -> (Verb noun-phrase))
		(Article -> the a)
		(Noun -> man ball woman table)
		(Verb -> hit took saw liked))
	"a grammar for a trivial subset of English"
)




; incorrect implementations
(defun Adj+ ()
	(one-of '(nil (append (Adj) (Adj+) )))  ;warning ! append will not be evaluated due to '
)
