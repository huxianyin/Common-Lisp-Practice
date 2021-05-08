;3.1
;implementation1 
((lambda (x) (+ x (* x x))) 6)

;implementation2
((lambda (x) (+ x ((lambda (y) (* y y)) x))) 6)

