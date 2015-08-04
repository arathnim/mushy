(defparameter *functions* nil)

(defun soft-eval (sexp)
	(if (not (listp sexp)) sexp
		(let ((func (find (car sexp) *functions* :key #'car)))
			(if func (apply func (mapcar #'soft-eval (cdr sexp)))))))

(push (list '+ (lambda (&rest a) (apply '+ a))) *functions*)
(push (list '- (lambda (&rest a) (apply '- a))) *functions*)