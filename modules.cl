(defvar *mushy-modules* nil)

(defun load-modules (mod)
	(loop for tmp in mod do 
		(let ((data (read-all-lines tmp)) 
			(modobj (make-instance 'obj)))
			(mapc (lambda (x) (let-eval nil modobj x)) data)
			(push modobj *mushy-modules*))))