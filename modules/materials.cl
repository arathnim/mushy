(defparameter *materials* nil)

(add-init-hook '(load-material (directory "data/materials/*.*")))

(defun load-material (templates)
	(loop for tmp in templates do 
		(let ((data (read-all-lines tmp)) 
			(mat (make-instance 'obj)))
			(mapc (lambda (x) (let-eval nil mat x)) data)
			(push mat *materials*))) nil)

(defun get-density (blk)
	(attr (attr blk "material") "density"))
	
(defun get-material (mat-name)
	(find mat-name *materials* :test #'equalp :key #'(lambda (x) (attr x "name"))))