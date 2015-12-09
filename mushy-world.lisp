(defparameter *materials* nil)



(defun rand-element (lst)
	(nth (random (length lst)) lst))

(defun apply-template (this templates)
	(loop for tmp in templates do 
		(let ((data (read-all-lines (concatenate 'string "data/templates/" 
			(string-downcase (string tmp)) ".lisp"))))
			(mapc (lambda (x) (let-eval nil this x)) data))) nil)

(defun load-material (templates)
	(loop for tmp in templates do 
		(let ((data (read-all-lines tmp)) 
			(mat (make-instance 'obj)))
			(mapc (lambda (x) (let-eval nil mat x)) data)
			(push mat *materials*))) nil)

(defun read-all-lines (loc)
	(let ((in (open loc :if-does-not-exist :create)) (msg nil))
		(when in
		(loop for line = (read in nil)
			while line do 
			(setq msg (append msg (list line))))
		(close in)) msg))

(defun make-wall (name blk) 
	(let ((wall (make-instance 'obj))) 
		(progn (push-flag wall "wall") (push-attr wall "name" name)
			(push-sub blk wall) (push-attr wall "vis" 2) wall)))

(defun name (blk)
	(attr blk "name"))

(defun put-into (container target)
	(cond ((not (has-flag container 'container))
			(return-from put-into "That's not a container!"))
		(t (progn (push-sub container target)
			(catstr "You place the " (name target) " into the " (name container) ".")))))

(defun get-weight (blk)
	(if (attr blk "weight")
		(attr blk "weight")
		(reduce #'+ (subs blk) :key #'get-physical-weight)))

(defun get-density (blk)
	(attr (attr blk "material") "density"))
	
(defun get-material (mat-name)
	(find mat-name *materials* :test #'equalp :key #'(lambda (x) (attr x "name"))))

(defun get-avaliable-space (container)
	(- (car (attr container "capacity"))
		(reduce #'+ (mapcar #'get-weight (subs container)))))

(defun find-sub (blk name)
  (find name (subs blk) :test #'equalp :key #'(lambda (x) (attr x "name"))))

(defun rfind-sub (blk name)
	(let ((res nil))
		(if (equalp name (attr blk "name"))
		(setq res blk))
 	(loop for s in (subs blk) do 
		  (if (rfind-sub s name) (setq res (rfind-sub s name))))
 	res))




(defun push-blood (blk weight)
	(push-parts blk :subs "blood" :flags "internal" :material "blood" :attrs "weight" weight))

(defun push-skin (blk weight)
	(push-parts blk :subs "skin" :flags "cover" :material "skin" :attrs "weight" weight))

