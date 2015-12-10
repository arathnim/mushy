(defun apply-template (this templates)
	(loop for tmp in templates do 
		(let ((data (read-all-lines (concatenate 'string "data/templates/" 
			(string-downcase (string tmp)) ".lisp"))))
			(mapc (lambda (x) (let-eval nil this x)) data))) nil)

(defun rand-element (lst)
	(nth (random (length lst)) lst))

(defun push-parts (&rest rest)
	(let* ((blocks nil) (subs nil) (flags nil) (attrs nil) (material nil) (current "blocks"))
		(declare (special blocks) (special subs) (special flags) (special attrs) (special material))
		(loop for r in rest do
			(if (symbolp r)
				(setq current (string r))
				(push r (symbol-value (find-symbol (string-upcase current))))))
		(setq blocks (flatten blocks))
		(setq subs (flatten subs))
		(setq subs (rep-string-list subs))
		(setq attrs (reverse attrs))
		(setq flags (loop for f in flags collect (intern (string-upcase f))))
		(loop for b in blocks do 
			(push-parts-backend b flags subs attrs material))))

(defun push-parts-backend (blk flags sub-list attrs material)
	(mapcar (lambda (x) 
		(let ((sub (make-sys-blk (make-instance 'obj) (string-capitalize x)))) 
			(push-sub blk sub)
			(push-attr-list sub attrs)
			(push-attr sub "material" (get-material (car material)))
			(loop for f in flags do (push-flag sub f)))) 
	sub-list))

(defun rep-string-list (llist)
	(setq llist (append llist '("")))
	(flatten 
	(loop for x in llist by #'cdr
			for y in (cdr llist) by #'cdr collect
				(cond ((numberp x) (rep (- x 1) y))
					(t x)))))

(defun find-subs (blk &rest subs)
	(setq subs (flatten subs))
	(flatten (mapcar #'(lambda (x) (rfind-sub blk x)) subs)))

(defun push-blood (blk weight)
	(push-parts blk :subs "blood" :flags "internal" :material "blood" :attrs "weight" weight))

(defun push-skin (blk weight)
	(push-parts blk :subs "skin" :flags "cover" :material "skin" :attrs "weight" weight))