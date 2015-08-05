(defstruct env symbol-table functions macros special-forms)

(defparameter *default-env* (make-env 
	:symbol-table (make-hash-table :test 'eq)))

(defun soft-eval (sexp &optional env)
	(ignore-errors
	(if (not env) (setf env *default-env*))
	(princ (format nil "soft-eval ~a - " sexp))
	(maphash (lambda (k v) (prin1 (list k v))) (env-symbol-table env)) 
	(princ (format nil "~%"))
	(if (listp sexp)
		(let ((f (find (car sexp) (env-functions env) :key #'car))
				(s (find (car sexp) (env-special-forms env) :key #'car))
				(m (find (car sexp) (env-macros env) :key #'car)))
		(cond (s (apply (cadr s) (list (cdr sexp) env)))
				(m (apply (cadr m) (cdr sexp)))
				(f (apply (cadr f)
					(mapcar (lambda (x) (soft-eval x env)) (cdr sexp))))))
		(cond ((or (numberp sexp) (stringp sexp)) sexp)
				((symbolp sexp) (gethash sexp (env-symbol-table env)))))))

(defun add-fun (name func)
	(push (list name func) (env-functions *default-env*)))

(defun add-fun-list (lst)
	(loop for e in lst do (add-fun e e)))

(defun add-special-form (name func)
	(push (list name func) (env-special-forms *default-env*)))

(defun set-symbol (name value env)
	(setf (gethash name (env-symbol-table env)) value))

;; arithmetic and logic
(add-fun-list '(+ - / * = < > /= eq eql equal equalp))

;; list functions
(add-fun-list '(car cdr list list-length cons remove remove-if
	delete delete-if map mapcar find))

;; strings and vectors
(add-fun-list '(subseq char concatenate string-capitalize aref 
	elt string-upcase string-downcase string-trim))
(add-fun 'format (lambda (str &rest args)
	(apply #'format (append (list nil str) 
		(mapcar #'soft-eval args)))))
(add-fun 'match 'cl-ppcre:all-matches-as-strings)
(add-fun 'split 'cl-ppcre:split)
(add-fun 'replace 'cl-ppcre:regex-replace-all)

;; mushy interface
(add-fun-list '(attr has-flag push-attr push-attrs push-flag push-sub 
	move-to exec-attr room-default-desc object-default-sdesc broadcast
	above id flags))

;; (let ((x 5) (y 4) (z 3)) &rest progn)
(add-special-form 'let
	(lambda (args env)
		(loop for i in (car args) do
			(set-symbol (car i) (cadr i) env))
		(car (last (mapcar (lambda (x) (soft-eval x env))
			(cdr args))))))

;; (if test t &optional nil)
(add-special-form 'if
	(lambda (args env)
		(if (soft-eval (car args) env) (soft-eval (cadr args) env) 
			(soft-eval (caddr args) env))))

(add-special-form 'progn
	(lambda (args env)
		(car (last (mapcar (lambda (x) (soft-eval x env)) args)))))
