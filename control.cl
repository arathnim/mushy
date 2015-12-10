(defparameter *commands* nil)

(defun defcommand (head &rest forms)
	(push (list head forms) *commands*))

;; WARNING! psychotic code - approach with caution

(defun mushy-eval (command-str &optional caller)
	(setf command-str (string-trim '(#\Space #\Tab #\Newline) command-str))
	(let* ((command (find-head command-str)) 
			 (forms (cadr command))
			 (form nil) (matches nil) (symbols nil)
			 (matched-symbols (make-hash-table :test 'eq)))
		(declare (special matched-symbols))
		(block outer 
			(loop for f in forms do
				(multiple-value-bind (a b) 
					(cl-ppcre:scan-to-strings (convert-form f) command-str)
						(if a (progn 
							(setf matches b form f symbols (collect-symbols (car f)))
							(return-from outer nil))))))
		(if (not form) (return-from mushy-eval 
			"Can't match your command form."))
		(if (and symbols (not (equalp matches #()))) 
			(loop for s in symbols for m across matches do
			(setf (gethash s matched-symbols) m)))
		(let ((player caller))
			(declare (special player))
			(eval (cadr form)))))

(defun convert-head (head)
	(format nil "^~a" head))

(defun find-head (command)
	(loop for c in *commands* do
		(if (cl-ppcre:all-matches (convert-head (car c)) command)
			(return-from find-head c))))

(defun convert-form (form)
	(setf form (car form))
	(format nil "^~a$"
		(build-regex form)))

(defun build-regex (elt)
	(cond ((stringp elt) elt)
			((symbolp elt) "(.+?)")
			((eq (car elt) 'optional) 
				(format nil "~{(?:~a)?~}" (mapcar #'build-regex (cdr elt))))
			((eq (car elt) 'switch) 
				(format nil "(?:~{~a~^|~})" (mapcar #'build-regex (cdr elt))))
			((listp elt) (format nil "~{~a~}" (mapcar #'build-regex elt)))
			(t (error "Invalid element in command declaration:~a" elt))))

(defun collect-symbols (form)
	(let ((res nil))
		(cond ((listp form) 
			(setf res (alexandria:flatten (mapcar #'collect-symbols form))))
				((and (symbolp form) (not (eq form 'optional)) (not (eq form 'switch))) 
					(setf res (push form res)))
				(t nil)) res))

(defmacro get- (sym) `(gethash ',sym matched-symbols))

(defun quote-list (lst)
	(mapcar (lambda (x) (car `(',x))) lst))

;; (let ((name (resolve-object val player))) (if name progn "error")) 
(defmacro with-object (name val &rest form)
	`(let ((,name (resolve-object ,val player)))
		(if (eql (list-length ,name) 1) 
			(progn (setf ,name (car ,name)) ,@form) 
			(format nil "Unable to resolve \"~a\".~{~%   ~a~}" ,val ,name))))

(defmacro defcom (head &rest forms)
	`(defcommand ,head ,@(quote-list forms)))

