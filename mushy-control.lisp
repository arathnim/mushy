(defparameter *commands* nil)

(defun defcommand (head &rest forms)
	(push (list head forms) *commands*))

;; WARNING! Psychotic code - approach with caution

(defun mushy-eval (command-str caller)
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
							(setf matches b form f symbols (collect-symbols f))
							(return-from outer nil))))))
		(if (not form) (return-from mushy-eval 
			"Can't match your command form, baka!"))
		(if (and symbols (not (equalp matches #()))) 
			(loop for s in symbols for m across matches do
			(setf (gethash s matched-symbols) m)))
		(let ((player caller))
			(declare (special player))
			(eval (cadr form)))))

(defun convert-head (head)
	(format nil "^~a" head))

(defun convert-optional (str)
	(format nil "(?:~a)?" str))

(defun convert-switch (list)
	(format nil "(?:~{~a~^|~})" list))

(defun cat-list (list)
	(format nil "~{~a~}" list))

(defun find-head (command)
	(loop for c in *commands* do
		(if (cl-ppcre:all-matches (convert-head (car c)) command)
			(return-from find-head c))))

(defun convert-form (form)
	(setf form (car form))
	(let ((res "^"))
		(loop for s in form do
			(cond ((stringp s) (setf res (catstr res s)))
					((symbolp s) (setf res (catstr res "(.+?)")))
					((eq (car s) 'optional) (setf res
						(catstr res (cat-list (mapcar #'convert-optional (cdr s))))))
					((eq (car s) 'switch) (setf res
						(catstr res (convert-switch (cdr s))))))) 
		(format nil "~a$" res)))

(defun collect-symbols (form)
	(remove nil (mapcar (lambda (x) (if (symbolp x) x)) (car form))))

(defmacro get- (sym) `(gethash ',sym matched-symbols))

;; (let ((name (resolve-object val player))) (if name progn "error")) 
(defmacro with-object (name val &rest form)
	`(let ((,name (resolve-object ,val player)))
		(if ,name (progn ,@form) (format nil "Unable to resolve \"~a\". ;_;" ,val))))
