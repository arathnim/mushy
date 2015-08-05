(defparameter *commands* nil)

(defun defcommand (head &rest forms)
	(push (list head forms) *commands*))

;; WARNING! Psychotic code - approach with caution

(defun mushy-eval2 (command-str caller)
	(setf command-str (string-trim '(#\Space #\Tab #\Newline) command-str))
	(let* ((command (find-head command-str)) 
			 (forms (cadr command))
			 (form nil)
			 (matches nil)
			 (symbols nil)
			 (matched-symbols (make-hash-table :test 'eq)))
		(declare (special matched-symbols))
		(block outer 
			(loop for f in forms do
				(multiple-value-bind (a b) 
					(cl-ppcre:scan-to-strings (convert-form f) command-str)
						(if a (progn 
							(setf matches b)
							(setf form f)
							(setf symbols (collect-symbols f))
							(return-from outer nil))))))
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

(defcommand "test"
	'(("test value2 " val1 " ~ " val2) (list (get- val1) (get- val2)))
	'(("test value " val) (list "Ohh Rah!" %val))
	'(("test test") "WOOOO!")
	'(("test") "Testing, testing, 1, 2, 3..."))

(defun ex (blk) 
	(format nil "Attributes:~%~{~a~} ~%Flags:~{~a ~}~%Above: ~a~%Subs:~%~{~a~%~}"
		(loop for key being each hash-key of (attrs blk)
			collect (print-hash-entry key (gethash key (attrs blk))))
		(flags blk) (if (above blk) (attr (above blk) "name") nil) 
		(get-sub-names blk)))

(defun print-hash-entry (key value)
    (format nil "~a : ~a~%" key value))

(defun extern-look (args player)
	(if (equal (car args) "at") 
		(let ((target (resolve-object-descriptor (cadr args) player)))
	 		(if target (exec-attr target "desc" player nil)
				"Can't see that here."))
		(exec-attr (above player) "desc" player nil)))

(defun extern-quit (args player)
  (let ((socket (get-socket (attr player "name"))))
		;;(remove (list (attr player "name") socket) *sockets*)
  		(usocket:socket-close socket)))

(defun extern-save (args player)
  (cl-store:store (list *world* *next-id* *users* *players*) "worlddata")
  "World saved.")

(defun extern-ex (args player)
  (let ((target (resolve-object-descriptor (car args) player)))
	 (if target (ex target) "Can't see that here.")))

(defun extern-go (args player)
  (let ((target (resolve-object-descriptor (car args) player)))
	 (if target (go-exit player target) "Can't see that here.")))

(defun go-exit (player target)
	(let ((blk (attr target "target")))
		(move-to player blk)
		(exec-attr blk "desc" player nil)))

(defun extern-place (args player)
  (let ((target (resolve-object-descriptor (car args) player))
  		  (container (resolve-object-descriptor (cadr args) player)))
	 (if (and target container) (put-into container target) "Can't see that here.")))

(defun extern-say (args player)
	(broadcast (format nil "[~a] ~a" (attr player "name") 
		(reconstruct args)) (above player)) "")

(defun extern-set (args player)
  (let ((target (find-sub (above player) (car args))))
	 (if target (set-attr (cdr args) target) "Can't see that here.")))

(defun set-attr (args target)
	(push-attr target (car args)
		(read-from-string (format nil "~{~a~^ ~}" (cdr args))))
	"Attribute set.")

(defun reconstruct (arg)
	(format nil "~{~a~^ ~}" arg))

(defun mushy-eval (str caller) 
	(print `(,caller ,str))
	(let* ((args (split-sequence:split-sequence #\Space str))
			(head (car args)))
		(if (equalp head "") (return-from mushy-eval ""))
		(if (member head *commands* :test #'equalp) 
		  (funcall (find-symbol (string-upcase (concatenate 'string "extern-" head))) 
				(cdr args) caller)
		  (concatenate 'string head " is not a valid command."))))

(defun resolve-object-descriptor (str player)
	(if (equalp str "here") (return-from resolve-object-descriptor (above player)))
	(if (equalp str "me") (return-from resolve-object-descriptor player))
	(find-sub (above player) str))
