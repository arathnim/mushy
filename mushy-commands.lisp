(defcom "test"
	(("test" (optional (" with value " val))) 
		(if %val (catstr "val set : " %val) "Testing, testing, 1, 2, 3...")))

(defcom "ex"
	(("ex " target) (with-object obj %target (ex obj))))

(defcom "look"
	(("look") 
		(exec-attr (above player) "desc" player nil))
	(("look through " portal) 
		(with-object portal %portal 
			(exec-attr portal "portal-desc" player nil)))
	(("look " (switch "inside" "into" "in") " " (optional "the ") container)
		(with-object container %container
			(exec-attr container "container-desc" player nil)))
	(("look " (optional "at the " "at ") target)
		(with-object target %target
			(exec-attr target "desc" player nil))))

(defcom "quit"
	(("quit")
		(let ((socket (get-socket (attr player "name"))))
			(usocket:socket-close socket))))

(defcom "(ls|cd|man)"
	((".*") "This isn't a shell, dummy!"))

(defcom "save-world"
	(("save-world " filename)
		(progn (cl-store:store
			(list *world* *next-id* *users* *players*) %filename) "World saved.")))

(defcom "go"
	(("go " (optional "through the" "the") door)
		(with-object door %door 
			(let ((target (attr door "target")))
				(move-to player target)
				(exec-attr blk "desc" player nil)))))

(defcom "say"
	(("say " message) 
	 (progn (broadcast (format nil "[~a] ~a" (attr player "name" %message))
			(above player) player) "")))

(defcom "eval"
	(("eval " exp)
		(soft-eval (read-from-string %exp))))

(defcom "exec-attr"
	(("exec-attr " object ":" attr) 
		(with-object object %object (exec-attr object %attr player nil))))

(defcom "syntax"
	(("syntax " command) 
		(format nil "~{~a~%~}" (mapcar #'car (cadr (find-head %command))))))

(defcom "create" 
	(("create " object-name) 
		(let ((obj (make-sys-blk (make-instance 'obj) %object-name)))
			(push-sub (above player) obj))))

(defcom "delete"
	(("delete " object)
		(with-object object %object (delete-block object))))

(defcom "set" 
	(("set " object ":" attr " " value)
		(with-object object %object
			(push-attr object %attr (read-from-string %value)))))

(defcom "add-flag"
	(("add-flag " object " " flag)
		(with-object object %object
			(push-flag object %flag))))

(defcom "place"
	(("place " target " into " container)
		(with-object target %target
		(with-object container %container
			(put-into container target)))))

(defun ex (blk)
	(format nil "Attributes~%~a~%Flags~%   ~a~%Above~%   ~a~%~%Subs~%   ~{~a~^, ~}"
		(write-attrs (attrs blk))
		(write-flags (flags blk))
		(if (above blk) (above blk) "none")
		(get-sub-names blk)))

(defun write-attrs (table)
	(format nil "~{~a~}"
		(loop for key being each hash-key of table collect
			(format nil "   ~a : ~a~%" key
				(prin1-to-string (gethash key table))))))

(defun write-flags (flags)
	(if (not flags) ""
		(format nil "~{~a~}~%"
			(mapcar (lambda (x) (string-downcase (string x))) flags))))

(defun resolve-object (str player)
	(if (member str '("here" "around") :test #'equalp) 
		(return-from resolve-object (list (above player))))
	(if (member str '("me" "my") :test #'equalp) 
		(return-from resolve-object (list player)))
	(rfind-all-subs (above player) str))
