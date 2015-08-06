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
		(broadcast (format nil "[~a] ~a" (attr player "name") %message)
			(above player)) ""))

(defcom "eval"
	(("eval " exp) (soft-eval exp player)))

(defcom "exec-attr"
	(("exec-attr " object ":" attr) 
		(with-object object %object (exec-attr object %attr player nil))))

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

(defun extern-place (args player)
  (let ((target (resolve-object (car args) player))
		  (container (resolve-object (cadr args) player)))
	 (if (and target container) (put-into container target) "Can't see that here.")))

(defun extern-set (args player)
	(let ((target (find-sub (above player) (car args))))
		(if target (set-attr (cdr args) target) "Can't see that here.")))

(defun set-attr (args target)
	(push-attr target (car args)
		(read-from-string (format nil "~{~a~^ ~}" (cdr args))))
	"Attribute set.")

(defun resolve-object (str player)
	(if (equalp str "here") (return-from resolve-object (above player)))
	(if (equalp str "me") (return-from resolve-object player))
	(find-sub (above player) str))
