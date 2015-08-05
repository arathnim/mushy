(defcommand "test"
	'(("test") "Testing, testing, 1, 2, 3..."))

(defcommand "ex"
	`(("ex " target) (with-object obj %target (ex obj))))

(defcommand "look"
	`(("look") 
		(exec-attr (above player) "desc" player nil))
	`(("look through " portal) 
		(with-object portal %portal 
			(exec-attr portal "portal-desc" player nil)))
	`(("look " (switch "in" "into" "inside") " " (optional "the ") container)
		(with-object container %container
			(exec-attr container "container-desc" player nil)))
	`(("look " (optional "at the " "at ") target)
		(with-object target %target 
			(exec-attr target "desc" player nil))))

(defcommand "quit"
	`(("quit") 
		(let ((socket (get-socket (attr player "name"))))
			(usocket:socket-close socket))))

(defcommand "(ls|cd .*|man .*)$"
	`((".*") "This isn't a shell, dummy!"))

(defcommand "save-world"
	`(("save-world " filename)
		(progn (cl-store:store
			(list *world* *next-id* *users* *players*) %filename) "World saved.")))

(defcommand "go"
	`(("go " (optional "through the" "the") door)
		(with-object door %door 
			(let ((target (attr door "target")))
				(move-to player target)
				(exec-attr blk "desc" player nil)))))

(defcommand "say"
	`(("say " message) 
		(broadcast (format nil "[~a] ~a" (attr player "name") 
			%message) (above player)) "")))

(defun ex (blk)
	(format nil "Attributes:~%~{~a~} ~%Flags:~{~a ~}~%Above: ~a~%Subs:~%~{~a~%~}"
		(loop for key being each hash-key of (attrs blk)
			collect (print-hash-entry key (gethash key (attrs blk))))
		(flags blk) (if (above blk) (attr (above blk) "name") nil) 
		(get-sub-names blk)))

(defun print-hash-entry (key value)
	(format nil "  ~a : ~a~%" key value))

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
