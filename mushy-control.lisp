(defun ex (blk) 
	(format nil "Attributes:~%~{(~{~a~^ . ~}) ~%~}~%Flags:~{~a ~}~%Above: ~a~%Subs:~%~{~a~%~}"
			 (attrs blk) (flags blk) (if (above blk) (attr (above blk) "name") nil) (get-sub-names blk)))

(defun extern-look (args player)
	(if (equal (car args) "at") 
		(let ((target (resolve-object-descriptor (cadr args) player)))
	 		(if target (xc-attr target "desc" player)
				"Can't see that here."))
		(xc-attr (above player) "desc" player)))

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

(defun extern-say (args player)
	(broadcast (format nil "[~a] ~a" (attr player "name") (reconstruct args)) (above player))
	"")

(defun extern-set (args player)
  (let ((target (find-sub (above player) (car args))))
	 (if target (set* (cdr args) target) "Can't see that here.")))

(defun set* (args target)
	(push-attr target (car args) 
		(read-from-string (format nil "~{~a~^ ~}" (cdr args))))
	"Attribute set.")

(defun reconstruct (arg)
	(format nil "~{~a~^ ~}" arg))

(defparameter *commands* '("look" "quit" "ex" "save" "set" "say"))

(defun mushy-eval (str caller) 
	(let ((head (car (split-sequence:SPLIT-SEQUENCE #\Space str))))
		(if (equalp head "") (return-from mushy-eval ""))
		(if (member head *commands* :test #'equalp) 
		  (funcall (find-symbol (string-upcase (concatenate 'string "extern-" head))) 
				(cdr (SPLIT-SEQUENCE::SPLIT-SEQUENCE #\Space str)) caller)
		  (concatenate 'string head " is not a valid command. "))))

(defun xc-attr (blk attr caller)
  (let-eval caller blk (attr blk attr)))

(defun resolve-object-descriptor (str player)
	(if (equalp str "here") (return-from resolve-object-descriptor (above player)))
	(if (equalp str "me") (return-from resolve-object-descriptor player))
	(find-sub (above player) str))