(defparameter mushy-socket (usocket:socket-listen "127.0.0.1" 4200))
(defparameter *users* nil) ;; username . password
(defparameter *players* nil) ;; username . player
(defparameter *sockets* nil) ;; username . socket

(defun stream-read (mushy-stream)
	(read-line (usocket:socket-stream mushy-stream)))

(defun stream-print (string mushy-stream)
	(princ string (usocket:socket-stream mushy-stream))
	(force-output (usocket:socket-stream mushy-stream)))

(defun start-server ()
  (loop (let ((socket (usocket:socket-accept mushy-socket))) 
		(sb-thread:make-thread 'handle-connection 
			:name "server-thread" 
			:arguments socket))))

(defun get-player (username)
  (cadr (assoc username *players* :test #'equalp)))

(defun get-socket (username)
  (cadr (assoc username *sockets* :test #'equalp)))

(defun repl (socket name player)
	(push (list name socket) *sockets*)
	(loop (stream-print
		(format nil "~a~% >>> "
		(string-trim '(#\Space #\Tab #\Newline)
			(wrap-to 80 (format nil "~%~a~%"
			(mushy-eval (stream-read socket) player))))) socket)))

(defun login (socket cmd)
	(let ((username (cadr cmd)) (password (caddr cmd)))
	(if (equal password (cadr (assoc username *users* :test #'equalp)))
		(repl socket username (get-player username))
		(stream-print (format nil "~a~%" "incorrect user/pass.") socket))))

(defun handle-connection (socket)
  	(let ((in (open "into-msg" :if-does-not-exist :create)))
	  (when in
		 (loop for line = (read-line in nil)
			while line do 
		 	(stream-print (format nil "~a~%" line) socket)))
		 (close in))
	(loop 
	(let* ((command (stream-read socket)) 
		(cmd (split-sequence:SPLIT-SEQUENCE #\Space command))
		(head (car cmd)))
		(if (equal head "login") (login socket cmd))
		(if (equal head "create") (create socket cmd)))))

(defun create (socket cmd)
  (let* ((username (cadr cmd)) (password (caddr cmd))
	(player (make-sys-blk (make-instance 'obj) username)))
	 (push (list username password) *users*)
	 (push (list username player) *players*)
	 (push-sub (get-spawn *world*) player)
	 (push-flag player "proper-name")
	 (stream-print (format nil "~a~%" "Account created.") socket)
		(repl socket username player)))

(defun wrap-to (length string)
	(format nil (catstr "~{~<~%~1," length ":;~A~> ~}") 
		(cl-ppcre:split " " string)))
