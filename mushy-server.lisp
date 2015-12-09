(defparameter mushy-socket (usocket:socket-listen "127.0.0.1" *port*))
;; username -> (password player)
(defparameter *users* (make-hash-table :test #'equalp))
;; username -> socket
(defparameter *connections* (make-hash-table :test #'equalp)) 

(defun stream-read (socket)
	(read-line (socket-stream socket)))

(defun stream-print (string socket)
	(princ string (socket-stream socket))
	(force-output (socket-stream socket)))

(defun start-server ()
  (loop (let ((socket (usocket:socket-accept mushy-socket))) 
		(sb-thread:make-thread 'handle-connection 
			:name "server-thread"
			:arguments socket))))

(defun get-player (username)
  (second (gethash username *users*)))

(defun get-password (username)
  (first (gethash username *users*)))

(defun get-socket (username)
  (gethash username *connections*))

(defun add-connection (username socket)
  (setf (gethash username *connections*) socket))

(defun add-user (username password player)
  (setf (gethash username *users*) (list password player)))

(defun print-file (file socket)
  (let ((in (open file :if-does-not-exist :create)))
	  (when in
		 (loop for line = (read-line in nil)
			while line do 
		 	(stream-print (format nil "~a~%" line) socket)))
		 (close in)))

(defun repl (socket username player)
	(add-connection username socket)
	(loop 
	 (stream-print
	  (format nil "~a~%" 
		(string-trim '(#\Space #\Tab #\Newline)
			(wrap-to 80 (format nil "~a"
				(mushy-eval (stream-read socket) player)))))
		socket)))

(defun login (socket cmd)
	(destructuring-bind (head username password) cmd 
	  (if (equalp password (get-password username))
		 (repl socket username (get-player username))
		 (stream-print (format nil "incorrect user/pass.~%") socket))))

(defun handle-connection (socket)
  	(print-file "intro-msg" socket)
	(loop 
	 (let ((command (split " " (stream-read socket))))
		(switch ((car command) :test #'equalp)
		   ("login" (login socket command))
			("create" (create socket command))))))

(defun create (socket cmd)
  (if *allow-account-creation*
	 (destructuring-bind (head username password) cmd
	  (let ((player (make-sys-blk (make-instance 'obj) username)))
		 (add-user username password player)
		 (push-sub (get-spawn *world*) player)
		 (push-flag player "proper-name")
		 (stream-print (format nil "Account created.~%") socket)
		 (repl socket username player)))
	 (stream-print (format nil "Account creation has been disabled for this server.~%") socket)))

(defun wrap-to (length string)
	(format nil (catstr "~{~<~%~1," length ":;~A~> ~}") 
		(split " " string)))
