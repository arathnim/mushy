;;; mushy server init
;;; (author *this*) => ("Dylan Ball" "arathnim@gmail.com")

(ql:quickload '(split-sequence usocket cl-store alexandria cl-ppcre anaphora iterate))
(defpackage mushy
  (:use cl split-sequence usocket cl-store alexandria cl-ppcre anaphora iterate))
(in-package mushy)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setq *print-pretty* 'nil)
(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; reader macros, need to be evaluated before the system is loaded
(defun single-quote-reader (stream char)
   (declare (ignore char))
   `(gethash ',(read stream t nil t) matched-symbols))

(set-macro-character #\% #'single-quote-reader)
;; end reader macros

(defvar *server-init-hooks* nil)

(defun add-init-hook (exp)
  (push exp *server-init-hooks*))

;; load server configuration variables
(load "server-config.cl")

;; load core systems
(load "core.cl")
(load "modules.cl")
(load "softcode.cl")
(load "control.cl")
(load "commands.cl")
(load "server.cl")

;; load all modules
(load-modules (directory "modules/*.cl"))

;; execute hooks
(mapcar #'eval *server-init-hooks*)

;; load world objects
(cond
   (*test-mode* (make-test-world))
	(*world-name* 
		(if (probe-file (format nil *world-name* ".world")) 
			(load-world (format nil *world-name* ".world"))
			(make-blank-world))))

;; start server
(sb-thread:make-thread 'ticker :name "ticker")
(sb-thread:make-thread 'start-server :name "server")