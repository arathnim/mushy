(require "asdf")
(ql:quickload '(split-sequence usocket cl-store alexandria cl-ppcre anaphora))
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
(asdf:load-system :mushy)
(sb-thread:make-thread 'ticker :name "ticker")
(sb-thread:make-thread 'start-server :name "server")
(load-material (directory "data/materials/*.*"))
;;(make-test-world)
(load-world "save1.sav")