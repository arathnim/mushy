(defclass obj () (
	(sub-blocks :initform nil :accessor subs)
	(attributes :initform (make-hash-table :test #'equalp) :accessor attrs)
	(flags :initform nil :accessor flags)
	(above :initform nil :accessor above)
	(id :type fixnum :initform (incf *next-id*) :accessor id)))

(defmethod print-object ((block obj) stream)
	(format stream "<obj #~a '~a'>" (id block) (attr block "name")))

(defun attr (blk name)
	(gethash name (attrs blk)))

(defun has-flag (blk flag)
	(find flag (flags blk)))

(defun push-attr (blk str sexp)
 	(setf (gethash str (attrs blk)) sexp))

(defun push-attrs (blk &rest list)
  (loop for x in list by #'cddr 
		  for y in (cdr list) by #'cddr
		  do (push-attr blk x y)))

(defun push-attr-list (blk list)
  (loop for x in list by #'cddr
		  for y in (cdr list) by #'cddr
		  do (push-attr blk x y)))

(defun push-flag (blk str)
	(push str (flags blk)))

(defun push-sub (blk blk2)
  (push blk2 (subs blk))
  (setf (above blk2) blk))

(defun move-to (blk blk2)
	(print `(,blk ,(above blk) ,(subs (above blk))))
	(delete blk (subs (above blk)))
	(push-sub blk2 blk))

(defun get-sub-names (blk)
  (mapcar (lambda (x) (attr x "name")) (subs blk)))

(defun add-flagged-list (flag block-list &rest name-list)
	(loop for b in block-list do 
		(add-flagged-subs flag b name-list)))

(defun add-part (flag blk name num)
  (loop repeat num do 
  	(add-flagged-subs flag blk (list name))))

(defun compound-find (list name)
	(let ((tmp nil)) 
		(loop for e in list do 
			(push (rfind-sub e name) tmp))
		tmp))

(defun push-to-attr (blk sexp name)
	(push sexp (gethash name (attrs blk))))