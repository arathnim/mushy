(defclass *block () (
	(sub-blocks :initform nil :accessor subs)
	(attributes :initform nil :accessor attrs)
	(flags :initform nil :accessor flags)
	(above :initform nil :accessor above)
	(id :type 'integer :initform (incf *next-id*) :accessor id)))

(defun attr (blk name) (cadr 
	(assoc name (attrs blk) :test #'equalp)))

(defun has-flag (blk flag) (find flag (flags blk)))

(defun push-attr (blk str sexp)
 	(push (list str sexp) (attrs blk)))

(defun push-attrs (blk &rest list)
  (loop for x in list by #'cddr 
		  for y in (cdr list) by #'cddr
		  do (push-attr blk x y)))

(defun push-attr-list (blk list)
  (loop for x in list by #'cddr 
		  for y in (cdr list) by #'cddr
		  do (push-attr blk x y)))

(defun push-flag (blk str) (push str (flags blk)))

(defun push-sub (blk blk2)
  (push blk2 (subs blk))
  (setf (above blk2) blk))

(defun get-sub-names (blk)
  (mapcar #'(lambda (x) (attr x "name")) (subs blk)))

(defun make-exit (name blk target) 
	(let ((exit (make-instance '*block))) 
		(progn (push-flag exit 'exit) (push-attr exit "name" name)
			(push-sub blk exit) exit)))

(defun add-flagged-list (flag block-list &rest name-list)
	(loop for b in block-list do 
		(add-flagged-subs flag b name-list)))

(defun add-part (flag blk name num)
  (loop repeat num do (add-flagged-subs flag blk (list name))))

(defun compound-find (list name)
	(let ((tmp nil)) 
		(loop for e in list do 
			(push (rfind-sub e name) tmp))
		tmp))

(defun push-to-attr (blk sexp name)
	(push sexp (cadr (assoc name (slot-value blk 'attributes) :test #'equalp))))