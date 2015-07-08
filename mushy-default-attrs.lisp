(defun build-object-string (blk)
  (if (eql (attr blk "vis") 0) 
	 (format nil "You see ~a~a" (build-name blk) (build-status blk)) ""))

(defun build-name (blk)
  (if (has-flag blk 'proper-name) 
	 (attr blk "name")
	 (format nil "a ~a" (attr blk "name"))))

(defun build-status (blk)
  (if (attr blk "status")
	 (format nil ", ~a." (attr blk "status"))
	 "."))

(defun room-default-desc (caller this)
	(format nil "~a~%==========================~%~a ~{~a ~}~%" 
		(string-capitalize (attr this "name")) 
		(attr this "room-desc")
		(remove nil (gather-sdesc caller (subs this)))))

(defun gather-sdesc (caller subs)
	(loop for s in subs collect 
		(let-eval caller s (attr s "sdesc"))))

(defun object-default-sdesc (caller this)
  (build-object-string this))