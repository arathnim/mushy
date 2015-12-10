(defun put-into (container target)
	(cond ((not (has-flag container 'container))
			(return-from put-into "That's not a container!"))
		(t (progn (push-sub container target)
			(catstr "You place the " (name target) " into the " (name container) ".")))))

(defun get-weight (blk)
	(if (attr blk "weight")
		(attr blk "weight")
		(reduce #'+ (subs blk) :key #'get-physical-weight)))

(defun get-avaliable-space (container)
	(- (car (attr container "capacity"))
		(reduce #'+ (mapcar #'get-weight (subs container)))))

(defcom "place"
	(("place " target " into " container)
		(with-object target %target
		(with-object container %container
			(put-into container target)))))