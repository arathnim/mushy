(defparameter *world* nil)
(defparameter *next-id* 0)
(defparameter *materials* nil)
(defvar *caller* nil)
(defvar *this* nil)

(defun load-world ()
	(let ((vals (cl-store:restore "worlddata")))
		(setq *world* (nth 0 vals) *next-id* (nth 1 vals) 
		*users* (nth 2 vals) *players* (nth 3 vals))))

(defun rand-element (lst)
	(nth (random (length lst)) lst))

(defun apply-template (this templates)
	(loop for tmp in templates do 
		(let ((data (read-all-lines (concatenate 'string "data/templates/" 
			(string-downcase (string tmp)) ".lisp"))))
			(mapc (lambda (x) (let-eval nil this x)) data))) nil)

(defun load-material (templates)
	(loop for tmp in templates do 
		(let ((data (read-all-lines tmp)) 
			(mat (make-instance 'obj)))
			(mapc (lambda (x) (let-eval nil mat x)) data)
			(push mat *materials*))) nil)

(defun read-all-lines (loc)
	(let ((in (open loc :if-does-not-exist :create)) (msg nil))
		(when in
		(loop for line = (read in nil)
			while line do 
			(setq msg (append msg (list line))))
		(close in)) msg))

(defun make-wall (name blk) 
	(let ((wall (make-instance 'obj))) 
		(progn (push-flag wall 'wall) (push-attr wall "name" name)
			(push-sub blk wall) (push-attr wall "vis" 2) wall)))

(defun name (blk)
	(attr blk "name"))

(defun put-into (container target)
	(cond ((not (has-flag container 'container))
			(return-from put-into "That's not a container!"))
		(t (progn (push-sub container target)
			(catstr "You place the " (name target) " into the " (name container) ".")))))

(defun get-weight (blk)
	(if (attr blk "weight")
		(attr blk "weight")
		(reduce #'+ (subs blk) :key #'get-physical-weight)))

(defun get-density (blk)
	(attr (attr blk "material") "density"))
	
(defun get-material (mat-name)
	(find mat-name *materials* :test #'equalp :key #'(lambda (x) (attr x "name"))))

(defun get-avaliable-space (container)
	(- (car (attr container "capacity"))
		(reduce #'+ (mapcar #'get-weight (subs container)))))

(defun find-sub (blk name)
  (find name (subs blk) :test #'equalp :key #'(lambda (x) (attr x "name"))))

(defun rfind-sub (blk name)
	(let ((res nil))
		(if (equalp name (attr blk "name"))
		(setq res blk))
 	(loop for s in (subs blk) do 
		  (if (rfind-sub s name) (setq res (rfind-sub s name))))
 	res))

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
	(format nil "~a~%==========================~%~a ~{~a ~}" 
		(string-capitalize (attr this "name")) 
		(attr this "room-desc")
		(remove nil (gather-sdesc caller (subs this)))))

(defun gather-sdesc (caller subs)
	(loop for s in subs collect 
		(let-eval caller s (attr s "sdesc"))))

(defun object-default-sdesc (caller this)
  (build-object-string this))

(defun rfind-all-subs (blk name)
	(let ((res nil))
		(if (equalp name (attr blk "name"))
		(push blk res))
 	(loop for s in (subs blk) do 
		  (if (rfind-all-subs s name) (push (rfind-sub s name) res)))
 	res))

(defun make-exit (name blk target)
	(let ((exit (make-sys-blk (make-instance 'obj) name)))
		(push-flag exit 'exit)
		(push-sub blk exit)
		(push-attr exit "target" target)
		exit))

(defun make-room ()
	(let ((blk (make-instance 'obj)))
		(mapc (lambda (x) (make-wall x blk))
		'("west wall" "east wall" "north wall" "south wall" "floor" "ceiling")) 
		(push-attr blk "desc" '(room-default-desc *caller* *this*)) 
		(push blk *world*) blk))

(defun make-sys-blk (blk name)
	(push-attrs blk 
		"name" name
		"sdesc" '(object-default-sdesc *caller* *this*)
		"vis" 0)
	blk)

(defun add-subs (blk sub-names)
	(mapcar #'(lambda (x) (push-sub blk (make-sys-blk 
		(make-instance 'obj) (string-capitalize x)))) sub-names))

(defun broadcast (msg room)
	(loop for b in (subs room) 
		do (send-msg msg b)))

(defun send-msg (msg blk)
	(let ((socket (get-socket (attr blk "name"))))
		(if socket (stream-print (format nil "~a" msg) socket))))

(defun rep (num sym)
	(let ((lst nil)) (loop repeat num do (push sym lst)) lst))

(defun push-parts (&rest rest)
	(let* ((blocks nil) (subs nil) (flags nil) (attrs nil) (material nil) (current "blocks"))
		(declare (special blocks) (special subs) (special flags) (special attrs) (special material))
		(loop for r in rest do
			(if (symbolp r)
				(setq current (string r))
				(push r (symbol-value (find-symbol (string-upcase current))))))
		(setq blocks (alexandria:flatten blocks))
		(setq subs (alexandria:flatten subs))
		(setq subs (rep-string-list subs))
		(setq attrs (reverse attrs))
		(setq flags (loop for f in flags collect (intern (string-upcase f))))
		(loop for b in blocks do 
			(push-parts-backend b flags subs attrs material))))

(defun push-parts-backend (blk flags sub-list attrs material)
	(mapcar (lambda (x) 
		(let ((sub (make-sys-blk (make-instance 'obj) (string-capitalize x)))) 
			(push-sub blk sub)
			(push-attr-list sub attrs)
			(push-attr sub "material" (get-material (car material)))
			(loop for f in flags do (push-flag sub f)))) 
	sub-list))

(defun rep-string-list (llist)
	(setq llist (append llist '("")))
	(alexandria:flatten 
	(loop for x in llist by #'cdr
			for y in (cdr llist) by #'cdr collect
				(cond ((numberp x) (rep (- x 1) y))
					(t x)))))

(defun push-blood (blk weight)
	(push-parts blk :subs "blood" :flags "internal" :material "blood" :attrs "weight" weight))

(defun push-skin (blk weight)
	(push-parts blk :subs "skin" :flags "cover" :material "skin" :attrs "weight" weight))

(defun find-subs (blk &rest subs)
	(setq subs (alexandria:flatten subs))
	(alexandria:flatten (mapcar #'(lambda (x) (rfind-sub blk x)) subs)))

(defun bind-exit (name room room2)
	(make-exit name room room2)
	(make-exit name room2 room))

(defun ticker ()
  (loop (progn (loop for r in *world* do (tick r)) (sleep 5))))

(defun exec-attr (blk attr caller args)
	(if (attr blk attr)
	(let ((env *default-env*))
		(set-symbol '*this* blk env)
		(set-symbol '*caller* caller env)
		(set-symbol '*args* args env)
 		(soft-eval (attr blk attr) env)))
		"No such attribute!  Go yell at Arathnim!")

(defun let-eval (caller this sexp)
 	(let ((*caller* caller) (*this* this)) (eval sexp)))

(defun tick (blk)
	(progn (if (attr blk "tick") (exec-attr blk "tick" nil nil)) 
		(mapc #'tick (subs blk))))

(defun catstr (&rest rest)
	(format nil "~{~a~}" rest))

(defun get-spawn (world)
  (find 'spawn world :key #'(lambda (x) (has-flag x 'spawn))))

(defun make-test-world ()
	(defparameter *tavern* (make-sys-blk (make-room) "the foyer of the tavern"))
	(defparameter *porch* (make-sys-blk (make-room) "the porch of the tavern"))
	(defparameter dog (make-sys-blk (make-instance 'obj) "dog"))
	(defparameter barmaid (make-sys-blk (make-instance 'obj) "barmaid"))
	(defparameter box (make-sys-blk (make-instance 'obj) "box"))
	(defparameter apple (make-sys-blk (make-instance 'obj) "apple"))
	(defparameter door (bind-exit "wooden-door" *tavern* *porch*))

	(push-flag box 'container)
	(push-attrs box
		"capacity" '(200 300))

	(push-attrs apple
		"desc" "A medium-sized red apple."
		"weight" 0.3)

	(push-attr dog "status" "wagging his tail")
	(apply-template dog '(dog))
	(push-attr barmaid "status" "washing glasses behind the bar")
	
	(push-flag *tavern* 'spawn)
	(push-attr *tavern* "room-desc" "The inn is lit by a small fire in the hearth, casting a warm light over the various tables and chairs in the room.")

	(mapcar (lambda (x) (push-sub *tavern* x)) (list dog barmaid box apple))

	(push-attr *porch* "room-desc" "You stand on the small wooden porch of the inn."))