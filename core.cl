;; (authors *this-code*) => 
;;   ((:name "arathnim" :email "Arathnim@gmail.com")
;;    (:name "lyk")
;;    (:name "winny"))

(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(ql:quickload '(alexandria iterate anaphora parmesan cl-store dynamic-classes) :silent t)

(defpackage mushy
   (:shadowing-import-from dynamic-classes defclass make-instance)
   (:use cl alexandria iterate anaphora parmesan cl-store))

(in-package mushy)

(defvar *next-id* 0)
(defparameter *world* nil)

(defclass obj ()
   (above nil)
   (sub-blocks nil)
   (attributes (make-hash-table :test #'equalp))
   (methods (make-hash-table :test #'equalp))
   (flags nil)
   (id (incf *next-id*) :type fixnum)
   
   (print-object (*this* stream)
      (format stream "<obj #~a '~a'>" (id block) (attr block "name"))))

(defun attr (blk name)
   (gethash name (attrs blk)))

(defun (setf attr) (value)
   (setf ))

(defun has-flag? (blk flag)
   (find flag (flags blk) :test #'equalp))

(defun push-attr (blk str sexp)
   (declare (string str))
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
   (if (symbolp str) (setf str (string str)))
   (push str (flags blk)))

(defun push-sub (blk blk2)
   (push blk2 (subs blk))
   (setf (above blk2) blk))

(defun move-to (blk blk2)
   (delete blk (subs (above blk)))
   (push-sub blk2 blk))

(defun delete-block (blk)
   (delete blk (subs (above blk))))

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

;;;; higher level stuff

(defun read-all-lines (loc)
   (let ((in (open loc :if-does-not-exist :create)) (msg nil))
      (when in
      (loop for line = (read in nil)
         while line do 
         (setq msg (append msg (list line))))
      (close in)) msg))

(defun make-wall (name blk) 
   (let ((wall (make-instance 'obj))) 
      (progn (push-flag wall "wall") (push-attr wall "name" name)
         (push-sub blk wall) (push-attr wall "vis" 2) wall)))

(defun name (blk)
   (attr blk "name"))

(defun load-world (filename)
   (let ((vals (cl-store:restore filename)))
      (setq *world* (first vals) *next-id* (second vals) 
      *users* (third vals))))

(defun build-object-string (blk)
  (if (eql (attr blk "vis") 0) 
    (format nil "You see ~a~a" (build-name blk) (build-status blk)) ""))

(defun build-name (blk)
  (if (has-flag blk "proper-name") 
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
   (alexandria:flatten res)))

(defun match-subs (blk name)
   (let ((res nil))
      (if (match-name name blk)
      (push blk res))
   (loop for s in (subs blk) do 
      (if (match-subs s name) (push (match-subs s name) res)))
   (alexandria:flatten res)))

(defun match-name (str obj)
   (if (or (equalp str (attr obj "name"))
           (member str (attr obj "alias") :test #'equalp))
      obj nil))

(defun make-exit (name blk target)
   (let ((exit (make-sys-blk (make-instance 'obj) name)))
      (push-flag exit "exit")
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

(defun broadcast (msg room sender)
   (loop for b in (subs room)
      do (send-msg msg b sender)))

(defun send-msg (msg blk sender)
   (let ((socket (get-socket (attr blk "name"))))
      (if socket 
         (stream-print (format nil "~a" msg) socket)
         (exec-attr blk "listen-trig" sender (list msg)))))

(defun rep (num sym)
   (let ((lst nil)) (loop repeat num do (push sym lst)) lst))

(defun bind-exit (name room room2)
   (make-exit name room room2)
   (make-exit name room2 room))

(defun find-sub (blk name)
  (find name (subs blk) :test #'equalp :key #'(lambda (x) (attr x "name"))))

(defun rfind-sub (blk name)
   (let ((res nil))
      (if (equalp name (attr blk "name"))
      (setq res blk))
   (loop for s in (subs blk) do 
        (if (rfind-sub s name) (setq res (rfind-sub s name))))
   res))

(defun ticker ()
  (loop (progn (loop for r in *world* do (tick r)) (sleep 5))))

(defun exec-attr (blk attr caller args)
   (if (attr blk attr)
   (let ((env *default-env*))
      (set-symbol '*this* blk env)
      (set-symbol '*caller* caller env)
      (set-symbol '*args* args env)
      (soft-eval (attr blk attr) env))
      (format nil "No such attribute: '~a' on '~a'" attr (attr blk "name"))))

(defun let-eval (caller this sexp)
   (let ((*caller* caller) (*this* this)) (eval sexp)))

(defun tick (blk)
   (progn (if (attr blk "tick") (exec-attr blk "tick" nil nil)) 
      (mapc #'tick (subs blk))))

(defun delay-exec (s exp)
   (sb-thread:make-thread 
      (lambda () 
         (sleep s)
         (eval exp))) nil)

(defun catstr (&rest rest)
   (format nil "~{~a~}" rest))

(defun get-spawn (world)
   (loop for r in world do 
      (if (has-flag r "spawn") (return-from get-spawn r)))
   nil)

(defun make-blank-world ()
   (defparameter *spawn* (make-sys-blk (make-room) "the void"))
   (push-flag *spawn* 'spawn)
   (push-attr *spawn* "room-desc" "You float in the endless void."))

(defun make-test-world ()
   (defparameter *tavern* (make-sys-blk (make-room) "the foyer of the tavern"))
   (defparameter *porch* (make-sys-blk (make-room) "the porch of the tavern"))
   (defparameter dog (make-sys-blk (make-instance 'obj) "dog"))
   (defparameter barmaid (make-sys-blk (make-instance 'obj) "barmaid"))
   (defparameter box (make-sys-blk (make-instance 'obj) "box"))
   (defparameter apple (make-sys-blk (make-instance 'obj) "apple"))
   (defparameter door (bind-exit "wooden door" *tavern* *porch*))

   (push-flag box "container")
   (push-attrs box
      "capacity" '(200 300))

   (push-attrs apple
      "desc" "A medium-sized red apple."
      "weight" 0.3)

   (push-attr dog "status" "wagging his tail")
   (apply-template dog '(dog))
   (push-attr dog "listen-trig" nil)

   (push-attr barmaid "status" "washing glasses behind the bar")
   
   (push-flag *tavern* 'spawn)
   (push-attr *tavern* "room-desc" "The inn is lit by a small fire in the hearth, casting a warm light over the various tables and chairs in the room.")

   (mapcar (lambda (x) (push-sub *tavern* x)) (list dog barmaid box apple))

   (push-attr *porch* "room-desc" "You stand on the small wooden porch of the inn."))
