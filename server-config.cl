;;; server configuration options
;;; for those of you unfamiliar with lisp, nil and t are boolean false and true

;; port to bind on startup
(defvar *port* 4200)

;; disregards the savefile and starts the server in test mode
(defvar *test-mode* nil)

;; string name of the world to load, using a new name will start a new savefile
(defvar *world-name* "world1")

;; indicates if anyone can make accounts on the server
(defvar *allow-account-creation* t)
