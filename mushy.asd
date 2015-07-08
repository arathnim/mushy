(in-package :asdf-user)

(defsystem "mushy"
	:description "Odd little text-based game."
	:version "0.1"
	:author "Arathnim"
	:serial t
	:components  
		((:file "mushy-core")
		(:file "mushy-world")
		(:file "mushy-control")
		(:file "mushy-default-attrs")
		(:file "mushy-server")))
