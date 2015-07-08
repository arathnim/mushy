;; Dog template!
(apply-template *this* '(quadruped furred))

(setq size (rand-element '("small" "large"))
		color (rand-element '("brown" "black" "beige" "white")))

(push-attr *this* "desc" (catstr "A " size ", cute " color
	" dog with floppy ears.  Capable of hunting and great loyalty."))