(push-parts *this* :subs "head" "neck" "torso" "left arm" "right arm"
	"left leg" "right leg" "face")

(setq head (find-subs *this* "head"))
	(push-parts head :subs "skull" "brain" "muscle" "fat" :flags "internal" 
		:material "simple-organic" :attrs "weight" 2.5)
	(push-parts head :subs "left eye" "right eye" "left ear" "right ear" 
		:flags "embedded" :material "simple-organic" :attrs "weight" 0.3)
	(push-skin head 0.8)
	(push-blood head 1.3)

(setq face (find-subs *this* "face"))
	(push-parts face :subs "tounge" :flags "embedded" 
		:material "simple-organic" :attrs "weight" 0.4)
	(push-parts face :subs "tooth" 20 :flags "embedded" 
		:material "tooth-enamel" :attrs "weight" 0.05)
	(push-skin face 0.4)
	(push-blood face 0.4)

	(setq ears (find-subs *this* "left ear" "right ear"))
		(push-skin ears 0.8)
		(push-blood ears 0.3)

(setq neck (find-subs *this* "neck"))
	(push-parts neck :subs "brainstem" "trachea" "fat" "muscle" 
		:flags "internal" :material "simple-organic" :attrs "weight" 0.5)
	(push-skin neck 0.8)
	(push-blood neck 0.6)

(setq torso (find-subs *this* "torso"))
	(push-parts torso :subs "heart" "stomach" "intestines" "muscle" "liver" "kidney" 2 "fat" 
		:flags "internal" :material "simple-organic" :attrs "weight" 1.5)
	(push-parts torso :subs "spine" :flags "internal" :material "bone" :attrs "weight" 4)
	(push-parts torso :subs "rib" 26 :flags "internal" :material "bone" :attrs "weight" 0.5)
	(push-skin torso 2)
	(push-blood torso 1.6)

(setq legs (find-subs *this* "left leg" "right leg"))
	(push-parts legs :subs "muscle" "fat" :flags "internal" :material "simple-organic" 
		:attrs "weight" 1)
	(push-parts legs :subs "femur" :flags "internal" :material "bone" :attrs "weight" 3)
	(push-parts legs :subs "hand" :flags "embedded")
	(push-skin legs 0.8)
	(push-blood legs 0.6)

(setq arms (find-subs *this* "left arm" "right arm"))
	(push-parts legs :subs "muscle" "fat" :flags "internal" :material "simple-organic" 
		:attrs "weight" 1)
	(push-parts legs :subs "femur" :flags "internal" :material "bone" :attrs "weight" 3)
	(push-parts legs :subs "hand" :flags "embedded")
	(push-skin legs 0.8)
	(push-blood legs 0.6)

(setq hands (compound-find legs "hand"))
	(push-parts hands :subs "claw" 5 :flags "embedded" :material "bone" :attrs "weight" 0.1)
	(push-skin hands 0.2)
	(push-blood hands 0.3)