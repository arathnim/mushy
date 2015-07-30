(defparameter *weather-table* nil) ;; table style, keys are weather:biome:season:time

(defun weather-push (weather biome season time data)
	(push (list weather biome season time data) *weather-table*))

(weather-push "stormy" "forest" "summer" "day"
	"Gusts of wind tear through the tree limbs and howl around rocks, turning the leaves over so that you can see their green undersides.")

(weather-push "stormy" "forest" "spring" "day"
	"Gusts of wind tear through the tree limbs and howl around rocks, turning the leaves over so that you can see their silver undersides.")

(weather-push "stormy" "forest" "fall" "day"
	"Red, orange, and gold leaves swirl past you on the wind, dancing around the trees.")