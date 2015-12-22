(defn square [x] (* x x))

(defn square-from [from up-to]
	(take-while #(<= % up-to) (iterate square from)))