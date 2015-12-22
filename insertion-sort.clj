(defn insert [[value & usrest] sorted]
	(println sorted "<-" value " / " usrest)
	(let [[r l] (split-with #(<= % value) sorted)] 
		[usrest (concat r [value] l)]))

(defn insertion_sort 
	([unsorted sorted]
		(if (not-empty unsorted)
			(let [[unsorted sorted] (insert unsorted sorted)]
				(recur unsorted sorted))
			sorted))
	([unsorted] (insertion_sort unsorted [])))


(def rand_coll (take 10 (repeatedly #(rand-int 10000000))))
(do (print rand_coll "->" (insertion_sort rand_coll))
(println " " (if (<= rand_coll) "pass" "fail!")))

