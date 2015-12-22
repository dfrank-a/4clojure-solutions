(defn print-n-pass [x] (println x) x)

(def divides 
	#{[8 4] [9 3] [4 2] [27 9]})
(def more-legs
    #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})
(def progeny
    #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]})

(defn transitive-closure [bin-rel]
	(letfn [
			(links?   [[_ a]] (fn [[b _]] (= a b))),
			(new-link [[a _]] (fn [_ b] [a b])),

			(get-links-to 	
						[node]
						(println node)
						(map (new-link node) 
							 (print-n-pass (filter (links? node) 
			    			         (disj bin-rel node)))))
		]
		
		(filter not-empty (apply concat (list) (map get-links-to bin-rel)))))
