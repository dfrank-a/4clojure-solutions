(defn print-n-pass [x] (println x) x)

(defn number-maze [start target]
	(letfn [
			(double [[x & _ :as X]] (conj X (* x 2)))
			(halve  [[x & _ :as X]] (if (even? x) (conj X (/ x 2))))
			(add2   [[x & _ :as X]] (conj X (+ x 2)))

			(=target? [x] (= (first x) target))

			(expand [element]
				(for [f (list double halve add2)
						:when (seq (f element))]
						(f element)))

			(breadth-search [fn-expand fn-target? [q1 & qrest :as queue]]
				(cond 
					(not (seq queue)) 	nil
					(fn-target? q1)		q1
					:else (recur fn-expand 
								 fn-target? 
								 (into (vec qrest) 
								 	   (fn-expand q1)))))
		]
	
	(count (print-n-pass (breadth-search expand 
										 =target? 
										 [(list start)] )))))

(= 1 (number-maze  1  1))
(= 3 (number-maze  3 12))
(= 3 (number-maze 12  3))
(= 3 (number-maze  5  9))
(= 9 (number-maze  9  2))
(= 5 (number-maze  9 12))