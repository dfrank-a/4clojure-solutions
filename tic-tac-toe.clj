(def board1 [[:o :e :e] 
             [:o :x :o] 
             [:x :x :e]])

(def board2 [[:x :o :o] 
             [:x :x :e] 
             [:e :o :e]])

(def board3 [[:x :e :x] 
             [:o :x :o] 
             [:e :o :e]])

(def board4 [[:x :x :o] 
             [:e :e :e] 
             [:e :e :e]])

(def board5 [[:x :x :o] 
             [:o :e :o] 
             [:x :e :e]])

(defn win-move [piece board]
	(let [
			rows (for [x [0 1 2]] (map (partial conj [x]) (range 3))),
			cols (for [c rows] (map reverse c)),
			diag [ [[0 0] [1 1] [2 2]], [[0 2] [1 1] [2 0]] ],
			runs (lazy-cat diag rows cols),
          
			get-run   (fn [coords] (for [[r c :as C] coords] [((board r) c), C])),
            my-piece? (fn [[P _]]  (= piece P))]

      (disj (set (for [run (map get-run runs)]
                      (let  [run-group (group-by my-piece? run)]
                            ;check if there are two of my pieces is in run
                            ;and other cell is empty
                            (if (and (= 2 (count (run-group true)))
                                     (= :e (((run-group false) 0) 0))) 

                                ;if true get the coordinate of the empty cell
                                (vec (((run-group false) 0) 1)))))) 
            nil))) ;remove nil from set


