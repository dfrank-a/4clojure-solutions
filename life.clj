(def board1 [ "      "  
              " ##   "
       		  " ##   "
        	  "   ## "
        	  "   ## "
       	      "      " ])

(def board2 ["     "
        	 "     "
        	 " ### "
        	 "     "
        	 "     "])

(def board3 ["      "
        	 "      "
        	 "  ### "
        	 " ###  "
           	 "      "
        	 "      "])

(defn life [board]
	(let 	[	rows (count board) 
				cols (count (board 0))
				live \# 
				dead \space
				adjacents (range -1 2)]
	(letfn 	[
			(coord-val [[row col]] (nth (board row) col))

			(live? [coord] (= live (coord-val coord)))

			(neighbors [[row col]]
				(for [ x adjacents, y adjacents
					  :when (and (not= 0 x y) 
							     (< -1 (+ row x) rows)
							     (< -1 (+ col y) cols))] 
					   [(+ row x) (+ col y)]))

			(live-count [neighborhood] (count (filter live? neighborhood)))
			
			(next-state [coord]
				  		(if (live? coord)
				  			(if (<= 2 (live-count (neighbors coord)) 3) 
				  				live 
				  				dead)
				  			(if (= 3  (live-count (neighbors coord))) 
				  				live 
				  				dead)))
		]

	;execute functions
	(vec (for [x (range rows)]
		 	  (apply str (for [y (range cols)] (next-state [x y]))))))))
