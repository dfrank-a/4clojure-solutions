(def words1 #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})
(def words2 #{"cot" "hot" "bat" "fat"})
(def words3 #{"to" "top" "stop" "tops" "toss"})
(def words4 #{"spout" "do" "pot" "pout" "spot" "dot"})
(def words5 #{"share" "hares" "shares" "hare" "are"})
(def words6 #{"share" "hares" "hare" "are"})

(def allsets 	[words1 words2 words3 words4 words5 words6])
(def checks 	[true false false true true false])

(defn word-chain? [words]
	(letfn [
		;;get levenshtein distance of two words
		(levenshtein [[x1 & xrest :as X] [y1 & yrest :as Y]]
				(cond 
					(empty? X) (count Y)
					(empty? Y) (count X)
					(= x1 y1) (levenshtein xrest yrest)
					:else (+ 1 
							 (min (levenshtein X yrest)
								  (levenshtein xrest Y)
								  (levenshtein xrest yrest))))),

		;;return fn that is true if a given word 
		;;is levenshtein 1 from target word
		(chainable? [link] 
			(fn [x] (= 1 (levenshtein link x))))

		;make a map w/ keys = words, 
		;values = words w/ levenshtein distance = 1
		(lev-graph []
			(into {} 
				(for [link (seq words)] 
				 	[link, (set (filter (chainable? link) words ))] )))

		;fn returns fn that removes node1 from the set of links
		(remove-link-to [node1]
		   (fn [[node2 links]] 
		   		[node2, (disj links node1)]))

		;fn removes node from graph 
		(remove-node [rnode graph]
				(into {} 
					 (map (remove-link-to rnode) 
					      (dissoc graph rnode))))

		;fn searches for a hamiltonian path (hits all nodes)
		(path? [graph start]
			(cond 
				;absorbed all values and reached end of graph--this is a path!
				(<= (count graph) 1) true 

				;remove start node from graph, find hamiltonian path of the
				;subgraph starting with each of the nodes linked to start
				:else 
					(let [	links 	 (graph start),
							subgraph (remove-node start graph)]
						(some #(path? subgraph %) links))))]

	(boolean (some #(path? (lev-graph) %) words))))

;test all cases
(println "TESTS " (if (= checks (map word-chain? allsets)) "PASS" "FAIL"))

