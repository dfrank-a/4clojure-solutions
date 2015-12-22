(defn insert [[value & unsorted] sorted]
	(let [[r l] (split-with #(<= % value) sorted)] 
		[unsorted (concat (doall (concat r [value])) l)]))

(defn inssrt 
	([unsorted sorted]
		(if (not-empty unsorted)
			(let [[unsorted sorted] (insert unsorted sorted)]
				(recur unsorted sorted))
			sorted))
	([unsorted] (inssrt unsorted [])))

(defn mrg [seq1 seq2]
  (let [head1 (first seq1)
        head2 (first seq2)]
    (cond
      (nil? head1) seq2
      (nil? head2) seq1
      :else (if (< head1 head2)
              (cons head1 (lazy-seq (mrg (rest seq1) seq2)))
              (cons head2 (lazy-seq (mrg seq1 (rest seq2))))))))

(defn half-split [X] 
	(let [n (int (Math/ceil (/ (count X) 2)))] 
		(partition n n [] X)))

(defn mrgsrt [X]
  (if (> (count X) 1)
    (apply mrg (map mrgsrt (half-split X)))
    X))

(defn mrgsrt_i [X]
  (if (> (count X) 16)
    (apply mrg (map mrgsrt_i (half-split X)))
    (inssrt X)))

(def max-workers 64)
(defn mrgsrt_p [X]
  (defn log2 [x] (int (/ (Math/log x) (Math/log 2))))

  (defn mrgsrt_internal [n X]
  	(if (> (count X) 1)
    		(apply mrg 
    					((if (< 0 n) pmap map) 
    						(partial mrgsrt_internal (dec n)) 
    						(half-split X)))
    		(seq X)))

  (mrgsrt_internal (log2 max-workers) X))

(defn result+bench [fun & args]
	(defn nanotime [] (. java.lang.System (clojure.core/nanoTime)))

	(let [stime (nanotime)]
		[ (apply fun args) 
		  (/ (- (nanotime) stime)
		  	  1000000.0)]))

(defn rand_arr [n] (take n (repeatedly #(rand-int 1000000))))

(defn test_s [sortfunc unsorted & {:keys [tname] :or {name "undef"}}]
	(defn correct? [s u] (and (= (count s) (count u)) (apply <= s)))
	(defn check-result [sortfunc unsorted]
		(let [[sorted tms] (result+bench sortfunc unsorted)]
			(if (correct? sorted unsorted) 
		  		(str tms "\tpass") 
		  		(str tms "\tfail!\t" sorted))))

	(println (count unsorted) 
			"\t" tname 
			"\t" (check-result sortfunc unsorted)))

(defn test_t [fun Ns & {:keys [tname] :or {name "undef"}}]
	(doseq [n Ns] 
		(do
			(print )
			(test_s fun (rand_arr n))))
	(println))

(def X [100 1000 10000 50000 100000 500000])
(do
	;(test_t inssrt 		X :tname "insert"	)
	(test_t mrgsrt  	X :tname "merge" 	)
	(test_t mrgsrt_i 	X :tname "merge_i"	)
	(test_t mrgsrt_p 	X :tname "merge_p"	)
	)

