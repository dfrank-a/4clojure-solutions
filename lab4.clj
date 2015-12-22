;;Domenic Ariaudo - Lab 4

(defn insert [value sorted]
  ;get left and right sides w/ left <= value and right < value
  (let [[r l] (split-with #(<= % value) sorted)] 
    (concat (doall (concat r [value])) l))) 

(defn insertion-sort
  ;inner functon
	([[value & urest :as unsorted] sorted]
		(if (not-empty unsorted)
				(recur urest (insert value sorted))
			   sorted))

  ;fn to be called by user
	([unsorted] (insertion-sort unsorted [])))

(defn mrg [[head1 & rest1 :as seq1] 
           [head2 & rest2 :as seq2]]
    (cond
      (nil? head1) seq2
      (nil? head2) seq1
      :else (if (< head1 head2)
              (cons head1 (lazy-seq (mrg rest1 seq2)))
              (cons head2 (lazy-seq (mrg seq1 rest2))))))

(defn half-split [X] 
	(let [n (int (Math/ceil (/ (count X) 2)))] 
		(partition n n [] X)))

(defn merge-sort [X]
  (if (> (count X) 1)
    (apply mrg (map merge-sort (half-split X)))
    X))

;get running time in nanoseconds, return sorted list and time in a tuple
(defn result+bench [fun & args]
  (defn nanotime [] (. java.lang.System (clojure.core/nanoTime)))

  (let [stime (nanotime)]
    [ (apply fun args) 
      (/ (- (nanotime) stime)
          1000000.0)]))

;given sort function, unsorted list and sorted name,
;print out benchmark and check if correct
(defn test-sort [sortfunc unsorted sortname]
  (defn correct? [s u] 
                 (and (= (count s) 
                         (count u)) 
                      (apply <= s)))

  (defn check-result [sortfunc unsorted]
    (let [[sorted tms] (result+bench sortfunc unsorted)]
      (if (correct? sorted unsorted) 
          (str tms "\tpass") 
          (str tms "\tfail!\t" sorted))))

  (println (count unsorted) 
      "\t" sortname
      "\t" (check-result sortfunc unsorted)))

(defn rand-arr [n] (take n (repeatedly #(rand-int 1000000))))

;sizes of random arrays to be generated
(def Ns [100 1000 10000 50000 100000 200000])
;sort functions to be called
(def titles ["merge" "insert"])
(def sorts [merge-sort insertion-sort])

(loop [[n & nrest :as N] Ns]
      (if (seq N)
          (do 
            (let [testarr (rand-arr n)]
                 (loop [[s & srest :as S] sorts
                        [t & trest :as T] titles]
                       (if (and (seq S) (seq T))
                           (do 
                               (test-sort s testarr t)
                               (recur srest trest)))))
            (recur nrest))))




