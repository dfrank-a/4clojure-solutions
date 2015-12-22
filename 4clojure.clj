(fn [sum-me]
	(loop [[x & xrest :as X] (seq sum-me) sum 0]
		(if (seq X) 
			(recur xrest (+ sum x)) 
			sum)))

(summate [1 2 3 4])

(defn fibo [x]
	(loop [x x f1 0 f2 1 fs []]
		(if (> x 0)
			(recur (dec x) f2 (+ f1 f2) (conj fs f2))
			fs)))

(fibo 0)
(fibo 1)
(fibo 2)
(fibo 3)
(fibo 10)

(defn palin [coll]
	(loop [[x & xrest :as X] coll]
		(if (seq xrest)
			(and (= x (last xrest)) 
				 (palin (take (dec (count xrest)) xrest)))
			true)))

(palin "a")
(palin "ab")
(palin "racecar")

(defn my-flatten [dstr]
	(loop [[q & qrest :as Q] dstr out []]
		(if (seq Q)
			(recur qrest (concat out (if (sequential? q) (my-flatten q) [q]))
			out))))

(my-flatten '((1 2) 3 [4 [5 6]]))

(defn rem-consec [in]
	(loop [[x1 & [x2 & xr :as xrest] :as X] in, out []]
		(if (seq X)
			(recur xrest (if (= x1 x2) out (concat out [x1])))
			out)))

(apply str (rem-consec "Leeeeeeroyyy"))

(defn pack [in]
	(loop [[x1 & [x2 & xr :as xrest] :as X] in, out [], ident `(~x1)]
		(if (seq X)
			(if (= x1 x2)
				(recur xrest out (conj ident x2))
				(recur xrest (concat out [ident]) `(~x2)))
			out)))

(pack [1 1 2 1 1 1 3 3 1])

(defn dup-all [in]
	(loop [[x1 & xrest :as X] in, out []]
		(if (seq X) 
			(recur xrest (conj out x1 x1)) 
			out)))

(dup-all [1 2 3])

(defn dup-n [in n]
	(loop [[x1 & xrest :as X] in, out []]
		(if (seq X) 
			(recur xrest (concat out (for [i (range n)] x1))) 
			out)))

(dup-n [1 2 3] 3)

(defn find-max [in1 & inrest]
	(loop [[x1 & xrest :as X] inrest, vmax in1]
		(if (seq X)
			(recur xrest (if (< vmax x1) x1 vmax))
			vmax)))

(defn inter [s1 s2]
	(loop [[x1 & xrest :as X] s1, [y1 & yrest :as Y] s2, out []]
		(if (and (seq X) (seq Y))
			(recur xrest yrest (concat out [x1 y1]))
			out)))

(defn interps [s1 s2]
	(loop [[x1 & xrest] s1, out []]
		(if (seq xrest)
			(recur xrest yrest (concat out [x1 s1]))
			out)))

(defn drop-nths [s1 n]
	(loop [[x1 & xrest :as X] s1, out [], i 1]
		(if (seq X)
			(recur 	xrest 
					(if (= (mod i n) 0) out (concat out [x1]))
					(inc i))
			out)))

(defn factorial [n] 
	(loop [i n, result 1]
		(if (<= i 0)
			result
			(recur (dec i) (* result i)))))
	
(factorial 3)

(defn de-inter [S n]
	(loop [i 0, [x1 & xrest :as X] S, out []]
		(println i X out)
		(if (seq X)
			(recur (inc i) xrest 
				(map #(concat (nth out % '()) (if (= % (mod i n)) [x1] [])) (range n)))
			out)))

(de-inter [1 2 3 4 5 6] 2)

(defn rot [n S]
	(loop [i (mod n (count S)) [x1 & xrest :as X] S]
		(if (> i 0)
			(recur (dec i) (concat xrest [x1]))
			X)))

(rot -4 '(:a :b :c))

(defn split [n S]
	[(take n S) (drop n S)])


(defn type-split [S]
	(loop [[x1 & xrest :as X] S, out {}]
		(if (seq X)
			(let [typ (type x1) cont (get out typ [])]
				(recur xrest (conj out [typ (conj cont x1)])))
			(vals out))))
	

(type-split [1 :a 2 :b 3 :c])

(defn longest [S]
	(loop [	[x1 & [x2 & xs :as xrest] :as X] S, 
			sub [x1]]

			(if (seq xrest)
				(if (< x1 x2) (recur xrest (concat sub [x2]))
					
					(let [	my-len (count sub), 
							nx (longest xrest), 
							nx-len (count nx)]

						(let [out (if (>= my-len nx-len) sub nx)]
							(if (= (count out) 1) [] out)))
				)
			sub)))

(defn cmp [fun & rfuns]
  (if (seq rfuns) 
  	(fn [& x] (fun (apply (apply cmp rfuns) x))) 
  	(fn [& x] (apply fun x))))

(= [3 2 1] ((cmp rest reverse) [1 2 3 4]))

(defn jxt [& X]
	(fn [& x] 
		(loop [[f & frest :as F] X, out []]
			(if (seq F)
				(recur frest (concat out [(apply f x)]))
				out))))

((jxt + max min) 2 3 5 1 6 4)

(defn lazy-reduce 
	([Fx [s1 & srest :as S]] 
     (lazy-reduce Fx (Fx s1) srest))

	([Fx xn-1 [s1 & srest :as S]]
			(cons xn-1 (lazy-seq 
				(if (seq S) 
					(lazy-reduce Fx (Fx xn-1 s1) srest))))))


(take 5 (laz-red + (range)))
(take 2 (laz-red conj [1] [2 3 4]))

(defn zip-map [X Y]
	(loop [[k1 & krest :as K] X, [v1 & vrest :as V] Y, out {}]
		(if (and (seq K) (seq V))
			(recur krest vrest (conj out [k1 v1]))
			out)))

(defn iter [Fx x0] (cons x0 (lazy-seq (iter Fx (Fx x0)))))

(defn group [Fx V]
	(loop [[v1 & vrest :as Vs] V, out {}]
		(if (seq Vs)
			(let [Y (Fx v1)]
				(recur vrest (conj out [Y, (conj (get out Y []) v1) ])))
		out)))

(group #(> % 5) [1 3 6 8])

(group #(apply / %) [[1 2] [2 4] [4 6] [3 6]])

(defn gcd [a b]
	(println a b)
	(if(= 0 b) a (recur b (mod a b))))

(defn prime? [n] 
	(let [rootN (Math/sqrt n)]
		(loop [i 2]
			(if (<= i rootN)
				(if (= 0 (mod n i)) false (recur (inc i)))
				true))))

(take 5 (for [x (drop 2 (range)) 
	:when (let [rootN (Math/sqrt x)]
			(loop [i 2]
				(if (<= i rootN)
					(if (= 0 (mod x i)) false (recur (inc i)))
					true)))] x))
	
(defn fjoin [func map1 & [m1 & mrest :as maps]]
  	(if (seq maps)
      (recur func 
        (loop [[[k v :as item] & m1rest :as M] (seq m1), out map1]
          (if (seq M)
            (if (contains? out k) 
              (recur m1rest (conj out [k (func (out k) v)]))
              (recur m1rest (conj out item)))
             out))
         mrest)
      map1))

(fjoin * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})

(def board [[:x :e :o]
            [:x :e :e]
            [:x :e :o]])


(defn check-win [board]
	(let [
			rows (for [x [0 1 2]] (map (partial conj [x]) (range 3))),
			cols (for [c rows] (map reverse c)),
			diag [ [[0 0] [1 1] [2 2]], [[0 2] [1 1] [2 0]] ],
			runs (lazy-cat diag rows cols),
          
			get-win-set (fn [coords] (set (for [[r c] coords] ((board r) c)))),
          
            win?		(fn [s] 
                            (cond 
                              (= #{:x} s) :x 
                              (= #{:o} s) :o 
                              :else nil)) 
         ]

		(some #(win? (get-win-set %)) runs)))


(defn perf-sq [s] 
  (apply str 
  	(interpose "," 
  		(filter 
  			#(= (Math/sqrt %) 
  				(float (int (Math/sqrt %)))) 
             (map 
             	#(Integer. %) 
             	(re-seq #"\b\d+\b" s))))))

(defn totient [x]
	(let [gcd (fn [a b] (if(= 0 b) a (recur b (mod a b))))]
		(count 
			(for [i (range x) :when (= 1 (gcd x i))] i))))

(def anas ["veer" "lake" "item" "kale" "mite" "ever"])

(defn anagram-set [input] 
  (letfn [(anag [s] (sort (seq s)))
          (enough? [x] (< 1 (count x)))]
	(set (map set (filter enough? (vals (group-by anag input)))))))

(defn tramp [fun & args]
  (loop [out (apply fun args)]
    (if (clojure.test/function? out) 
      (recur (fun))
      out)))

(fn min-path [tri]
  (letfn [(sub-triangles [[_ & trest :as T]]
                         (loop [n 1, [v & rvecs :as V] trest, T1 (list), T2 (list)]
                           (if (seq V)
                             (recur (inc n) 
                                    rvecs 
                                    (cons T1 (vec (take n v))) 
                                    (cons T2 (vec (drop 1 v))))
                             (list T1 T2))))
                           
    ))

(def tri1 '([1]
           [2 4]
          [5 1 4]
         [2 3 4 5]))

(def tri2 '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4]))

(defn min-path [tri]
	(let   [ max-level (dec (count tri)), L 0, R 1 ]
	(letfn [
			;fn gets right or left child index
			(tri-child [lr [level indx]] 
					   (vec (inc level) 
					   		(+ lr indx))),

			;fn returns index for the two children: empty when no children
			(tri-children [[level indx :as C]] 
						  (if  (< level max-level) 
						       (list (tri-child L C) 
						       	     (tri-child R C))
						       nil))

			;fn gets value of node
			(tri-value [ [level indx] ] 
				       ((nth tri level) indx)),

			;fn adds current value and the min-path result 
			;of serching on each child
			(search [C]
				(let [ [<< >> :as childs] (tri-children C)]
				(cond 
					;I have no children, return my value
					(empty? childs) (tri-value C)

					;add my value to the least value of my children
					:else (+ 	(tri-value C)      	      
								(min (search <<) (search >>))) )))]

			;start search at top of triangle
			(search [0 0]))))

(println (map min-path [tri1 tri2]))
(sub-triangles tri1)
(sub-triangles tri2)

(defn perfect? [x]
	(= x (apply + 1 (for [i (range 2 (inc (quot x 2))) :when (= 0 (mod x i))] i))))


(defn not-all [ & args ]
	(and (reduce #(or %1 %2) (first args) args)
		 (not (reduce #(and %1 %2) (first args) args))))




