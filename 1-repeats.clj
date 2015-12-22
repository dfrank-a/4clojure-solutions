(defn ** [x n] 
  (cond
    (= n 0) 1 
    (= n 1) x
    :else 
    (let [half1 (quot n 2) half2 (- n half1)]
      (* (** x half1) 
         (** x half2)))))

(defn rep1s 
  ([] 
   (rep1s 1M))
  
  ([n] 
   (lazy-seq (cons (* n n) 
                   (rep1s (inc (* 10 n)))))))

(map println (take 11 (rep1s)))