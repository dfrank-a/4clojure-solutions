;;solution for problem specified at
;;http://www.4clojure.com/problem/79

; Write a function which calculates the sum of the minimal path 
; through a triangle. The triangle is represented as a collection 
; of vectors. The path should start at the top of the triangle 
; and move to an adjacent number on the next row until the bottom of 
; the triangle is reached.

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

(def L 0)
(def R 1)

(defn tri-child 
  "fn gets right or left child index"
  [[level indx] lr]            
  [(inc level) (+ lr indx)])

(defn tri-children 
  "fn returns index for the two children: empty when no children"
  [[level indx :as C] tri] 
  (if  (< level (dec (count tri))) 
       (map (partial tri-child C) [L R])))

(defn tri-value 
  "fn gets value of node"
  [[level indx] tri] 
  ((nth tri level) indx))

(defn tri-min-cost
  "fn finds minimum cost from top of triangle to bottom"
  ;start search from a point in triangle
  ([tri C]
    (+  (tri-value C tri)
        (let  [children (map #(tri-min-cost tri %) (tri-children C tri))] 
          (if (not-empty children) 
              (apply min children) 
              0))))

  ;start search from top
  ([tri] (tri-min-cost tri [0 0])))

(defn check 
  "given function, expected output and arguments, perform check"
  [fun exp & args]
  (println "Given args: " args)
  (let [ret (apply fun args)]
    (println "got: " ret)
    (println "exp: " exp)
    (println (if (= ret exp) "PASS" "FAIL") "\n")
    (= ret exp)))

(every? true? (map #(apply check tri-min-cost %) [[7 tri1] [20 tri2]]))

