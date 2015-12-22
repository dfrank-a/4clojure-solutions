(defn rand100 [] (float (/ (int (* 100 (rand))) 100)))

(defn rand-neural-layer 
  "given n inputs and m outputs, create a list of m units
  containing n + 1 random weights 
  (first element is the threshold value)"
  [[inputs outputs]]
  (take outputs (repeatedly 
                  #(take (+ inputs 1) (repeatedly rand100)))))

(defn rand-neural-weights
  "takes list defining count of perceptrons in 
  layer [input hidden1 hidden2 ... hidden n output]
  returns random connection weights for each hidden 
  perceptron through ouput"
  [layers]
  (map rand-neural-layer
       (map vector layers (rest layers))))

(defn sigmoid 
  "sigmoid activation function"
  [x] 
  (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn perceptron 
  "given activation function and n+1 weights, 
  yields a perceptron function taking n inputs"
  [activation-fn weights]
  (fn 
    [inputs] 
    (activation-fn (reduce + 
                           (first weights)
                           (map * (rest weights) inputs)))))
(defn solve-layer
  "given seq of inputs and array of perceptrons, 
  return list of outputs yielded by applying each 
  perceptron to the inputs"
  [inputs perceptrons]
  (map #(% inputs) perceptrons))

(defn solve-network 
  "starting with inputs, apply each layer of perceptrons
  to yield output"
  [network input]
  )

(defn neural-network
  "given activation fn and seq-of-seqs weight definition,
  outputs functional representation of neural network"
  [activation-fn activation-deriv weights]
  (let [network (map #(map (partial perceptron activation-fn) %) weights)]
    (fn [keyw & input] 
      (cond (= keyw :eval) 
              (reduce #(solve-layer %1 %2) 
                      (first input) 
                      network)
              
            (= keyw :detail)
              (reverse (reductions #(solve-layer %1 %2) 
                                   (first input) 
                                   network))
            (= keyw :deriv) activation-deriv
            (= keyw :weights) 
              weights
            :else nil))))

;;USE EXAMPLE
(def rand-weights (rand-neural-weights [2 3 2]))
(def test-network (neural-network sigmoid rand-weights))

(map println 
     (test-network :weights))
   
(test-network :eval [1 1])

;;this doesn't work yet!!
(defn sigmoid-deriv [x] (* x (- 1 x)))

(defn error
  ([deriv outputs target]
   (map (fn [out targ] (* (deriv out) (- targ out))) outputs target))
  
  ([deriv outputs next-errors next-weights]
    (map *
         (map deriv outputs)
         (for [trans-weights (apply map list (map rest next-weights))]
           (reduce + (map * next-errors trans-weights))))))

 (defn backpropagate
   [network input target]
   (let [weights (reverse (network :weights))
         output  (network :test input)
         deriv   (network :deriv)]
     (reductions #(error deriv %1 %2) )
     ))


