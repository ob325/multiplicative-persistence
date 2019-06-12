(ns multiplicative-persistence.core
  (:gen-class))

(defn product-of-digits [x]
  (let [to-string (str x)
        partitioned (partition 1 to-string)
        flattened (flatten partitioned)
        to-digits (map #(Character/digit % 10) flattened)
        product (reduce * to-digits)]
    product))


(defn iterations [x]
  "Returns non-lazy iterations of x's persistence"
  (let [iters (iterate product-of-digits x)
        n-iters (count (take-while #(not= 0 %) (map - iters (rest iters))))
        iterations (take (inc n-iters) iters)]
    {:i x
     :iterations iterations
     :persistence n-iters
     :root (last iterations)}))

(defn lazy-iterations []
  (map iterations (range)))
  

(defn first-persistence-of [x]
  (doall (println (take 1 (filter #(= (:persistence %) x) (lazy-iterations))))))


(defn first-persistences-thru [x]
  (map first-persistence-of (range x)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
