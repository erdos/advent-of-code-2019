(ns day10)

(def positions
  (->> "/home/erdos/day10.txt"
       (clojure.java.io/reader)
       (line-seq)
       (map-indexed (fn [y line] (map-indexed (fn [x c] (when (= \# c) [x y])) line)))
       (apply concat)
       (remove nil?)
       (set)))

(defn nr-sights [[x y]]
  (->> (for [[xx yy] (disj positions [x y])]
         (Math/atan2 (- y yy) (- x xx)))
       (set)
       (count)))

(println "Solution 1:" (apply max (map nr-sights positions)))

(def pos (apply max-key nr-sights positions))

(defn angle [[xx yy]]
  (-> (Math/atan2  (- xx (pos 0)) (- yy (pos 1))  )
      (-)
      (+ Math/PI)
      (rem (* 2 Math/PI))))

(defn pos-dist [[x y]]
  (+ (Math/pow (- x (pos 0)) 2) (Math/pow (- y (pos 1)) 2)))

(->> (disj positions pos)
     (sort-by pos-dist)
     (group-by angle)
     (sort-by key)
     (map val)
     (iterate (partial keep next))
     (map (partial keep first))
     (apply concat)
     (#(nth % (dec 200)))
     (println "Second solution:"))
