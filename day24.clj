(ns day24)

(def input
  (-> "/home/erdos/day24.txt"
      (slurp)
      (.split "\n")
      (->> (mapv vec))))

(defn biodiversity [data]
  (reduce + (map *
                 (mapcat (partial map {\. 0 \# 1}) data)
                 (iterate (partial * 2) 1))))

(defn get-state [data x y]
  (get-in data [y x]))

(defn adjacent [data x y]
  (reduce + (for [[i j] [[-1 0] [1 0] [0 1] [0 -1]]]
              ({nil 0 \. 0 \# 1} (get-state data (+ x i) (+ y j))))))

(defn new-state [data x y]
  (case [(get-state data x y) (adjacent data x y)]
    [\. 1] \#
    [\. 2] \#
    [\# 1] \#
    \.))

(defn step [data]
  (mapv (fn [y row] (mapv (fn [x cell] (new-state data x y)) (range) row)) (range) data))

(defn first-repeating [xs]
  (reduce (fn [acc x] (if (acc x) (reduced x) (conj acc x))) #{} xs))

#_
(doseq [state (take 10 (iterate step input))]
  (doseq [row state]
    (doseq [c row]
      (print c))
    (println))
  (println \space))

(println (first-repeating (map biodiversity (iterate step input))))
