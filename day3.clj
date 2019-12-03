(defn parse [^String x]
  (mapv (fn [c] [(first c) (Long/parseLong (.substring c 1))]) (.split x ",")))

(defn coords [path]
  (assert (vector? path))
  (reductions (fn [[x y] direction]
                (case direction
                  \R [(inc x) y]
                  \L [(dec x) y]
                  \U [x (dec y)]
                  \D [x (inc y)]))
              [0 0]
              (mapcat (fn [[dir length]] (repeat length dir)) path)))

(defn taxi [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(let [[s1 s2] (clojure.string/split-lines (slurp "/home/erdos/input3.txt"))
      c1 (set (coords (parse s1)))
      c2 (set (coords (parse s2)))
      solution (apply min (map taxi (disj (clojure.set/intersection c1 c2) [0 0])))]
  (println "Day 3, first star:" solution))

(defn path-intersections [path1 path2]
  (let [c1 (set (coords path1))
        c2 (set (coords path2))]
    (disj (clojure.set/intersection c1 c2) [0 0])))

(defn coords-steps [path]
  (mapv vector (coords path) (range)))

(let [[p1 p2] (map parse (clojure.string/split-lines (slurp "/home/erdos/input3.txt")))
      s1 (coords-steps p1)
      s2 (coords-steps p2)
      solution (apply min
                      (for [ci (path-intersections p1 p2)
                            :let [f (comp (partial some second)
                                          (partial filter (comp #{ci} first)))]]
                        (+ (f s1) (f s2))))]
  (println "Day 3, second star:" solution))
