(defn parse [^String x]
  (mapv
   (fn [c]
     (cond (.startsWith c "R") [:right (Long/parseLong (.substring c 1))]
           (.startsWith c "D") [:down (Long/parseLong (.substring c 1))]
           (.startsWith c "L") [:left (Long/parseLong (.substring c 1))]
           (.startsWith c "U") [:up (Long/parseLong (.substring c 1))]
           :else (assert false)))
   (.split x ",")))

(defn coords [path]
  (assert (vector? path))
  (reductions (fn [[x y] direction]
                (case direction
                  :right [(inc x) y]
                  :left  [(dec x) y]
                  :up    [x (dec y)]
                  :down  [x (inc y)]))
              [0 0]
              (mapcat (fn [[dir length]] (repeat length dir)) path)))

(defn taxi [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solve [s1 s2]
  (let [p1 (parse s1)
        p2 (parse s2)
        c1 (set (coords p1))
        c2 (set (coords p2))]
    (apply min (map taxi (disj (clojure.set/intersection c1 c2) [0 0])))))

(let [[in1 in2] (clojure.string/split-lines (slurp "/home/erdos/input3.txt"))]
  {:day3-first (solve in1 in2)})

;; -----------------------

(defn path-intersections [path1 path2]
    (let [c1 (set (coords path1))
          c2 (set (coords path2))]
      (disj (clojure.set/intersection c1 c2) [0 0])))

(defn coords-steps [path] (mapv vector (coords path) (range)))

(let [[p1 p2] (map parse (clojure.string/split-lines (slurp "/home/erdos/input3.txt")))
      s1 (coords-steps p1)
      s2 (coords-steps p2)]
  {:day3-second
   (apply min
          (for [ci (path-intersections p1 p2)
                :let [f (comp (partial some second)
                              (partial filter (comp #{ci} first)))]]
            (+ (f s1) (f s2))))})
