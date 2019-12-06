(def input
  (vec (for [line (line-seq (clojure.java.io/reader "/home/erdos/day6.txt"))]
         (vec (.split line "\\)")))))

(def node->children
  (reduce (fn [m [from to]] (update m from conj to)) {} input))

(def nodes (set (mapcat vec input)))

(defn topsort [children roots]
  (loop [roots roots visited #{} output ()]
    (if-let [xs (seq (set (remove (set roots) (remove visited (mapcat children roots)))))]
      (recur xs (into visited roots) (into output roots))
      output)))

(def roots (remove (into {} (for [[k vs] node->children v vs] [v k])) nodes))

(def topsorted (topsort node->children roots))

(def length
  (memoize
   (fn [node]
     (let [ch (node->children node)]
       (reduce + (count ch) (map length ch))))))

;; to prevent stack overflow
(run! length topsorted)

(println "First answer:" (reduce + (map length nodes)))

(def parents (into {} (for [[n ch] node->children c ch] [c n])))

(def you-path (reverse (take-while some? (iterate parents "YOU"))))
(def san-path (reverse (take-while some? (iterate parents "SAN"))))

(def common-len (count (take-while true? (map = you-path san-path))))

(-> (count you-path)
    (+ (count san-path))
    (- common-len) (- common-len)
    (- 2)
    (->> (println "Second answer:")))
