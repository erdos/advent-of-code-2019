(ns day4)

(def input [124075 580769])

(def candidates
  (doall (for [i (range (first input) (second input))
               :let [s (str i)]
               :when (or (.contains s "00")
                         (.contains s "11")
                         (.contains s "22")
                         (.contains s "33")
                         (.contains s "44")
                         (.contains s "55")
                         (.contains s "66")
                         (.contains s "77")
                         (.contains s "88")
                         (.contains s "99"))
               :when (apply <= (map int s))]
           i)))

(->> candidates
     (count)
     (println "First solution:"))

(->> (map str candidates)
     (map (partial partition-by int))
     (filter (partial some (comp #{2} count)))
     (count)
     (println "Second solution: "))
