
(def input [124075 580769])

;; first star
(->> (range (first input) (second input))
     (map str candidates)
     (map (partial partition-by int))
     (filter (partial some (comp #{2 3 4 5 6} count)))
     (count))


;; second star
(->> (range (first input) (second input))
     (map str candidates)
     (map (partial partition-by int))
     (filter (partial some (comp #{2} count)))
     (count))
