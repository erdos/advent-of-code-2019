(ns day14)

(defn parse-line [line]
  (let [[from to] (.split line " => ")
        from (for [x (.split from ", ")
                   :let [[a b] (.split x " ")]]
               [b (read-string a)])
        [amt to] (.split to " ")]
    {:to     to
     :amount (read-string amt)
     :from   (into {} from)}))

(-> "/home/erdos/day14.txt"
    (slurp)
    (str)
    (clojure.string/split-lines)
    (->> (map parse-line))
    (vec)
    (->> (def recipes)))

(def type->recipe (into {} (map (juxt :to identity) recipes)))

(defn multi [kell van]
  (assert (integer? kell))
  (assert (integer? van))
  (if (zero? (rem kell van))
    (quot kell van)
    (inc (quot kell van))))

;; 1 (multi 1 1)
;; 1 (multi 4 4)
;; 3 (multi 11 4)
;; 2 (multi 8 4)

(defn topsort [deps]
  (loop [out    (mapv key (filter (comp empty? val) deps))
         others (set (remove (set out) (keys deps)))]
    (if (seq others)
      (when-let [kk (seq (remove  #(some others (deps %)) others))]
        (recur (into out kk) (reduce disj others kk)))
      out)))

(defn cost-of-fuel [fuel]
  (->
   (reduce (fn [m [k v]] (update m k conj v))
           (sorted-map "FUEL" [])
           (for [[k recipe] type->recipe
                 [j] (:from recipe)]
             [j k]))
   (topsort)
   (->>
    (reduce (fn [needed item]
              (let [req-amt    (needed item)
                    recipe     (or (type->recipe item) {:amount 1})
                    multiplier (multi req-amt (:amount recipe))]
                (merge-with +
                            needed
                            (into {} (for [[a b] (:from recipe)]
                                       [a (* multiplier b)])))))
            {"FUEL" fuel}))
   (get "ORE")))

(def cargo 1000000000000)

;; last item where pred is still true
(defn search
  ([pred from]
   (assert (pos? from))
   (search pred from (first (drop-while pred (iterate #(* 2 %) from)))))
  ([pred from to]
   (assert (pred from))
   (assert (false? (pred to)))
   (loop [from from, to to]
     (if (= from (dec to))
       from
       (let [half (quot (+ from to) 2)]
         (if (pred half)
           (recur half to)
           (recur from half)))))))



(println "Solution 1" (cost-of-fuel 1))

(->>
 (search #(< (cost-of-fuel %) cargo) 1)
 (println "Second solution:"))
