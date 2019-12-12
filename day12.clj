(ns day12)

(def state
  [{:pos [-7 -8 +9]  :vel [0 0 0]}
   {:pos [-12 -3 -4] :vel [0 0 0]}
   {:pos [+6 -17 -9] :vel [0 0 0]}
   {:pos [+4 -10 -6] :vel [0 0 0]}])

(defn add [a b] (mapv + a b))

(defn step [state]
  (doall
   (for [m state
         :let [new-velocities
               (reduce
                add
                (:vel m)
                (for [n state] (mapv compare (:pos n) (:pos m))))]]
     (-> m
         (assoc :vel new-velocities)
         (update :pos add new-velocities)))))

(defn norm [xs] (transduce (map #(Math/abs (double %))) + xs))

(defn total [moon] (* (norm (:vel moon)) (norm (:pos moon))))
(defn total-system [state] (reduce + (map total state)))

(println "Total energy:" (total-system (nth (iterate step state) 1000)))

(defn third [x] (nth x 2))
(defn gcd [a b] (if (zero? b) a (recur b, (mod a b))))
(defn lcm [& v] (reduce (fn [a b] (/ (* a b) (gcd a b))) v))

(defn first-repeat [step state proj-fn]
  (reduce (fn [acc state]
            (if (contains? acc state)
              (reduced (count acc))
              (conj acc state)))
          #{} (map proj-fn (iterate step state))))

(let [offsets (for [proj [first second third]]
                (first-repeat step state (partial mapv (juxt (comp proj :pos) (comp proj :vel)))))]
  (println "Second solution:" (apply lcm offsets)))
