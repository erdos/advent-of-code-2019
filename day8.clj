(ns day7)

(def w 25)
(def h 6)

(def image (slurp "/home/erdos/day8.txt"))

(def layers (partition (* w h) image))

(def min-layer (apply min-key (comp count (partial filter #{\0})) layers))

(->>
 (* (count (filter #{\1} min-layer))
    (count (filter #{\2} min-layer)))
 (println "First solution:"))

(defn merge-pixel [b a] (if (= \2 b) a b))

(def total (reduce (partial mapv merge-pixel) layers))

(doseq [row (partition-all w total)]
  (doseq [c row]
    (print (case c \0 "#" \1 "." \2 " ")))
  (println))
