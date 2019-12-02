(defn fuel [x] (max 0 (- (Math/floor (/ x 3)) 2)))

;; (int (reduce + (map fuel ...)))

(defn total-fuel [x] (apply + (take-while pos? (next (iterate fuel x)))))

;; (int (reduce + (map total-fuel (map read-string (clojure.string/split-lines (slurp "/Users/janos.erdos/input.txt"))))))
