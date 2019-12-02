(defn exec-intcode [code pos]
  (case (code pos) 
    99 (reduced code)
    1 (assoc code 
        (code (+ pos 3))
        (+ (code (code (+ 1 pos))) (code (code (+ 2 pos)))))
    2 (assoc code
        (code (+ pos 3))
        (* (code (code (+ 1 pos))) (code (code (+ 2 pos)))))))

(defn intcode [code]
  (first (reduce exec-intcode
                 (assoc (vec code) 1 12 2 2)
                 (range 0 (count code) 4))))


(def input [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,13,27,1,6,27,31,1,9,31,35,2,10,35,39,1,39,6,43,1,6,43,47,2,13,47,51,1,51,6,55,2,6,55,59,2,59,6,63,2,63,13,67,1,5,67,71,2,9,71,75,1,5,75,79,1,5,79,83,1,83,6,87,1,87,6,91,1,91,5,95,2,10,95,99,1,5,99,103,1,10,103,107,1,107,9,111,2,111,10,115,1,115,9,119,1,13,119,123,1,123,9,127,1,5,127,131,2,13,131,135,1,9,135,139,1,2,139,143,1,13,143,0,99,2,0,14,0])

(intcode input)

(defn intcode [code noun verb]
  (first (reduce exec-intcode
                 (assoc (vec code) 1 noun 2 verb)
                 (range 0 (count code) 4))))

(doseq [noun (range 0 100)
        verb (range 0 100)
        :when (= 19690720 (intcode input noun verb))]
  (println (+ (* 100 noun) verb)))
