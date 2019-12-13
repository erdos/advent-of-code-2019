(ns day13)

(-> "/home/erdos/day13.txt"
    (slurp)
    (str)
    (.split ",")
    (->> (map read-string))
    (vec)
    (->> (def code)))

(def ^:dynamic *input-fn*)
(def ^:dynamic *output-fn*)
(def ^:dynamic *relative-base* nil)

(defn parse-opcode [opcode]
  (let [[p3 p2 p1] (take-last 3 (drop-last 2 (str "0000" opcode)))]
    [(rem opcode 100)
     (case p1
       \0 (fn pos [code pos] (code (code (+ 1 pos))))
       \1 (fn imm [code pos] (code (+ 1 pos)))
       \2 (fn rel [code pos] (code (+ @*relative-base* (code (+ 1 pos))))))
     (case p2
       \0 (fn pos [code pos] (code (code (+ 2 pos))))
       \1 (fn imm [code pos] (code (+ 2 pos)))
       \2 (fn rel [code pos] (code (+ @*relative-base* (code (+ 2 pos))))))]))

(defn parse-target-pos [opcode]
  (let [[p3 p2 p1] (take-last 3 (drop-last 2 (str "0000" opcode)))]
    [(case p1
       \0 (fn pos [code pos] (code (+ 1 pos)))
       \1 (fn imm [code pos] (+ 1 pos))
       \2 (fn rel [code pos] (+ @*relative-base* (code (+ 1 pos)))))
     (case p2
       \0 (fn pos [code pos] (code (+ 2 pos)))
       \1 (fn imm [code pos] (+ 2 pos))
       \2 (fn rel [code pos] (+ @*relative-base* (code (+ 2 pos)))))
     (case p3
       \0 (fn pos [code pos] (code (+ 3 pos)))
       \1 (fn imm [code pos] (assert false "Not used!"))
       \2 (fn rel [code pos] (+ @*relative-base* (code (+ 3 pos)))))]))

(defn exec-intcode [code pos]
  (assert (sequential? code) (str "Not seq: " code))
  (assert (integer? pos))
  (assert (not (neg? pos)))
  (let [[opcode get-1 get-2] (parse-opcode (code pos))
        [set-1 _ set-3] (parse-target-pos (code pos))]

    (case opcode
      99 nil
      1 [(+ pos 4)
         (assoc code (set-3 code pos) (+ (get-1 code pos) (get-2 code pos)))]
      2 [(+ pos 4)
         (assoc code (set-3 code pos) (* (get-1 code pos) (get-2 code pos)))]
      3 [(+ pos 2)
         (assoc code (set-1 code pos) (*input-fn*))]
      4 [(+ pos 2)
         (do (*output-fn* (get-1 code pos))
             code)]

      5 (if (zero? (get-1 code pos))
            [(+ pos 3) code]
            [(get-2 code pos) code])
      6 (if-not (zero? (get-1 code pos))
            [(+ pos 3) code]
            [(get-2 code pos) code])
      7 (if (< (get-1 code pos) (get-2 code pos))
            [(+ pos 4) (assoc code (set-3 code pos) 1)]
            [(+ pos 4) (assoc code (set-3 code pos) 0)])
      8 (if (= (get-1 code pos) (get-2 code pos))
            [(+ pos 4) (assoc code (set-3 code pos) 1)]
            [(+ pos 4) (assoc code (set-3 code pos) 0)])

      9 [(+ pos 2)
         (do (swap! *relative-base* + (get-1 code pos))
             code)])))

(defn joystick [tiles]
  (let [[ball-x] (some #(when (= 4 (val %)) (key %)) tiles)
        [pad-x] (some #(when (= 3 (val %)) (key %)) tiles)]
    (compare ball-x pad-x)))

(defn eval-intcode [code]
  (let [tiles     (atom {[0 0] 1})
        output-state (atom -1)
        pos (atom [nil nil])
        score (atom 0)]
    (binding [*input-fn*  (fn [] (joystick @tiles))
              *output-fn*
             (fn [x]
               (case (dec (swap! output-state (fn [x] (rem (inc x) 3))))
                 -1 (swap! pos assoc 0 x)
                 0  (swap! pos assoc 1 x)
                 1  (if (= [-1 0] @pos)
                      (reset! score x)
                      (swap! tiles assoc @pos x))))
              *relative-base* (atom 0)]
    (loop [pos 0
           code code]
      (if-let [[next-pos code] (exec-intcode code pos)]
        (recur next-pos code)
        {:tiles @tiles
         :score @score})))))

(defn print-tiles [tiles]
  (doseq [y (range (apply min (map second (keys tiles)))
                   (inc (apply max (map second (keys tiles)))))]
    (doseq [x (range (apply min (map first (keys tiles)))
                     (inc (apply max (map first (keys tiles)))))]
      (print (case (tiles [x y] 0)
               0 " " ;; empty
               1 "#" ;; wall
               2 "B" ;; block
               3 "-" ;; hor paddle
               4 "o" ;; ball
               )))
    (println)))

(-> (mapv bigint (concat code (repeat 10000 0)))
    (eval-intcode)
    :tiles vals (->> (filter #{2N})) count
    (->> (println "First solution:")))

(-> (mapv bigint (concat (assoc code 0 2) (repeat 10000 0)))
    (eval-intcode)
    :score
    (->> (println "Second solution:")))
