(ns day11)

(def input [3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,28,1,1104,0,10,1006,0,71,2,1002,5,10,2,1008,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,66,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,87,1006,0,97,2,1002,6,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,116,1006,0,95,1,1009,10,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,145,1,1002,19,10,2,1109,7,10,1006,0,18,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,179,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,200,1,1105,14,10,1,1109,14,10,2,1109,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,235,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,257,2,101,9,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,282,2,1109,19,10,1,105,0,10,101,1,9,9,1007,9,1033,10,1005,10,15,99,109,633,104,0,104,1,21102,937268368140,1,1,21102,328,1,0,1106,0,432,21102,1,932700599052,1,21101,0,339,0,1105,1,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,209421601831,1,21102,1,386,0,1106,0,432,21102,235173604443,1,1,21102,1,397,0,1106,0,432,3,10,104,0,104,0,3,10,104,0,104,0,21101,825439855372,0,1,21102,1,420,0,1106,0,432,21101,0,988220907880,1,21102,431,1,0,1106,0,432,99,109,2,22101,0,-1,1,21101,40,0,2,21102,1,463,3,21102,453,1,0,1106,0,496,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1102,1,0,458,109,-2,2106,0,0,0,109,4,2102,1,-1,495,1207,-3,0,10,1006,10,513,21102,0,1,-3,22102,1,-3,1,21202,-2,1,2,21102,1,1,3,21101,532,0,0,1105,1,537,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,21201,-4,0,-4,1106,0,628,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,1,579,0,1106,0,537,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,598,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,21201,-1,0,1,21102,1,620,0,105,1,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0])

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

      9
      [(+ pos 2)
       (do (swap! *relative-base* + (get-1 code pos))
           code)])))

(def next-dir {:up    [:left :right]
               :left  [:down :up]
               :down  [:right :left]
               :right [:up :down]})

(defn eval-intcode [code]
  (let [direction (atom :up)
        tiles     (atom {[0 0] 1})
        position  (atom [0 0])
        output-color (atom nil)]
  (binding [*input-fn*  (fn [] (@tiles @position 0))
            *output-fn* (fn [x]
               (if @output-color
                (do
                  (swap! direction (fn [dir] (get-in next-dir [dir x])))
                  (case @direction
                      :up    (swap! position (fn [[x y]] [x (dec y)]))
                      :left  (swap! position (fn [[x y]] [(dec x) y]))
                      :right (swap! position (fn [[x y]] [(inc x) y]))
                      :down  (swap! position (fn [[x y]] [x (inc y)])))
                  (reset! output-color nil))
                (do (reset! output-color x)
                    (swap! tiles assoc @position @output-color))))
            *relative-base* (atom 0)]
    (loop [pos 0, code code]
      (if-let [[next-pos code] (exec-intcode code pos)]
        (recur next-pos code)
        {:tiles @tiles})))))

(defn print-tiles [tiles]
  (doseq [y (range (apply min (map second (keys tiles)))
                   (inc (apply max (map second (keys tiles)))))]
    (doseq [x (range (apply min (map first (keys tiles)))
                     (inc (apply max (map first (keys tiles)))))]
        (print ({0 "." 1 "#"} (tiles [x y] 0))))
    (println)))

(-> (mapv bigint (concat input (repeat 10000 0)))
    (eval-intcode)
    :tiles print-tiles)
