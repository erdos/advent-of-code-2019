(def code
  [3,225,1,225,6,6,1100,1,238,225,104,0,1002,43,69,224,101,-483,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,67,60,225,1102,5,59,225,1101,7,16,225,1102,49,72,225,101,93,39,224,101,-98,224,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1102,35,82,225,2,166,36,224,101,-4260,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,102,66,48,224,1001,224,-4752,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1001,73,20,224,1001,224,-55,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1102,18,41,224,1001,224,-738,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1101,68,71,225,1102,5,66,225,1101,27,5,225,1101,54,63,224,1001,224,-117,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1,170,174,224,101,-71,224,224,4,224,1002,223,8,223,1001,224,4,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1007,226,226,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,677,224,102,2,223,223,1006,224,344,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,374,101,1,223,223,8,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,7,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,419,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,434,101,1,223,223,1008,226,677,224,102,2,223,223,1006,224,449,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,494,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,1001,223,1,223,1107,226,226,224,102,2,223,223,1005,224,539,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,554,101,1,223,223,107,226,677,224,102,2,223,223,1005,224,569,1001,223,1,223,1108,226,677,224,1002,223,2,223,1005,224,584,1001,223,1,223,1107,226,677,224,1002,223,2,223,1005,224,599,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,614,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,629,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,644,101,1,223,223,107,677,677,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,677,226,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226])

(def ^:dynamic *input* [])
(def ^:dynamic *output* [])

(defn parse-opcode [opcode]
  (let [[p3 p2 p1] (take-last 3 (drop-last 2 (str "0000" opcode)))]
    [(rem opcode 100)
     (case p1
       \0 (fn pos [code pos] (code (code (+ 1 pos))))
       \1 (fn imm [code pos] (code (+ 1 pos))))
     (case p2
       \0 (fn pos [code pos] (code (code (+ 2 pos))))
       \1 (fn imm [code pos] (code (+ 2 pos))))
     (case p3
       ;; never called.
       \0 (fn pos [code pos] (code (code (+ 3 pos))))
       \1 (fn imm [code pos] (code (+ 3 pos))))]))

(defn exec-intcode [code pos]
  (assert (sequential? code) (str "Not seq: " code))
  (assert (integer? pos))
  (assert (not (neg? pos)))
  (let [[opcode get-1 get-2 get-3] (parse-opcode (code pos))]
    (case opcode
      99 nil
      1 [(+ pos 4)
         (assoc code (code (+ pos 3)) (+ (get-1 code pos) (get-2 code pos)))]
      2 [(+ pos 4)
         (assoc code (code (+ pos 3)) (* (get-1 code pos) (get-2 code pos)))]
      3 [(+ pos 2)
         (let [n (first @*input*)]
           (swap! *input* next)
           (assoc code (code (+ pos 1)) n))]
      4 [(+ pos 2)
         (do
            (swap! *output* conj (get-1 code pos))
            code)]
      ;; second part:
      5 (if (zero? (get-1 code pos))
            [(+ pos 3) code]
            [(get-2 code pos) code])
      6 (if-not (zero? (get-1 code pos))
            [(+ pos 3) code]
            [(get-2 code pos) code])
      7 (if (< (get-1 code pos) (get-2 code pos))
            [(+ pos 4) (assoc code (code (+ pos 3)) 1)]
            [(+ pos 4) (assoc code (code (+ pos 3)) 0)])
      8 (if (= (get-1 code pos) (get-2 code pos))
            [(+ pos 4) (assoc code (code (+ pos 3)) 1)]
            [(+ pos 4) (assoc code (code (+ pos 3)) 0)]))))

(defn eval-intcode [code input]
  (binding [*input*  (atom (seq input))
            *output* (atom [])]
    (loop [pos 0
           code code]
      (if-let [[next-pos code] (exec-intcode code pos)]
        (recur next-pos code)
        {:output (last @*output*)}))))

(eval-intcode code [1])
(eval-intcode code [5])
