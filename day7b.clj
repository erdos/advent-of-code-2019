(ns advent-of-code.day7b)

(defn permutations [n]
  (if (zero? n)
    [[0]]
    (for [pn-1 (permutations (dec n))
          i (range (inc n))
          :let [before (take i pn-1)
                after (drop i pn-1)]]
      `[~@before ~n ~@after])))

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
       \1 (fn imm [code pos] (code (+ 2 pos))))]))

(defn exec-intcode [code pos]
  (assert (sequential? code) (str "Not seq: " code))
  (assert (integer? pos))
  (assert (not (neg? pos)))
  (let [[opcode get-1 get-2] (parse-opcode (code pos))]
    (case opcode
      99 nil
      1 [(+ pos 4)
         (assoc code (code (+ pos 3)) (+ (get-1 code pos) (get-2 code pos)))]
      2 [(+ pos 4)
         (assoc code (code (+ pos 3)) (* (get-1 code pos) (get-2 code pos)))]
      3 [(+ pos 2)
         (assoc code (code (+ pos 1)) (.take *input*))]
      4 [(+ pos 2)
         (do (.put *output* (get-1 code pos))
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

(defn eval-intcode [code input-queue output-queue]
  (binding [*input*  input-queue
            *output* output-queue]
    (loop [pos 0
           code code]
      (if-let [[next-pos code] (exec-intcode code pos)]
        (recur next-pos code)
        {:output *output*}))))

;; day 7

(def day7-code [3,8,1001,8,10,8,105,1,0,0,21,34,51,64,73,98,179,260,341,422,99999,3,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,1001,9,4,9,1002,9,3,9,1001,9,5,9,4,9,99,3,9,101,5,9,9,102,5,9,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,1002,9,5,9,1001,9,3,9,102,2,9,9,101,5,9,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99])


(defn pipeline [code [p1 p2 p3 p4 p5]]
  (let [input-queue (new java.util.concurrent.ArrayBlockingQueue 1 true [p1])
        queue-1     (new java.util.concurrent.ArrayBlockingQueue 1 true [p2])
        queue-2     (new java.util.concurrent.ArrayBlockingQueue 1 true [p3])
        queue-3     (new java.util.concurrent.ArrayBlockingQueue 1 true [p4])
        queue-4     (new java.util.concurrent.ArrayBlockingQueue 1 true [p5])

        machine1    (future (eval-intcode code input-queue queue-1))
        machine2    (future (eval-intcode code queue-1 queue-2))
        machine3    (future (eval-intcode code queue-2 queue-3))
        machine4    (future (eval-intcode code queue-3 queue-4))
        machine5    (future (eval-intcode code queue-4 input-queue))
        ]
    (.put input-queue 0) ;; init signal

    @machine5 ;; wait for machines to halt
    @machine1

    (last (vec input-queue))))

(apply max
  (for [perm (map (partial mapv (partial + 5)) (permutations 4))]
    (pipeline day7-code perm)))
