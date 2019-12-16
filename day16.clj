(ns day16)

(defn base-seq [i]
  (let [i (inc i)]
    (next (cycle (concat (repeat i 0) (repeat i 1) (repeat i 0) (repeat i -1))))))

(defn lastdigit [n] (Math/abs (rem n 10)))

(defn fft [n]
  (let [ns (map read-string (map str (str n)))]
    (apply str
      (for [i (range (count ns))]
        (lastdigit (reduce + (map * ns (base-seq i))))))))

(defn repeat-fft [r n]
  (nth (iterate fft n) r))

;; 48226158
(println (repeat-fft 100 59731816011884092945351508129673371014862103878684944826017645844741545300230138932831133873839512146713127268759974246245502075014905070039532876129205215417851534077861438833829150700128859789264910166202535524896960863759734991379392200570075995540154404564759515739872348617947354357737896622983395480822393561314056840468397927687908512181180566958267371679145705350771757054349846320639601111983284494477902984330803048219450650034662420834263425046219982608792077128250835515865313986075722145069152768623913680721193045475863879571787112159970381407518157406924221437152946039000886837781446203456224983154446561285113664381711600293030463013N))

