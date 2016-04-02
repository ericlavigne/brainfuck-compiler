(ns brainfuck)

(defn tokenize [source]
  (loop [compiled [] remaining source]
    (let [next (first remaining)]
      (cond (empty? remaining)
              compiled
            (some #{next} "[]")
              (recur (conj compiled [(str (first remaining))]) (rest remaining))
            (some #{next} "+-")
              (let [taken (take-while (set "+-") remaining)
                    sum (reduce + (map {\+ 1 \- -1} taken))
                    sum-mod-256 (rem (+ 256 (rem sum 256)) 256)]
                (recur (conj compiled ["+" sum-mod-256])
                       (drop-while (set "+-") remaining)))
            (some #{next} "<>.,")
              (recur (conj compiled
                           [(str next)
                            (count (take-while #{next} remaining))])
                     (drop-while #{next} remaining))
            :else (str "Unrecognized character: " next)))))

(defn match-brackets [tokens]
  (let [left-positions (filter #(= ["["] (get tokens %))
                               (range (count tokens)))
        right-positions (filter #(= ["]"] (get tokens %))
                                (range (count tokens)))
        bracket-matches (merge (zipmap left-positions right-positions)
                               (zipmap right-positions left-positions))]
    (vec (map (fn [token index]
                (cond (= token ["["]) ["[" (bracket-matches index)]
                      (= token ["]"]) ["]" (bracket-matches index)]
                      :else token))
              tokens
              (range (count tokens))))))

(def cache-loops-to-finish (atom {}))

(defn precalculate-loops-to-finish [inc-per-loop]
  (let [cached-answers (@cache-loops-to-finish inc-per-loop)]
    (when (nil? cached-answers)
      (let [reverse-inc-per-loop (rem (- 256 inc-per-loop) 256)]
        (loop [current 0 steps 0 history {}]
          (if (history current)
            (swap! cache-loops-to-finish assoc inc-per-loop history)
            (recur (rem (+ current reverse-inc-per-loop)
                        256)
                   (inc steps)
                   (assoc history current steps))))))))

(defn loops-to-finish [start-value inc-per-loop]
  (let [cached-answers (@cache-loops-to-finish inc-per-loop)]
    (cached-answers start-value)))

(defn compile-loop-contents-to-loop [tokens]
  (loop [offset 0 remaining tokens offsets-to-increments {0 0}]
    (let [[op op-data] (first remaining)
          next-remaining (rest remaining)]
      (cond (empty? remaining)
              (do (assert (= 0 offset)
                          (str "Loop optimization requires balancing < and > "
                               "(offset is " offset ")"))
                  (precalculate-loops-to-finish (get offsets-to-increments 0))
                  ["loop" {:start (get offsets-to-increments 0)
                           :others (dissoc offsets-to-increments 0)}])
            (= op "+")
              (recur offset next-remaining
                     (update-in offsets-to-increments [offset]
                                #(rem (+ op-data (or % 0)) 256)))
            (= op ">")
              (recur (+ offset op-data) next-remaining offsets-to-increments)
            (= op "<")
              (recur (- offset op-data) next-remaining offsets-to-increments)
            :else (assert false (str "Unrecognized token " op))))))

(defn optimize-incrementing-loops [tokens]
  (loop [optimized [] remaining tokens in-loop false loop-contents [] total-shift 0]
    (cond (empty? remaining)
            (do (assert (not in-loop))
                (assert (empty? loop-contents))
                optimized)
          in-loop
            (let [op-with-data (first remaining)
                  op (first op-with-data)
                  op-data (second op-with-data)]
              (cond
                (= op "[") (recur (vec (concat optimized [["["]] loop-contents))
                                  (rest remaining) true [] 0)
                (= op "]") (recur (if (= 0 total-shift)
                                      (conj optimized (compile-loop-contents-to-loop loop-contents))
                                      (vec (concat optimized [["["]] loop-contents [["]"]])))
                                  (rest remaining) false [] 0)
                (= op ">") (recur optimized (rest remaining) in-loop (conj loop-contents op-with-data) (+ total-shift op-data))
                (= op "<") (recur optimized (rest remaining) in-loop (conj loop-contents op-with-data) (- total-shift op-data))
                (= op "+") (recur optimized (rest remaining) in-loop (conj loop-contents op-with-data) total-shift)
                :else (recur (vec (concat optimized [["["]] loop-contents [op-with-data]))
                             (rest remaining) false [] 0)))
          (not in-loop) (if (= (first remaining) ["["])
                            (recur optimized (rest remaining) true [] 0)
                            (recur (conj optimized (first remaining))
                                   (rest remaining) in-loop [] 0))
          :else (assert false "Previous cases should have been exhaustive"))))

(defn compile-source [source]
  (let [compiled (tokenize source)
        _ (println (str "Tokenized: " compiled))
        compiled (optimize-incrementing-loops compiled)
        _ (println (str "Optimize by loop compression: " compiled))
        compiled (match-brackets compiled)
        _ (println (str "Optimize by matching brackets: " compiled))]
    compiled))
    
(defn create-bf [instructions input]
  {:instructions instructions
   :instruction-pointer 0
   :data {}
   :data-pointer 0
   :input (map int input) ; convert to ASCII codes
   :output []})
   
(defn current-data [bf]
  (get-in bf [:data (:data-pointer bf)] 0))

(defn set-current-data [bf new-data]
  (assoc-in bf [:data (:data-pointer bf)] new-data))
  
(defn update-current-data [bf function]
  (set-current-data bf (function (current-data bf))))

(defmulti operation
  (fn [bf op op-data] op))

(defmethod operation "loop" [bf op op-data]
  (println (str "Executing loop operation with op " op " and data " op-data))
  (let [start-increment (op-data :start)
        start-index (:data-pointer bf)
        start-data (current-data bf)
        other-increments (:others op-data)
        _ (println (str "start-index: " start-index
                        ", start-data: " start-data
                        ", start-increment: " start-increment
                        ", other-increments: " other-increments))
        start-to-num-loops (@cache-loops-to-finish start-increment)
        _ (assert start-to-num-loops (str "loops-to-finish not precalculated for " start-increment))
        _ (println (str "Pre-calculated loop data: " start-to-num-loops))
        num-loops (start-to-num-loops (current-data bf))
        _ (println (str "Expected number of loops: " num-loops))
        _ (assert num-loops (str "Infinite loop for starting data " (current-data bf)
                                 ", increment " start-increment
                                 ", precalculated paths " start-to-num-loops))
        data-changes (merge {start-index 0}
                            (into {}
                                  (map (fn [[rel-index increment-per-loop]]
                                         (let [abs-index (+ rel-index start-index)
                                               old-data (get-in bf [:data abs-index] 0)
                                               new-data (rem (+ old-data
                                                                (* num-loops
                                                                   increment-per-loop))
                                                             256)]
                                           [abs-index new-data]))
                                       other-increments)))
        _ (println (str "Data changes: " data-changes))
        new-bf (update-in bf [:data] merge data-changes)]
    (println (str "Finished loop operation with bf: " new-bf))
    new-bf))
  
(defmethod operation ">" [bf op op-data]
  (update-in bf [:data-pointer] #(+ % op-data)))

(defmethod operation "<" [bf op op-data]
  (update-in bf [:data-pointer] #(- % op-data)))

(defmethod operation "+" [bf op op-data]
  (update-current-data bf #(rem (+ % op-data) 256)))

(defmethod operation "." [bf op op-data]
  (assoc bf :output
    (vec (concat (:output bf)
                 (repeat op-data
                         (char (current-data bf)))))))

(defmethod operation "," [bf op op-data]
  (let [consumed (vec (take op-data (:input bf)))
        remaining (drop op-data (:input bf))
        new-data (last consumed)]
    (if (< (count consumed) op-data)
      (assoc bf :error :insufficient-input)
      (-> bf
        (assoc-in [:data (:data-pointer bf)]
                  new-data)
        (assoc-in [:input] remaining)))))

(defmethod operation "[" [bf op op-data]
  (if (> (current-data bf) 0)
    bf
    (assoc-in bf [:instruction-pointer] op-data)))

(defmethod operation "]" [bf op op-data]
  (if (= 0 (current-data bf))
    bf
    (assoc-in bf [:instruction-pointer] op-data)))

(defn execute-once [bf start-millis start-op-prep-millis]
  (let [instruction (get (:instructions bf)
                         (:instruction-pointer bf))
        op (first instruction)
        op-data (second instruction)
        start-op-millis (.getTime (java.util.Date.))
        result (-> bf
                 (operation op op-data)
                 (update-in [:instruction-pointer] inc))
        end-op-millis (.getTime (java.util.Date.))
        _ (println (str op "(" op-data ") ...\n" result "\n"))
                        ;" prep:" (- start-op-millis start-op-prep-millis)
                        ;" op:" (- end-op-millis start-op-millis)
                        ;" total:" (- end-op-millis start-millis)))
        ]
    result))

(defn execute-to-end [bf start-millis]
  ;(println (str "Executing: " bf))
  (let [start-op-prep-millis (.getTime (java.util.Date.))]
    (cond (:error bf)
            nil
          (>= (:instruction-pointer bf)
              (count (:instructions bf)))
            (apply str (:output bf))
          (> (- start-op-prep-millis start-millis) 15000)
            "Timeout"
          :else
            (recur (execute-once bf start-millis start-op-prep-millis) start-millis))))

(defn execute-string
  "Evaluate the Brainfuck source code in `source` using `input` as a source of
  characters for the `,` input command.

  Either returns a sequence of output characters, or `nil` if there was
  insufficient input."
  [source input]
  (println (str "Starting execution at " (java.util.Date.)))
  (println (str "Source: " source))
  (let [compiled (time (compile-source source))
        bf (create-bf compiled input)
        ;_ (println (str "Compiled source: " compiled))
        _ (println (str "Input(" (count input) "): " (vec (take 50 input))))
        result (execute-to-end bf (.getTime (java.util.Date.)))
        _ (println (str "Final result at " (java.util.Date.) ": " result))
        ]
    result))

