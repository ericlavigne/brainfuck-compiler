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

(defn compile-loop-contents-to-loop [tokens]
  (loop [offset 0 remaining tokens offsets-to-increments {0 0}]
    (let [[op op-data] (first remaining)
          next-remaining (rest remaining)]
      (cond (empty? remaining)
              (do (assert (= 0 offset)
                          (str "Loop optimization requires balancing < and > "
                               "(offset is " offset ")"))
                  [:loop {:start (get offsets-to-increments 0)
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

(defn compile-source [source]
  (let [tokenized (tokenize source)
        _ (println (str "Tokenized: " tokenized))
        matched (match-brackets tokenized)
        _ (println (str "Matched brackets: " matched))]
    matched))
    
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
        _ (println (str op "(" op-data ") ... "
                        " prep:" (- start-op-millis start-op-prep-millis)
                        " op:" (- end-op-millis start-op-millis)
                        " total:" (- end-op-millis start-millis)))]
    result))

(defn execute-to-end [bf start-millis]
  ;(println (str "Executing: " bf))
  (let [start-op-prep-millis (.getTime (java.util.Date.))]
    (cond (:error bf)
            nil
          (>= (:instruction-pointer bf)
              (count (:instructions bf)))
            (apply str (:output bf))
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

