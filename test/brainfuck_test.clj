(ns brainfuck-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [brainfuck :refer :all]))

(deftest optimize-loop-test
  (testing "compilation of simple loop contents"
    (is (= (compile-loop-contents-to-loop (tokenize ">>+>+<<<-"))
           ["loop" {:start 255 :others {2 1 3 1}}])))
)

(defn- ord->str [n] (str (char n)))

(deftest basic-program-tests
  (testing "output until inclusive 0"
    (let [in (-> (range 1 256) vec (conj 0)
                 (->> (map char) (apply str)))]
      (= in (execute-string "+[,.]" in))))

  (testing "output n exclamation marks"
    (let [pluses (s/join (repeat (int \!) \+))
          src (str ">" pluses "<,[>.<-]")
          ! #(s/join (repeat % \!))]
      (are [n] (= (execute-string src (ord->str n))
                  (! n))
           1 2 3 4 5 6 7 8 9 10)))

  (testing "memory 0 initialized"
    (is (= (execute-string ".>." "")
           "\0\0")))

  (testing "memory operations"
    (is (= (execute-string ".>.+.<." "")
           "\0\0\1\0")))

  (testing "input output"
    (are [n] (= (execute-string ".,." (ord->str n))
                (str "\0" (char n)))
         11 12 13 14 15 16 17 18 19 20))

  (testing "loops"
    (let [z #(s/join (repeat % "\0"))]
      (are [n] (= (execute-string "[>.<],[>.<-]" (ord->str n))
                  (z n))
           21 22 23 24 25 26 27 28 29 30))))

(deftest error-handling-test
  (testing "insufficient input"
    (is (nil? (execute-string "," "")))))

(deftest complex-tests
  (testing "Hello World!"
    (is (= (execute-string
             (str "++++++++++[>+++++++>++++++++++>+++>+<<<<-]"
                  ">++.>+.+++++++..+++.>++.<<+++++++++++++++."
                  ">.+++.------.--------.>+.")
             "")
           "Hello World!")))
  ;(testing "Lots of looping takes forever"
  ;  (is (= "1, 1, 2, 3, 5, 8, 13, 21, 34, 55"
  ;         (execute-string
  ;           (str ",>+>>>>++++++++++++++++++++++++++++++++++"
  ;                "++++++++++>++++++++++++++++++++++++++++++"
  ;                "++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<"
  ;                "<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]"
  ;                ">>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>"
  ;                "[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]"
  ;                "<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++"
  ;                "+++++++++++++++++++++++++++++++++++++.[-]]"
  ;                "++++++++++<[->-<]>+++++++++++++++++++++++"
  ;                "+++++++++++++++++++++++++.[-]<<<<<<<<<<<<"
  ;                "[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]"
  ;                "<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]"
  ;                "<<<-]")
  ;           "\n"))))
)


