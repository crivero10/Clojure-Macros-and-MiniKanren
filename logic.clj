;----------------------------------------------------------
; Mini Kanren Examples
; Author: Carlos Rivero
;----------------------------------------------------------

(use '[clojure.test :rename {is test-is}])
(use '[clojure.core.logic :rename {== ===}])
(declare oddsizeo)
(declare reverseo)
(require '[clojure.core.logic.fd :as fd])

(defn removeo
  [x lst result]
  (fresh [head tail temp]
         (conso head tail lst)
         (conde [(=== x head)
                 (=== tail result)]
                [(removeo x tail temp)
                 (appendo [head] temp result)])))

(defn rotateo
  [lst result]
  (fresh [head tail]
         (conso head tail lst)
         (appendo tail [head] result)))

(defn evensizeo
  [lst]
  (conde [(=== lst [])]
         [(fresh [head tail]
                 (conso head tail lst)
                 (oddsizeo tail))]))
(defn oddsizeo
  [lst]
  (fresh [head tail]
         (conso head tail lst)
         (evensizeo tail)))

(defn reverseo
  [lst result]
  (conde
    [(=== lst ())
     (=== result ())]
    [(fresh [head tail temp]
            (conso head tail lst)
            (reverseo tail temp)
            (appendo temp [head] result))]))

(defn palindromeo
  [lst]
  (fresh [rev]
         (reverseo lst rev)
         (=== lst rev)))

(defn facto
  "Logical function that succeeds if the factorial of
   n is result."
  [n result]
       (conde [(=== n 0)
               (=== result 1)]
              [(fresh [temp1 temp2]
                      (fd/> n 0)
                      (fd/- n 1 temp1)
                      (facto temp1 temp2)
                      (fd/* temp2 n result))]))

(defn converto
  [d k]
  (conde
    [(=== k :one)
     (=== d 1)]
    [(=== k :two)
     (=== d 2)]
    [(=== k :three)
     (=== d 3)]
    [(=== k :four)
     (=== d 4)]
    [(=== k :five)
     (=== d 5)]
    [(=== k :six)
     (=== d 6)]
    [(=== k :seven)
     (=== d 7)]
    [(=== k :eight)
     (=== d 8)]
    [(=== k :nine)
     (=== d 9)]
    [(=== k :zero)
     (=== d 0)]))

(defn translateo
  [lst result]
  (conde [(=== lst [])
          (=== result [])]
         [(fresh [head tail key temp]
                 (conso head tail lst)
                 (translateo tail temp)
                 (converto head key)
                 (appendo [key] temp result))]))

(defn splito
  [lst a b]
  (conde [(=== lst [])
          (=== a [])
          (=== b [])]
         [(fresh [temp element]
                 (=== lst [element])
                 (splito [] temp b)
                 (appendo [element] temp a))]
         [(fresh [head second tail temp1 temp2 res]
                 (conso head tail lst)
                 (firsto tail second)
                 (resto tail res)
                 (splito res temp1 temp2)
                 (appendo [head] temp1 a)
                 (appendo [second] temp2 b))]))

(defn equalo
  [lst]
  (conde [(=== lst [])]
         [(fresh [head tail]
                 (conso head tail lst)
                 (=== tail [])
                 (equalo []))]
         [(fresh [head second tail]
                 (conso head tail lst)
                 (firsto tail second)
                 (=== head second)
                 (equalo tail))]))


(defn counto
  [lst result]
  (conde [(=== lst [])
          (=== result 0)]
         [(fresh [head tail temp]
                 (conso head tail lst)
                 (fd/- result 1 temp)
                 (counto tail temp))]))



(defn rangeo
  [start end result]
  (conde [(fd/> start end)
          (=== result [])]
         [(=== start end)
          (=== result [start])]
         [(fresh [temp res]
                 (fd/< start end)
                 (fd/+ 1 start temp)
                 (rangeo temp end res)
                 (appendo [start] res result))]))


(deftest test-removeo
  (test-is (= [[:b :c :d :e]]
              (run 1 [q] (removeo :a [:a :b :c :d :e] q))))
  (test-is (= [[:a :b :d :e]]
              (run 1 [q] (removeo :c [:a :b :c :d :e] q))))
  (test-is (= [:d]
              (run 1 [q]
                   (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (test-is (= []
              (run 1 [q] (removeo :x [:a :b :c :d :e] q))))
  (test-is (= [[:x :a :b :c :d :e]
               [:a :x :b :c :d :e]
               [:a :b :x :c :d :e]
               [:a :b :c :x :d :e]
               [:a :b :c :d :x :e]
               [:a :b :c :d :e :x]]
              (run 6 [q] (removeo :x q [:a :b :c :d :e]))))
  (test-is (= [[:a [:b :c :d :e]]
               [:b [:a :c :d :e]]
               [:c [:a :b :d :e]]
               [:d [:a :b :c :e]]
               [:e [:a :b :c :d]]]
              (run* [q1 q2]
                    (removeo q1 [:a :b :c :d :e] q2)))))

(deftest test-rotateo
  (test-is (= [:yes]
              (run 1 [q]
                   (rotateo [:a :b :c :d :e]
                            [:b :c :d :e :a])
                   (=== q :yes))))
  (test-is (= []
              (run 1 [q]
                   (rotateo [:a :b :c :d :e]
                            [:a :b :c :d :e])
                   (=== q :yes))))
  (test-is (= []
              (run 1 [q] (rotateo [] q))))
  (test-is (= [[:a]]
              (run 1 [q] (rotateo [:a] q))))
  (test-is (= [[:b :c :d :e :a]]
              (run 1 [q] (rotateo [:a :b :c :d :e] q))))
  (test-is (= [[:e :a :b :c :d]]
              (run 1 [q] (rotateo q [:a :b :c :d :e]))))
  (test-is (= '[[[_0] [_0]]
                [[_0 _1] [_1 _0]]
                [[_0 _1 _2] [_1 _2 _0]]
                [[_0 _1 _2 _3] [_1 _2 _3 _0]]
                [[_0 _1 _2 _3 _4] [_1 _2 _3 _4 _0]]
                [[_0 _1 _2 _3 _4 _5] [_1 _2 _3 _4 _5 _0]]
                [[_0 _1 _2 _3 _4 _5 _6] [_1 _2 _3 _4 _5 _6 _0]]]
              (run 7 [q1 q2] (rotateo q1 q2)))))

(deftest test-evensizeo-oddsizeo
  (test-is (= [:yes]
              (run 1 [q] (evensizeo []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (oddsizeo [:x]) (=== q :yes))))
  (test-is (= []
              (run 1 [q] (evensizeo [:x]) (=== q :yes))))
  (test-is (= []
              (run 1 [q] (oddsizeo []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (evensizeo [:a :b :c :d :e :f]) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (oddsizeo [:a :b :c :d :e]) (=== q :yes))))
  (test-is (= '[[]
                [_0 _1]
                [_0 _1 _2 _3]
                [_0 _1 _2 _3 _4 _5]
                [_0 _1 _2 _3 _4 _5 _6 _7]]
              (run 5 [q] (evensizeo q))))
  (test-is (= '[[_0]
                [_0 _1 _2]
                [_0 _1 _2 _3 _4]
                [_0 _1 _2 _3 _4 _5 _6]
                [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
              (run 5 [q] (oddsizeo q)))))

(deftest test-palindromeo
  (test-is (= [:yes]
              (run 1 [q] (palindromeo []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (palindromeo [:a]) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (palindromeo [:a :b :c :b :a]) (=== q :yes))))
  (test-is (= []
              (run 1 [q]
                   (palindromeo [:a :b :c :d]) (=== q :yes))))
  (test-is (= '[[]
                [_0]
                [_0 _0]
                [_0 _1 _0]
                [_0 _1 _1 _0]
                [_0 _1 _2 _1 _0]
                [_0 _1 _2 _2 _1 _0]]
              (run 7 [q] (palindromeo q)))))

(deftest test-converto
  (test-is (= [:yes]
              (run 1 [q] (converto 0 :zero) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (converto 0 :zero)
                   (converto 1 :one)
                   (converto 2 :two)
                   (converto 3 :three)
                   (converto 4 :four)
                   (converto 5 :five)
                   (converto 6 :six)
                   (converto 7 :seven)
                   (converto 8 :eight)
                   (converto 9 :nine)
                   (=== q :yes))))
  (test-is (= []
              (run 1 [q] (converto 12 :twelve) (=== q :yes))))
  (test-is (= [7]
              (run 1 [q] (converto q :seven))))
  (test-is (= [:seven]
              (run 1 [q] (converto 7 q))))
  (test-is (= [[1 :two 3]]
              (run 1 [q1 q2 q3]
                   (converto q1 :one)
                   (converto 2 q2)
                   (converto q3 :three)))))

(deftest test-translateo
  (test-is (= [:yes]
              (run 1 [q]
                   (translateo [1 2 3] [:one :two :three])
                   (=== q :yes))))
  (test-is (= []
              (run 1 [q]
                   (translateo [1 2 3] [:one :two :four])
                   (=== q :yes))))
  (test-is (= [:three]
              (run 1 [q] (translateo [1 2 3] [:one :two q]))))
  (test-is (= [[:four :five :six :seven :eight :nine]]
              (run 1 [q] (translateo [4 5 6 7 8 9] q))))
  (test-is (= [[1 2 0]]
              (run 1 [q] (translateo q [:one :two :zero]))))
  (test-is (= [[[] []]]
              (run 1 [q1 q2] (translateo q1 q2)))))

(deftest test-splito
  (test-is (= [:yes]
              (run 1 [q] (splito [] [] []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (splito [:a] [:a] []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (splito [:a :b] [:a] [:b]) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (splito [:a :b :c :d :e :f]
                           [:a :c :e]
                           [:b :d :f])
                   (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (splito [:a :b :c :d :e :f :g]
                           [:a :c :e :g]
                           [:b :d :f])
                   (=== q :yes))))
  (test-is (= [[[:a :c :e] [:b :d :f]]]
              (run 1 [q1 q2]
                   (splito [:a :b :c :d :e :f] q1 q2))))
  (test-is (= [[:a :b :c :d :e :f :g]]
              (run 1 [q] (splito q [:a :c :e :g] [:b :d :f]))))
  (test-is (= '[[[] [] []]
                [[_0] [_0] []]
                [[_0 _1] [_0] [_1]]
                [[_0 _1 _2] [_0 _2] [_1]]
                [[_0 _1 _2 _3] [_0 _2] [_1 _3]]
                [[_0 _1 _2 _3 _4] [_0 _2 _4] [_1 _3]]
                [[_0 _1 _2 _3 _4 _5] [_0 _2 _4] [_1 _3 _5]]]
              (run 7 [q1 q2 q3] (splito q1 q2 q3)))))

(deftest test-equalo
  (test-is (= [:yes]
              (run 1 [q] (equalo []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (equalo [:x]) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (equalo [:x :x]) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (equalo [:x :x :x :x :x]) (=== q :yes))))
  (test-is (= [:x]
              (run 1 [q] (equalo [:x :x q :x]))))
  (test-is (= '[_0]
              (run 1 [q] (equalo [q q q q q q]))))
  (test-is (= '([_0 _0 _0 _0 _0])
              (run 1 [q1 q2 q3 q4 q5]
                   (equalo [q1 q2 q3 q4 q5]))))
  (test-is (= []
              (run 1 [q] (equalo [:x :y]) (=== q :yes))))
  (test-is (= []
              (run 1 [q1 q2]
                   (equalo [q1 q1 q2 q1 q1]) (!= q1 q2))))
  (test-is (= '([]
                 [_0]
                 [_0 _0]
                 [_0 _0 _0]
                 [_0 _0 _0 _0]
                 [_0 _0 _0 _0 _0]
                 [_0 _0 _0 _0 _0 _0])
              (run 7 [q] (equalo q)))))

(deftest test-counto
  (test-is (= [0]
              (run 1 [q]
                   (fd/in q (fd/interval 0 10))
                   (counto [] q))))
  (test-is (= [1]
              (run 1 [q]
                   (fd/in q (fd/interval 0 10))
                   (counto [:a] q))))
  (test-is (= [2]
              (run 1 [q]
                   (fd/in q (fd/interval 0 10))
                   (counto [:a :b] q))))
  (test-is (= [3]
              (run 1 [q]
                   (fd/in q (fd/interval 0 10))
                   (counto [:a :b :c] q))))
  (test-is (= [10]
              (run 1 [q]
                   (fd/in q (fd/interval 0 10))
                   (counto (repeat 10 :x) q))))
  (test-is (= '([_0])
              (run 1 [q]
                   (fd/in q (fd/interval 0 10))
                   (counto q 1))))
  (test-is (= '([_0 _1 _2 _3 _4])
              (run 1 [q]
                   (fd/in q (fd/interval 0 10))
                   (counto q 5))))
  (test-is (= '([[] 0]
                 [(_0) 1]
                 [(_0 _1) 2]
                 [(_0 _1 _2) 3]
                 [(_0 _1 _2 _3) 4]
                 [(_0 _1 _2 _3 _4) 5]
                 [(_0 _1 _2 _3 _4 _5) 6])
              (run 7 [q1 q2]
                   (fd/in q1 q2 (fd/interval 0 10))
                   (counto q1 q2)))))

(deftest test-rangeo
  (test-is (= [[3 4 5 6 7 8 9 10]]
              (run 1 [q]
                   (rangeo 3 10 q))))
  (test-is (= [[7]]
              (run 1 [q]
                   (rangeo 7 7 q))))
  (test-is (= [[]]
              (run 1 [q]
                   (rangeo 10 1 q))))
  (test-is (= [6]
              (run 1 [q]
                   (fd/in q (fd/interval 1 10))
                   (rangeo 2 q [2 3 4 5 6]))))
  (test-is (= [[2 6]]
              (run 1 [q1 q2]
                   (fd/in q1 q2 (fd/interval 1 10))
                   (rangeo q1 q2 [2 3 4 5 6])))))
(test-is (= #{[]
              [1] [1 2] [1 2 3] [1 2 3 4]
              [2] [2 3] [2 3 4]
              [3] [3 4]
              [4]}
            (set
              (run* [q]
                    (fresh [start end]
                           (fd/in start end (fd/interval 1 4))
                           (rangeo start end q))))))

(deftest test-facto
  (test-is (= [1]
              (run 1 [q] (facto 0 q))))
  (test-is (= [1]
              (run 1 [q] (facto 1 q))))
  (test-is (= [720]
              (run 1 [q] (facto 6 q))))
  (test-is (= [2432902008176640000]
              (run 1 [q] (facto 20 q))))
  (test-is (= [0 1]
              (run 2 [q] (facto q 1))))
  (test-is (= [5]
              (run 1 [q] (facto q 120))))
  (test-is (= [10]
              (run 1 [q] (facto q 3628800))))
  (test-is (= [:yes]
              (run 1 [q] (facto 4 24)
                         (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (facto 15 1307674368000)
                         (=== q :yes))))
  (test-is (= [[0 1]
               [1 1]
               [2 2]
               [3 6]
               [4 24]
               [5 120]
               [6 720]
               [7 5040]
               [8 40320]
               [9 362880]]
              (run 10 [n r] (facto n r)))))
              
(run-tests)


