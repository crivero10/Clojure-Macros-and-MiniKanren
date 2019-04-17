;----------------------------------------------------------
; Basic clojure examples
; Author: Carlos Rivero
;----------------------------------------------------------


(use 'clojure.test)
(use 'clojure.math.numeric-tower)

(defn my-repeat
  "Takes a number n and any data x as its arguments. It returns a list that contains n copies of x."
  [n x]
  (if (zero? n)
    ()
    (cons x (my-repeat (dec n) x))))

 
(defn invert-pairs
  "Takes as an argument a list of vectors containing two elements each and returns a list with each vector reversed"
  [lst]
  (if (empty? lst)
    ()
    (cons (into [] (reverse (first lst))) (invert-pairs (rest lst)))))

 
(defn enlist
  "Surrounds in a list every upper-level element of the list it takes as input."
  [lst]
  (if (empty? lst)
    ()
    (cons (list (first lst)) (enlist (rest lst)))))

 
(defn my-interleave
  "Takes two arguments: the lists a and b. It returns a list containing the first element of a, followed by the first element of b, followed by the second element of a, followed by the second element of b, and so on"
  [a b]
  (if (or (empty? a) (empty? b))
    ()
    (concat (cons (first a) (list (first b))) (my-interleave (rest a) (rest b)))))

 
(defn my-flatten
  "Removes all the interior parenthesis of the list it takes as input"
  [lst]
  (if (empty? lst)
    ()
    (if (list? (first lst))
         (concat (my-flatten (first lst)) (my-flatten (rest lst)))
         (if (vector? lst)
           (concat (my-flatten (first lst)) (my-flatten (into () (rest lst))))
           (cons (first lst) (my-flatten (rest lst)))))))

 
(defn exchange
  "Takes three arguments: two non-list values x1 and x2, and a list lst. It returns a list with the same elements as lst, except that all occurrences of x1 are replaced by x2 and vice versa"
  [x1 x2 lst]
  (if (empty? lst)
    ()
    (if (list? (first lst))
         (cons (exchange x1 x2 (first lst))
               (exchange x1 x2 (rest lst)))
         (if (= (first lst) x1)
           (cons x2 (exchange x1 x2 (rest lst)))
           (if (= (first lst) x2)
             (cons x1 (exchange x1 x2 (rest lst)))
             (cons (first lst) (exchange x1 x2 (rest lst))))))))

 
(defn insert
  "Takes two arguments: a number n and a list of numbers lst in ascending order. It returns a new list with the same elements as lst but inserting n in its corresponding place"
  [n lst]
  (if (empty? lst)
    (cons n ())
    (if (< n (first lst))
      (cons n lst)
      (cons (first lst) (insert n (rest lst))))))

 
(defn my-sort
  "Takes an unordered list of numbers as an argument, and returns a new list with the same elements but in ascending order"
  [lst]
  (if (empty? lst)
    ()
    (insert (first lst) (my-sort (rest lst)))))

 
(defn binary
  "Takes an integer n as input (assume that n ≥ 0). If n is equal to zero, it returns an empty list. If n is greater than zero, it returns a list with a sequence of ones and zeros equivalent to the binary representation of n."
  [n]
  (if (zero? n)
    ()
    (concat (binary (quot n 2)) (list (mod n 2)))))

 
(defn prime-factors
  "Takes an integer n as input (assume that n > 0), and returns a list containing the prime factors of n in ascending order"
  ([n] (prime-factors n 2 '()))
  ([n kst l]
   (if (= n 1)
     (reverse l)
     (if (zero? (mod n kst))
       (prime-factors (/ n kst) kst (cons kst l))
       (prime-factors n (inc kst) l)))))

 
(defn compress
  "Takes a list lst as its argument. If lst contains consecutive repeated elements, they should be replaced with a single copy of the element"
  [lst]
  (if (empty? lst)
    ()
    (if (not (= (first lst) (first (rest lst))))
      (cons (first lst) (compress (rest lst)))
      (compress (rest lst)))))

 
(defn pack
  "Takes a list lst as its argument. If lst contains consecutive repeated elements they should be placed in separate sublists"
  [lst]
  (loop [l lst t ()]
    (if (empty? l)
      (reverse t)
      (if (= (first (first t)) (first l))
        (recur (rest l) (cons (cons (first l) (first t)) (rest t)))
        (recur (rest l) (cons (list (first l)) t ))))))

 
(defn encode
  "Takes a list lst as its argument. Consecutive duplicates of elements in lst are encoded as vectors [n e], where n is the number of duplicates of the element e"
  [lst]
  (loop [n 1 l lst t ()]
    (if (empty? l)
      (reverse t)
      (if (= (first l) (first (rest l)))
        (recur (inc n) (rest l) t)
        (recur 1 (rest l) (cons [n (first l)] t))))))

 
(defn encode-modified
  "Takes a list lst as its argument. It works the same as the previous problem, but if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are converted to [n e] vectors."
  [lst]
  (loop [n 1 l lst t ()]
    (if (empty? l)
      (reverse t)
      (if (= (first l) (first (rest l)))
        (recur (inc n) (rest l) t)
        (if (> n 1)
          (recur 1 (rest l) (cons [n (first l)] t))
          (recur 1 (rest l) (cons (first l) t)))))))

 
(defn decode
  "Takes as its argument an encoded list lst that has the same structure as the resulting list from the previous problem. It returns the decoded version of lst"
  [lst]
  (if (empty? lst)
    ()
    (if (vector? (first lst))
      (concat (repeat (get (first lst) 0)
                      (get (first lst) 1))
              (decode (rest lst)))
      (cons (first lst) (decode (rest lst))))))

(defn my-map-indexed
  "Takes two arguments: a function f and a list lst. It returns a list consisting of the result of
  applying f to 0 and the first item of lst, followed by applying f to 1 and the second item in lst,
  and so on until lst is exhausted"
  [f lst]
  (loop [l lst i 0 r ()]
    (if (empty? l)
      (reverse r)
      (recur (rest l) (inc i) (cons (f i (first l)) r)))))

(defn my-drop-while [f lst]
  "Takes two arguments: a function f and a list lst. It returns a list of items from lst dropping the
  initial items that evaluate to true when passed to f. Once a false value is encountered, the rest of
  the list is returned. "
  (loop [l lst]
    (if (empty? l)
      ()
      (if (not (f (first l)))
        l
        (recur (rest l))))))

(defn bisection
  "Finds a real root of function f within the closed interval [a, b]"
  [a b f]
  (loop [m a n b c (float (/ (+ a b) 2))]
    (if (< (abs (f c)) 0.000000000000001)
      c
      (if (pos? (* (f c) (f m)))
        (recur c n (bigdec (/ (+ n c) 2)))
        (recur m c (bigdec (/ (+ m c) 2)))))))

(defn deriv
  "Takes f and h as its arguments, and returns a new function that takes x as argument, which
  represents the derivative of f given a certain value for h."
  [f h]
  (fn [x] (/ (- (f (+ x h)) (f x)) h)))

(defn integral
  "Takes as arguments a, b, n, and f. It returns the value of the integral, using Simpson's rule."
  [a b n f]
  (let [h (/ (- b a) n)]
    (loop [k 1 s (f a)]
      (if (= k n)
        (* (/ h 3) (+ s (f b)))
        (recur (inc k) (+ s (if (even? k) (* 2 (f (+ a (* k h)))) (* 4 (f (+ a (* k h)))))))))))

(defn triangular
  "Returns n if x is the n-th triangular number, otherwise
  returns nil."
  [x]
  (loop [i 0]
    (if (= ((fn tri [n] (/ (* (+ 1 n) n) 2)) i) x)
      i
      (if (> i x)
        nil
        (recur (inc i))))))

(defn positives
  "Takes a list of numbers lst as its argument, and returns a new list that only
  contains the positive numbers of lst"
  [lst]
  (filter pos? lst))

 
(defn dot-product
  "Takes two arguments: the lists a and b.
  It returns the result of performing the dot product of a times b."
  [a b]
  (reduce + (map-indexed (fn [i x] (* x (nth b i))) a)))

 
(defn pow
  "Takes two arguments as input: a number a and a positive integer b.
  It returns the result of computing a raised to the power b"
  [x n]
  (reduce * (repeat n x)))

 
(defn replic
  "Takes two arguments: a list lst and an integer number n, where n ≥ 0.
  It returns a new list that replicates n times each element contained in lst"
  [n lst]
  (mapcat (fn [x] (repeat n x)) lst))

 
(defn expand
  "Takes a list lst as its argument. It returns a list where the first element of lst appears one time
  , the second elements appears two times, the third element appears three times, and so on"
  [lst]
  (mapcat
    (fn [x] x)
    (map-indexed (fn [i x] (repeat (inc i) x)) lst)))

 
(defn largest
  "Takes as argument a nonempty list of numbers lst. It returns the largest value contained in lst"
  [lst]
  (reduce (fn [x y] (if (> x y) x y)) lst))

 
(defn drop-every
  "Takes two arguments: an integer number n, where n ≥ 1, and a list lst. It returns a new list that drops every n-th element from lst"
  [n lst]
  (filter
    (fn [x] (not (nil? x)))
    (map-indexed
      (fn [i x] (if (zero? (mod (inc i) n)) nil x)) lst)))

 
(defn rotate-left
  "Takes two arguments: an integer number n and a list lst. It returns the list that results from rotating lst a total of n elements to the left"
  [n lst]
  (cond
    (>= n 0) (drop n (take (+ n (count lst)) (cycle lst)))
    :else (drop (- (count lst) (mod (* n -1) (count lst)))
                (take (- (* 2 (count lst)) (mod (* n -1) (count lst))) (cycle lst)))))

 
(defn gcd
  [a b]
  (cond
    (> b a) (last
              (filter
                (fn [x] (= 0 (mod a x)))
                (filter (fn [x] (= 0 (mod b x))) (range 1 b))))
    :else (last
            (filter
              (fn [x] (= 0 (mod b x)))
              (filter (fn [x] (= 0 (mod a x))) (range 1 a))))))

 
(defn insert-everywhere
  "Takes two arguments as input: an object x and a list lst. It returns a new list with all the possible ways in which x can be inserted into every position of lst"
  [x lst]
  (map-indexed
    (fn [i l]
      (concat (take i l)
              (list x)
              (drop i (take (count l) l))))
    (repeat (+ 1 (count lst)) lst)))

(defn my-interpose [x lst]
  (loop [l lst r ()]
    (if (= 1 (count l))
      (reverse (cons (first l) r))
      (recur (rest l) (cons x (cons (first l) r))))))

(defn my-map [f lst]
  (if (empty? lst)
    ()
    (cons (f (first lst)) (lazy-seq (my-map f (rest lst))))))

(defn binary-read [num]
  (loop [i (- (count num) 1) l num s 0]
    (if (empty? l)
      s
      (recur (dec i)
             (rest l)
             (+ s (if (= (first l) \1)
                    ((fn [n] (reduce * (repeat n 2))) i) 0))))))

(defn symmetric [a b]
  (set (filter
         #(not (nil? %))
         (concat
           (for [j b] (if (not (contains? a j)) j))
           (for [i a] (if (not (contains? b i)) i))))))


;==========================================================


(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))
(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))
(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1))))))
(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))
(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
                   (fn [x]
                     (integral 3 4 10
                               (fn [y]
                                 (* x y))))))))

(deftest test-triangular
  (is (= 0 (triangular 0)))
  (is (= 1 (triangular 1)))
  (is (nil? (triangular 2)))
  (is (= 2 (triangular 3)))
  (is (nil? (triangular 4)))
  (is (nil? (triangular 5)))
  (is (= 3 (triangular 6)))
  (is (= 4 (triangular 10)))
  (is (= 10 (triangular 55)))
  (is (= 1000 (triangular 500500)))
  (is (nil? (triangular 500000)))
  (is (= 10000000 (triangular 50000005000000))))

(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))
(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))
(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four) (my-flatten '(((one) ((two))) () (three (())) four)))))
(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
      (exchange true 42 '((true) 42 ((cool (42)) (true))))))
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))
(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))
(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4)
                            '(-4.5 3.0 1.5 0.9 0.0)))))
(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))
(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))
(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))
(deftest test-largest
  (is (= 31 (largest '(31))))
  (is (= 5 (largest '(1 2 3 4 5))))
  (is (= -1 (largest '(-1 -2 -3 -4 -5))))
  (is (= 52 (largest '(32 -1 45 12 -42 52 17 0 21 2)))))
(deftest test-drop-every
  (is (= () (drop-every 5 ())))
  (is (= '(1 2 3) (drop-every 4 '(1 2 3 4))))
  (is (= '(1 3 5 7) (drop-every 2 '(1 2 3 4 5 6 7 8))))
  (is (= '(1 3 5 7 9) (drop-every 2 '(1 2 3 4 5 6 7 8 9))))
  (is (= '(a b d e g h j)
         (drop-every 3 '(a b c d e f g h i j))))
  (is (= '(a b c d e f g h i j)
         (drop-every 20 '(a b c d e f g h i j))))
  (is (= () (drop-every 1 '(a b c d e f g h i j)))))
(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))
(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))
(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
            (a 1 b c d e)
            (a b 1 c d e)
            (a b c 1 d e)
            (a b c d 1 e)
            (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
            (1 x 2 3 4 5 6 7 8 9 10)
            (1 2 x 3 4 5 6 7 8 9 10)
            (1 2 3 x 4 5 6 7 8 9 10)
            (1 2 3 4 x 5 6 7 8 9 10)
            (1 2 3 4 5 x 6 7 8 9 10)
            (1 2 3 4 5 6 x 7 8 9 10)
            (1 2 3 4 5 6 7 x 8 9 10)
            (1 2 3 4 5 6 7 8 x 9 10)
            (1 2 3 4 5 6 7 8 9 x 10)
            (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))
(run-tests)