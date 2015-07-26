(ns complex.roots
  "construtable numbers with exact precision"
  (:require [clojure.core.match :refer [match]]))

;; a number is
;; a number
;; or
;; a vector
;; [:numter num [:root base1 multiplier1] [:root base2 mult2] [:root base3]]
;; a one :number folowed by the number and a list of roots
;; or
;; a seq
;; a list of numbers and roots
;; (list num1 [:root base1] [:root base2 multiplier2] num2)
;; collect-terms will transform such sequences into a tagged :number vector

;; roots are :root tagged vectors [:root base number] where
;; base is the number to take the square root of and
;; number is a optional multiplier, defaulting to 1

(defn factors [n]
  (reverse
   (sort
    (loop [j 1 res []]
      (if (> (* j j) n) res
          (recur (inc j) (if (zero? (rem n j))
                           (conj res (/ n j) j)
                           res)))))))

(def squares (map #(* % %)))

(defn less-than [n]
  (take-while #(<= % n)))

(defn squares-less-than [k]
  (into #{} (comp squares (less-than k)) (range)))

(defn largest-square-factor [n]
  (let [squares (squares-less-than n)
        factors (factors n)]
    (loop [factors factors]
      (let [f (first factors)]
        (if (squares f) f
            (recur (rest factors)))))))

(defn collect-roots
  "simplify sequence of numbers and roots
  collect common bases, collect numbers"
  [roots]
  (let [r-map
        (reduce
         (fn [result item]
           (cond
             (number? item) (update-in result [:number] #(+ % item))
             (vector? item)
             (match item
                    [:root base & mult]
                    (update-in result [:bases base] (fnil #(+ % (reduce * mult)) 0)))))
         {:number 0
          :bases {}}
         roots)]
    (into (vector :number (:number r-map))
          (for [[k v] (:bases r-map) :when (not= 0 v)] [:root k v]))))

(defn collect-terms
  "collect terms of a sequence
  returning a tagged :number vector"
  [num-seq]
  (cond (number? num-seq) (collect-terms (list num-seq))
        (seq? num-seq)
        (loop [num 0 roots {} nums num-seq]
          (if-let [n (first nums)]
            (do
              (if (number? n)
                (recur (+ num n)
                       roots
                       (rest nums))
                (match n
                       [:root b & m]
                       (recur num
                              (update-in roots [b] (fnil #(+ % (reduce * m)) 0))
                              (rest nums))
                       [:number num1 & nroots]
                       (recur (+ num num1)
                              (reduce (fn [result root]
                                        (match root [:root b & m]
                                               (update-in result [b]
                                                          (fnil #(+ % (reduce * m)) 0))))
                                      roots
                                      nroots)
                              (rest nums)))))
            (into [:number num] (for [[k v] roots :when (not= 0 v)]
                                  [:root k v]))))))

(defn evaluate
  "evaluate number tree to a Double"
  [tree]
  (cond
    (number? tree) tree
    (seq? tree) (reduce + (map evaluate tree))
    (vector? tree) (match tree
                          [:root base & rest] (reduce * (Math/sqrt base) rest)
                          [:number n & rest]  (reduce + n (map evaluate rest)))))

(defn mult-number
  "mult num2 by a regular number n"
  [n num2]
  (assert (number? n))
  (cond
    (number? num2) (* n num2)
    (vector? num2) (match num2
                          [:root base & mult]
                          [:root base (reduce * n mult)]
                          [:number q & roots]
                          (into [:number (* n q)]
                                (map #(mult-number n %) roots)))
    (seq? num2) (mult-number n (collect-roots num2))))

(defn reduce-base [base rest]
  (let [lsf (largest-square-factor base)]
    (cond
      (= lsf 1) [:root base rest]
      (= lsf base) (* (int (Math/sqrt base)) rest)
      :else [:root (/ base lsf) (* rest (int (Math/sqrt lsf)))])))

(defn mult-root
  "mult num2 by root r"
  [r num2]
  (assert (vector? r))
  (assert (= :root (first r)))
  (match r
         [:root base1 & rest1]
         (cond
           (number? num2) (mult-number num2 r)
           (vector? num2)
           (match num2
                  [:root base2 & rest2]
                  (reduce-base (* base1 base2)
                               (* (reduce * rest1) (reduce * rest2)))
                  [:number num & roots]
                  (let [mr #(mult-root r %)]
                    (collect-roots (concat (list (mr num)) (map mr roots)))))
           (seq? num2) (mult-root r (collect-roots num2)))))

(defn mult [num1 num2]
  (cond
    (number? num1) (mult-number num1 num2)
    (vector? num1) (match num1
                          [:root b & m] (mult-root num1 num2)
                          [:number n & roots]
                          (collect-terms
                           (concat (list (mult-number n num2))
                                   (map #(mult-root % num2) roots))))
    (seq? num1) (mult (collect-roots num1) num2)))

(defn invert-root
  "1 / root"
  [root]
  (let [[:root b & m] root]
    [:root b (* (/ (reduce * m)) (/ b))]))

(defn assert-root [root]
  (assert (vector? root))
  (let [[k b & m] root]
    (assert (= k :root))
    (assert (number? b))
    ))

(defn assert-single-root-num [num]
  (assert (vector? num))
  (let [[k n & roots] num]
    (assert (= k :number))
    (assert (number? n))
    (assert (= 1 (count roots)))
    (doseq [r roots]
      (assert-root r))))

(defn assert-num [num]
  (assert (vector? num))
  (let [[k n & roots] num]
    (assert (= k :number))
    (assert (number? n))
    (doseq [r roots]
      (assert-root r))))

;; for single root numbers
(defn conjugate-number
  "conjugate for single root number"
  [num]
  (assert-single-root-num num)
  (let [[_ n root] num
        [_ base & mult] root]
    [:number n [:root base (reduce * -1 mult)]]))

(defn invert-num
  "invert a single root num: 1/num"
  [num]
  (assert-single-root-num num)
  (let [conj (conjugate-number num)
        [_ n root] num
        [_ b & ms] root
        m (reduce * ms)
        k (/ (- (* n n) (* b m m)))]
    (mult-number k conj)))

(comment
  (require '[complex.roots] :reload)
  (in-ns 'complex.roots)
  (evaluate [:root 2])
  (evaluate [:root 3 (/ 2)])
  (def t (list (/ 2) [:root 3] [:root 5 (/ 2)]))

  ;; a rational plus it roots
  ;; a root and its multiplicity

  (= (evaluate t)
     (+ (/ 2) (Math/sqrt 3) (* (Math/sqrt 5) (/ 2))))

  (def alpha (list (/ 2) [:root 5 (/ 2)]))
  (def beta  (list  (/ 2) [:root 5 (/ -2)]))
  (== 1 (evaluate (list alpha beta)))
  ;;=> true

  (mult-number (/ 3) (/ 2))
  (mult-number (/ 3) [:root 2])
  (mult-number (/ 3) [:root 2 1/3])
  (mult-number (/ 3) (list (/ 2)))
  (mult-number (/ 3) (list (/ 2) [:root 2] [:root 3 (/ 2)]))

  (= (collect-roots alpha)
     (mult-number (/ 2) (list 1 [:root 5])))
  ;;=> true

  (< (Math/abs (- (evaluate (mult alpha beta))
                  (* (evaluate alpha) (evaluate beta))))
     1e-15)
  ;;=> true

  (< (Math/abs (- (evaluate (mult alpha beta))
                  (* (evaluate alpha) (evaluate beta))))
     1e-16)
  ;;=> false

  (* (evaluate alpha) (evaluate beta))
  ;;=> -1.0000000000000002

  (mult alpha beta)
  ;;=> [:number -1N]
  (evaluate (mult alpha beta))
  ;;=> -1N

  (mult (/ 4) alpha)
  ;;=> (1/8 [:root 5 1/8])
  )

(comment
  (let [x [1 2 3]]
    (match [x]
           [([1] :seq)] :a0
           [([1 & r] :seq)] [:a1 r]
           :else nil))
  ;;=> [:a1 (2 3)]
  )