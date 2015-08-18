(ns complex.complex
  (:require [complex.roots :as n]))

(def ^:const PI Math/PI)
(def ^:const TAU (* 2 PI))
(defn mod-tau [x] (mod x TAU))
(defn mod-1 [x] (mod x 1))

(def keywords [:rect :polar :tau :deg :radians])

(def one       [:tau 0])
(def i         [:tau (/ 4)])
(def omega     [:tau (/ 6)])
(def iota      [:tau (/ 10)])

(defn mult-tau
  ([] [:tau 0])
  ([t1 t2]
   (let [[_ f1] t1
         [_ f2] t2]
     [:tau (mod-1 (+ f1 f2))]))
  ([t1 t2 & rest]
   (reduce
    (fn [result item]
      (mult-tau result item))
    (mult-tau t1 t2)
    rest)))

(defn pow-tau
  "tau to the n power"
  [tau n]
  (cond (= n 0) [:tau 0]
        (= n 1) tau
        :else (reduce (fn [result item] (mult-tau result tau)) [:tau 0] (range n))))

(defn eq-omega [x]
  (#{[:rect (/ 2) [:root 3 (/ 2)]]
     [:tau (/ 6)]} x))

(def square (take 4 (iterate #(mult-tau i %) one)))
(def square-set (set square))
(def hexagon (take 6 (iterate #(mult-tau omega %) one)))
(def hex-set (set hexagon))
(def triangle (take 3 (iterate #(mult-tau (mult-tau omega omega) %) one)))
(def triangle-set (set triangle))
(def pentagon (take 5 (iterate #(mult-tau (mult-tau iota iota) %) one)))
(def decagon (take 10 (iterate #(mult-tau iota %) one)))

(defn omega-nth
  "the nth power of omega looked up in a vector"
  [n]
  (let [k (mod n 6)]
    (nth hexagon k)))

(comment
  (require '[complex.complex] :reload)
  (in-ns 'complex.complex)

  (n/one? (pow-tau omega 6))
  (= (pow-tau omega 6) one)
  ;;=> true

  (take-while #(not= one %) (iterate #(mult-tau % omega) omega))

  (for [f hexagon g hexagon] (mult-tau f g))

  (= (n/evaluate (n/mult (/ 2) n/Phi)) (first (n/evaluate [:tau (/ 10)])))
  (= (n/evaluate [:polar 1 [:tau (/ 10)]]) (n/evaluate [:polar 1 [:deg 36]]))
  ;;=> true
  )
