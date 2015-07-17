(ns complex.geometry
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length conjugate]]
   [schema.core :as s :include-macros true]))

;; inversion in a general circle C
;; where C has center P and radius r
;; is z ->

(defn inversion
  "inversion in a circle"
  ([] (fn [z] (recip (conjugate z))))
  ([circle]
   (let [{:keys [center radius]} circle
         T (inversion)
         Q (n/c center)
         r (n/c [radius 0])
         S (fn [z] (add (mult r z) Q))
         S-inv (fn [w] (mult (recip r) (add w (minus Q))))]
     (comp S T S-inv))))

(defn midpoint
  "midpoint -f two complex numbers"
  [z w]
  (mult (add z w) (/ 2)))

;; parameterized circles
;; from deaux
(defn param-circle
  "returns a parameterized circle
  given four complex numbers
  where :infinity is an accepted value"
  [a b c d]
  (fn [t]
    (if (= t :infinity)
      (div a c)
      (div (add (mult a t) b)
                   (add (mult c t) d)))))

(defn three-point->param
  [p q r]
  (let [a (mult r (sub p q))
        b (mult q (sub r p))
        c (sub p q)
        d (sub r p)]
    [a b c d]))

(defn circle
  "return a parameterized circle through the p q r
  for paramter values of 0 1 and :infinity, repectully"
  [p q r]
  (apply param-circle (three-point->param q p r)))

(defn param->general
  "return given parameterized circle in general form
  "
  [a b c d]
  (let [f (fn [u v] (sub (mult u (conjugate v)) (mult v (conjugate u))))
        g (fn [a b c d] (sub (mult a (conjugate d)) (mult b (conjugate c))))
        alpha-bar (minus (div (g a b c d) (f c d)))
        beta (minus (div (f a b) (f c d)))]
    [one (conjugate alpha-bar) alpha-bar beta]))

(defn param->standard
  [a b c d]
  (let [[_ alpha alpha-bar beta] (param->general a b c d)]
    [:circle {:center (coords (minus alpha-bar))
              :radius (length (sub (mult alpha alpha-bar) beta))}]))

;; representation of circles and lines by hermitian matrices
;; from schwerdtfeger
(defn circle-as-matrix
  "return matrix representing circle with given center and radius"
  [center radius]
  (let [gamma (n/c center)
        A 1
        B (n/minus (conjugate gamma))
        C gamma
        D (- (n/len-sq gamma) (* radius radius))]
    [A B C D]))

(defn line-as-matrix
  "return matrix representing line
  through origin and perpendicular to complex number b"
  [b]
  (let [b-bar (conjugate b)]
    [0 b-bar b 0]))

;; methods that take a generalized circle
(defn to-string [[A B C D]]
  (pr-str [A (coords B) (coords C) D]))

(defn vaild-circle?
  "determine if given vector represents a hermitian matrix
need A and B to be real and B anc C complex conjugates"
  [[A B C D]]
  (assert (and (number? A) (number? D)))
  ;; (= B (conjugate C))
  true)

(defn to-circle
  "return center and radius for given hermitian matrix"
  [[A B C D]]
  (let [c (mult C (/ (- A)))
        c-coords (coords c)
        c-sq (n/len-sq c)
        r-sq (- c-sq (/ D A))
        r (if (< r-sq 0)
            (n/sqrt r-sq)
            (Math/sqrt r-sq))]
    [:circle {:center c-coords :radius r}]))

(defn determinant
  "determinant of a hermitian matrix"
  [[A B C D]]
  (let [b (n/len-sq B)]
    (- (* A D) b)))

(comment
  (require 'complex.geometry :reload)
  (in-ns 'complex.geometry)

  )
