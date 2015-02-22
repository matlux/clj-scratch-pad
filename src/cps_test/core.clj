(ns ^{:doc "Playing with continuation-passing style."
      :author "Mathieu Gauthron"
    cps-test.core)


;; ********************** Factorial **********************
 ;;direct style
(defn fact [n]
  (if (= n 1) 1
      (* n (fact (dec n)))))

;;loop recur
(defn fact-loop [x]
  (loop [n x r 1]
    (if (= n 1) r
        (recur (dec n) (* n r)))))



;;CPS
(defn fact-cps [n k]
  (if (= n 0) (k 1)
      (fact-cps (dec n) (fn [r] (k (* n r))))))

(comment
  ;; let's test the 3 style of implementations
  (fact 5) ;;direct
  (fact-loop 5)
  (fact-cps 5 identity)

  ;; let's decompose the previous call step by step by applying the substitution model
  (fact-cps 4 (fn [r] (identity (* 5 r))))
  (fact-cps 3 (fn [r] ((fn [r] (identity (* 5 r))) (* 4 r))))
  (fact-cps 2 (fn [r] ((fn [r] ((fn [r] (identity (* 5 r))) (* 4 r))) (* 3 r))))
  (fact-cps 1 (fn [r] ((fn [r] ((fn [r] ((fn [r] (identity (* 5 r))) (* 4 r))) (* 3 r))) (* 2 r))))
  (fact-cps 0 (fn [r] ((fn [r] ((fn [r] ((fn [r] ((fn [r] (identity (* 5 r))) (* 4 r))) (* 3 r))) (* 2 r))) (* 1 r))))
  ((fn [r] ((fn [r] ((fn [r] ((fn [r] ((fn [r] (identity (* 5 r))) (* 4 r))) (* 3 r))) (* 2 r))) (* 1 r))) 1)


  ((fn [r] (
           (fn [r] (
                   (fn [r] (
                           (fn [r] (
                                   (fn [r] (
                                           identity
                                           (* 5 r)))
                                   (* 4 r)))
                           (* 3 r)))
                   (* 2 r)))
           (* 1 r)))
   1)

  ((fn [r] (
           (fn [r] (
                   (fn [r] (
                           (fn [r] (
                                   identity
                                   (* 5 r)))
                           (* 4 r)))
                   (* 3 r)))
           (* 2 r)))
   (* 1 1))

  ((fn [r] (
           (fn [r] (
                   (fn [r] (
                           identity
                           (* 5 r)))
                   (* 4 r)))
           (* 3 r)))
   (* 2 1))

  ((fn [r] (
           (fn [r] (
                   identity
                   (* 5 r)))
           (* 4 r)))
   (* 3 2))

  ((fn [r] (
           identity
           (* 5 r)))
   (* 4 6))

  (identity
   (* 5 24))
)
;; ***************** Pythagorean theorem **********************
;;direct style
(defn pyth [a b] (+ (* a a) (* b b)))
;;CPS
(defn *-cps [x y k] (k (* x y)))
(defn +-cps [x y k] (k (+ x y)))
(defn pyth-cps [a b k]
  (*-cps a a (fn [a2]
               (*-cps b b (fn [b2]
                            (+-cps a2 b2 k))))))

(defn pyth-cps [a b k]
  ((fn [a2]
     ((fn [b2]
        (k (+ a2 b2)))
      (* b b)))
   (* a a)))
(comment
  (pyth 2 3)
  (pyth-cps 2 3 identity)

  ;; let's decompose the previous call step by step by applying the substitution model
  (*-cps 2 2 (fn [a2]
               (*-cps 3 3 (fn [b2]
                            (+-cps a2 b2 identity)))))
  ((fn [a2]
     (*-cps 3 3 (fn [b2]
                  (+-cps a2 b2 identity)))) (* 2 2))

  (*-cps 3 3 (fn [b2]
               (+-cps 4 b2 identity)))
  ((fn [b2]
     (+-cps 4 b2 identity)) (* 3 3))
  (+-cps 4 9 identity)
  (identity (+ 4 9))
)

;; ******************* fibonacci **********************

;; direct style
(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

;; cps
(defn fib-cps [n k]
  (letfn [(cont [n1]
            (fib-cps (- n 2) (fn [n2]
                               (k (+ n1 n2)))))]
    (if (<= n 1)
      (k n)
      (recur (- n 1) cont))))

(defn fib-cps [n k]
  (if (<= n 1)
    #(k n)
    (recur (- n 1) (fn [n1]
                     (fib-cps (- n 2) (fn [n2]
                                        (k (+ n1 n2))))))))

(defn fib-cps-tramp [n k]
  (if (<= n 1)
    #(k n)
    (recur (- n 1) (fn [n1]
                     #(fib-cps-tramp (- n 2) (fn [n2]
                                               (k (+ n1 n2))))))))
(defn fib-cps-tramp [n k]
  (if (<= n 1)
    #(k n)
    (fn [] (fib-cps-tramp (- n 1) (fn [n1]
                              #(fib-cps-tramp (- n 2) (fn [n2]
                                                        (k (+ n1 n2)))))))))

(comment
  ;; lets test the various implementations
  (fib 10) ;; direct
  (fib-cps 10 identity);55
  (trampoline (fib-cps-tramp 10 identity))

  (time (fib 34))
  (fib-cps 34 identity);55
  (time (trampoline #(fib-cps-tramp 34 identity)))

(fib-cps 10 identity)
;; let's decompose the previous call step by step by applying the substitution model

(fib-cps 4 identity)
(fib-cps 3 (fn [n1]
             (fib-cps (- 4 2) (fn [n2]
                                (identity (+ n1 n2))))))
(fib-cps 2 (fn [n1]
             (fib-cps (- 3 2) (fn [n2]
                                ((fn [n1]
                                   (fib-cps (- 4 2) (fn [n2]
                                                      (identity (+ n1 n2))))) (+ n1 n2))))))
(fib-cps 1 (fn [n1]
             (fib-cps (- 2 2) (fn [n2]
                                ((fn [n1]
                                   (fib-cps (- 3 2) (fn [n2]
                                                      ((fn [n1]
                                                         (fib-cps (- 4 2) (fn [n2]
                                                                            (identity (+ n1 n2))))) (+ n1 n2))))) (+ n1 n2))))))
((fn [n1]
             (fib-cps (- 2 2) (fn [n2]
                                ((fn [n1]
                                   (fib-cps (- 3 2) (fn [n2]
                                                      ((fn [n1]
                                                         (fib-cps (- 4 2) (fn [n2]
                                                                            (identity (+ n1 n2))))) (+ n1 n2))))) (+ n1 n2))))) 1)

(fib-cps 0 (fn [n2]
                   ((fn [n1]
                      (fib-cps (- 3 2) (fn [n2]
                                         ((fn [n1]
                                            (fib-cps (- 4 2) (fn [n2]
                                                               (identity (+ n1 n2))))) (+ n1 n2))))) (+ 1 n2))))

((fn [n2]
                   ((fn [n1]
                      (fib-cps (- 3 2) (fn [n2]
                                         ((fn [n1]
                                            (fib-cps (- 4 2) (fn [n2]
                                                               (identity (+ n1 n2))))) (+ n1 n2))))) (+ 1 n2))) 0)

((fn [n1]
   (fib-cps (- 3 2) (fn [n2]
                      ((fn [n1]
                         (fib-cps (- 4 2) (fn [n2]
                                            (identity (+ n1 n2))))) (+ n1 n2))))) 1)
(fib-cps 1 (fn [n2]
             ((fn [n1]
                (fib-cps (- 4 2) (fn [n2]
                                   (identity (+ n1 n2))))) (+ 1 n2))))

((fn [n2]
             ((fn [n1]
                (fib-cps (- 4 2) (fn [n2]
                                   (identity (+ n1 n2))))) (+ 1 n2))) 1)

((fn [n1]
   (fib-cps (- 4 2) (fn [n2]
                      (identity (+ n1 n2))))) 2)

(fib-cps (- 4 2) (fn [n2]
                   (identity (+ 2 n2))))
(fib-cps 2 (fn [n2]
                   (identity (+ 2 n2))))



(fib-cps 1 (fn [n1]
                     (fib-cps (- 2 2) (fn [n2]
                               ((fn [n2]
                                  (identity (+ 2 n2))) (+ n1 n2))))))

((fn [n1]
   (fib-cps (- 2 2) (fn [n2]
                      ((fn [n2]
                         (identity (+ 2 n2))) (+ n1 n2))))) 1)

(fib-cps 0 (fn [n2]
                   ((fn [n2]
                      (identity (+ 2 n2))) (+ 1 n2))))

((fn [n2]
   ((fn [n2]
      (identity (+ 2 n2))) (+ 1 n2))) 0)

((fn [n2]
   (identity (+ 2 n2))) 1)
(identity (+ 2 1))

)
