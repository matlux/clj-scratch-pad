(ns ^{:doc "Playing with continuation-passing style."
      :author "Mathieu Gauthron"}
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

;; quite efficient and does not recur as deeply as fib-cps. got it through luck but not really trampoline
(defn fib-cps2 [n k]
  (let [cont (fn [n1] (fib-cps2 (- n 2) (fn [n2]
                                       (k (+ n1 n2)))))]
    (if (<= n 1)
     #(k n)
     (recur (- n 1) (fn [n1]
                      (cont n1))))))

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


(def max-count 2000)

(defn jump [c f]
  (if (zero? c)
    #(f)
    (f)))
(defn next-count [c]
  (if (< c 0)
    max-count
    (dec c)))
(defn fib-cps-tramp-count [n k c]
  (let [cont (fn [n1] (fib-cps-tramp-count (- n 2) (fn [n2]
                                       (k (+ n1 n2))) (dec c)))]
    (if (<= n 1)
     #(k n)
     (recur (- n 1) (fn [n1]
                      (jump c #(cont n1))) (next-count c)))))

(defn fib-cps-tramp-count [n k c]
  (if (<= n 1)
    #(k n)
    (recur (- n 1) (fn [n1]
                     (jump c #(fib-cps-tramp-count (- n 2) (fn [n2]
                                                             (k (+ n1 n2))) (dec c)))) (next-count c))))



(comment
  ;; lets test the various implementations
  (fib 10) ;; direct
  (time (fib-cps 10 identity))               ;55
  (time (trampoline (fib-cps-tramp 10 identity)))

  (time (fib 34))
  (fib-cps 34 identity);55
  (time (trampoline #(fib-cps-tramp 34 identity)))
  (time (trampoline #(fib-cps-tramp 35 identity)))
  (time (trampoline #(fib-cps-tramp-count 34 identity max-count)))

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





(defn map-direct [f coll]
  (if (empty? coll)
    ()
    (cons (f (first coll)) (map-direct f (rest coll)))))

(defn map-cps [f coll k]
  (if (empty? coll)
    (k ())
    (map-cps f (rest coll) (fn [r]
                             (k (cons (f (first coll)) r))) )))
(defn map-cps [f coll k]
  (if (empty? coll)
    (k ())
    (recur f (rest coll) (fn [r]
                           (k (cons (f (first coll)) r))) )))

(defn map-cps-tramp [f coll k]
  (if (do (empty? coll))
    #(k ())
    #(map-cps-tramp f (rest coll) (fn [r]
                                   (fn [] (k (cons (f (first coll)) r)))) )))


(map-direct fib (map-direct (fn [_] 6) (range 60)))
(map-cps (fn [_] 6) (range 60) identity)
(do (empty? (rest (range 60))))
(map-cps (fn [_] 6) (range 6000) (fn [r]
                                  (map-cps fib r identity)))

(time (first (trampoline #(map-cps-tramp (fn [_] 6) (range 6000) (fn [r]
                                                                   (fn [] (map-cps-tramp fib (do (println r) r) identity)))) )))
(time (first (trampoline #(map-cps-tramp (fn [_] 6) (range 600000) (fn [r]
                                                            (fn [] (map-cps-tramp fib (do (println r) r) identity)))) )))






)

(comment
(def -main (fn random-f [{board :board am-i-white? :white-turn valid-moves :valid-moves ic :in-check? h :history s :state}]
   (let [v (into [] valid-moves)
         iteration (if (nil? s) (+ 1 (if am-i-white? 0 1)) (+ 2 s))]
     (let [move (rand-int (count valid-moves))]
       {:move (get v move) :state iteration})) ))

(def -main [{board :board am-i-white? :white-turn valid-moves :valid-moves ic :in-check? h :history s :state}]
  (let [v (into [] valid-moves)
        iteration (if (nil? s) (+ 1 (if am-i-white? 0 1)) (+ 2 s))]
    (let [move (rand-int (count valid-moves))]
      {:move (get v move) :state iteration})) )


)
(def ZERO (fn [f] (fn [x] x)))
(def ONE (fn [f] (fn [x] (f x))))
(def TWO (fn [f] (fn [x] (f (f x)))))

(def AND (fn [p] (fn [q] ((p q) p) )))
(def pred (fn [n] (fn [f] (fn [x] (((n (fn [g] (fn [h] (h (g f))))) (fn [u] x) ) (fn [u] u)  )  ))))
(def minus (fn [m] (fn [n] ((n pred) m))))
(def TRUE (fn [a] (fn [b] a)))
(def FALSE (fn [a] (fn [b] b)))
(def isZero (fn [n] ((n (fn [x] FALSE)) TRUE)))
(def LEQ (fn [m] (fn [n] (isZero ((minus m) n) ))))
(def EQ (fn [m] (fn [n] ((AND ((LEQ m) n)) ((LEQ n) m)))))




(defn d [b] ((b 'TRUE) 'FALSE))

(comment

  (d TRUE)

  (d FALSE)
  (d ((AND TRUE) TRUE))
  (d ((AND TRUE) FALSE))
  (d ((AND FALSE) TRUE))
  (d ((AND FALSE) FALSE))
  (d (isZero ZERO ))
  (d (isZero ONE ))
  (d (isZero TWO ))
  (d ((EQ ZERO) ONE))
  (d ((EQ ONE) ZERO))
  (d ((EQ TWO) ZERO))
  (d ((EQ ONE) ONE))
  (d ((EQ ONE) TWO))
  (d ((EQ TWO) ONE))
  (d ((EQ TWO) TWO))


  (def a TWO)
  (def b TWO)
  ;; this if statement can be re-writen
  ;; (if (= a a)
  ;;   (+ 1 1)
  ;;   (+ 1 2))
  ;; into the below lambda expression. the "then" (+ 1 1) gets evaluated and returned

  ((((EQ a) b) (+ 1 1)) (+ 1 2))


  ;; here the "else" logic is evaluated and returned"

  ((((EQ a) ONE) (+ 1 1)) (+ 1 2))


    ;;However the above evaluates both then and else because clojure follows an eager evaluation model
    ;; in a lazy language this wouldn't be the case
    ;; let's try to delay the evaluation of the "then" exp and "else" exp to recreate the true branching logic

  (((((EQ a) b) (fn [] (+ 1 1))) (fn []  (+ 1 2))))


  ;; now let's try to write a macro
  (defmacro if->lambda [clause then else]
    `((((if ~clause TRUE FALSE) (fn [] ~then)) (fn [] ~else))))

  (if->lambda
   (= 1 1)
   (+ 1 1)
   (+ 1 2))

  ;; the above macro expands into the following:
  ;; (macroexpand '(if->lambda (= 1 2) (+ 1 1) (+ 1 2)))

    ((((if (= 1 2) TRUE FALSE)
       (fn [] (+ 1 1)))
      (fn [] (+ 1 2))))



  (def f
    (fn [a]
      ((+ a) (fn [b]
               ((+ a) ((+ a) b))))))

(f x)
(fn [a]
  ((+ a) (fn [b]
           ((+ a) ((+ x) b)))))


(def fact
  ([n]
     (if (= n 1) 1
         (* n (fact (dec n))))))






  )
