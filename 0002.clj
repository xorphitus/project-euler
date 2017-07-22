(ns pe2)

(defn fib
  ([] (fib 1 2))
  ([a b] (lazy-seq (cons a (fib b (+ a b))))))

(->> (fib)
     (take-while #(<= % (* 4 1000 1000)))
     (filter even?)
     (reduce +))
