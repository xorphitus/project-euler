(ns pe3)

(defn prime? [n]
  (let [nums (->> (lazy-seq (concat [2] (iterate #(+ 2 %) 3)))
                  (take-while #(<= % (Math/sqrt n))))
        nums2 (filter #(zero? (rem n %)) nums)]
    (empty? nums2)))

(let [given 600851475143
      primes (->> (iterate inc 2)
                  (filter prime?)
                  (take-while #(<= % (Math/sqrt given)))
                  reverse)]
  (->> primes
       (filter #(zero? (rem given %)))
       first))
