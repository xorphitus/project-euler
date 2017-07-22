(ns pe1)

(->> (iterate inc 1)
     (filter #(or (zero? (rem % 3)) (zero? (rem % 5))))
     (take-while #(< % 1000))
     (reduce +))
