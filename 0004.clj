(ns pe4)

(defn palindrome? [num]
  (let [s (str num)
        center (int (/ (count s) 2))
        [front rear] (split-at center s)]
    (->> (map vector front (reverse rear))
         (every? #(= (% 0) (% 1))))))

(defn three-digits
  ([] (three-digits 100))
  ([n] (range n 1000)))

(->> (three-digits)
     (map (fn [d] (map #(* d %) (three-digits d))))
     flatten
     (filter palindrome?)
     sort
     last)
