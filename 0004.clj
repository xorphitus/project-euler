(ns pe4)

(defn palindrome? [num]
  (let [s (str num)]
    (= (seq s) (reverse s))))

(defn three-digits
  ([] (three-digits 100))
  ([n] (range n 1000)))

(->> (three-digits)
     (map (fn [d] (map #(* d %) (three-digits d))))
     flatten
     (filter palindrome?)
     sort
     last)
