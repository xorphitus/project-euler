(ns pe54)

(defn flush? [hand]
  (or (every? #(re-matches #".S" %) hand)
      (every? #(re-matches #".C" %) hand)
      (every? #(re-matches #".H" %) hand)
      (every? #(re-matches #".D" %) hand)))

(defn card->num [card]
  (let [c (first card)]
    (condp = c
      \T 10
      \J 11
      \Q 12
      \K 13
      \A 14
      (Integer. (str c)))))

(defn hand->nums [hand]
  (vec (sort (map card->num hand))))

(defn hand->pairs [hand]
  (->> hand
       hand->nums
       (partition-by identity)
       (map count)
       reverse))

(defn four-of-a-kind? [hand]
  (= [4 1] (hand->pairs hand)))

(defn full-house? [hand]
  (= [3 2] (hand->pairs hand)))

(defn two-pairs? [hand]
  (= [2 2 1] (hand->pairs hand)))

(defn one-pair? [hand]
  (= [2 1 1 1] (hand->pairs hand)))

(defn three-of-a-kind? [hand]
  (= [3 1 1] (hand->pairs hand)))

(defn straight? [hand] false)

(defn royal-flush? [hand]
  (and (flush? hand)
      (= (+ 10 11 12 13 14)
         (reduce + (hand->nums hand)))))

(defn straight-flush? [hand]
  (and (not (royal-flush? hand))
      (straight? hand)
      (flush? hand)))

(defn hand->highcard [hand]
  (-> hand
      hand->nums
      last))

(defn hand->score [hand]
  (let [rank (cond
               (royal-flush? hand)     9
               (straight-flush? hand)  8
               (four-of-a-kind? hand)  7
               (full-house? hand)      6
               (flush? hand)           5
               (straight? hand)        4
               (three-of-a-kind? hand) 3
               (two-pairs? hand)       2
               (one-pair? hand)        1
               :else 0)]
    [rank (hand->highcard hand)]))

(defn p1-win? [hands]
  (let [p1 (hand->score (first hands))
        p2 (hand->score (last hands))]
    (if (> (p1 0) (p2 0))
      true
      (and (= (p1 0) (p2 0))
          (> (p1 1) (p2 1))))))

(let [text (slurp "/path/to/p054_poker.txt")
      lines (clojure.string/split text #"\n")
      lines2 (map #(clojure.string/split % #"\s") lines)
      hands (map #(split-at 5 %) lines2)]
  (count (filter p1-win? hands)))
