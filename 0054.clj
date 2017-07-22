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

(defn four-of-a-kind? [hand]
  (->> (hand->nums hand)
       (partition-by identity)
       (filter #(= 4 (count %)))
       seq))

(defn full-house? [hand]
  (let [nums (hand->nums hand)
        two (->> nums
                 (partition-by identity)
                 (filter #(= 2 (count %)))
                 seq)
        three (->> nums
                   (partition-by identity)
                   (filter #(= 3 (count %)))
                   seq)]
    (and two three)))

(defn two-pairs? [hand]
  (and (not full-house? hand)
      (->> (hand->nums hand)
           (partition-by identity)
           (filter #(= 2 (count %)))
           (count)
           (= 2))))

(defn one-pair? [hand]
  (let [nums (hand->nums hand)]
    (and (not (full-house? nums))
        (->> nums
             (partition-by identity)
             (filter #(= 2 (count %)))
             (count)
             (= 1)))))

(defn three-of-a-kind? [hand]
  (let [nums (hand->nums hand)]
    (and (not (full-house? nums))
        (->> nums
             (partition-by identity)
             (filter #(= 3 (count %)))
             seq))))

(defn straight? [hand] false)

(defn royal-flush? [hand] false)

(defn straight-flush? [hand]
  (and (not (royal-flush? hand))
      (straight? hand)
      (flush? hand)))

(defn hand->score [hand]
  (cond
    (royal-flush? hand)    9
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand)     6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (one-pair? hand) 1
    :else 0))

(defn p1-win? [hands]
  (let [p1 (hand->score (first hands)
            )
        p2 (hand->score (last hands)
            )]
    (> p1 p2)))

(let [text (slurp "/path/to/p054_poker.txt")
      lines (clojure.string/split text #"\n")
      lines2 (map #(clojure.string/split % #"\s") lines)
      hands (map #(split-at 5 %) lines2)]
  (count (filter p1-win? hands)))
