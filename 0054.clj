(ns pe54)

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

(defn hand->pairs [hand pattern]
  (let [pairs (->> hand
                   hand->nums
                   (partition-by identity)
                   (sort-by count)
                   (filter #(> (count %) 1))
                   reverse)]
    (when (= pattern (map count pairs))
      (map first pairs))))

(defn one-pair? [hand]
  (hand->pairs hand [2]))

(defn two-pairs? [hand]
  (hand->pairs hand [2 2]))

(defn three-of-a-kind? [hand]
  (hand->pairs hand [3]))

(defn full-house? [hand]
  (hand->pairs hand [3 2]))

(defn four-of-a-kind? [hand]
  (hand->pairs hand [4]))

(defn flush? [hand]
  (when (or (every? #(re-matches #".S" %) hand)
            (every? #(re-matches #".C" %) hand)
            (every? #(re-matches #".H" %) hand)
            (every? #(re-matches #".D" %) hand))
    [(-> hand hand->nums sort reverse)]))

(defn straight? [hand]
  (let [nums (hand->nums hand)
        i (first nums)
        s (take 5 (iterate inc i))]
    (when (and (not (flush? hand))
              (or (= nums s)
                  (= nums [11 12 13 14 2])
                  (= nums [   12 13 14 2 3])
                  (= nums [      13 14 2 3 4])))
      [(-> nums sort last)])))

(defn royal-flush? [hand]
  (when (and (flush? hand)
            (= [10 11 12 13 14] (hand->nums hand)))
    [14]))

(defn straight-flush? [hand]
  (when (and (not (royal-flush? hand))
            (straight? hand))
    (flush? hand)))

(defn hand->highcard [hand]
  (-> hand
      hand->nums
      last))

(defn hand->score [hand]
  (cond
    (royal-flush? hand)     [9 (royal-flush? hand)]
    (straight-flush? hand)  [8 (straight-flush? hand)]
    (four-of-a-kind? hand)  [7 (four-of-a-kind? hand)]
    (full-house? hand)      [6 (full-house? hand)]
    (flush? hand)           [5 (flush? hand)]
    (straight? hand)        [4 (straight? hand)]
    (three-of-a-kind? hand) [3 (three-of-a-kind? hand)]
    (two-pairs? hand)       [2 (two-pairs? hand)]
    (one-pair? hand)        [1 (one-pair? hand)]
    :else                   [0 [(hand->highcard hand)]]))

(defn compare-highcards [cards1 cards2]
  (let [ranked-cards (map vector cards1 cards2)
        win-lose (map #(compare (first %) (last %)) ranked-cards)
        first-conclusion (->> win-lose (filter zero?) first)]
    (= first-conclusion 1)))

(defn p1-win? [hands]
  (let [p1 (hand->score (first hands))
        p2 (hand->score (last hands))]
    (or (> (p1 0) (p2 0))
        (and (= (p1 0) (p2 0))
            (compare-highcards (last p1) (last p2))))))

(def s-split clojure.string/split)

(let [text (slurp "/path/to/p054_poker.txt")
      lines (s-split text #"\n")
      lines2 (map #(s-split % #"\s") lines)
      hands (map #(split-at 5 %) lines2)]
  (count (filter p1-win? hands)))
