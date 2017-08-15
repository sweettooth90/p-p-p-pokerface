(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card] (let [[_ suit] card]
                    (str suit)))

(defn pair? [hand] (> (apply max (vals (frequencies (map rank hand))))1))

(defn three-of-a-kind? [hand] (> (apply max (vals (frequencies (map rank hand))))2))

(defn four-of-a-kind? [hand] (> (apply max (vals (frequencies (map rank hand))))3))

(defn flush? [hand] (== (apply max (vals (frequencies (map suit hand))))5))

(defn full-house? [hand] (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand] (or
                         (= 2 (get (frequencies (vals (frequencies (map rank hand))))2))
                         (= 1 (get (frequencies (vals (frequencies (map rank hand))))4))))

(defn straight? [hand] (let [sort1 (sort (map rank hand)) min (first sort1)
                             sort2 (sort (replace {14 1} sort1)) min-2 (first sort2) ]
                         (or
                          (= sort1 (range min (+ min 5)))
                          (= sort2 (range min-2 (+ min-2 5))))))

(defn straight-flush? [hand] (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand] (let [checkers #{[high-card? 0] [pair? 1]
                                    [two-pairs? 2] [three-of-a-kind? 3]
                                    [straight? 4] [flush? 5]
                                    [full-house? 6] [four-of-a-kind? 7]
                                    [straight-flush? 8]}]
                     (apply max (map second (filter (fn [check] ((first check) hand)) checkers)))))
