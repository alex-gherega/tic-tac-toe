(ns tic-tac-toe)

(defn make-empty-matrix [n m]
  (let [mpty-mx-fn (fn [coll n val]
                     (if (zero? n)
                       coll
                       (recur (conj coll val) (dec n) val)))
        line (mpty-mx-fn [] n nil)]
    (mpty-mx-fn [] m line)))

;; generate environment
(defn make-empty-matrix [n m]
  (let [mpty-mx-fn #(vec (repeat %1 %2))]
    (mpty-mx-fn m (mpty-mx-fn n nil))))

;; utilities to determine valid spaces on board
(defn- transpose [coordinate & dimensions]
  "Transpose from 1D to 2D for now"
  (let [[n m] dimensions]
    [(quot coordinate m) (rem coordinate n)]))

(defn- determine-move [x val & dimensions]
  "given an index in a vector add to "
  {:pre [dimensions]}
  (let [[n m] dimensions]
    (if (nil? val)
      [(transpose x n m)])))

(defn- determine-moves [board n m]
  "board is a flatten vector with size n x m"

  (loop [pos (count board)
         moves []]
    (if (zero? pos)
      moves
      (recur (dec pos)
             (into moves (determine-move (dec pos)
                                           (-> pos dec board)
                                           n m))))))
;; player strategy and action
(defn chose-randomly [board]
  (let [[n m] [(-> board first count) (-> board count)]
        moves (determine-moves (-> board flatten vec) n m)
        rand-move (-> moves count rand-int)]
    (moves rand-move)))

(defn place-mark [board mark]
  ;; Strategy: randomly
  (update-in board
             (chose-randomly board)
             (fn [_] mark)))

;; check lines, columns and diagonals
(defn- check-line [line]
  (if (apply = line) (first line) nil))

(defn- check-lines [board]
  (loop [winner nil
         f (first board)
         r (rest board)]
    (if (or (nil? f) winner)
      winner
      (recur (check-line f)
             (first r)
             (rest r)))))

(defn- check-lines [board]
  (loop [winner nil
         f (first board)
         r (rest board)]
    (if (or (nil? f) winner)
      winner
      (recur (check-line f)
             (first r)
             (rest r))))
  (-> (filter check-line board) first first))


(defn- check-cols [board]
  ;; this is really easy to do with map; but for now let's stick to what we know
  (let [n (count board)
        transpose (fn [matrix]
                    (loop [idx 0 t-mat []]
                      (if (= idx n)
                        t-mat
                        (recur (inc idx)
                               (conj t-mat ((apply juxt matrix) idx))))))]

    (-> board transpose check-lines)))

(defn- select-diag-item
  ([idx n]
   (select-diag-item (fn [x _] x) idx n))
  ([sel-fn idx n]
   [(rem idx n) (rem (sel-fn idx n) n)]))

(defn- second-diag-fn [idx n]
  [(rem idx n) (rem (- idx n 1) n)])

(defn- select-diag [diag-fn board]
  (loop [n (count board)
         idx 0
         diag []]

    (if (= idx n)
      diag
      (recur n (inc idx)
             (conj diag (get-in board (diag-fn idx n)))))))

(defn check [board]

  (or (check-lines board)
      (check-cols board)
      (check-line (select-diag select-diag-item board))
      (check-line (select-diag (partial select-diag-item #(- %2 %1 1))
                               board))))

(defn eog? [board]
  (not (some {nil true} (-> board flatten vec))))

(defn- take-turn [board]
  (let [safe-run #(if (eog? %1) %1 (%2))
        board-fst-pl (safe-run board #(place-mark board "x"))
        board-snd-pl (safe-run board-fst-pl #(place-mark board-fst-pl "y"))
        fst-pl-res (check board-fst-pl)
        snd-pl-res (check board-snd-pl)]

    (cond fst-pl-res {fst-pl-res board-fst-pl}
          snd-pl-res {snd-pl-res board-snd-pl}
          (eog? board-snd-pl) :eog
          :default board-snd-pl)))

(defn- run [board]
  (loop [n 0 result (take-turn board)]
    (println n)
    (cond (= result :eog) "It's a tie"
          (-> result vector? not) (str "The winner is player with mark: "
                                       (-> result first first) "\n "
                                       (-> result first second))
          :else (recur (inc n) (take-turn result)))))

(defn main [& args]
  (let [board (make-empty-matrix 3 3)]
    (run board)))
