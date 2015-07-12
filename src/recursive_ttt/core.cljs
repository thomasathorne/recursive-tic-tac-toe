(ns ^:figwheel-always recursive-ttt.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [sablono.core :as html :refer-macros [html]]))

(defn make-3x3 [x] [[x x x] [x x x] [x x x]])

(def initial-state
  {:board (make-3x3 {:board [[{} {} {}]
                             [{} {} {}]
                             [{} {} {}]]
                     :active true
                     :turn "X"})
   :active true
   :turn "X"})

(defonce app-state (atom initial-state))

(def switch {"X" "O" "O" "X"})

(defn row
  [board n]
  (get board n))

(defn column
  [board n]
  (mapv #(get % n) board))

(defn diagonals
  [board r c]
  (let [forward  [(get-in board [0 0])
                  (get-in board [1 1])
                  (get-in board [2 2])]
        backward [(get-in board [0 2])
                  (get-in board [1 1])
                  (get-in board [2 0])]]
    (cond
     (= [1 1] [r c]) [forward backward]
     (= r c)         [forward]
     (= r (- 2 c))   [backward]
     :else           [])))

(defn lines-to-check
  [board r c]
  (-> (diagonals board r c)
      (conj (row board r))
      (conj (column board c))))

(defn winning-line
  [p line]
  (= [p p p] (mapv :result line)))

(defn check-for-winner
  [{:keys [board] :as game} player row col]
  (if (some #(winning-line player %) (lines-to-check board row col))
    (assoc game :result player :active false)
    game))

(defn check-for-draw
  [{:keys [board result] :as game}]
  (if result
    game
    (if (every? (fn [row]
                  (every? (fn [s]
                            (not (nil? (:result s)))) row)) board)
      (assoc game :result "Draw" :active false)
      game)))

(defn play
  [{:keys [board active] :as game} player row col]
  (if active
    (-> game
        (assoc-in [:board row col] {:result player})
        (check-for-winner player row col)
        (check-for-draw)
        (assoc :turn (switch player)))
    (assoc game :turn player)))

(defn all-inactive
  [game]
  (update-in game [:board]
             (fn [b]
               (mapv (fn [r]
                       (mapv #(assoc % :active false) r)) b))))

(defn all-active
  [game]
  (update-in game [:board]
             (fn [b]
               (mapv (fn [r]
                       (mapv #(assoc % :active (nil? (:result %))) r)) b))))

(defn set-active
  [{:keys [board] :as game} row col]
  (if (get-in board [row col :result])
    (all-active game)
    (-> (all-inactive game)
        (assoc-in [:board row col :active] true))))

(defn metaplay
  [{:keys [board turn] :as data} m-row m-col row col]
  (let [played (play (get-in data [:board m-row m-col]) turn row col)]
    (cond
     (= played (get-in data [:board m-row m-col]))
     data

     (get played :result)
     (-> data
         (assoc-in [:board m-row m-col] played)
         (check-for-winner turn m-row m-col)
         (check-for-draw)
         (assoc :turn (:turn played))
         (set-active row col))

     :else
     (-> data
         (assoc-in [:board m-row m-col] played)
         (assoc :turn (:turn played))
         (set-active row col)))))

(defn square
  [{:keys [result]} _ [m-row m-col row col data]]
  (reify
    om/IRender
    (render [_]
      (html [(case result
               "X" :div.square.X
               "O" :div.square.O
               :div.square)
             {:on-click (if (nil? result)
                          (fn [e]
                            (om/transact!
                             data
                             #(metaplay % m-row m-col row col))))}
             result]))))

(defn ttt-component
  [{:keys [board result active] :as game} _ [m-row m-col data]]
  (reify
    om/IRender
    (render [_]
      (html
       [(case result
          "X" :div.X
          "O" :div.O
          :div)
        [(if active :table :table.inactive)
         [:tbody
          (for [row (range 3)]
            [:tr
             (for [col (range 3)]
               [:td
                (om/build square
                          (get-in board [row col])
                          {:opts [m-row m-col row col data]})])])]]]))))

(defn recursive-ttt-component
  [data _]
  (reify
    om/IRender
    (render [_]
      (html
       [:div
        [(if (:active data) :table :table.inactive)
         [:tbody
          (for [row (range 3)]
            [:tr
             (for [col (range 3)]
               [:td
                (om/build ttt-component
                          (get-in data [:board row col])
                          {:opts [row col data]})])])]]
        (if-let [winner (:result data)]
          [:p (str "The winner: " winner)]
          [:p (str "It is " (:turn data) "'s turn.")])]))))

(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (om/build recursive-ttt-component data))))
  app-state
  {:target (. js/document (getElementById "app"))})
