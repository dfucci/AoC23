; Games
; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
; (local game1 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
; (local game2 "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
; (local game3 "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
; (local game4 "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
; (local game5 "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

; Bag
; 12 red cubes, 13 green cubes, and 14 blue cubes
(local bag {:red 12 :green 13 :blue 14})

; Utils
(fn mult [vec]
  (accumulate [s 1 _ v (ipairs vec)]
    (* s v)))

(fn get-keys [tbl]
  (let [keys []]
    (each [k (pairs tbl)]
      (table.insert keys k))
    keys))

(local colors (get-keys bag))

(fn split-string [s d]
  (icollect [r (string.gmatch s (.. "[^" d "]+"))] r))

(fn remove-whitespaces [s]
  (string.gsub s "%s" ""))

(fn get-game-number [g]
  (tonumber (string.match g "%d+")))

; Part 1
(fn draw->table [draw]
  (let [number (tonumber (string.match draw "%d+")) 
        color (string.match draw "[^ %d+]+")]
    {:color color :number number}))

(fn is-more-than-bag? [draw]
  (let [t (draw->table draw)
        color (. t :color) 
        number (. t :number)
        bag-number (tonumber (. bag color))]
    (> number bag-number)))

(fn line->table [line]
  (let [clean-game (remove-whitespaces line)
        game-split (split-string clean-game ":")
        game-number (get-game-number (. game-split 1))
        game-results (. game-split 2)
        draws (split-string game-results ";") 
        draw (icollect [_ v (ipairs draws)] (split-string v ","))]
    [draw game-number]))

; TODO reimplement using flat-game
(fn line-score [line]
  (var impossible false)
  (let [[draw game-number] (line->table line)]
    (each [_ v (pairs draw) ]  
      (each [_ y (pairs v) &until impossible]
        (if (is-more-than-bag?  y)
          (set impossible true))))
    (if (not impossible) game-number 0)))

; Solution to Part 1
(fn game-score []
  (accumulate [sum 0 l (io.lines "cubes/input")] 
    (+ sum (line-score l))))
(print (game-score))

; Part 2
(fn all-draws [game]
  (let [d (icollect [_ v (ipairs (. (line->table game) 1))]
            (icollect [_ y (ipairs v)]
              (draw->table y)))]
    d))

(fn flat-game [game]
  (let [flat []]
    (each [_ el (ipairs (all-draws game))]
      (each [_ tbl (ipairs el)]
        (table.insert flat tbl)))
    flat))

(fn max-for-color [fgame color]
  (let [vals []]
  (each [_ el (ipairs fgame)]
    (if (= color (. el :color))
           (table.insert vals (. el :number))))
  (math.max (unpack vals))))

(fn max-for-game [game]
  (let [total []]
    (each [_ c (pairs colors)]
      (table.insert total (max-for-color game c)))
    (mult total)))

; Solution to part 2
(fn min-set []
  (accumulate [sum 0 l (io.lines "cubes/input")] 
    (+ sum (max-for-game (flat-game l)))))
(print (min-set))
