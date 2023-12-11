; Games
; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
(local game1 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
(local game2 "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
(local game3 "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
(local game4 "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
(local game5 "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

; Bag
; 12 red cubes, 13 green cubes, and 14 blue cubes
(local bag {:red 12 :green 13 :blue 14})

(fn split-string [s d]
  (icollect [r (string.gmatch s (.. "[^" d "]+"))] r))

(fn remove-whitespaces [s]
  (string.gsub s "%s" ""))

(fn get-game-number [g]
  (tonumber (string.match g "%d+")))

(fn is-more-than-bag? [draw]
  (let [number (tonumber (string.match draw "%d+")) color (string.match draw "[^ %d+]+") bag-number (tonumber (. bag color))]
    (> number bag-number)))

(fn line-score [line]
  (var impossible false)
  (let [clean-game (remove-whitespaces line)
        game-split (split-string clean-game ":")
        game-number (get-game-number (. game-split 1))
        game-results (. game-split 2)
        draws (split-string game-results ";") 
        draw (icollect [_ v (ipairs draws)] (split-string v ","))]
    (each [_ v (pairs draw) ]  
      (each [_ y (pairs v) &until impossible]
        (if (is-more-than-bag?  y)
          (set impossible true))))
    (if (not impossible) game-number 0)))

(fn game-score []
  (accumulate [sum 0 l (io.lines "cubes/input")] 
  (+ sum (line-score l))))
(print (game-score))

 ; Part 2 
