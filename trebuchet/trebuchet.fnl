; The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. 
; On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
; For example:
; 1abc2
; pqr3stu8vwx
; a1b2c3d4e5f
; treb7uchet
; In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
(local string->digit {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"})

(fn replace-letters [str]
  (var _str str)
  (each [k v (pairs string->digit)]
    (set _str (string.gsub _str k v)))
  (string.gsub _str "%a+" "")
  _str)
  
(fn extract-number [str]
  (let [n (replace-letters str)]
    (if (= (length n) 1)
      (tonumber (.. n n))
      (tonumber (.. (string.sub n 1 1) (string.sub n -1))))))

(fn sum-lines [filename]
  (var sum 0)
    (each [l (io.lines filename)]
      (set sum (+ sum (extract-number l))))
    sum)
(print (sum-lines "examples")) 

(print (replace-letters "one1two2three3four4five5six6seven7eight8nine9"))

; Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
; Equipped with this new information, you now need to find the real first and last digit on each line. For example:
; two1nine
; eightwothree
; abcone2threexyz
; xtwone3four
; 4nineeightseven2
; zoneight234
; 7pqrstsixteen
; In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
(var sum 0)
(each [l (io.lines "examples")]
  (set sum (+ sum (extract-number l))))
(io.lines "examples") 
