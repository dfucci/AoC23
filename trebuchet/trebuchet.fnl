; The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. 
; On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
; For example:
; 1abc2
; pqr3stu8vwx
; a1b2c3d4e5f
; treb7uchet
; In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
(local string->digit {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"})

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

; Lua does not support fancy regexes. This function creates a table of decreasing substrings of a string.
(fn substrings [x]
  (local acc [])
  (for [i 1 (length x)]
    (table.insert acc (string.sub x i (length x))))
  acc)

; The idea is to check if the first character of the substring is a number. If it is, return it. If not, check if there is a match (i.e., the substring starts at the beginning) in the string-->digit table
(fn extract-digits-from-string [x]
  (icollect [_ v (pairs (substrings x))]
    (let [first (string.sub v 1 1)]
      (if (tonumber first) first 
        (. (icollect [string digit (pairs string->digit)]
             (let [(found _) (string.find v string)]
               (if  (= found 1) digit
                 nil))) 1)))))

(fn join-number-line [n]
  (if (= (length n) 1)
    (tonumber (.. (. n 1) (. n 1)))
    (tonumber (.. (. n 1) (. n (length n)))))) 

(fn sum-lines [filename]
  (var sum 0)
    (each [l (io.lines filename)]
      (set sum (+ sum (join-number-line (extract-digits-from-string l)))))
    sum)


(print (sum-lines "input")) ; 55652


