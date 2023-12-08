; The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. 
; On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
; For example:
; 1abc2
; pqr3stu8vwx
; a1b2c3d4e5f
; treb7uchet
; In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

(fn remove-letters [str]
  (string.gsub str "%a+" ""))
  
(fn extract-number [str]
  (let [n (remove-letters str)]
    (if (= (length n) 1)
      (tonumber (.. n n))
      (tonumber (.. (string.sub n 1 1) (string.sub n -1))))))

(fn sum-lines [filename]
  (var sum 0)
    (each [l (io.lines filename)]
      (set sum (+ sum (extract-number l))))
    sum)
(print (sum-lines "input")) ; 56108

