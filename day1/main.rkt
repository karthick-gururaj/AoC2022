#lang racket


; Accumulate calories for a given elf and return total calories
(define (accumulate-elf-calories file [cal 0])
  (let ([token (read-line file)])
    (cond
      ((eof-object? token) cal)
      ((eq? token newline) cal) ; TODO: Not working as expected
      (else (let ([num-token (string->number token)])
        (if num-token
            (accumulate-elf-calories file (+ cal num-token))
            cal))))))

; Insert new-num into in-list (keep sorted) and return the list truncated to
; max n elements
(define (maximal-n-list n in-list new-num)
   (let ([in-list-sorted (sort (append in-list (list new-num)) > )])
      (if (> (length in-list-sorted) n)
          (take in-list-sorted n)
          in-list-sorted)))

; Loop through all entries and pick the max n entries
(define (get-max-calorie file n [current_max '()])
  (if (eof-object? (peek-byte file))
      current_max
      (let ([next-calorie (accumulate-elf-calories file)])
        (get-max-calorie file n (maximal-n-list n current_max next-calorie)))))


(define (day1-part1 file)
  (define in-file (open-input-file file))

  ; For the first part - return the elf with max calories
  (let ([result (foldr + 0 (get-max-calorie in-file 1))])
    (close-input-port in-file)
    result))

(define (day1-part2 file)
  (define in-file (open-input-file file))

  ; For the second part - top 3 elf calories, summed up
  (let ([result (foldr + 0 (get-max-calorie in-file 3))])
    (close-input-port in-file)
    result))

(day1-part1 "./input.txt")
(day1-part2 "./input.txt")
