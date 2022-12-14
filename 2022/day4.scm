(use-modules (ice-9 textual-ports)
             (ice-9 curried-definitions)
             (srfi srfi-171)
             (srfi srfi-1)
             (ice-9 match))


(define (contains f s)
  (>= (* (- (first f) (first s)) (- (second s) (second f))) 0))

(define ((is? pred) groups)
  (pred (first groups) (second groups)))

(define (overlaps f s)
  (>= (* (- (second s) (first f)) (- (second f) (first s))) 0))


(define (parse-range range)
  (map string->number (string-split range #\-)))

(define (line->pair line)
  (map parse-range (string-split line #\,)))

(define (part pred)
  (call-with-input-file "day4.txt"
    (lambda (port)
    (let ((trans (compose (tmap line->pair)
                          (tfilter (is? pred)))))
      (port-transduce trans rcount get-line port)))))




(display "Part 1: ")
(display (part contains))
(newline)

(display "Part 2: ")
(display (part overlaps))
(newline)
