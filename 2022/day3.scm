(use-modules (ice-9 textual-ports)
             (srfi srfi-171)
             (srfi srfi-1))


(define (chars->compartments line)
  (let ((mid (/ (length line) 2)))
    (list (take line mid) (take-right line mid))))

(define (item-priority item)
  (- (char->integer item) (if (char-lower-case? item) 96 38)))

(define (calc-priority items)
  (item-priority (first items)))

(define (compartments-intersect compartments)
    (apply lset-intersection eq? compartments))


(define priority (call-with-input-file "day3.txt"
  (lambda (port)
    (let ((trans (compose (tmap string->list)
                          (tmap chars->compartments)
                          (tmap compartments-intersect)
                          (tmap calc-priority))))
      (port-transduce trans + get-line port)))))

(define (groups-badge groups)
  (apply lset-intersection eq? groups))

(define badges (call-with-input-file "day3.txt"
  (lambda (port)
    (let ((trans (compose (tmap string->list)
                          (tsegment 3)
                          (tmap groups-badge)
                          (tmap calc-priority))))
      (port-transduce trans + get-line port)))))


(display "Part 1: ")
(display priority)
(newline)

(display "Part 2: ")
(display badges)
(newline)
