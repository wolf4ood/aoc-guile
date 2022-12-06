(use-modules (ice-9 textual-ports)
             (srfi srfi-171)
             (ice-9 match))


(define (sum-to-first num lst)
  (cond
   ((null? lst) (cons num lst))
   (else (cons (+ num (car lst)) (cdr lst)))))


(define rgroup
  (case-lambda
    (() '())
    ((result) result)
    ((result input)
     (cond
      ((string-null? input) (cons 0 result))
      (else (sum-to-first (string->number input) result))))))


(define groups (call-with-input-file "input.txt"
  (lambda (port)
      (port-transduce identity rgroup '() get-line port))))

(define (solve n)
    (list-head (sort groups >) n))


(display "Part 1: ")
(display (apply + (solve 1)))
(newline)

(display "Part 2: ")
(display (apply + (solve 3)))
(newline)
