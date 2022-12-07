(use-modules (ice-9 textual-ports)
             (ice-9 curried-definitions)
             (srfi srfi-171)
             (ice-9 match))



(define (letter->choice letter)
  (match letter
   ((or "A" "X") 'rock)
   ((or "B" "Y") 'paper)
   ((or "C" "Z") 'scissors)))

(define (letter->choice-or-strategy letter)
  (match letter
   ("A" 'rock)
   ("B" 'paper)
   ("C" 'scissors)
   ("X" 'lose)
   ("Y" 'draw)
   ("Z" 'win)))

(define (round->score round)
  (let ((score (cond ((won? round) 6)
                     ((draw? round) 3)
                     (else 0))))
    (begin
      (+ score (choice->score (car (cdr round)))))))

(define (won? round)
  (match round
    (('rock 'paper) #t)
    (('paper 'scissors) #t)
    (('scissors 'rock) #t)
    (_ #f)))

(define (draw? round)
  (equal? (car round) (car (cdr round))))

(define (choice->score choice)
  (match choice
    ('rock 1)
    ('paper 2)
    ('scissors 3)))

(define (apply->strategy round)
  (list (car round) (strategy->choice round)))

(define (strategy->choice round)
  (match round
    ((other 'draw) other)
    (('rock 'lose) 'scissors)
    (('paper 'lose) 'rock)
    (('scissors 'lose) 'paper)
    (('rock 'win) 'paper)
    (('paper 'win) 'scissors)
    (('scissors 'win) 'rock)))

(define ((line->round mapper) line)
  (list-transduce (tmap mapper) rcons (string-split line char-whitespace?)))


(define part1 (call-with-input-file "day2.txt"
  (lambda (port)
    (let ((trans (compose (tmap (line->round letter->choice))
                          (tmap round->score))))
      (port-transduce trans + get-line port)))))

(define part2 (call-with-input-file "day2.txt"
  (lambda (port)
    (let ((trans (compose (tmap (line->round letter->choice-or-strategy))
                          (tmap apply->strategy)
                          (tmap round->score))))
      (port-transduce trans + get-line port)))))


(display "Part 1: ")
(display part1)
(newline)


(display "Part 2: ")
(display part2)
(newline)
