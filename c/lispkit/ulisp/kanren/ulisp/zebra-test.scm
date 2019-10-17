(display "Zebra") (newline)

;   1. There are five houses in a row, each of a different color
;       and inhabited by men of different nationalities,
;       with different pets, drinks, and cigarettes.
;   2. The Englishman lives in the red house.
;   3. The Spaniard owns a dog.
;   4. Coffee is drunk in the green house.
;   5. The Ukrainian drinks tea.
;   6. The green house is directly to the right of the ivory house.
;   7. The Old Gold smoker owns snails.
;   8. Kools are being smoked in the yellow house.
;   9. Milk is drunk in the middle house.
;  10. The Norwegian lives in the first house on the left.
;  11. The Chesterfield smoker lives next to the fox owner.
;  12. Kools are smoked in the house next to the house where the horse is kept.
;  13. The Lucky Strike smoker drinks orange juice.
;  14. The Japanese smokes Parliaments.
;  15. The Norwegian lives next to the blue house.

; (define memb 
;   (extend-relation (a1 a2)
;     (fact (item) item `(,item . ,_))
;     (relation (item rest) (to-show item `(,_ . ,rest)) (memb item rest))))

(define memb 
  (relation (head-let item lst) 
    (any (== lst `(,item . ,_))
      (exists (rest)
	(if-only (== lst `(,_ . ,rest)) (memb item rest))))))


(define next-to
  (relation (head-let item1 item2 rest)
    (any (on-right item1 item2 rest) (on-right item2 item1 rest))))

(define on-right
  (extend-relation (a0 a1 a2)
    (fact (item1 item2) item1 item2 `(,item1 ,item2 . ,_))
    (relation ((once item1) (once item2) rest)
      (to-show item1 item2 `(,_ . ,rest))
      (on-right item1 item2 rest))))
        
(define zebra
  (relation (head-let h)
    (if-only
      (all!
        (== h `((norwegian ,_ ,_ ,_ ,_) ,_ (,_ ,_ milk ,_ ,_) ,_ ,_))
        (memb `(englishman ,_ ,_ ,_ red) h)
        (on-right `(,_ ,_ ,_ ,_ ivory) `(,_ ,_ ,_ ,_ green) h)
        (next-to `(norwegian ,_ ,_ ,_ ,_) `(,_ ,_ ,_ ,_ blue) h)
        (memb `(,_ kools ,_ ,_ yellow) h)
        (memb `(spaniard ,_ ,_ dog ,_) h)
        (memb `(,_ ,_ coffee ,_ green) h) 
        (memb `(ukrainian ,_ tea ,_ ,_) h)
        (memb `(,_ luckystrikes oj ,_ ,_) h)
        (memb `(japanese parliaments ,_ ,_ ,_) h)
        (memb `(,_ oldgolds ,_ snails ,_) h)
        (next-to `(,_ ,_ ,_ horse ,_) `(,_ kools ,_ ,_ ,_) h)
        (next-to `(,_ ,_ ,_ fox ,_) `(,_ chesterfields ,_ ,_ ,_) h)
        )
      (all (memb `(,_ ,_ water ,_ ,_) h)
	(memb `(,_ ,_ ,_ zebra ,_) h)))))


(equal?
  (solution (h) (zebra h))
  '((h.0 ((norwegian kools water fox yellow)
          (ukrainian chesterfields tea horse blue)
          (englishman oldgolds milk snails red)
          (spaniard luckystrikes oj dog ivory)
          (japanese parliaments coffee zebra green)))))
