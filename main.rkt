#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;Graphics;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define game-over #f)


(define redcoin (bitmap/file "images/redcoin.png"))
(define yellowcoin (bitmap/file "images/yellowcoin.png"))
(define youwin (bitmap/file "images/youwin.png"))
(define compwin (bitmap/file "images/computerwins.png"))
(define x 0)
(define click-count 0)
(define drawgame (bitmap/file "images/drawgame.png"))
(define play2win (bitmap/file "images/redwins.png"))
(define play1win (bitmap/file "images/yellowwins.png"))

(define y 0)
(define boardpage (bitmap/file "images/board.png"))

(define page 'startpage)
(define startpage "images/start-image.png")
(define second "images/second.png")
(define third "images/thirdpage.png")
(define instructions "images/instructions.png")
(define (create-image t)
  (cond [(eq? page 'startpage) (bitmap/file startpage)]
        [(eq? page 'second) (bitmap/file second)]
        [(eq? page 'third) (bitmap/file third)]
        [(eq? page 'instructions) (bitmap/file instructions)]
        [(eq? page 'game1) (place-image boardpage 390 335 (empty-scene 780 670))]
        [(eq? page 'game2) (place-image boardpage 390 335 (empty-scene 780 670))]))

;;;;;; structs for construction of tree ;;;;;;;;;;;;;;;;;;;;

(struct gnode (val subtrees) #:transparent)

(struct leaf (val) #:transparent)

;;;;;;; class for each slot in board ;;;;;;;;;;;;;;;;;;;;;;;
(define slot%
  (class object%
    (super-new)
    (init-field color)
    (field (counted #f))))

;;;;;;; functions for operations on 2d-vectors ;;;;;;;;;;;;;;

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

;;;;;;;; class for board  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define board%
  (class object%
    (super-new)
    (init-field ccolor) 
    (init-field pcolor)
    (define state (make-2d-vector 6 7 0))
    (define/public (show) state 
      (define (show-h row col)
        (cond [(= row 6) (void)]
              [(= col 7) (begin (newline) (show-h (+ row 1) 0))]
              [else (begin (if (eq? (2d-vector-ref state row col) 0) (display 0)
                               (display (color row col)))
                           (display " ") (show-h row (+ col 1)))]))
      (show-h 0 0))
    
    (define/public (pullback! col)
      (define (helper row)
        (cond [(number? (2d-vector-ref state row col)) (helper (+ row 1))]
              [else (2d-vector-set! state row col 0)]))
      (helper 0))
    
    (define/public (filled? n)
      
      (not (number? (2d-vector-ref state 0 n))))
    
    (define/public (turn player col)
      (fill! player col state))
    
    
    (define (color i j)
      (get-field color (2d-vector-ref state  i j)))
    
    (define/public (checkboard  r c color)
      (cond ((= c 7) #f)
            ((= r 6) (checkboard 0 (+ c 1) color))
            ((number? (2d-vector-ref state r c)) (checkboard (+ r 1) c color))  
            (else (if (checkwin r c color) #t (checkboard (+ r 1) c color))))) 
    
    (define/public (checkwin r c coin)
      (define (checkwin-h dir-x dir-y row col)
        (cond [(or (> row 5) (< row 0) (> col 6) (< col 0)) 0]
              [(number? (2d-vector-ref state row col)) 0]
              [(eq? (color row col) coin) (+ 1 (checkwin-h dir-x dir-y 
                                                           (+ row dir-y) 
                                                           (+ col dir-x)))]
              [else 0]))
      (cond [(>=  (checkwin-h  0 1 r c) 4) #t]
            [(>=  (+ (checkwin-h -1 0 r  c)
                     (checkwin-h 1 0 r  (+ c 1))) 4) #t]
            [(>= (+ (checkwin-h 1 1 r c)
                    (checkwin-h -1 -1 (- r 1) (- c 1))) 4) #t]
            [(>= (+ (checkwin-h -1 1 r c)
                    (checkwin-h 1 -1 (- r 1) (+ c 1))) 4) #t]
            [else #f]))
    
    (define/public 
      (count dir-x dir-y row col n )
      
      (cond [(or (> row 5) (< row 0) (> col 6) (< col 0)) 0]
            [(= n 0) 0]
            
            [(number? (2d-vector-ref state row col)) (count dir-x dir-y 
                                                            (+ row dir-y) 
                                                            (+ col dir-x) (- n 1)) ]
            [ (get-field counted (2d-vector-ref state row col) ) 0]
            [(eq? (color row col) ccolor)
             (begin (set-field! counted  (2d-vector-ref state row col) #t)  
                    (+ 1 (count dir-x dir-y 
                                (+ row dir-y) 
                                (+ col dir-x) (- n 1))))]
            [else 0]))    
    
    
    (define/public (score)
      (define threes 0)
      (define twos 0)
      (define (helper dir-x dir-y r c )
        (define (set-uncounted! r c)
          (cond [(= c 7) (void)]
                [(= r 6) (set-uncounted! 0 (+ c 1))]
                [ (not (number? (2d-vector-ref state r c)) ) 
                  (begin (set-field! counted (2d-vector-ref state r c) #f) 
                         ( set-uncounted! (+ r 1) c))]
                [else (set-uncounted! (+ r 1) c)]))
        (define
          (countincempty dir-x dir-y row col n )
          
          (cond [(or (> row 5) (< row 0) (> col 6) (< col 0)) 0]
                [(= n 0) 0]
                
                [(number? (2d-vector-ref state row col)) 
                 (+ 1 (countincempty dir-x dir-y 
                                     (+ row dir-y) 
                                     (+ col dir-x) (- n 1))) ]
                
                [(eq? (color row col) ccolor)
                 (begin 
                   (set-field! counted (2d-vector-ref state row col) #t)                                                 
                   (+ 1 (countincempty dir-x dir-y 
                                       (+ row dir-y) 
                                       (+ col dir-x) (- n 1))))]
                [else 0]))
        
        (define
          (count dir-x dir-y row col n)
          
          (cond [(or (> row 5) (< row 0) (> col 6) (< col 0)) 0]
                [(= n 0) 0]
                
                [(number? (2d-vector-ref state row col))  
                 (count dir-x dir-y 
                        (+ row dir-y) 
                        (+ col dir-x) 
                        (- n 1))  ]
                [(eq? (color row col) ccolor)
                 (begin 
                   (set-field! counted (2d-vector-ref state row col) #t)                               
                   (+ 1 (count dir-x dir-y 
                               (+ row dir-y) 
                               (+ col dir-x) (- n 1))))]
                [else 0]))
        
        (cond [(= c 7)
               (begin (set-uncounted! 0 0) 
                      (cond [(= dir-x 1) (helper (- dir-y) dir-x 0 0) ]
                            [(= dir-x 0) (helper 1 dir-y 0 0)]))] 
              
              [(= r 6) (helper dir-x dir-y 0 (+ c 1))]
              
              [(number? (2d-vector-ref state r c))  (helper dir-x dir-y (+ r 1) c)]
              
              [else (begin (let ((n (count dir-x dir-y r c 3)))
                             (if (> (countincempty dir-x dir-y r c 4) 3)
                                 (cond [ (= n 3) (set! threes (+ 1 threes))]
                                       [ (= n 2) (set! twos (+ 1 twos))])
                                 (void))
                             (helper dir-x dir-y (+ r 1) c)
                             ))]))
      
      (cond[(send board checkboard 0 0 pcolor) -10000000]
           [(send board checkboard 0 0 ccolor) 10000000]
           [else
            (begin
              (helper 1 0 0 0)
              (+ (* 10000 threes) (* 10 twos)))
            ]))
    
    (define (fill! coin c vec)
      (define (fill-h r)
        (if (number?  (2d-vector-ref vec r c)) 
            (begin (2d-vector-set! vec r c 
                                   (new slot% [color coin])) r)
            (fill-h (- r 1))))
      (fill-h 5))))


(define (build-tree board depth color)
  (define (buildtree-h board alpha beta minormax col subtrees color dep)
    (cond [(= col 7) (gnode  beta subtrees)]
          [(= dep 0) (leaf (send board score))]
          [(send board filled? col) (buildtree-h board alpha beta minormax (+ col 1) 
                                                 (append subtrees (list 'avoid)) color dep)]
          [else 
           (begin
             
             (let* ((row (send board turn color col))
                    (subtree (  
                              if (send board checkwin row col 
                                       color)
                                 (if (eq? color (get-field ccolor board))
                                     (gnode (* dep 10000000) '())
                                     (gnode (* dep -10000000) '()))
                                 
                                 
                                 (buildtree-h board beta 
                                              (if (eq? 'max minormax) +inf.0 -inf.0)  
                                              (if (eq? 'max minormax) 'min 'max)
                                              0 '() 
                                              (if (eq? color (get-field ccolor board)) 
                                                  (get-field pcolor board)
                                                  (get-field ccolor board)) (- dep 1))))
                    (val (if (gnode? subtree)
                             (gnode-val subtree)
                             (leaf-val subtree))))
               
               (begin (send board pullback! col)
                      (if (eq? 'max minormax) 
                          (if (> val alpha)
                              (buildtree-h board alpha +inf.0
                                           minormax 7 subtrees color dep)
                              (buildtree-h board alpha (if (> val beta) val beta) 
                                           minormax (+ col 1) 
                                           (append subtrees (list subtree)) color dep)
                              )
                          (if (< val alpha)
                              (buildtree-h board alpha -inf.0 
                                           minormax 7 subtrees color dep)
                              (buildtree-h board alpha (if (< val beta) val beta)
                                           minormax (+ col 1) 
                                           (append subtrees (list subtree)) color dep)
                              ) ))
               ))]))
  (buildtree-h board +inf.0 -inf.0 'max 0 '() (get-field ccolor board) depth))

;;;;;;;; creation of board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define board
  (new board% (ccolor 'y) (pcolor 'r)))
(define (checkdraw n)
  (cond [(= n 7) #t]
        [(send board filled? n) 
         (checkdraw (+ n 1))]
        
        [else #f]))
(define (drop n l)
  (if (= n 0) l
      (drop (- n 1) (cdr l))))

(define (dropnpull n)
  (cond [(= n 7) #f]
        [(send board filled? n) (dropnpull (+ n 1))]
        [else (let ((row (send board turn 'y n)))
                (if (send board checkwin row n 'y)
                    (set! boardpage 
                          (place-image  yellowcoin 
                                        (+ 59 (* 110 n)) 
                                        (+ 59 (* row 110)) 
                                        boardpage))
                    (begin (send board pullback! n)
                           (dropnpull (+ n 1)))))]))

(define (mouse-f world x1 y1 event)
  
  (if (and (mouse=? event "button-down") (not game-over))
      (cond [(and (eq? page 'startpage)
                  (<= x1 590) (<= y1 435) (>= x1 190) (>= y1 235)) 
             (set! page 'instructions)]
            [(eq? page 'instructions) (set! page 'second)]
            
            [(eq? page 'second) (cond
                                  [(and (>= y1 230) (<= y1 330) (>= x1 120) (<= x1 660)) 
                                   (set! page 'third)]
                                  [(and (>= x1 120) (<= x1 660) (>= y1 340) (<= y1 440))
                                   (set! page 'game2)])]
            
            [(eq? page 'third) (cond [(and (>= x1 200) (<= x1 354) (<= y1 450) (>= y1 354))
                                      (set! page 'game1)]
                                     [(and (>= x1 425) (<= x1 579) (>= y1 360) (<= y1 450))
                                      (begin (set! page 'game1)
                                             (send board turn 'y 3)
                                             (set! boardpage
                                                   (place-image yellowcoin
                                                                (+ 59 (* 110 3))
                                                                (+ 59 (* 5 110)) boardpage)))])]
            [(eq? page 'game2) 
             (if  (and (>= x1 0) (<= x1 780) (>= y1 0) (<= y1 670))
                  
                  (let ((move (if (> x1 770) 6 (floor (/ x1 110)))))
                    
                    (if (send board filled? move)
                        (void)
                        
                        (if (even? click-count)
                            (let ((row (send board turn 'y move)))
                              (begin 
                                (set! boardpage 
                                      (place-image yellowcoin 
                                                   (+ 59 (* 110 move)) 
                                                   (+ 59 (* row 110)) boardpage))
                                (set! click-count (+ click-count 1)) 
                                (if (send board checkwin row move 'y) 
                                    (begin (set! game-over #t)
                                           (set! boardpage (place-image play1win 390 335 boardpage)))
                                    (void))))
                            
                            (let ((row (send board turn 'r move)))
                              (begin 
                                (set! boardpage 
                                      (place-image redcoin 
                                                   (+ 59 (* 110 move)) 
                                                   (+ 59 (* row 110)) 
                                                   boardpage))
                                
                                (set! click-count (+ click-count 1))
                                (if (send board checkwin row move 'r) 
                                    (begin (set! game-over #t)
                                           (set! boardpage (place-image play2win 390 335 boardpage)))
                                    (void)))))))
                  (void))]
            
            [(eq? page 'game1)
             (if  (and (>= x1 0) (<= x1 780) (>= y1 0) (<= y1 670))
                  (let ((move (if (> x1 770) 6 (floor (/ x1 110)))))
                    (if (send board filled? move)
                        (void)
                        
                        (begin (let (
                                     (row (send board turn 'r move)))
                                 (begin (set! boardpage 
                                              (place-image redcoin
                                                           (+ 59 (* 110 move))
                                                           (+ 59 (* row 110)) boardpage))
                                        (cond [(checkdraw 0)
                                               (begin (set! game-over #t)
                                                      (set! boardpage 
                                                            (place-image  drawgame
                                                                          390 335 boardpage)))]
                                              
                                              [(send board checkboard 0 0 'r) 
                                               (begin (set! game-over #t)
                                                      (set! boardpage 
                                                            (place-image  youwin
                                                                          390 335 boardpage)))]
                                              [(dropnpull 0) (void)]
                                              [else
                                               (let* ((tree (build-tree board 4 'y))
                                                      (col (bestmove (gnode-subtrees tree) ))
                                                      (row1 (send board turn 'y  col)))
                                                 
                                                 (set! boardpage (place-image  
                                                                  yellowcoin
                                                                  (+ 59 (* 110 col)) 
                                                                  (+ 59 (* row1 110)) 
                                                                  boardpage)))]))
                                 (cond [(checkdraw 0)
                                        (begin (set! game-over #t)
                                               (set! boardpage 
                                                     (place-image  drawgame
                                                                   390 335 boardpage)))]
                                       [(send board checkboard 0 0 'y)
                                        (begin (set! game-over #t) 
                                               (set! boardpage 
                                                     (place-image  compwin  390 335 boardpage)))]
                                       )   ))))
                  (void))])
      (void)))




(define (bestmove l)
  
  (define (maxbranch l bestmovesofar currentpos maxvaluesofar)
    (cond [(null? l) (cons bestmovesofar maxvaluesofar)]
          [(eq? 'avoid (car l))
           (maxbranch (cdr l)                                            
                      (if (= currentpos bestmovesofar) 
                          (+ currentpos 1) 
                          bestmovesofar)
                      
                      (+ currentpos 1)
                      maxvaluesofar)]
          [(> (gnode-val (car l)) maxvaluesofar) (maxbranch (cdr l) currentpos (+ currentpos 1) 
                                                            (gnode-val (car l)))]
          [else  (maxbranch (cdr l) bestmovesofar (+ currentpos 1) maxvaluesofar)]))
  
  
  (let ((max1 (maxbranch (drop 3 l) 0 0 -inf.0))
        (max2 (maxbranch (drop 4 (reverse l)) 0 0 -inf.0)))
    (if (>= (cdr max1) (cdr max2)) (+ 3 (car max1))
        (- 2 (car max2))))
  )    


(big-bang 0
          (to-draw create-image)
          (on-mouse mouse-f)
          )
