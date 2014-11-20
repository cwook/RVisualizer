;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname networktest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

(define (s sec) (* sec 44100))

(define SONG (rs-read/clip "/tmp/rct2theme.wav" (s 15) (s 45)))
(define SONGLEN (rs-frames SONG))
(define ctr (box 5))

(define-struct world 
  [p c1now c1go] ;; we should remove c1now and c1go at some point and implement an object list
  )

(define INT-WORLD (make-world 1 100 100))

(define play-speed (box (world-p INT-WORLD)))
(define cur-frame (box 0))

;; increment "old" by "incr", and wrap around if necessary
;; number number number -> number
(define (maybe-wrap old incr len)
  (local [(define new (+ old incr))]
    (cond [(<= len new) (- new len)]
          [(< new 0) (+ len new)]
          [else new])))

;; a network that smoothly moves through a sound
(define (flexloop len)
  (network (incr)
           [ctr = 
                (+ (prev ctr 0) (unbox play-speed)) ;; in (prev ctr 0), "0" is where the ctr starts, aka the song start position in frames
                #;(maybe-wrap 
                   (prev ctr 0)
                   incr
                   len)
                ]))

(define (netwrk) 
  (network ()
   [ctr <= (flexloop SONGLEN) (unbox play-speed)]
   [out = (begin
             (set-box! cur-frame (abs (inexact->exact (floor ctr))))
            (* 1 (rs-ith/left SONG (floor ctr))))] ;; todo: make 1 a variable (determines volume)
   ))

(signal-play (netwrk))


(define (draw w)
  (begin
    (set-box! play-speed (world-p w))
    (overlay
     (above
      (text (number->string (unbox play-speed)) 16 "black")
      (text (string-append "f: " (number->string (unbox cur-frame))) 16 "black"))
     (circle (world-c1now w) "solid" "red")
     (empty-scene 1200 720)
     )))

(define (input w key)
    (if (= (world-p w) 1) 
        (make-world 0 (world-c1now w) (world-c1go w)) 
        (make-world 1 (world-c1now w) (world-c1go w))
        ))

(define (tock w)
  (begin
    (if (> (unbox ctr) 0)
        (set-box! ctr (sub1 (unbox ctr)))
        (set-box! ctr 3)
        )
  (if (= (unbox ctr) 0)
      (make-world (world-p w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (+ 35 (* 150 (rs-ith/left SONG (unbox cur-frame)))))
      (make-world (world-p w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (world-c1go w))
  )))

(big-bang INT-WORLD
          [to-draw draw]
          [on-key input]
          [on-tick tock 1/60]
          )
