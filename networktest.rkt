;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname networktest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

(define (s sec) (* sec 44100))

(define SONG (rs-read/clip "/tmp/rct2theme.wav" (s 15) (s 45)))
(define SONGLEN (rs-frames SONG))

(define INT-WORLD 1.0)
(define world-box (box INT-WORLD))

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
           [ctr = (maybe-wrap 
                   (prev ctr 0)
                   incr
                   len)]))

(define (playing?)
  (unbox world-box))

(define (netwrk) 
  (network ()
   [ctr <= (flexloop SONGLEN) (playing?)]
   [out = (rs-ith/left SONG (floor ctr))]
   ))

(signal-play (netwrk))


(define (draw w)
  (begin
    (set-box! world-box w)
    (overlay
     (text (number->string (unbox world-box)) 16 "black")
     (empty-scene 100 100))
    ))

(define (input w key)
    (if (= w 1) 0.0 1.0)
  )

(big-bang INT-WORLD
          [to-draw draw]
          [on-key input]
          [state true]
          )