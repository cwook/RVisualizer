;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname networktest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

(define (s sec) (* sec 44100))

(define SONG (rs-read/clip "/tmp/rct2theme.wav" (s 15) (s 45)))
(define SONGLEN (rs-frames SONG))

(define INT-WORLD 1)
(define world-box (box INT-WORLD))
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
                (+ (prev ctr 0) incr) ;; (* 10 44100) is where the ctr starts, aka the song start position in frames
                #;(maybe-wrap 
                   (prev ctr 0)
                   incr
                   len)
                ]))

(define (netwrk) 
  (network ()
   [ctr <= (flexloop SONGLEN) (unbox world-box)]
   [out = (begin
            (set-box! cur-frame (floor ctr))
            (rs-ith/left SONG (floor ctr)))]
   ))

(signal-play (netwrk))


(define (draw w)
  (begin
    (set-box! world-box w)
    (overlay
     (above
      (text (number->string (unbox world-box)) 16 "black")
      (text (string-append "f: " (number->string (unbox cur-frame))) 16 "black"))
     (circle (+ 10 (* 10 (rs-ith/left SONG (unbox cur-frame)))) "solid" "red")
     (empty-scene 100 100)
     )))

(define (input w key)
    (if (= w 1) 0 1)
  )

(define (tock w)
  w
  )

(big-bang INT-WORLD
          [to-draw draw]
          [on-key input]
          [on-tick tock]
          )