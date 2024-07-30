#!/usr/bin/env -S csi -s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sunflower.scm - Tools for designing and generating sunflower LED PCBs.
;;;
;;; Lays out arrays of LEDs in pleasing patterns reminiscent of sunflowers
;;; based on the descriptions and Excel Spreadsheet at
;;; https://web.archive.org/web/20090916234127/http://www.mcs.surrey.ac.uk/Personal/R.Knott/Fibonacci/fibnat2.html#demos
;;;
;;;
;;;  Copyright (C) 2023, Andy Bennett
;;;  All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person obtaining a
;;;  copy of this software and associated documentation files (the "Software"),
;;;  to deal in the Software without restriction, including without limitation
;;;  the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;  and/or sell copies of the Software, and to permit persons to whom the
;;;  Software is furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be included in
;;;  all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;  DEALINGS IN THE SOFTWARE.
;;;
;;; Andy Bennett <andyjpb@ashurst.eu.org>, 2023/09/01 20:37
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use waffle)
(use numbers srfi-1)
(use matchable irregex)

; Process command line arguments

(if (not (= 3 (length (command-line-arguments))))
  (begin
    ;(printf "Usage: sunflower.scm <ratio> <n>\n")
    (fprintf (current-error-port) "Usage: sunflower.scm <n> <pcb> <output>\n")
    ;(fprintf (current-error-port) "  ratio - Seeds are placed <ratio> of a turn from the previous seed.\n")
    (fprintf (current-error-port) "      n - Number of seeds to place.\n")
    (fprintf (current-error-port) "    pcb - Kicad PCB input file.\n")
    (fprintf (current-error-port) " output - Prefix for output file names.\n")
    (exit 1)))

;(define ratio        (string->number (first  (command-line-arguments))))
(define ratio        0.61805556)  ; 0.61805 0.61805556 0.61818182 0.61834
;(define n_seeds      (string->number (second (command-line-arguments))))
(define n_seeds      (string->number (first (command-line-arguments))))
(define pcb-input    (second (command-line-arguments)))
(define output       (third  (command-line-arguments)))
(define pi           3.14159265359)
(define scale        3.45)
(define led-diameter 5.85)
(define lead-spacing 2.54)
(define pad-size     1.8)
(define hole-size    0.9)
(define hole-keepout 2.2)
(define track-width  0.250)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SVG stuff

; Things in source code that are actually widgets.
(define declared-widgets (make-parameter '()))

; Bless a symbol in the source code as a widget.
; (declare-widget <widget-definition> <section>
;                 documentation: "<url-to-higher-level-documentation-or-standard>"
;                 note:          "text"
;                 contains:      `(<widget> '<literal-symbol> "literal-text" literal-number ,anything)
;                 attributes:    `((attribute type: <type> note: "note" url: "url" contains: `(<widget> '<literal-symbol> "literal-text" literal-number ,anything))
;                                  ...))
(define-syntax declare-widget
  (syntax-rules ()
    ((declare-widget widget-name category . rest)
     (declared-widgets
       (cons
         (cons 'widget-name widget-name)
         (declared-widgets))))))

; Widget Definition Template
(define (widget)
  `((markup . `("hi"))

    (attributes . ())))

(declare-widget widget template)


(define (svg-template)
  `((markup .
            `(*TOP*
               (literal "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
               (svg (@
                      (xmlns "http://www.w3.org/2000/svg")
                      (version "1.0")
                      (width   ,width "mm")
                      (height  ,height "mm")
                      ; Set up the user co-ordinate system in the same units as width and height (mm).
                      (viewBox ,(sprintf "~A ~A ~A ~A" 0 0 width height)))

                    (style
                      "*{stroke:#000;stroke-width:0.1;}"
                      ".led-bg{stroke:#000;fill:yellow;font-size:0.5mm}"
                      ".led-fg{stroke:none;fill:yellow;font-size:0.5mm}"
                      ".led-txt{stroke:#000;fill:yelllow;font-size:0.5mm}"
                      ".led-anode{fill:red;}"
                      ".led-cathode{fill:green;}"
                      ".hole{stroke:#000;fill:white;}"
                      ".track-top{fill:none;stroke-width:0.250;stroke:red}"
                      ".track-bottom{fill:none;stroke-width:0.250;stroke:red}"
                      ".outline{stroke:#000;fill:#EEE;}"
                      ".box{stroke:#000;fill:none;stroke-opacity:0.5;stroke-linecap:square;}"
                      ".guides{stroke:#808080;fill:none;stroke-opacity:0.5;stroke-linecap:square;}"
                      ".minor{stroke-dasharray:1;stroke-linecap:butt;}"
                      ".watermark{fill:#000;;fill-opacity:0.5;}"
                      "text{font-family:sans-serif;}"
                      ".t10{font-size:10px;}"
                      ".t20{font-size:20px;}"
                      ".tr{text-anchor:end;}"
                      ".sn{stroke:#C8F;}"  ; Series with no data.
                      ".line-series {stroke-linecap: round;stroke-line-join: round;fill:none;}"
                      ".histogram-series {stroke-linecap: round;stroke-line-join: round;fill:none;}"
                      ".s0{stroke:#080;}"
                      ".s1{stroke:#800;}"
                      ".s2{stroke:#008;}"
                      ".s3{stroke:#088;}"
                      ".l0{fill:#080;}"
                      ".l1{fill:#800;}"
                      ".l2{fill:#008;}"
                      ".l3{fill:#088;}")

                    ,@contents)))
    (attributes . ((width  #f)
                   (height #f)))))

(declare-widget svg-template layout
                documentation: ""
                attributes:    `((width  type: integer
                                         note: "Width of the entire image.")
                                 (height type: integer
                                         note: "Height of the entire image.")))


(define (led)
  `((markup .
            `(use (@ (href "#led") (x ,x) (y ,y)
                     ,@(if style `((style ,style)) '()))))  ; Doesn't work!
    (attributes . ((x     #f)
                   (y     #f)
                   (style #f)))))

; Inkscape doesn't seem to support `use`.
(define (led)
  `((markup .
            `(g (@ (id ,id))
                (circle (@ (class "led-bg") (r ,(exact->inexact (/ led-diameter 2)))
                           (cx ,x) (cy ,y)
                           (style ,(conc "fill:" bg))))
                (circle (@ (class "led-fg") (r ,(exact->inexact (- (/ led-diameter 2) 1)))
                           (cx ,x) (cy ,y)
                           (style ,(conc "fill:" fg))))
                ,@(if id
                    `((text (@ (x ,x) (y ,(+ y 0.5)) (text-anchor "middle") (class "led-txt")
                               (transform "rotate(" ,(+ (/ (* ang 180) pi) 90) " " ,x " " ,y ")")
                               ,@(if txt
                                   `((style ,(conc "stroke:" txt)))
                                   '()))
                            ,id))
                    '())
                ,@contents))
    (attributes . ((id    #f)
                   (x     #f)
                   (y     #f)
                   (ang   #f)
                   (bg    #f)
                   (fg    #f)
                   (txt   #f)
                   (style #f)))))

(declare-widget led component)


(define (anode)
  `((markup .
            `(*TOP*
               (circle (@ (class "keepout")   (r ,(/ hole-keepout 2))
                          (cx ,x) (cy ,y)))
               (circle (@ (class "led-anode") (r ,(/ pad-size 2))
                          (cx ,x) (cy ,y)))
               (circle (@ (class "hole") (r ,(/ hole-size 2))
                          (cx ,x) (cy ,y)))))
    (attributes . ((x #f)
                   (y #f)))))

(declare-widget anode component)


(define (cathode)
  `((markup .
            `(*TOP*
               (circle (@ (class "keepout")   (r ,(/ hole-keepout 2))
                          (cx ,x) (cy ,y)))
               (circle (@ (class "led-cathode") (r ,(/ pad-size 2))
                          (cx ,x) (cy ,y)))
               (circle (@ (class "hole") (r ,(/ hole-size 2))
                          (cx ,x) (cy ,y)))))
    (attributes . ((x #f)
                   (y #f)))))

(declare-widget cathode component)


; Draws an SVG that represents a single layer of the PCB.
(define (draw-svg tracks)
  `(svg-template
     (@ (width ,width) (height ,height))

     ; Board outline.
     (circle (@ (class "outline") (cx ,mid-width) (cy  ,mid-height) (r 68)))
     (circle (@ (class "outline") (cx ,mid-width) (cy  ,mid-height) (r 55)))
     (circle (@ (class "outline") (cx ,mid-width) (cy  ,mid-height) (r 50)))

     ; Scale.
     (line (@ (x1 5) (y1 10) (x2 ,(- width 5)) (y2 10)
              (style "stroke-linecap: butt; strokewidth: 15;")))
     (text (@ (x 5) (y 20) (style "font-size: 2mm")) ,(conc (- width 10) "mm"))

     ; Parameters
     (text (@ (x 5) (y ,(- height 20)) (style "font-size: 2mm")) ,n_seeds)
     (text (@ (x 5) (y ,(- height 10)) (style "font-size: 2mm")) ,scale)

     (g (@ (transform ,(conc "translate(" mid-width "," mid-height ")")))

        ; Centre hole
        (circle (@ (cx 0) (cy 0) (r 1.5) (class "hole")))

        ; LED footprints.
        ,@(reverse
            (map
              (lambda (s)
                (let* ((n    (seed-n    s))
                       (r    (seed-r    s))
                       (ang  (seed-ang  s))
                       (x    (seed-x    s))
                       (y    (seed-y    s))
                       (row  (seed-row  s))
                       (col  (seed-col  s))
                       (chip (seed-chip s)))
                  `(led (@
                          (id ,n)
                          (x ,x) (y ,y) (ang ,ang)
                          ; Highlight the last one so that we can infer where future ones might be placed.
                          #;(bg ,(vector-ref colours row))
                          #;(fg ,(if (= chip 0)
                                 (vector-ref colours col)
                                 "#000"))
                          (txt ,(if (= chip 0)
                                  "#000"
                                  "#AAA"))
                          )
                ; Anode lead
                ,(let* ((lead (seed-anode s))
                        (x    (lead-x lead))
                        (y    (lead-y lead)))
                   `(anode (@ (x ,x) (y ,y))))
                ; Cathode lead
                ,(let* ((lead (seed-cathode s))
                        (x    (lead-x lead))
                        (y    (lead-y lead)))
                   `(cathode (@ (x ,x) (y ,y))))
                )))
              seeds))

  ,@tracks
     )
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Waffle Integration

; Loads our widgets and stashes them somewhere safe.
; Returns a set of Waffle widget-rules containing just our widgets.
(import data-structures extras)
(define (load-widget-set)
  (parameterize ((widget-rules '()))
    (for-each
      (lambda (widget)
        (pp (conc "loading " (car widget)) (current-error-port))
        (add-widget (car widget) ((cdr widget))))
      (declared-widgets))

    (widgets (widget-rules))))

(define w (load-widget-set))

(define (write-svg seeds)
  (parameterize ((widget-rules w))
    (waffle-sxml->svg
      seeds)))

(define (write-svg-to-file filename seeds)
  (with-output-to-file filename
    (lambda ()
      (write-svg seeds))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A Sunflower Seed

(define-record lead x y)

(define-record seed r ang x y row col chip n anode cathode)

; Geometry of the LED drivers.
(define n_rows 8)
(define n_cols 16)
(define n_leds (* n_rows n_cols))

; How many LED drivers do we need?
(define n_chips 4)

(define (make-lead* r ang)
  (let ((x (* r (cos ang)))
        (y (* r (sin ang))))
    (make-lead x y)))

(define (make-anode r ang)
  (make-lead* (+ r (/ lead-spacing 2)) ang))

(define (make-cathode r ang)
  (make-lead* (- r (/ lead-spacing 2)) ang))

(define (make-seed* n)
  ; Maths from seedsDYNAMIC.xls
  (let* ((r    (* scale (sqrt n)))
         (ang  (* n ratio 2 pi))
         (x    (* r (cos ang)))
         (y    (* r (sin ang)))
         (row  (modulo (- n 1) n_rows))
         (col  (modulo (floor (/ (- n 1) 8)) n_cols))
         (chip (floor (/ (- n 1) n_leds))))
    (make-seed r ang x y row col chip
               n
               (make-anode   r ang)
               (make-cathode r ang))))

(define (seed-location seed)
  (cons (seed-x seed) (seed-y seed)))

(define location-x car)
(define location-y cdr)

; Cons up all the seeds we will need.
(define seeds
  (map
    make-seed*
    (iota n_seeds 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pcbnew stuff

; Reader for pcbnew specific symbol format.
; ${KISYS3DMOD}/LED_THT.3dshapes/LED_D5.0mm.wrl
; Reads the symbol in as a string because Chicken would escape a symbol
; containing these characters, symbol-escape doesn't seem to help (as it only
; affects the reader) and pcbnew seems to accept it formatted as a string.
(set-read-syntax! #\$
  (lambda (port)
    (let loop ((c (peek-char port))
               (l '(#\$)))
      (if (or (eof-object? c)
              (eq? #\) c)
              (char-whitespace? c))
        (list->string (reverse l))
        (let ((c (read-char)))
          (loop (peek-char) (cons c l)))))))

(define (set-angle properties ang)
  (map
    (match-lambda
      (('at x y . _)  ; (at 0 0) and (at 0 0 0) but not (at (xyz 0 0 0)).
       (list 'at x y (* -1 (/ (* ang 180) pi))))
      (a a))  ; else
    properties))

; Takes the sexprs from a kicad_pcb file, positions the LEDs, adds track hints
; for the top and bottom tracks to user layers and returns sexprs that
; represent the transformed kicad_pcb file.
(define (transform-pcb input)
  (append
    (map  ; Position the LEDs.
      (lambda (expr)
        (match expr
          (('module type . properties)       ; Select (module ...) forms.
           (let* ((reference (any            ; ...that have references.
                               (match-lambda
                                 (('fp_text 'reference reference . rest)
                                  reference)
                                 (else #f))
                               properties))
                  (reference (and reference  ; ...in the format that we want.
                                  (irregex-match
                                    '(: "D" (submatch (+ numeric)))  ; D([0-9]+)
                                    (symbol->string reference))))
                  (reference (and reference
                                  (string->number
                                    (irregex-match-substring reference 1)))))
             (if reference
               (let* ((seed (make-seed* reference))
                      (properties
                        (map
                          (match-lambda
                            (('at _x _y . _)  ; Set the location of the module itself.
                             (list 'at
                                   (+ mid-width  (seed-x seed))
                                   (+ mid-height (seed-y seed))
                                   (* -1 (/ (* (seed-ang seed) 180) pi))))
                            ; Set the angle of constituent parts of the module.
                            (('fp_text . properties) `(fp_text ,@(set-angle properties (seed-ang seed))))
                            (('pad     . properties) `(pad     ,@(set-angle properties (seed-ang seed))))
                            (a a))  ; else
                          properties)))
                 `(module ,type ,@properties))
               expr)))
          (else expr)))
      input)
    `((gr_circle  ; Board outline
        (center ,mid-width         ,mid-height)
        (end    ,(+ mid-width 50) ,(+ mid-height 50))
        (layer  "Edge.Cuts")
        (width  0.05))
      (gr_circle  ; Centre hole
        (center ,mid-width         ,mid-height)
        (end    ,(+ mid-width 1.5) ,(+ mid-height 1.5))
        (layer  "Edge.Cuts")
        (width  0.05))

      ; Row traces
      ; Visit the seeds in order as they form a spiral.
      ,@(fold
          (lambda (row+chip lines)
            (let* ((row       (car row+chip))
                   (chip      (cdr row+chip))
                   (seeds     (filter
                                (lambda (s)
                                  (and (= row  (seed-row  s))
                                       (= chip (seed-chip s))))
                                seeds))
                   (leads     (map seed-anode seeds)))
              (append
                lines
                (if (null? leads)
                  '()  ; No LEDs on this row for this chip!
                  (cdr  ; Chop off the entry that passes the end of the last line to the next iteration of the fold.
                    (fold
                      (lambda (l p)
                        (let* ((start   (car p))
                               (start-x (first  start))
                               (start-y (second start))
                               (end-x   (lead-x l))
                               (end-y   (lead-y l))
                               (line    (cdr p)))
                          `((,end-x ,end-y)  ; End of this segment; start of the next.
                            (gr_line
                              (start ,(+ mid-width start-x) ,(+ mid-height start-y))
                              (end   ,(+ mid-width end-x)   ,(+ mid-height end-y))
                              (layer "Eco1.User")
                              (width 0.15))
                            ,@line)))
                      (list (list (lead-x (car leads)) (lead-y (car leads))))
                      (cdr leads)))))))
          '()
          (product
            (iota n_rows)
            (iota n_chips)))

      ; Col traces
      ; Sort the seeds by angle to form a circle.
      ; TODO: work out how to transform from the SVG arc format to the Kicad one.
      #;,@(fold
          (lambda (col+chip lines)
            (let* ((col       (car col+chip))
                   (chip      (cdr col+chip))
                   (seeds     (filter
                                (lambda (s)
                                  (and (= col  (seed-col  s))
                                       (= chip (seed-chip s))))
                                seeds))
                   (min-n     (if (null? seeds) '() (seed-n (car seeds))))
                   (seeds     (sort
                                seeds
                                (lambda (a b)
                                  (<
                                    (normalise (seed-ang a))
                                    (normalise (seed-ang b))))))
                   (head      (find-tail (lambda (x) (= min-n (seed-n x))) seeds))
                   (seeds     (if (null? seeds)
                                seeds
                                (let loop ((tail      seeds)
                                           (new-seeds head))
                                  (if (eq? tail head)
                                    new-seeds
                                    (loop (cdr tail)
                                          (append new-seeds (list (car tail))))))))
                   (leads     (map seed-cathode seeds)))
              (append
                lines
              (if (null? seeds)
                '()  ; No LEDs on this column for this chip!
                (cdr  ; Chop off the entry that passes the end of the last line to the next iteration of the fold.
                  (fold
                    (lambda (s p)
                        (let* ((start     (car p))
                               (start-x   (first  start))
                               (start-y   (second start))
                               (start-ang (third start))
                               (end       (seed-cathode s))
                               (end-x     (lead-x end))
                               (end-y     (lead-y end))
                               (end-ang   (seed-ang s))
                               (line      (cdr p)))
                          `((,end-x ,end-y ,end-ang)  ; End of this segment; start of the next.
                            (gr_arc
                              (start ,mid-width           ,mid-height)
                              (end   ,(+ mid-width end-x) ,(+ mid-height end-y))
                              (angle ,(* -1 (/ (* (- start-ang end-ang) 180) pi)))
                              (layer "Eco2.User")
                              (width 0.15))
                            ,@line)))
                      (list (list (lead-x (seed-cathode (car seeds))) (lead-y (seed-cathode (car seeds))) (seed-ang (car seeds))))
                      (cdr seeds)))))))
          '()
          (product
            (iota n_cols)
            (iota n_chips)))



      )))

(define (write-kicad-pcb input)
  (pp
    (transform-pcb input)))

(define (write-kicad-pcb-to-file filename input)
  (with-output-to-file filename
    (lambda ()
      (write-kicad-pcb input))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Program

(define width  200)
(define height 200)

(define mid-width  (exact->inexact (/ width  2)))
(define mid-height (exact->inexact (/ height 2)))

(define colours
  (vector
    "antiquewhite"
    "aqua"
    "blue"
    "blueviolet"
    "cadetblue"
    "coral"
    "thistle"
    "gold"
    "peru"
    "pink"
    "plum"
    "red"
    "tan"
    "lightsteelblue"
    "palegreen"
    "yellowgreen"))

; Cartesian product of two lists.
(define (product a b)
  (flatten
    (map
      (lambda (a)
        (map
          (lambda (b)
            (cons a b))
          b))
      a)))

; Normalise an angle to a value between 0 and 2pi.
(define (normalise ang)
  (let ((turns (floor (/ ang (* 2 pi)))))
    (- ang (* turns 2 pi))))


(write-svg-to-file
  (conc output ".top.svg")
  (draw-svg

    ; Row traces
    ; Visit the seeds in order as they form a spiral.
    (map
      (lambda (row+chip)
        (let* ((row       (car row+chip))
               (chip      (cdr row+chip))
               (seeds     (filter
                            (lambda (s)
                              (and (= row  (seed-row  s))
                                   (= chip (seed-chip s))))
                            seeds))
               (leads     (map seed-anode seeds)))
          (if (null? leads)
            '()  ; No LEDs on this row for this chip!
            `(path (@ (class "track-top")
                      (style ,(conc "stroke:" (vector-ref colours row)))
                      (d
                        ,(fold
                           (lambda (l p)
                             (conc p " " (lead-x l) " " (lead-y l)))
                           (conc "M" (lead-x (car leads)) " " (lead-y (car leads)))
                           (cdr leads))))))))
      (product
        (iota n_rows)
        (iota n_chips)))))

(write-svg-to-file
  (conc output ".bottom.svg")
  (draw-svg

    ; Col traces
    ; Sort the seeds by angle to form a circle.
    (map
      (lambda (col+chip)
        (let* ((col       (car col+chip))
               (chip      (cdr col+chip))
               (seeds     (filter
                            (lambda (s)
                              (and (= col  (seed-col  s))
                                   (= chip (seed-chip s))))
                            seeds))
               (min-n     (if (null? seeds) '() (seed-n (car seeds))))
               (seeds     (sort
                            seeds
                            (lambda (a b)
                              (<
                                (normalise (seed-ang a))
                                (normalise (seed-ang b))))))
               (head      (find-tail (lambda (x) (= min-n (seed-n x))) seeds))
               (seeds     (if (null? seeds)
                            seeds
                            (let loop ((tail      seeds)
                                       (new-seeds head))
                              (if (eq? tail head)
                                new-seeds
                                (loop (cdr tail)
                                      (append new-seeds (list (car tail))))))))
               (leads     (map seed-cathode seeds)))
          (if (null? seeds)
            '()  ; No LEDs on this column for this chip!
            `(path (@ (class "track-bottom")
                      (style ,(conc "stroke:" (vector-ref colours col)))
                      (d
                        ,(fold
                           (lambda (s p)
                             (let ((l (seed-cathode s)))
                               (conc p "A " (seed-r s) " " (seed-r s) " 0 0 1 " (lead-x l) " " (lead-y l))))
                           (conc "M" (lead-x (seed-cathode (car seeds))) " " (lead-y (seed-cathode (car seeds))))
                           (cdr seeds))))))))
      (product
        (iota n_cols)
        (iota n_chips)))))

(write-kicad-pcb-to-file
  (conc output ".kicad_pcb")
  (with-input-from-file pcb-input read))

