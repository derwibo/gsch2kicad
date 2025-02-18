#!/usr/bin/guile -s
!#

; gsch2kicad.scm
;
; gEDA schematic to KiCad converter
;
; usage :
; (sch->kicad "filenameout.kicad_sch")
;
; Copyright (C) 2025 Michael Martens
;
; This program is free software: you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation, version 2.
;
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of 
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License 
; along with this program. If not, see <http://www.gnu.org/licenses/>.
;


(use-modules (geda page))
(use-modules (geda object))
(use-modules (geda attrib))
(use-modules (geda log))

(set! *random-state* (random-state-from-platform))

(define pi (acos -1))
(define xrefnum 0)


; Calculate KiCAD dimensions from gschem dimensions.
; To match a similar grid a reasonable factor seems to be 1.27/100

(define (coord->kicad coord)
	(/ (* coord 1.27) 100)
)

; Generate UUID's
; This function creates ver simple UUID's. But this seems to have enough uniqueness for this purpose here

(define (uuidgen)
	(define (uuidlstgen len)
		(let ((uuidchrs '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
			)
			(cond
				((> len 0) (cons (list-ref uuidchrs (random 16)) (uuidlstgen (- len 1))))
				(else '())
			)
		)
	)
	(string-append "706c9459-bd39-48a1-953c-" (list->string (uuidlstgen 12)))
)

; Find attribute object 'name'
; Returns the first matching attribute or #f if not found
(define (get-attrib-obj attribs name)
	(cond
		((null? attribs) #f) ; '())
		((equal? (attrib-name (car attribs)) name)
			(car attribs)
		)
		(else (get-attrib-obj (cdr attribs) name))
	)
)

; Find all attribute objects with name 'name'
; Returns a list with all matching attribute objects
(define (get-attrib-objs attribs name)
	(cond
		((null? attribs) '())
		((equal? (attrib-name (car attribs)) name)
			(cons (car attribs) (get-attrib-objs (cdr attribs) name))
		)
		(else (get-attrib-objs (cdr attribs) name))
	)
)

; Get the value for attribute 'name'
; Returns the text value of the first matching attribute or the default value if none found
(define (get-attrib attribs name default)
	(cond
		((eq? attribs '()) default)
		((equal? (attrib-name (car attribs)) name)
			(attrib-value (car attribs))
		)
		(else (get-attrib (cdr attribs) name default))
	)
)

; Get the number of attributes with name 'name'
(define (get-num-attrib attribs name)
	(cond
		((eq? attribs '()) 0)
		(else
			(+ (if (equal? (attrib-name (car attribs)) name) 1 0) (get-num-attrib (cdr attribs) name))
		)
	)
)

; Get the slotdef definition for slot 'slot'
; Returns a list with the pinnames for the queried slot
(define (get-slotdef attribs slot)
	;(format #t "Query slotdef for slot ~A\n" slot)
	(cond
		((null? attribs) '())
		((and (equal? (attrib-name (car attribs)) "slotdef") (string=? (substring (attrib-value (car attribs)) 0 (string-length slot)) slot))
			(string-split (substring (attrib-value (car attribs)) (+ (string-length slot) 1)) #\,)
		)
		(else (get-slotdef (cdr attribs) slot))
	)
)

; Print all names and values of attribute list 'attribs' to the log window and console
(define (log-attrib attribs)
	(cond
		((eq? attribs '()) #t)
		(else
			(format #t "\t~A : ~A\n" (attrib-name (car attribs)) (attrib-value (car attribs)))
			(log! 'debug "\t~A : ~A\n" attrib-name (car attribs) (attrib-value (car attribs)))
			(log-attrib (cdr attribs))
		)
	)
)

; Get the number of pins for symbol 'comp'
(define (get-pincount comp)
	(define (accumulate-pins items)
		(cond ((null? items) 0)
			(else
				(+ (cond ((pin? (car items)) 1)(else 0)) (accumulate-pins (cdr items)))
			)
		)
	)
	(accumulate-pins (component-contents comp))
)

; Print the symbol definition to the KiCAD-file
; stream    : Output-file
; complist  : List of comonent symbols that belong to this symbol definition
; kicad-name: name for the symbol definition to be used in the KiCAD-file
(define (componentsymbol->kicad stream complist kicad-name)
	(let* (
			(symbolmap '())
			(comp (car complist))
			(basename (component-basename comp))
			(unit 1)
			(compsym (make-component/library basename '(0 . 0) 0 #f #t))
			(attr (inherited-attribs compsym))
			(refdes (get-attrib attr "refdes" ""))
			(device (get-attrib attr "device" ""))
			(value (get-attrib attr "value" ""))
			(footprint (get-attrib attr "footprint" ""))
			(netname (get-attrib attr "netname" ""))
			(netcnt (get-num-attrib attr "net"))
			(pincount (get-pincount compsym))
			(pinnames-visible? #f)
			;(ispwr  (and (string=? refdes "") (not (string=? netname ""))))
			;(isxref (and (string=? refdes "") (string=? device "none") (equal? (get-num-attrib attr "net") 1)))
			(ispwr  (and (equal? pincount 1) (not (string=? netname ""))))
			(isxref (and (equal? pincount 1) (string=? device "none") (equal? netcnt 1)))
			(ismulti (or (> (length complist) 1) (and (not isxref) (not ispwr) (> netcnt 0)))) 
		)
		(format #t "Create symbol ~A with ~D units\n" kicad-name (length complist))
		;(log! 'info "Create symbol ~A with ~D units\n" kicad-name (length complist))
		(format stream "\t\t(symbol \"geda:~A\"\n" kicad-name)
		(cond
			((or ispwr isxref)
				(format stream "\t\t\t(power)\n")
				(format stream "\t\t\t(pin_names hide)\n")
			)
		)
		(format stream "\t\t\t(property \"Reference\" \"~A\" (id 0)\n" refdes)
		(format stream "\t\t\t\t(at 0 0 0)\n")
		(format stream "\t\t\t)\n")
		(format stream "\t\t\t(property \"Value\" \"~A\" (id 1)\n" value)
		(format stream "\t\t\t\t(at 0 0 0)\n")
		(format stream "\t\t\t)\n")
		(format stream "\t\t\t(property \"Footprint\" \"~A\" (id 2)\n" (if (not (string-null? footprint)) (string-append "geda:" footprint) ""))
		(format stream "\t\t\t\t(at 0 0 0)\n")
		(format stream "\t\t\t)\n")
		(format stream "\t\t\t(property \"Datasheet\" \"\" (id 3)\n")
		(format stream "\t\t\t\t(at 0 0 0)\n")
		(format stream "\t\t\t)\n")
		(while #t
			(let* (
					(slot (get-attrib (object-attribs comp) "slot" ""))
					(attr (inherited-attribs compsym))
					(slotdef (if (not (string-null? slot)) (get-slotdef attr slot) '()))
				)
				(cond
					(ismulti
						(format #t ".Adding Subsymbol ~A as unit ~D\n" basename unit)
						(format stream "\t\t\t(symbol \"~A_~D_~D\"\n" kicad-name unit 1)
					)
				)
				(for-each
					(lambda (item)
						(cond
							((box? item)
								(let* ((attr (object-attribs item))
										(p1 (box-top-left item))
										(p2 (box-bottom-right item))
										(x1 (car p1))
										(y1 (cdr p1))
										(x2 (car p2))
										(y2 (cdr p2))
									)
									(format stream "\t\t\t(rectangle\n")
									(format stream "\t\t\t\t(start ~F ~F)\n" (coord->kicad x1) (coord->kicad y1))
									(format stream "\t\t\t\t(end ~F ~F)\n" (coord->kicad x2) (coord->kicad y2))
									(format stream "\t\t\t\t(stroke\n")
									(format stream "\t\t\t\t\t(width 0)\n")
									(format stream "\t\t\t\t\t(type solid)\n")
									(format stream "\t\t\t\t\t(color 0 0 0 0)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t\t(fill\n")
									(format stream "\t\t\t\t\t(type none)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t)\n")
								)
							)
							((line? item)
								(let* ((attr (object-attribs item))
										(p1 (line-start item))
										(p2 (line-end item))
										(x1 (car p1))
										(y1 (cdr p1))
										(x2 (car p2))
										(y2 (cdr p2))
									)
									(format stream "\t\t\t(polyline\n")
									(format stream "\t\t\t\t(pts\n")
									(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x1) (coord->kicad y1))
									(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x2) (coord->kicad y2))
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t\t(stroke\n")
									(format stream "\t\t\t\t\t(width 0)\n")
									(format stream "\t\t\t\t\t(type solid)\n")
									(format stream "\t\t\t\t\t(color 0 0 0 0)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t\t(fill\n")
									(format stream "\t\t\t\t\t(type none)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t)\n")
								)
							)
							((circle? item)
								(let* ((attr (object-attribs item))
										(p1 (circle-center item))
										(r (circle-radius item))
										(x1 (car p1))
										(y1 (cdr p1))
										;(color (object-color item))
									)
									(format stream "\t\t\t(circle\n")
									(format stream "\t\t\t\t(center ~F ~F)\n" (coord->kicad x1) (coord->kicad y1))
									(format stream "\t\t\t\t(radius ~F)\n" (coord->kicad r))
									(format stream "\t\t\t\t(stroke\n")
									(format stream "\t\t\t\t\t(width 0)\n")
									(format stream "\t\t\t\t\t(type solid)\n")
									(format stream "\t\t\t\t\t(color 0 0 0 0)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t\t(fill\n")
									(format stream "\t\t\t\t\t(type none)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t)\n")
								)
							)
							((path? item)
								(let* (
										(attr (object-attribs item))
									)
									(format stream "\t\t\t(polyline\n")
									(format stream "\t\t\t\t(pts\n")
									(do ((i 0 (1+ i)))
										((>= i (path-length item)))
										(let* (
												(p (path-ref item i))
												(type (car p))
											)
											(cond
												((eq? type 'moveto)
													(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad (caadr p)) (coord->kicad (cdadr p)))
												)
												((eq? type 'lineto)
													(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad (caadr p)) (coord->kicad (cdadr p)))
												)
												((eq? type 'closepath)
													(set! p (path-ref item 0))
													(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad (caadr p)) (coord->kicad (cdadr p)))
												)
											)
										)
									
									)
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t\t(stroke\n")
									(format stream "\t\t\t\t\t(width 0)\n")
									(format stream "\t\t\t\t\t(type solid)\n")
									(format stream "\t\t\t\t\t(color 0 0 0 0)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t\t(fill\n")
									(format stream "\t\t\t\t\t(type none)\n")
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t)\n")
								)
							)
							((and (text? item) (not (attribute? item)))
								(let* (
										(obj item)
										(attr (object-attribs obj))
										(p1 (text-anchor obj))
										(x1 (car p1))
										(y1 (cdr p1))
										(align (text-align obj))
										(size 1.27);(size (* 0.127 (text-size obj)))
									)
									(format stream "\t\t\t(text \"~A\"\n" (text-string obj))
									(format stream "\t\t\t\t(at ~F ~F ~D)\n" (coord->kicad x1) (- (coord->kicad y1)) (* 90 (text-angle obj)))

									(format stream "\t\t\t\t(effects\n")
									(format stream "\t\t\t\t\t(font\n")
									(format stream "\t\t\t\t\t\t(size ~F ~F)\n" size size)
									(format stream "\t\t\t\t\t)\n")
									(cond
										((equal? align 'lower-left) (format stream "\t\t\t\t\t(justify left bottom)\n"))
										((equal? align 'middle-left) (format stream "\t\t\t\t\t(justify left)\n"))
										((equal? align 'upper-left) (format stream "\t\t\t\t\t(justify left top)\n"))
										((equal? align 'lower-center) (format stream "\t\t\t\t\t(justify bottom)\n"))
										((equal? align 'upper-center) (format stream "\t\t\t\t\t(justify top)\n"))
										((equal? align 'lower-right) (format stream "\t\t\t\t\t(justify right bottom)\n"))
										((equal? align 'middle-right) (format stream "\t\t\t\t\t(justify right)\n"))
										((equal? align 'upper-right) (format stream "\t\t\t\t\t(justify right top)\n"))
									)
									(format stream "\t\t\t\t)\n")
									(format stream "\t\t\t)\n")
								)
							)
						)
					)
					(component-contents compsym)
				)
				(for-each
					(lambda (item)
						(cond
							((pin? item)
								(let ((attr (object-attribs item))
									(p1 (line-start item))
									(p2 (line-end item)))
									(let* (
											(name  (get-attrib attr "pinlabel" ""))
											(number (get-attrib attr "pinnumber" ""))
											(pintype (get-attrib attr "pintype" ""))
											(x1 (car p1))
											(y1 (cdr p1))
											(x2 (car p2))
											(y2 (cdr p2))
											(rot 0)
											(len 3)
										)
										(cond
											((or ispwr isxref)
												(set! name kicad-name)
												(set! pintype "pwr") 
											)
										)
										(if (not (null? slotdef))
											(let (
													(pinseq (string->number (get-attrib attr "pinseq" "")))
												)
												(format #t "Query pinseq: ~D\n" pinseq)
												(set! number (list-ref slotdef (- pinseq 1)))
											)
										)
										(cond
											((and (equal? x1 x2) (> y1 y2)) (set! rot 270) (set! len (- y1 y2)))
											((and (equal? x1 x2) (< y1 y2)) (set! rot 90)  (set! len (- y2 y1)))
											((and (equal? y1 y2) (> x1 x2)) (set! rot 180) (set! len (- x1 x2)))
											(else (set! rot 0)   (set! len (- x2 x1)))
										)
										(format stream "\t\t\t(pin\n")
		;								(format stream "\t\t\t\tinput\n")
										(format stream "\t\t\t\t~A\n"
											(cond
												((string=? pintype "in")  "input")
												((string=? pintype "out") "output")
												((string=? pintype "io")  "bidirectional")
												((string=? pintype "oc")  "open_collector")
												((string=? pintype "oe")  "open_emitter")
												((string=? pintype "pas") "passive")
												((string=? pintype "tp")  "tri_state")
												((string=? pintype "tri") "tri_state")
												((string=? pintype "clk") "input")
												((string=? pintype "pwr") "power_in")
												(else "unspecified")
											)
										)
										(format stream "\t\t\t\tline\n")
										(format stream "\t\t\t\t(at ~F ~F ~F)\n" (coord->kicad x1) (coord->kicad y1) rot)
										(format stream "\t\t\t\t(length ~F)\n" (coord->kicad len))
										;(if ispwr (format stream "\t\t\t\thide\n"))
										(format stream "\t\t\t\t(name \"~A\")\n" name)
										(format stream "\t\t\t\t(number \"~A\")\n" number)
										(format stream "\t\t\t)\n")
										(if (not pinnames-visible?)
											(let
												((pinname-attr (get-attrib-obj attr "pinlabel")))
												(if (and (text? pinname-attr) (text-visible? pinname-attr)) (set! pinnames-visible? #t))
											)
										)
									)
								)
							)
						)
					)
					(component-contents compsym)
				)
				(cond
					(ismulti
						(format stream "\t\t\t(unit_name \"Unit ~D\")\n\t\t\t)\n" unit)
					)
				)
				(set! symbolmap (cons (cons (if (not (string-null? slot)) (string-append basename "-" slot) basename) (cons kicad-name unit)) symbolmap))
				(set! complist (cdr complist))
				(set! unit (+ unit 1))
				(if (null? complist) (break #f))
				(set! comp (car complist))
				(set! basename (component-basename comp))
				(set! compsym (make-component/library basename '(0 . 0) 0 #f #t))
			)
		)
		(cond
			; If the component is not a power or xref, but has some 'net' atributes (e.g. power and gnd connection definitions),
			; we create and add an additional symbol with a pin for each 'net' attribute since KiCad seems not to have a similar
			; solution
			((and (not isxref) (not ispwr) (> netcnt 0))
				(let (
						(netattr (get-attrib-objs attr "net"))
						(index 0)
						(x 0.0)
						(len 3.81)
					)
					(format stream "\t\t\t(symbol \"~A_~D_~D\"\n" kicad-name unit 1)
					(for-each
						(lambda (netattr)
							(let* (
									(pindef (string-split  (attrib-value netattr) #\:))
									(pinnumber (cadr pindef))
									(pinname (car pindef))
									(rot (if (even? index) 270 90))
									(y (if (even? index) 5.08 -5.08))
								)
								(format stream "\t\t\t(pin power_in line\n")
								(format stream "\t\t\t\t(at ~F ~F ~F)\n" x y rot)
								(format stream "\t\t\t\t(length ~F)\n" len)
								(format stream "\t\t\t\t(name \"~A\")\n" pinname)
								(format stream "\t\t\t\t(number \"~A\")\n" pinnumber)
								(format stream "\t\t\t)\n")
							)
							(if (odd? index) (set! x (+ x 7.62)))
							(set! index (+ index 1))
						)
						netattr
					)
					(format stream "\t\t\t(unit_name \"Unit ~D\")\n\t\t\t)\n" unit)
				)
			)
		)
		(cond
			((and (not (or ispwr isxref)) (not pinnames-visible?))
				(format stream "\t\t\t(pin_names hide)\n")
			)
		)
		(format stream "\t\t)\n")
		symbolmap
	)
)

; Output a symbol instance to the KiCAD-file
; stream          : Output-file
; comp            : gschem component to print
; kicad-symbolname: name of the KiCAD symbol definition
; kicad-unit      : unit of the KiCAD symbol definition
; project         : KiCAD project identifier string
; owner           : KiCAD sheet identifier
(define (component->kicad stream comp kicad-symbolname kicad-unit project owner)
	(let*
		(
			(pos (component-position comp))
			(x (car pos))
			(y (cdr pos))
			(rot (component-angle comp))
			(mirror (component-mirror? comp))
			(attr (object-attribs comp))
			(iattr (inherited-attribs comp))
			(refdes (get-attrib attr "refdes" ""))
			(graphical (get-attrib iattr "graphical" ""))
			(device (get-attrib attr "device" (get-attrib iattr "device" "")))
			(value (get-attrib attr "value" (get-attrib iattr "value" "")))
			(footprint (get-attrib attr "footprint" (get-attrib iattr "footprint" "")))
			(netname (get-attrib attr "netname" (get-attrib iattr "netname" "")))
			(net (get-attrib attr "net" ""))
			(pincount (get-pincount comp))
			(isxref (and (equal? pincount 1) (string=? device "none") (equal? (get-num-attrib attr "net") 1)))
			(ispwr  (and (equal? pincount 1) (not (string=? netname ""))))
			(ta-left "left")
			(ta-right "right")
			(ta-top "top")
			(ta-bottom "bottom")
		)
		(format #t "Symbol instance: ~A of ~A unit ~D\n" refdes kicad-symbolname kicad-unit)
		;(format #t " device: ~A\n" device)
		(if (not (string-null? graphical)) (format #t " graphical\n"))
		(cond
			(isxref
				(set! xrefnum (+ xrefnum 1))
				(set! refdes (format #f "#PWR~D" xrefnum))
				(format #t " xref ~A\n" refdes)
			)
		)
		(cond
			(ispwr
				(set! xrefnum (+ xrefnum 1))
				(set! refdes (format #f "#PWR~D" xrefnum))
				(set! value netname)
				(format #t " pwr ~A: ~A\n" refdes value)
			)
		)
		(format stream "\t(symbol\n")
		(format stream "\t\t(lib_id \"geda:~A\")\n" kicad-symbolname)
		(format stream "\t\t(at ~F ~F ~F)\n" (coord->kicad x) (- (coord->kicad y)) rot)
		(cond
			((eq? mirror #t)
				(cond
					((or (equal? rot 90) (equal? rot 270))
						(format stream "\t\t(mirror x)\n")
					)
					(else
						(format stream "\t\t(mirror y)\n")
					)
				)
				(set! ta-left "right")
				(set! ta-right "left")
			)
		)
		(format stream "\t\t(unit ~D)\n" kicad-unit)
		(cond
			((or (string=? graphical "1") (string=? device "none") (string>? netname "")) 
				(format stream "\t\t(in_bom no)\n\t\t(on_board no)\n")
			)
		)
		(if (not (string-null? footprint)) (set! footprint (string-append "geda:" footprint)))
		(let (
			(properties `(("Reference" 0 ,(get-attrib-obj attr "refdes") ,refdes) ("Value" 1 ,(get-attrib-obj attr "value") ,value) ("Footprint" 2 ,(get-attrib-obj attr "footprint") ,footprint) ("Datasheet" 3 ,(get-attrib-obj attr "datasheet"))))
			)
			(cond 
				(ispwr (set! properties (cons `("Netname" 100 ,(get-attrib-obj attr "netname")) properties)))
				((not (or ispwr isxref))
					(let ((netattr (get-attrib-objs attr "net"))
						(id 200)
						)
						(for-each
							(lambda (netattr)
								(append! properties `(("Net" ,id ,netattr ,(attrib-value netattr))))
								(format #t "Property Net: ~A\n" (attrib-value netattr))
								(set! id (+ id 1))
							)
							netattr
						)
					)
				)
			)
			(for-each
				(lambda (prop)
					(let*
						(
							(pname (car prop))
							(pid   (cadr prop))
							(pattr (caddr prop))
							(pvalue (cond
										((>= (length prop) 4) (cadddr prop))
										((attribute? pattr)  (attrib-value pattr))
										(else "")
									)
							)
							(ppos  (if (text? pattr) (text-anchor pattr) (cons x y)))
							(align (if (text? pattr) (text-align pattr) 'middle-center))
							(prot (if (text? pattr) (text-angle pattr) 0))
							(size 1.27) ; (if (text? pattr) (* (text-size pattr) 0.127) 1.27))
							(visible (if (text? pattr) (text-visible? pattr) #f))
							(show (if (text? pattr) (text-attribute-mode pattr) 'both))
						)
						(format stream "\t\t(property \"~A\" \"~A\" (id ~D)\n" pname pvalue pid)
						(format stream "\t\t\t(at ~F ~F ~F)\n" (coord->kicad (car ppos)) (- (coord->kicad (cdr ppos))) (- prot rot))
						(format stream "\t\t\t(effects\n")
						(format stream "\t\t\t\t(font\n")
						(format stream "\t\t\t\t\t(size ~F ~F)\n" size size)
						(format stream "\t\t\t\t)\n")
						(cond
							((equal? align 'lower-left) (format stream "\t\t\t\t(justify ~A ~A)\n" ta-left ta-bottom))
							((equal? align 'middle-left) (format stream "\t\t\t\t(justify ~A)\n" ta-left))
							((equal? align 'upper-left) (format stream "\t\t\t\t(justify ~A ~A)\n" ta-left ta-top))
							((equal? align 'lower-center) (format stream "\t\t\t\t(justify ~A)\n" ta-bottom))
							((equal? align 'upper-center) (format stream "\t\t\t\t(justify ~A)\n" ta-top))
							((equal? align 'lower-right) (format stream "\t\t\t\t(justify ~A ~A)\n" ta-right ta-bottom))
							((equal? align 'middle-right) (format stream "\t\t\t\t(justify ~A)\n" ta-right))
							((equal? align 'upper-right) (format stream "\t\t\t\t(justify ~A ~A)\n" ta-right ta-top))
						)
						(if (not visible) (format stream "\t\t\t\t(hide)\n"))
						(format stream "\t\t\t)\n")
						(format stream "\t\t)\n")
					)
				)
				properties
			)
		)
		(cond
			((or isxref ispwr)
				(format stream "\t\t(property \"ki_keywords\" \"power-flag\"\n")
				(format stream "\t\t\t(at 0 0 0)\n")
				(format stream "\t\t\t(effects\n")
				(format stream "\t\t\t\t(font\n")
				(format stream "\t\t\t\t\t(size 1.27 1.27)\n")
				(format stream "\t\t\t\t)\n")
				(format stream "\t\t\t\t(hide yes)\n")
				(format stream "\t\t\t)\n")
				(format stream "\t\t)\n")
			)
		)
		(format stream "\t\t(instances\n")
		(format stream "\t\t	(project \"~A\"\n" project)
		(format stream "\t\t\t\t(path \"/~A\"\n" owner)
		(format stream "\t\t\t\t\t(reference \"~A\")\n" refdes);
		(format stream "\t\t\t\t\t(unit ~D)\n" kicad-unit)
		(format stream "\t\t\t\t)\n")
		(format stream "\t\t\t)\n")
		(format stream "\t\t)\n")
		(format stream "\t)\n")
	)
)

; Output a gschem page to a KiCAD-file
; page            : gschem page
; file-name       : name to be used for the output file
(define (page->kicad page file-name)
	(let (
			(stream (open-output-file file-name))
			(components '())
			(symbols '())
			(symbolmap '())
			(project "gsch2kicad")
			(uuid (uuidgen))
		)
		(format stream "(kicad_sch\n\t(version 20240118)\n\t(generator GSCH2KICAD)\n")
		(format stream "\t(uuid \"~A\")\n" uuid)

		; Iterate the gschem page contents, collect all components and map them into an association list indexed by reference designators
		; For components with more than one symbol the symbols are concatented to a list as value for the refdes key
		; Symbols with no valid refernce designator are treated as single components and added with an empty name to the association list 
		(for-each
			(lambda (obj)
				(cond
					((component? obj)
						(let* (
								(attr (object-attribs obj))
								(ref (get-attrib attr "refdes" ""))
								(slot (get-attrib attr "slot" ""))
							)
							(cond 
								((or (string-null? ref) (string-any #\? ref))
;									(format #t "Adding nameless component: ~A\n" (component-basename obj))
									(set! components (append components (cons (cons "" (cons obj '())) '())))
								)
								(else
									(let (
											(refentry (assoc ref components))
										)
										(cond
											(refentry
;												(format #t "Adding to component: ~A to ~A\n" (component-basename obj) ref)
												(set-cdr! refentry (append (cdr refentry) (cons obj '())))
											)
											(else
;												(format #t "Adding new component: ~A of ~A\n" (component-basename obj) ref)
												(set! components (append components (cons (cons ref (cons obj '())) '())))
											)
										)
									)
								)
							)
						)
					)
				)
			)
			(page-contents page)
		)
		; Write all symbol definitions to the KiCAD file
		(format stream "\t(lib_symbols\n")
		(for-each
			(lambda (component)
				(let* (
						(ref (car component))
						(items (cdr component))
						(kicadname (component-basename (car items)))
					)
					(cond
						((not (member kicadname symbols))
							(set! symbolmap (append (componentsymbol->kicad stream items kicadname) symbolmap))
							(set! symbols (cons kicadname symbols))
						)
					)
				)
			)
			components
		)
		;(display symbolmap) (newline)
		; end of 'lib_symbol' section
		(format stream "\t)\n")
		
		(for-each
			(lambda (obj)
				(cond
					((component? obj)
						(let* (
								(basename (component-basename obj))
								(attr (object-attribs obj))
								(slot (get-attrib attr "slot" ""))
								(symboldef (if (not (string-null? slot)) (string-append basename "-" slot) basename))
								(kicaditem (assoc symboldef symbolmap))
							)
							(cond
								((pair? kicaditem)
									(component->kicad stream obj (cadr kicaditem) (cddr kicaditem) project uuid)
								)
								(else
									(format #t "ERROR: Component: ~A has no symboldef\n" symboldef)
								)
							)
						)
					)
					((net? obj)
						(let* (
								(attr (object-attribs obj))
								(p1 (line-start obj))
								(p2 (line-end obj))
								(x1 (car p1))
								(y1 (cdr p1))
								(x2 (car p2))
								(y2 (cdr p2))
							)
							(format stream "\t\t\t(wire\n")
							(format stream "\t\t\t\t(pts\n")
							(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x1) (- (coord->kicad y1)))
							(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x2) (- (coord->kicad y2)))
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t\t(stroke\n")
							(format stream "\t\t\t\t\t(width 0)\n")
							(format stream "\t\t\t\t\t(type solid)\n")
							(format stream "\t\t\t\t\t(color 0 0 0 0)\n")
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t)\n")
						)
					)
					((bus? obj)
						(let* (
								(attr (object-attribs obj))
								(p1 (line-start obj))
								(p2 (line-end obj))
								(x1 (car p1))
								(y1 (cdr p1))
								(x2 (car p2))
								(y2 (cdr p2))
							)
							(format stream "\t\t\t(bus\n")
							(format stream "\t\t\t\t(pts\n")
							(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x1) (- (coord->kicad y1)))
							(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x2) (- (coord->kicad y2)))
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t\t(stroke\n")
							(format stream "\t\t\t\t\t(width 0)\n")
							(format stream "\t\t\t\t\t(type solid)\n")
							(format stream "\t\t\t\t\t(color 0 0 0 0)\n")
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t)\n")
						)
					)
					((and (text? obj) (not (attribute? obj)))
						(let* (
								(attr (object-attribs obj))
								(p1 (text-anchor obj))
								(x1 (car p1))
								(y1 (cdr p1))
								(align (text-align obj))
								(size 1.27) ; (* 0.127 (text-size obj)))
							)
							(format stream "\t\t\t(text \"~A\"\n" (text-string obj))
							(format stream "\t\t\t\t(at ~F ~F ~D)\n" (coord->kicad x1) (- (coord->kicad y1)) (* 90 (text-angle obj)))

							(format stream "\t\t\t\t(effects\n")
							(format stream "\t\t\t\t\t(font\n")
							(format stream "\t\t\t\t\t\t(size ~F ~F)\n" size size)
							(format stream "\t\t\t\t\t)\n")
							(cond
								((equal? align 'lower-left) (format stream "\t\t\t\t\t(justify left bottom)\n"))
								((equal? align 'middle-left) (format stream "\t\t\t\t\t(justify left)\n"))
								((equal? align 'upper-left) (format stream "\t\t\t\t\t(justify left top)\n"))
								((equal? align 'lower-center) (format stream "\t\t\t\t\t(justify bottom)\n"))
								((equal? align 'upper-center) (format stream "\t\t\t\t\t(justify top)\n"))
								((equal? align 'lower-right) (format stream "\t\t\t\t\t(justify right bottom)\n"))
								((equal? align 'middle-right) (format stream "\t\t\t\t\t(justify right)\n"))
								((equal? align 'upper-right) (format stream "\t\t\t\t\t(justify right top)\n"))
							)
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t)\n")
						)
					)
					((line? obj)
						(let* ((attr (object-attribs obj))
								(p1 (line-start obj))
								(p2 (line-end obj))
								(x1 (car p1))
								(y1 (cdr p1))
								(x2 (car p2))
								(y2 (cdr p2))
							)
							(format stream "\t\t\t(polyline\n")
							(format stream "\t\t\t\t(pts\n")
							(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x1) (coord->kicad y1))
							(format stream "\t\t\t\t\t(xy ~F ~F)\n" (coord->kicad x2) (coord->kicad y2))
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t\t(stroke\n")
							(format stream "\t\t\t\t\t(width 0)\n")
							(format stream "\t\t\t\t\t(type solid)\n")
							(format stream "\t\t\t\t\t(color 0 0 0 0)\n")
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t\t(fill\n")
							(format stream "\t\t\t\t\t(type none)\n")
							(format stream "\t\t\t\t)\n")
							(format stream "\t\t\t)\n")
						)
					)
					
				)
			)
			(page-contents page)
		)

		(format stream ")\n")
		(close-port stream)
	)
)

; Output the active page to a KiCAD-file
(define (sch->kicad file-name)
	(page->kicad (active-page) file-name)
)
