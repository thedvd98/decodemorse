;; Programma scritto in chicken scheme
;; TODO codificare in codice morse
;; TODO prendere comandi linea di comando
(define ordered-alphabet "**ETIANMSURWDKGOHVF?L?PJBXCYZQ??54?3???2??+????16=/?????7???8?90")

;; array di lettere in ordinate per creare
;; l'albero per la decodifica del codice morse
(define ordered-alphabet '(#\* #\* #\E #\T #\I #\A #\N #\M #\S #\U #\R #\W #\D #\K #\G #\O #\H #\V #\F #\? #\L #\? #\P #\J #\B #\X #\C #\Y #\Z #\Q #\? #\? #\5 #\4 #\? #\3 #\? #\? #\? #\2 #\? #\? #\+ #\? #\? #\? #\? #\1 #\6 #\= #\/ #\? #\? #\? #\? #\? #\7 #\? #\? #\? #\8 #\? #\9 #\0))

(define list-position)

(define (entry tree)
  (car tree))

(define (make-tree index left right)
  (list index left right))

(define (go-left entry)
  (cadr entry))

(define (go-right entry)
  (caddr entry))

(define (init-albero n fine)
  (cond ((>= n fine) '())
		((< n fine)
		 (make-tree
		   n
		   (init-albero (* n 2) fine)
		   (init-albero (+ (* n 2) 1) fine)))))

;; decodifica di codice morse di un solo carattere
(define (decode-morse morsecode albero)
  (cond ((null? albero)
		 "")

		((null? morsecode)
		 (make-string
		   1
		   (list-ref
			 ordered-alphabet (entry albero))))

		((char=? (car morsecode) #\.)
		 (decode-morse
		   (cdr morsecode)
		   (go-left albero)))

		((char=? (car morsecode) #\-)
		 (decode-morse
		   (cdr morsecode)
		   (go-right albero)))))

;; decodifica di una lista di stringhe
;; ogni stringa deve essere una lettera in morse
(define (decode-morse-string morsecode albero)
  (cond ((null? morsecode) "")
		(else
		  (string-append
			(decode-morse
			  (string->list
				(car morsecode))
			  albero)
			(decode-morse-string
			  (cdr morsecode)
			  albero)))))

(define (morse-decode morse)
  (define albero (init-albero 1 63))
  (decode-morse-string
	(string-split morse)
	albero))
;; Example
;;(print (morse-decode ".- - . ... -"))
