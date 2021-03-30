#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)



;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (struct-copy counter C [tt (+ (match C
    [(counter index tt queue)
     tt]) minutes)]))


; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (min-tt counters)
  (min-tt-helper (car counters) (cdr counters))
  )

(define (min-tt-helper acc counters)
  (if (null? counters)
      (cons (match acc [(counter index tt queue) index]) (match acc [(counter index tt queue) tt]))
      (if (> (match acc [(counter index tt queue) tt]) (match (car counters) [(counter index tt queue) tt]))
          (min-tt-helper (car counters) (cdr counters))
          (min-tt-helper acc (cdr counters))
          )))


; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (define newC (struct-copy counter C [tt (+ (match C [(counter index tt queue) tt]) n-items)] [queue (append (match C [(counter index tt queue) queue]) (list (cons name n-items)))]))
  newC
  )


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes)
         (cond
               ((= index 1) (serve (cdr requests) (make-counter (counter-index C1) (+ (counter-tt C1) minutes) (counter-queue C1) ) C2 C3 C4))
               ((= index 2) (serve (cdr requests) C1 (make-counter (counter-index C2) (+ (counter-tt C2) minutes) (counter-queue C2)) C3 C4))
               ((= index 3) (serve (cdr requests) C1 C2 (make-counter (counter-index C3) (+ (counter-tt C3) minutes) (counter-queue C3)) C4))
               (else (serve (cdr requests) C1 C2 C3 (make-counter (counter-index C4) (+ (counter-tt C4) minutes) (counter-queue C4))))
               )
         ]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (cond
               ((= (match (select-counter (list C1 C2 C3 C4)) [(counter index tt queue)index]) 1) (serve (cdr requests) (make-counter (counter-index C1) (+ (counter-tt C1) n-items) (append (counter-queue C1) (list (cons name n-items)))) C2 C3 C4))
               ((= (match (select-counter (list C1 C2 C3 C4)) [(counter index tt queue)index]) 2) (serve (cdr requests) C1 (make-counter (counter-index C2) (+ (counter-tt C2) n-items) (append (counter-queue C2) (list (cons name n-items)))) C3 C4))
               ((= (match (select-counter (list C1 C2 C3 C4)) [(counter index tt queue)index]) 3) (serve (cdr requests) C1 C2 (make-counter (counter-index C3) (+ (counter-tt C3) n-items) (append (counter-queue C3) (list (cons name n-items)))) C4))
               (else (serve (cdr requests) C1 C2 C3 (make-counter (counter-index C4) (+ (counter-tt C4) n-items) (append (counter-queue C4) (list (cons name n-items))))))
               )
             (cond
               ((= (match (select-counter (list C2 C3 C4)) [(counter index tt queue)index]) 2) (serve (cdr requests) C1 (make-counter (counter-index C2) (+ (counter-tt C2) n-items) (append (counter-queue C2) (list (cons name n-items)))) C3 C4))
               ((= (match (select-counter (list C2 C3 C4)) [(counter index tt queue)index]) 3) (serve (cdr requests) C1 C2 (make-counter (counter-index C3) (+ (counter-tt C3) n-items) (append (counter-queue C3) (list (cons name n-items)))) C4))
               (else (serve (cdr requests) C1 C2 C3 (make-counter (counter-index C4) (+ (counter-tt C4) n-items) (append (counter-queue C4) (list (cons name n-items))))))
               )
             )
         ]
        )))

(define (get-tt C)
  (match C [(counter index tt queue) tt])
  )

(define (select-counter counters)
  (select-counter-helper (car counters) (cdr counters))
  )

(define (select-counter-helper acc counters)
  (if (null? counters)
      acc
      (if (> (match acc [(counter index tt queue) tt]) (match (car counters) [(counter index tt queue) tt]))
          (select-counter-helper (car counters) (cdr counters))
          (select-counter-helper acc (cdr counters))
          )))