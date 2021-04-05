#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (update-helper f counters index '())
      )

(define (update-helper f counters index acc)
  (if (null? counters)
      acc
      (if (= index (match (car counters) [(counter index tt et queue) index]))
          (update-helper f (cdr counters) index (append acc (map f (list (car counters)))))
          (update-helper f (cdr counters) index (append acc (list (car counters))))
           ))
  )

(define (tt+ minutes)
    (lambda (C)
      (struct-copy counter C [tt (+ (match C [(counter index tt et queue) tt]) minutes)] [et (+ (counter-et C) minutes)]))
  )

(define (et+ minutes)
    (lambda (C)
      (struct-copy counter C [et (+ (match C [(counter index tt et queue) et]) minutes)]))
  )

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
        (struct-copy counter C [tt (+ (counter-tt C) items)] [et (+ (counter-et C) items)] [queue (enqueue (cons name items) (counter-queue C))])
        (struct-copy counter C [tt (+ (counter-tt C) items)] [queue (enqueue (cons name items) (counter-queue C))])
        )
    )
  )

(define (min-function acc counters)
  (lambda (f)
    (if (null? counters)
      (cons (counter-index acc) (f acc))
      (if (> (f acc) (f (car counters)))
          ((min-function (car counters) (cdr counters)) f)
          ((min-function acc (cdr counters)) f)
          )))
  )

(define (min-tt counters)
  ((min-function (car counters) (cdr counters)) counter-tt))
(define (min-et counters)
  ((min-function (car counters) (cdr counters)) counter-et))

(define (remove-first-from-counter C)   ; testată de checker
  (struct-copy counter C [tt (if (= (queue-size-l (counter-queue C)) 0)
                                 (if (= (queue-size-r (counter-queue C)) 0)
                                     0
                                     (apply + (apply list (map cdr (cdr (reverse (queue-right (counter-queue C))))))))
                                 (if (= (queue-size-r (counter-queue C)) 0)
                                     (apply + (apply list (map cdr (cdr (queue-left (counter-queue C))))))
                                     (apply + (append (apply list (map cdr (cdr (queue-left (counter-queue C))))) (apply list (map cdr (cdr (queue-right (counter-queue C)))))))))
                              ]
               [et (if (null? (top (dequeue (counter-queue C))))
                       0
                       (cdr (top (dequeue (counter-queue C)))))]
               [queue (dequeue (counter-queue C))]
               )
  )


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!


(define (pass-time-through-counter minutes)
  (λ (C)
    (if (queue-empty? (counter-queue C))
        (struct-copy counter C [tt 0] [et 0])
        (struct-copy counter C [tt (- (counter-tt C) minutes)] [et (- (counter-et C) minutes)]))
    )
  )
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  (serve-helper requests '() fast-counters slow-counters))

(define (serve-helper requests outs fast-counters slow-counters)
  (if (null? requests)
      (cons outs (append fast-counters slow-counters))
      (match (car requests)

        [(list 'ensure average)        
         (if (< average (/ (apply + (apply list (map counter-tt (append slow-counters fast-counters)))) (length (append fast-counters slow-counters))))
             (serve-helper requests outs fast-counters (append slow-counters (list (empty-counter (add1 (counter-index (car (reverse slow-counters))))))))
             (serve-helper (cdr requests) outs fast-counters slow-counters)
             )]

        [(list name n-items)
         (if (> n-items ITEMS)
             (serve-helper (cdr requests) outs fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))
             (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                 (serve-helper (cdr requests) outs (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters)
                 (serve-helper (cdr requests) outs fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))
                 )
             )
         ]

        [(list 'delay index minutes)   
         (serve-helper (cdr requests) outs (update (tt+ minutes) fast-counters index) (update (tt+ minutes) slow-counters index))
         ]

        [x
         (serve-helper (cdr requests) (append (create-out-pair x (append slow-counters fast-counters)) outs) (minute-by-minute-list x fast-counters) (minute-by-minute-list x slow-counters))
         ]
        )
      )
  )

(define (non-empty-counters counters)
  (filter (lambda (C) (not (queue-empty? (counter-queue C)))) counters)
  )

(define (create-out-pair x counters)
  (map cons (map counter-index (filter (lambda (C) (<= (counter-et C) x)) (non-empty-counters counters))) (map car (map top (map counter-queue (filter (lambda (C) (<= (counter-et C) x)) (non-empty-counters counters))))))
  )

(define (minute-by-minute x)
  (lambda (C)
    (if (= x 0)
        (if (= (counter-et C) 0)
            (if (not (queue-empty? (counter-queue C)))
                (remove-first-from-counter C)
                C
                )
            C
            )
        (if (= (counter-et C) 0)
            (if (not (queue-empty? (counter-queue C)))
                ((minute-by-minute x) (remove-first-from-counter C))
                C
                )
            ((minute-by-minute (sub1 x)) (struct-copy counter C [tt (sub1 (counter-tt C))] [et (sub1 (counter-et C))]))
            )
        )
    
    )
  )

(define (minute-by-minute-list x counters)
  (map (minute-by-minute x) counters)
  )