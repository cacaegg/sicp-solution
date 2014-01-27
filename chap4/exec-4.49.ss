(define (require q)
  (if (not q) (amb)))

;; Speech of Words
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define adjectives '(adj interesting shocking lovely))

;; Grammars
;(define (parse-sentence)
;  (list 'sentence
;        (parse-noun-phrase)
;        (parse-word verbs)))
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

;(define (parse-noun-phrase)
;  (list 'noun-phrase
;        (parse-word articles)
;        (parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-noun-phrase)
  (define (maybe-extend article)
    (amb (list 'simple-noun-phrase
               article
               (parse-word nouns))
         (list 'simple-noun-phrase
               article
               (parse-word adjectives)
               (parse-word nouns))))
  (maybe-extend (parse-word articles)))


(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-word word-list)
  (define (choose-word ls)
    (require (not (null? ls)))
    (amb (car ls) (choose-word (cdr ls))))
  (require (not (null? *unparsed*)))
  (let ((found-word (choose-word (cdr word-list))))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;; Start Here
(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(parse '(the professor lectures to the student in the class with the cat))
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
