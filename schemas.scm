(define-public (find_restaurant) (begin
    (let (
        (restaurants (va-get-restaurant-recommendation))
    ) (begin
        ; Delete previous recommendations
        (va-remove-generic-fact "restaurant recommendation")

        ; Generate the new recommendations
        (EvaluationLink
            (PredicateNode (va-prefix "restaurant recommendation"))
            (ListLink restaurants)
        )
    ))
))

(define-public (get_reservation_pax) (begin
    (let* (
        (pax (va-get-pax))
    ) (begin
        (cond
            ((null? pax) (cog-logger-warn "No mention of PAX yet!") (False))
            ((> (length pax) 2) (cog-logger-warn "More than one PAX mention found!") (False))
            (else (WordNode (car (string-split (cog-name (car pax)) #\.))))
        )
    ))
))

(define-public (get_recommendation) (begin
    (let* (
        (query
            (EvaluationLink
                (PredicateNode (va-prefix "restaurant recommendation"))
                (ListLink (GlobNode "$X"))
            )
        )
        (restaurants (va-flat-node-list (cog-execute! (GetLink query)) 'RestaurantNode))
        (names (map va-get-restaurant-name restaurants))
    ) (begin
        (if (= 1 (length names)) (begin
            (ListLink (map WordNode (string-split (cog-name (car names)) #\sp)))
        ) (begin
            (ListLink (map WordNode (string-split (va-str-list names " , " " and ") #\sp)))
        ))
    ))
))

(define-public (retrieve_review) (begin
    (let (
        (restaurant-selected (va-get-restaurant-review))
    ) (begin
        (StateLink
            (AnchorNode (va-prefix "restaurant for review"))
            restaurant-selected
        )
    ))
))

(define-public (get_review) (begin
    (let* (
        (query
            (AndLink
                (EvaluationLink
                    (PredicateNode (va-prefix "review"))
                    (ListLink
                        (VariableNode "$X")
                        (VariableNode "$Y")
                    )
                )
                (StateLink
                    (AnchorNode (va-prefix "restaurant for review"))
                    (VariableNode "$X")
                )
            )
        )
        (pm-result (cog-execute! (GetLink query)))
        (reviews (va-flat-node-list pm-result 'PhraseNode))
        (restaurant-node (car (va-flat-node-list pm-result 'RestaurantNode)))
        (restaurant-name (va-get-restaurant-name restaurant-node))
        (selected-reviews (va-random-select-up-to-n 3 reviews))
        (text (cons restaurant-name selected-reviews))
    ) (begin
        ; To record which reviews have been read
        (EvaluationLink
            (PredicateNode (va-prefix "review read"))
            (ListLink restaurant-node)
        )
        ; Return a list of words
        (ListLink (map WordNode (string-split (va-str-list text " , " " . ") #\sp)))
    ))
))

(define-public (make_reservation) (begin
    (va-em-add-reservation)
    (True)
))

(define-public (get_reservation_details) (begin
    (define (get-details pred-name atom-type) (begin
        (let (
            (results
                (va-flat-node-list
                    (cog-execute! (GetLink
                        (EvaluationLink
                            (PredicateNode (va-prefix pred-name))
                            (ListLink (VariableNode "$X"))
                        )
                    ))
                    atom-type
                )
            )
        ) (begin
            (if (null? results) "" (cog-name (car results)))
        ))
    ))

    (let* (
        (time (get-details "user-mentioned-time" 'WordNode))
        (date (get-details "user-mentioned-date" 'WordNode))
        (restaurant-raw (get-details "user-mentioned-restaurant" 'RestaurantNode))
        (restaurant
            (if (string-null? restaurant-raw)
                ""
                (cog-name (va-get-restaurant-name (RestaurantNode restaurant-raw)))
            )
        )
        (pax (car (string-split (get-details "user-mentioned-pax" 'NumberNode) #\.)))
        (detail-txt (string-append
            (if (string-null? time) "" (string-append "for " time))
            (if (string-null? date) "" (string-append " on " date))
            (if (string-null? restaurant) "" (string-append " at the " restaurant))
            (if (string-null? pax) "" (string-append " for " pax))
        ))
    ) (begin
        (if (string-null? detail-txt)
            (ListLink)
            (ListLink (map WordNode (string-split detail-txt #\sp)))
        )
    ))
))

(define-public (get_reservation_time) (begin
    (let* (
        (time (va-get-time))
    ) (begin
        (cond
            ((null? time) (cog-logger-warn "No mention of TIME yet!") (False))
            ((> (length time) 2) (cog-logger-warn "More than one TIME mention found!") (False))
            (else (WordNode (car (string-split (cog-name (car time)) #\.))))
        )
    ))
))

(define-public (get_name arg) (begin
    (ListLink (map WordNode
        (string-split
            (va-str-list
                (cog-outgoing-set (cond
                    ((string-ci=? (cog-name arg) "user")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$VA")
                                     (TypeNode "VirtualAssistantNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$P")
                                     (TypeNode "PersonNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$N")
                                     (TypeNode "WordNode")
                                 )
                             )
                             (AndLink
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "this-va"))
                                     (ListLink
                                         (VariableNode "$VA")
                                     )
                                 )
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "is-va-user"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$VA")
                                     )
                                 )
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "name"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$N")
                                     )
                                 )
                             )
                             (VariableNode "$N")
                         )
                    ))
                    ((string-ci=? (cog-name arg) "known-cuisine-preference")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$P")
                                     (TypeNode "PersonNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$C")
                                     (TypeNode "WordNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$N")
                                     (TypeNode "WordNode")
                                 )
                             )
                             (AndLink
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "user-mentioned-known-person"))
                                     (ListLink
                                         (VariableNode "$P")
                                     )
                                 )
                                 (ChoiceLink
                                     (EvaluationLink
                                         (PredicateNode (va-prefix "likes"))
                                         (ListLink
                                             (VariableNode "$P")
                                             (VariableNode "$C")
                                         )
                                     )
                                     (EvaluationLink
                                         (PredicateNode (va-prefix "dislikes"))
                                         (ListLink
                                             (VariableNode "$P")
                                             (VariableNode "$C")
                                         )
                                     )
                                 )
                                 (InheritanceLink
                                     (VariableNode "$C")
                                     (WordNode "cuisine")
                                 )
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "name"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$N")
                                     )
                                 )
                             )
                             (VariableNode "$N")
                         )
                    ))
                    ((string-ci=? (cog-name arg) "unknown-cuisine-preference")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$P")
                                     (TypeNode "PersonNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$N")
                                     (TypeNode "WordNode")
                                 )
                             )
                             (AndLink
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "unknown-cuisine-preference"))
                                     (ListLink
                                         (VariableNode "$P")
                                     )
                                 )
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "name"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$N")
                                     )
                                 )
                             )
                             (VariableNode "$N")
                         )
                    ))
                    ((string-ci=? (cog-name arg) "retrieved-cuisine-preference")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$P")
                                     (TypeNode "PersonNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$C")
                                     (TypeNode "WordNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$N")
                                     (TypeNode "WordNode")
                                 )
                             )
                             (AndLink
                                 (ExecutionLink
                                     (SchemaNode "scm: retrieve_unknown_cuisine_preference")
                                     (ChoiceLink
                                         (EvaluationLink
                                             (PredicateNode (va-prefix "likes"))
                                             (ListLink
                                                 (VariableNode "$P")
                                                 (VariableNode "$C")
                                             )
                                         )
                                         (EvaluationLink
                                             (PredicateNode (va-prefix "dislikes"))
                                             (ListLink
                                                 (VariableNode "$P")
                                                 (VariableNode "$C")
                                             )
                                         )
                                     )
                                 )
                                 (InheritanceLink
                                     (VariableNode "$C")
                                     (WordNode "cuisine")
                                 )
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "name"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$N")
                                     )
                                 )
                             )
                             (VariableNode "$N")
                         )
                    ))
                    ((string-ci=? (cog-name arg) "conflicted-cuisine-preference")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$C")
                                     (TypeNode "WordNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$P")
                                     (TypeNode "PersonNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$N")
                                     (TypeNode "WordNode")
                                 )
                             )
                             (AndLink
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "conflicted-cuisine-preference"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$C")
                                     )
                                 )
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "name"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$N")
                                     )
                                 )
                             )
                             (VariableNode "$N")
                         )
                    ))
                    (else (False))
                ))
                " , "
                " and "
            )
            #\sp
        )
    ))
))

(define-public (get_preference arg) (begin
    (ListLink (map WordNode
        (string-split
            (va-str-list
                (cog-outgoing-set (cond
                    ((string-ci=? (cog-name arg) "retrieved-cuisine")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$P")
                                     (TypeNode "PersonNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$C")
                                     (TypeNode "WordNode")
                                 )
                             )
                             (AndLink
                                 (ExecutionLink
                                     (SchemaNode "scm: retrieve_unknown_cuisine_preference")
                                     (EvaluationLink
                                         (PredicateNode (va-prefix "likes"))
                                         (ListLink
                                             (VariableNode "$P")
                                             (VariableNode "$C")
                                         )
                                     )
                                 )
                                 (InheritanceLink
                                     (VariableNode "$C")
                                     (WordNode "cuisine")
                                 )
                             )
                             (VariableNode "$C")
                         )
                    ))
                    ((string-ci=? (cog-name arg) "conflicted-cuisine")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$C")
                                     (TypeNode "WordNode")
                                 )
                                 (TypedVariableLink
                                     (VariableNode "$P")
                                     (TypeNode "PersonNode")
                                 )
                             )
                             (AndLink
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "conflicted-cuisine-preference"))
                                     (ListLink
                                         (VariableNode "$P")
                                         (VariableNode "$C")
                                     )
                                 )
                             )
                             (VariableNode "$C")
                         )
                    ))
                    ((string-ci=? (cog-name arg) "inferred-cuisine")
                     (cog-execute!
                         (BindLink
                             (VariableList
                                 (TypedVariableLink
                                     (VariableNode "$C")
                                     (TypeNode "WordNode")
                                 )
                             )
                             (AndLink
                                 (EvaluationLink
                                     (PredicateNode (va-prefix "inferred-cuisine"))
                                     (ListLink
                                         (VariableNode "$C")
                                     )
                                 )
                             )
                             (VariableNode "$C")
                         )
                    ))
                ))
                " , "
                " and "
            )
            #\sp
        )
    ))
))

(define-public (retrieve_unknown_cuisine_preference) (begin
    ; These records will be used by other predicates/schemas
    (define (record-result result)
        (ExecutionLink
            (SchemaNode "scm: retrieve_unknown_cuisine_preference")
            result
        )
    )

    ; TODO: This should actually send queries to different VA to get the info, but I'll just add these
    ; to the AtomSpace for the use-case-1 until that function is ready
    (record-result (EvaluationLink (PredicateNode "VA: likes") (ListLink (PersonNode "VA: person-3") (WordNode "Chinese"))))
    (record-result (EvaluationLink (PredicateNode "VA: likes") (ListLink (PersonNode "VA: person-3") (WordNode "Thai"))))
    (record-result (EvaluationLink (PredicateNode "VA: dislikes") (ListLink (PersonNode "VA: person-4") (WordNode "Italian"))))

    ; Also delete those 'unknown-cuisine-preference' EvaluationLinks after successfully getting those info
    (for-each
        cog-extract-recursive
        (cog-outgoing-set
            (cog-execute!
                (GetLink
                    (TypedVariableLink
                        (VariableNode "$x")
                        (SignatureLink
                            (EvaluationLink
                                (PredicateNode (va-prefix "unknown-cuisine-preference"))
                                (ListLink
                                    (TypeNode "PersonNode")
                                )
                            )
                        )
                    )
                    (VariableNode "$x")
                )
            )
        )
    )

    ; Return an Atom
    (True)
))

(define-public (analyze_known_cuisine_preference) (begin
    (let (
        (known-person-query
            (GetLink
                (TypedVariableLink
                    (VariableNode "$P")
                    (TypeNode "PersonNode")
                )
                (EvaluationLink
                    (PredicateNode (va-prefix "user-mentioned-known-person"))
                    (ListLink
                        (VariableNode "$P")
                    )
                )
            )
        )
        (known-cuisine-preference-query
            (BindLink
                (VariableList
                    (TypedVariableLink
                        (VariableNode "$P")
                        (TypeNode "PersonNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$C")
                        (TypeNode "WordNode")
                    )
                )
                (AndLink
                    (EvaluationLink
                        (PredicateNode (va-prefix "user-mentioned-known-person"))
                        (ListLink
                            (VariableNode "$P")
                        )
                    )
                    (ChoiceLink
                        (EvaluationLink
                            (PredicateNode (va-prefix "likes"))
                            (ListLink
                                (VariableNode "$P")
                                (VariableNode "$C")
                            )
                        )
                        (EvaluationLink
                            (PredicateNode (va-prefix "dislikes"))
                            (ListLink
                                (VariableNode "$P")
                                (VariableNode "$C")
                            )
                        )
                    )
                    (InheritanceLink
                        (VariableNode "$C")
                        (WordNode "cuisine")
                    )
                )
                (VariableNode "$P")
            )
        )
    ) (begin
        (let* (
            (known-people (cog-outgoing-set (cog-execute! known-person-query)))
            (known-cuisine-preferences (cog-outgoing-set (cog-execute! known-cuisine-preference-query)))
            (preference-not-known (lset-difference equal? known-people known-cuisine-preferences))
        ) (begin
            (for-each (lambda (x) (begin
                (EvaluationLink
                    (PredicateNode (va-prefix "unknown-cuisine-preference"))
                    (ListLink x)
                )
            )) preference-not-known)
        ))
    ))

    ; Return an Atom
    (True)
))

(define-public (analyze_retrieved_cuisine_preference) (begin
    (let (
        (query-conflict
            (BindLink
                (VariableList
                    (TypedVariableLink
                        (VariableNode "$P1")
                        (TypeNode "PersonNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$P2")
                        (TypeNode "PersonNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$C")
                        (TypeNode "WordNode")
                    )
                )
                (AndLink
                    (ExecutionLink
                        (SchemaNode "scm: retrieve_unknown_cuisine_preference")
                        (EvaluationLink
                            (PredicateNode (va-prefix "likes"))
                            (ListLink
                                (VariableNode "$P1")
                                (VariableNode "$C")
                            )
                        )
                    )
                    (EvaluationLink
                        (PredicateNode (va-prefix "user-mentioned-known-person"))
                        (ListLink
                            (VariableNode "$P2")
                        )
                    )
                    (EvaluationLink
                        (PredicateNode (va-prefix "dislikes"))
                        (ListLink
                            (VariableNode "$P2")
                            (VariableNode "$C")
                        )
                    )
                    (InheritanceLink
                        (VariableNode "$C")
                        (WordNode "cuisine")
                    )
                )
                (EvaluationLink
                    (PredicateNode (va-prefix "conflicted-cuisine-preference"))
                    (ListLink
                        (VariableNode "$P2")
                        (VariableNode "$C")
                    )
                )
            )
        )
        (query-like
            (BindLink
                (VariableList
                    (TypedVariableLink
                        (VariableNode "$P1")
                        (TypeNode "PersonNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$P2")
                        (TypeNode "PersonNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$C")
                        (TypeNode "WordNode")
                    )
                )
                (AndLink
                    (ExecutionLink
                        (SchemaNode "scm: retrieve_unknown_cuisine_preference")
                        (EvaluationLink
                            (PredicateNode (va-prefix "likes"))
                            (ListLink
                                (VariableNode "$P1")
                                (VariableNode "$C")
                            )
                        )
                    )
                    (EvaluationLink
                        (PredicateNode (va-prefix "user-mentioned-known-person"))
                        (ListLink
                            (VariableNode "$P2")
                        )
                    )
                    (EvaluationLink
                        (PredicateNode (va-prefix "likes"))
                        (ListLink
                            (VariableNode "$P2")
                            (VariableNode "$C")
                        )
                    )
                    (InheritanceLink
                        (VariableNode "$C")
                        (WordNode "cuisine")
                    )
                )
                (VariableNode "$C")
            )
        )
        (query-dislike
            (BindLink
                (VariableList
                    (TypedVariableLink
                        (VariableNode "$P1")
                        (TypeNode "PersonNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$P2")
                        (TypeNode "PersonNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$C1")
                        (TypeNode "WordNode")
                    )
                    (TypedVariableLink
                        (VariableNode "$C2")
                        (TypeNode "WordNode")
                    )
                )
                (AndLink
                    (ExecutionLink
                        (SchemaNode "scm: retrieve_unknown_cuisine_preference")
                        (EvaluationLink
                            (PredicateNode (va-prefix "dislikes"))
                            (ListLink
                                (VariableNode "$P1")
                                (VariableNode "$C1")
                            )
                        )
                    )
                    (EvaluationLink
                        (PredicateNode (va-prefix "user-mentioned-known-person"))
                        (ListLink
                            (VariableNode "$P2")
                        )
                    )
                    (EvaluationLink
                        (PredicateNode (va-prefix "dislikes"))
                        (ListLink
                            (VariableNode "$P2")
                            (VariableNode "$C2")
                        )
                    )
                    (InheritanceLink
                        (VariableNode "$C1")
                        (WordNode "cuisine")
                    )
                    (InheritanceLink
                        (VariableNode "$C2")
                        (WordNode "cuisine")
                    )
                )
                (SetLink
                    (VariableNode "$C1")
                    (VariableNode "$C2")
                )
            )
        )
    ) (begin
        (let* (
            (cuisine-conflicted (cog-execute! query-conflict))
            (cuisine-liked (delete-duplicates (va-flat-node-list (cog-execute! query-like) 'WordNode)))
            (cuisine-disliked (delete-duplicates (va-flat-node-list (cog-execute! query-dislike) 'WordNode)))
            (cuisine-inferred (filter (lambda (x) (not (list? (member x cuisine-disliked)))) cuisine-liked))
        ) (begin
            (for-each (lambda (c) (begin
                (EvaluationLink
                    (PredicateNode (va-prefix "inferred-cuisine"))
                    (ListLink c)
                )
            )) cuisine-inferred)
        ))
    ))

    ; Return an Atom
    (True)
))


;--------------------------------------Recommendation 2----------------------------
;---Find nearby events
(define (get-events-nearby)
        (let* ((query
                (GetLink
                  (TypedVariableLink
                    (VariableNode "$E")
                    (TypeNode (symbol->string 'PhraseNode)))
                  (EvaluationLink
                    (PredicateNode (va-prefix "near-location-event"))
                    (ListLink
                      (va-get-generic-fact-1 "user-mentioned-restaurant" 'RestaurantNode)
                      (VariableNode "$E")))))
               (query-result (va-flat-node-list (cog-execute! query) 'PhraseNode)))
              (ListLink (map (lambda (str) (if (string=? str "") (list) (WordNode str)))
                             (string-split (va-str-list query-result " , " " . ") #\sp))))
)

;----Get weather Information near city
(define (get-temperature city)
  (let ((query
          (AtTimeLink
            (EvaluationLink
              (PredicateNode (va-prefix "temperature-at-city"))
              city
              (AssociativeLink
                (ConceptNode "degree-celsius")
                (VariableNode "$T")))
          (VariableNode "$S"))))
       (let* ((result (cog-execute! (GetLink query)))
              (temperature '())
              (timestamp '()))
             (begin
              (for-each (lambda (atom)
                                (set! temperature
                                      (append!
                                       (list (car (cog-outgoing-set atom)))
                                       temperature)))
                        (cog-outgoing-set result))
              (for-each (lambda (atom)
                                (set! timestamp
                                      (append!
                                       (list (cadr (cog-outgoing-set atom)))
                                       timestamp)))
                        (cog-outgoing-set result))
              (let ((temp-date-pairs (map (lambda (temp tstmp) (list temp tstmp))
                                        temperature timestamp)))
                   ;Expected format "Fri Jul 12 14:03:24 +0800 2019"
                   (begin
                    (sort! temp-date-pairs
                           (lambda (tn1 tn2)
                                   ;template might change based on the timestamp format we follow
                                   (let* ((template "~a ~b ~d ~H:~M:~S ~z ~Y")
                                          (d1 (string->date (cog-name (cadr tn1)) template))
                                          (d2 (string->date (cog-name (cadr tn2)) template)))
                                         (time>? (date->time-utc d1) (date->time-utc d2)))))
                    ; Get the latest temperature recording
                    (car (car temp-date-pairs))
))))))

(define (get-humidity city)
  (let ((query
         (AtTimeLink
          (EvaluationLink
            (PredicateNode (va-prefix "humidity-at-city"))
            city
            (VariableNode "$H"))
          (VariableNode "$T")))
        )
       (let* ((result (cog-execute! (GetLink query)))
              (humidity '())
              (timestamp '()))
             (begin
              (for-each (lambda (atom)
                                (set! humidity
                                      (append!
                                       (list (car (cog-outgoing-set atom)))
                                       humidity)))
                        (cog-outgoing-set result))
              (for-each (lambda (atom)
                                (set! timestamp
                                      (append!
                                       (list (cadr (cog-outgoing-set atom)))
                                       timestamp)))
                        (cog-outgoing-set result))
              (let ((humidity-date-pairs (map (lambda (humidity tstmp) (list humidity tstmp))
                                        humidity timestamp)))
                   ;Expected format "Fri Jul 12 14:03:24 +0800 2019"
                   (begin
                    (sort! humidity-date-pairs
                           (lambda (tn1 tn2)
                                   ;template might change based on the timestamp format we follow
                                   (let* ((template "~a ~b ~d ~H:~M:~S ~z ~Y")
                                          (d1 (string->date (cog-name (cadr tn1)) template))
                                          (d2 (string->date (cog-name (cadr tn2)) template)))
                                         (time>? (date->time-utc d1) (date->time-utc d2)))))
                    ; Get the latest humidity recording
                    (car (car humidity-date-pairs))
))))))


(define (get-rain-info city)
  (let ((query
         (AtTimeLink
          (EvaluationLink
            (PredicateNode (va-prefix "rain-at-city"))
            city
            (VariableNode "$H"))
          (VariableNode "$T")))
        )
       (let  ((result (cog-execute! (GetLink query)))
              (raininess '())
              (timestamp '()))
             (begin
              (for-each (lambda (atom)
                                (set! raininess
                                      (append!
                                       (list (car (cog-outgoing-set atom)))
                                       raininess)))
                        (cog-outgoing-set result))
              (for-each (lambda (atom)
                                (set! timestamp
                                      (append!
                                       (list (cadr (cog-outgoing-set atom)))
                                       timestamp)))
                        (cog-outgoing-set result))
              (let ((rain-date-pairs (map (lambda (rain tstmp) (list rain tstmp))
                                        raininess timestamp)))
                   ;Expected format "Fri Jul 12 14:03:24 +0800 2019"
                   (begin
                    (sort! rain-date-pairs
                           (lambda (tn1 tn2)
                                   ;XXX template might change based on the timestamp
                                   ;format convention we follow
                                   (let ((template "~a ~b ~d ~H:~M:~S ~z ~Y")
                                          (d1 (string->date (cog-name (cadr tn1)) template))
                                          (d2 (string->date (cog-name (cadr tn2)) template)))
                                     (time>? (date->time-utc d1) (date->time-utc d2)))))
                    ; Get the latest humidity recording
                    (car (car rain-date-pairs))
))))))

(define (is_rainy? city)
  (let ((result (get-rain-info city)))
          (if (null? result) #f
              (eq? result RAINY))))

(define (is_humid? city)
  (let ((THRESHOLD 50))
        (resutl (get-humidity city))
    (>= (string->number (cog-name result)) THRESHOLD)))

;---Find route

(define-public (get_route)
                 (let* (#|(get-location
                          (lambda (item)
                            (let* ((query
                                     (GetLink
                                       (TypedVariableLink
                                         (VariableNode "$x")
                                         (SignatureLink
                                           (EvaluationLink
                                             (PredicateNode (va-prefix "geolocation"))
                                             (ListLink
                                               item
                                               (TypeNode "NumberNode")
                                               (TypeNode "NumberNode")))))
                                       (VariableNode "$x")))
                                  (result (cog-outgoing-set (cog-execute! query))))
                                  (if (null? result) result (car result)))))
                        |# (get-location (lambda (item)
                          (let*
                            ((links
                               (letrec
                                 ((list-of-lists (map cog-incoming-set (cog-incoming-set item)))
                                  (flatten-list-of-lists (lambda (ll) (if (null? ll) (list)
                                     (append (car ll) (flatten-list-of-lists (cdr ll)))))))
                                 (flatten-list-of-lists list-of-lists)))
                             (eval-links (filter (lambda (link)
                                           (eq? 'EvaluationLink (cog-type link))) links))
                             (locations
                                (filter (lambda (eval-link)
                                                  (string=? (va-prefix "geolocation")
                                                  (cog-name (car (cog-outgoing-set eval-link)))))
                                         eval-links)))
                            (if (null? locations) (list) (car locations)))))
                        (restaurant (va-get-generic-fact-1 "user-mentioned-restaurant" 'RestaurantNode))
                        (query-result
                          (cog-execute!
                           (GetLink
                             (EvaluationLink
                               (PredicateNode (va-prefix "route_information"))
                               (ListLink
                                 (get-location (get-va-user))
                                 (if (null? restaurant) (ListLink) (get-location restaurant))
                                 (VariableNode "$R")))))))
                      (if (null? (cog-outgoing-set query-result))
                          (ListLink)
                          (let* ((result (car (cog-outgoing-set query-result)))
                                 (Lst (ListLink (cddr (cog-outgoing-set result))))
                                 (route (va-flat-node-list Lst 'PhraseNode))
                                 (ret   (ListLink
                                            (map WordNode
                                                 (string-split (va-str-list route " , " " . ") #\sp)))))
                                (begin
                                  ;(cog-logger-info "ROUTE_LIST: ")
                                  ;(cog-logger-info (format #f "~a" ret))
                                  ret)
                            )))
)

;--Vibe-----
;TODO replace (WordNode "romantic") with with a dynamic assignment where vibe is extracted from user uttered sentence.
(define-public (set_vibe_mentioned)(begin
  (cog-logger-info "Setting user-mentioned-restaurant-vibe to va-true")
  (va-add-generic-fact-1 "user-mentioned-restaurant-vibe" (WordNode "romantic")))
)

;------------------------END----------------------------------------------------------
(define-public (get_em_reserved) (begin
   (define company 
                 (cog-execute! 
                       (Get
                          (Evaluation
                             (Predicate (va-prefix "name"))
                              (List
                                (cog-value-ref (ValueOf (get-details "user-mentioned-known-person" 'PersonNode))0)
                                (Variable "$X"))))
                   )          
  )  
  (let 
      ((temp 
           (cog-execute! 
               (Get
                   (Evaluation
                        (Predicate "em-reservation")
                        (List
                            (Variable "$X")
                            (Variable "$T")
                            (Variable "$D")
                            (cog-value-ref company 0)))))
       )) 
    (Word (string-join(cog-value->list(cog-value-ref (cog-value-ref temp 0)0))))
  )
))

(define (va-em-add-reservation)
    ; make the following structure
    ; Evaluation
    ;   Predicate "em-reservation"
    ;   List
    ;     Restaurant "x-restaurant"
    ;     Word "date"
    ;     Word "time"
    ;     Person "company-1"
    ;     Person "company-2"...
    (let*
        ((time (car (get-details "user-mentioned-time" 'WordNode)))
         (date (car (get-details "user-mentioned-date" 'WordNode)))
         (restaurant
            (va-flat-node-list
                (cog-execute! (GetLink
                    (EvaluationLink
                        (PredicateNode (va-prefix "name"))
                        (ListLink
                            (car (get-details "user-mentioned-restaurant"
                            'RestaurantNode))
                            (VariableNode "$R")
                        )
                    )
                ))
                'PhraseNode
            )
          )
         (company
            (map (lambda (person)
                (va-flat-node-list
                    (cog-execute! (GetLink
                        (EvaluationLink
                            (PredicateNode (va-prefix "name"))
                            (ListLink
                                person
                                (VariableNode "$X")
                            )
                        )
                    ))
                    'WordNode
                ))
                (get-details "user-mentioned-known-person" 'PersonNode)
            )
         )
         (restaurant-latitude 
            (cog-value-ref 
              (cog-value-ref  
                (cog-execute! 
                     (Get 
                        (Evaluation (Predicate "VA: geolocation")
                           (List
                             (get-details "user-mentioned-restaurant" 'RestaurantNode) 
                             (Variable "$lat") (Variable "$long"))))) 0)0))
         (restaurant-longitude 
            (cog-value-ref 
              (cog-value-ref  
                (cog-execute! 
                     (Get 
                        (Evaluation (Predicate "VA: geolocation")
                           (List
                             (get-details "user-mentioned-restaurant" 'RestaurantNode) 
                             (Variable "$lat") (Variable "$long"))))) 0)1))
         (user-latitude 
             (cog-value-ref 
               (cog-value-ref  
                 (cog-execute! 
                      (Get 
                        (Evaluation (Predicate "VA: geolocation")
                           (List
                             (PersonNode "VA: person-1")
                             (Variable "$lat") (Variable "$long"))))) 0)0))
         (user-longitude 
            (cog-value-ref 
              (cog-value-ref  
                (cog-execute! 
                     (Get 
                        (Evaluation (Predicate "VA: geolocation")
                           (List
                              (PersonNode "VA: person-1")
                             (Variable "$lat") (Variable "$long"))))) 0)1))
         (em-atm
            (Evaluation
                (Predicate "em-reservation")
                    (List
                        restaurant
                        date
                        time
                        company))))
             (get-timestamp date time)
             (display "atoms")(newline)(newline)
             (display "user latitude:")(display user-latitude)
            (newline) (display "user longitude:")(display user-longitude)(newline)
            (display "restaurant latitude:")(display restaurant-latitude)
            (newline) (display "restaurant longitude:")(display restaurant-longitude)(newline)
            
            (newline)(display "number values")(newline)(newline)
            (display "user latitude:")(display (cog-value-ref user-latitude 0))(newline)
            (newline) (display "user longitude:")(display (cog-value-ref user-longitude 0))(newline)
            (newline)(display "restaurant latitude:")(display (cog-value-ref restaurant-latitude 0))(newline)
            (newline) (display "restaurant longitude:")(display (cog-value-ref restaurant-longitude 0))(newline)(newline)
        ;reservation made
        (if(equal? (Set ) user-latitude)
                va-false; (va-spt-add-atom em-atm 'reservation_made' #:timestamp current timestamp  user-latitude user-longitude)
                va-true; (va-spt-add-atom em-atm 'reservation_made' #:timestamp current timestamp)           
         )
         ;reservation info
        (if(equal? (Set ) restaurant-latitude)
           va-false;(va-spt-add-atom em-atm 'reservation_info' #:timestamp  timestamp(date_time)  restaurant-latitude restaurant-longitude )
           va-true     ; (va-spt-add-atom em-atm 'reservatio_info' #:timestamp current timestamp)           
         )

      


     em-atm
    )
)
(use-modules (srfi srfi-19))


(define-public (get-timestamp date time)(begin


           (define d(string-capitalize (cog-value-ref date 0)))      
           (define h
                      (if (string-contains(string-capitalize (cog-value-ref time 0)) "Pm")
                            (if(string->number(substring (cog-value-ref time 0) 0 2))
                                    (+(string->number(substring (cog-value-ref time 0) 0 2)) 12)
                                    (+(string->number(substring (cog-value-ref time 0) 0 1)) 12)
                            )
                            (if(string->number(substring (cog-value-ref time 0) 0 2))
                                   (+(+(string->number(substring (cog-value-ref time 0) 0 2)) 12)12)
                                    (+(+(string->number(substring (cog-value-ref time 0) 0 1)) 12)12)
                             )
             
   ))
  (define num_date
      (cond
            [(equal? "Sunday" d) 0]
            [(equal? "Monday" d) 1] 
            [(equal? "Tuesday" d) 2] 
            [(equal? "Wednesday" d) 3]
            [(equal? "Thursday" d) 4]
            [(equal? "Friday" d) 5] 
            [(equal? "Saturday" d) 6]
            [(equal? "Today" d) (date-week-day(current-date))]
            [else 0]))

   (define date_diff
        (cond
             [(string-contains(string-capitalize (cog-value-ref time 0)) "Pm") 
             (if (> (- num_date (date-week-day (current-date))) 0)
                (- num_date (date-week-day (current-date)))
                (- (+ 7 num_date)(date-week-day (current-date))))
           ]
            [(string-contains(string-capitalize (cog-value-ref time 0)) "Am") 
             (if (> (-  num_date (date-week-day (current-date))) 0)
                (- (- num_date 1) (date-week-day (current-date)))
                (- (+ 6 num_date)(date-week-day (current-date))))
           ]
))

  (define timestamp (+(+(+(* 24 date_diff 3600) (*(-  h (date-hour (current-date)))3600))(car(gettimeofday)))10800)) 
  (newline)(display "timestamp: ")
      (display timestamp)
   (newline)                                                                  
))

(define-public (reserve_from_em) (begin
   (define restaurant 
                 (cog-execute! 
                       (Get
                          (Evaluation
                             (Predicate (va-prefix "name"))
                              (List
                                (Variable "$R")
                               (PhraseNode (cog-value-ref (get_em_reserved) 0)))))
                   )          
   )  
   (define (set-rest pred-name atom-type)
                     (Evaluation
                            (Predicate (va-prefix pred-name))
                            (List (cog-value-ref restaurant 0))
                        )
    )
    (set-rest "user-mentioned-restaurant" 'RestaurantNode)
    va-true
))
