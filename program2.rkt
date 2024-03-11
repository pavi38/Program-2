#lang racket


;if eof pressed on "enter the name or number" than program should ask user to "enter the catagry" again
;user can enter wrong name and would just return not found


(require csv-reading)

(define (numeric? str)
  (if(equal? (string->number str) #f) #f #t))

;return lambda funcation used in fliter funcation to get game-name
(define (filter-by-name game-name)
  (lambda (data-entery) (if (equal? (third data-entery) game-name) #t #f)
))

;return lambda funcation used in fliter funcation to get entries between the date range
(define (filter-by-date date)
  (define list-of-years (sort (map string->number (string-split date "-")) <))
  (lambda (data-entery) (if (and ( >= (string->number (fifth data-entery)) (first list-of-years)) ( <= (string->number (fifth data-entery)) (second list-of-years))) #t #f)  
  )
 )

;return funcation used in fliter funcation to get enteries based on publisher
(define (filter-by-publisher publisher-name)
  (lambda (data-entery) (if (string-contains? (seventh data-entery) publisher-name) #t #f)
  ))
;return funcation used in fliter funcation to filter genre 
(define (filter-by-genre genre)
  (lambda (data-entery) (if (equal? (sixth data-entery) genre) #t #f)
  ))

;return list operation funcation used in display-results to display result based on the regions entered
(define (filter-by-region region)
  (cond
    [(equal? region "North America") eighth]
    [(equal? region "Europe") ninth]
    [(equal? region "Japan") tenth]
    [(equal? region "Rest Of World") (lambda (x) (tenth (rest x)))]
    [(equal? region "Global") (lambda (x) (tenth (rest (rest x))))]
    [else "none"]
    )
  )

;based on the user input catagory retun pair of funcation and user input for that category
;i.e return: (filter-by-name . "tetris")

(define (catagry->funcation catagry input)
  (cond
    [(equal? catagry "1") (cons filter-by-name input)]
    [(equal? catagry "2") (cons filter-by-date input)]
    [(equal? catagry "3") (cons filter-by-publisher input)]
    [(equal? catagry "4") (cons filter-by-region input)]
    [(equal? catagry "5") (cons filter-by-genre input)]
    [else "none"]
    )
  )


(define (display-option)
  (displayln "Choose 3 option from following:")
  (displayln "1 Name")
  (displayln "2 Date YYYY-YYYY")
  (displayln "3 Publisher")
  (displayln "4 Region")
  (displayln "5 Genre\n")

  
)

;get input like name of genre, game and publisher and more

(define (add-to-input-list catagry user-input-list)
    (define a (read-line (current-input-port) 'any))
    (cond
       [(equal? a "q")(cons a user-input-list)]
       [(and (equal? catagry "4")(equal? (filter-by-region (string-titlecase a)) "none")) (displayln "notvaild region\n")(add-to-input-list catagry user-input-list)]
       [else (cons (catagry->funcation catagry (string-titlecase a)) user-input-list)]
      )
    
)

;get catagory number
(define (get-catagry user-input-list)
  (displayln "\nenter catagry number:\n")
  (displayln "Enter q to quit the database")
  (displayln "Press eof to skip to next stage.\n")
  (define catagry (read-line (current-input-port) 'any))
  (cond
      [(or (eof-object? catagry) (equal? catagry "q")) (cons catagry user-input-list)]
      [(cond
         [(not (numeric? catagry)) (get-catagry user-input-list)]
         [(> (string->number catagry) 5) (get-catagry user-input-list)]
         [else (displayln "enter the data to filter with:\n")(add-to-input-list catagry user-input-list)]
      )]
   )
)

;get user input for the oder they want the result to b displayed
(define (get-order)
  (displayln "\nHow would you like the data ordered?")
  (displayln "1. sales")
  (displayln "2. rating")
  (define a (read-line (current-input-port) 'any))
  (cond
    [(not (numeric? a)) (get-order)]
    [(> (string->number a) 2)(get-order)]
    [(equal? a "1") second]
    [(equal? a "2") last])
  )


(define (get-filter-input user-input)
  (cond
    [(and (cons? user-input) (equal? "q" (first user-input))) "quit"]
    [(and (cons? user-input) (eof-object? (first user-input)))(rest user-input)] ;(get-order (rest user-input))
    [(>= (length user-input) 3)  user-input]
    [else (get-filter-input (get-catagry user-input))])
  
  )

;apply filter funcation here
;everything except region is filtered here

(define (filter-data data conditions)
  (cond
    [(empty? conditions) data]
    [else (define condition (first conditions))
          (filter-data (filter ((car condition) (cdr condition)) data) (rest conditions))]
    )
  )

;all the results are displayed here based on the regions secleted by user

(define (display-results filter-data regions)
        (cond
        [(empty? filter-data) (displayln "THE END!!")]
        [else
        (define filtered-data (first filter-data))
        (define (display-six data count)
          (cond
            [(and (cons? regions)(equal? count 6))(display " ")]
            [(empty? data) (display " ")]
            [else (display (first data))(display "    ")(display-six (rest data) (+ count 1))]
            )
          )
        
        ;displays everything except region
        ;if no region filter selected every entry displayed by this func
        
        (display-six (rest filtered-data) 0)

        ;displays region based on user selection
        
        (define (print-regions regions)
          (cond
            [(empty? regions)(display (last filtered-data))(display "   ")(display "\n")]
            [else
             (define region (first regions))
             ;(display region)
             (display (((car region) (cdr region)) filtered-data))(display "   ")(print-regions (rest regions))
             ]
            )
          )
       (print-regions regions)(display-results (rest filter-data) regions)])
       
  
       )

;func for sort
(define (greater-than-int x y)
   (> (string->number x) (string->number y))
  )


;main funcation

(define (menu)
  (display-option)
  (define a (get-filter-input '()))
  
  (define data (rest (all-rows "Video Games Sales.csv" make-game-csv-reader)))
  (cond
    [(string? a) (displayln "\nThanks for using our database!!")]
    [else
     (define b (get-order))
     (define region-list (filter (lambda (x)(equal? filter-by-region (car x))) a))
     ;(displayln region-list)
     (define filter-list (filter (lambda (x)( not (equal? filter-by-region (car x)))) a)) ;do if filter list empty
     ;(displayln filter-list)
      (define filtered-data (filter-data data filter-list))
     (cond
       [(empty? filtered-data)(displayln "\n\nNo results found\n\n")]
       [else (displayln "\n")(display-results (sort filtered-data #:key b greater-than-int) region-list)(displayln "\n")]
       )
     (menu)
     ]
    )
  )

;reading file and converting it list of list

(define make-game-csv-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (all-rows file-name make-reader)
  (define next-row (make-reader (open-input-file file-name)))
  (define (loop)
    (define row (next-row))
    (if (empty? row)
        '()
        (cons row (loop))))
  (loop))




;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(menu)