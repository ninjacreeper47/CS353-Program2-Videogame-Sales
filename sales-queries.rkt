#lang racket

(require csv-reading)  ;https://docs.racket-lang.org/csv-reading/index.html


(define assignment-input-file
  "Video Games Sales.csv")

;Main function of program. RUN THIS
;Loads data and starts user query loop
(define (run [filename assignment-input-file])
  (let ([data (load-data filename)])
    (let ([header (first data)])
      (user-loop (rest data) header) 
      )))

(define (user-loop data header [prev-search (list "Welcome to the video game sales database")])
  (print-search prev-search)
  (println "Continue [Yes/No]: ")
  (if (string-ci=? "No" (read-line))
  (println "Goodbye!") ;Program terminates
  (user-loop data header (new-search data header))))

(define (print-search search)
  (if (empty? search)
      (println "...")
      (begin
        (println (first search))
        (print-search (rest search)))))
  
  
(define (new-search data header)
  (sort-by (prompt-sort-style) 
  (triple-query-fold header (prompt-query) (prompt-query) (prompt-query) data)
  header))
  
 
;Precondition:   File must be a csv file, which matches the  default csv-reader specs

;Postcondition: returns a list of lists,  where each line in the file is a list containing an element for each data attribute.
;Numeric values are stored as numeric types everything else is strings
(define (load-data [filename assignment-input-file])
  (map list<string>->list<number/string>
 (csv->list (open-input-file filename))))

;hard-coded values for testing
(define my-data-header
  (list "index" "Rank" "Game Title" "Platform" "Year" "Genre" "Publisher" "North America" "Europe" "Japan" "Rest of the World" "Global" "Review"))
(define wii-sports
  (list 0 1 "Wii Sports" "Wii" 2006 "Sports" "Nintendo" 40.43 28.39 3.77 8.54 81.12 76.28))

;Precondition:  takes in a list where each element corresponds to an entry in the header list
;(ex: if "Game Title" is the 3rd spot of header then the input list should have it's title at the 3rd spot)
;Postcondition: returns the element of the list that matches the given attribute
(define (find-attribute data-line header attribute)
  (list-ref data-line (index-of header attribute)))

(define (list<string>->list<number/string> list)
  (map string->number-or-string list))

;Converts a string to a number if the string is numeric, otherwise returns the string back. Returns null if given null
(define (string->number-or-string string)
  (if (null? string)
      null
      (if (string->number string)
          (string->number string)
          string)))

;Postcondition Prompts the user for a type of query, and then calls the child function which ensures the type is valid and prompts for specific criteria
;Returns a query (a list containing a query type string and an additionial user input string)
(define (prompt-query)
 (print "Enter Querry Type [Game Title,Year,Publisher,Region,Genre,Skip]: ")
 (prompt-for-specifics-of-query(string-titlecase  (read-line))))

;Sub function for prompt-query
;Postcondition:  Prints a message relevant to the query type.  Then prompts the user for input,  and returns a query
;If query type is invalid, this function will call the parent prompt-query function again. This means that the recursion will not terminate until a valid query type is entered
(define (prompt-for-specifics-of-query query-type)
  (println (query-type->message query-type))
  (if (equal? (query-type->message query-type) "INVALID QUERY TYPE!")
      (prompt-query)
      (list query-type (read-line))))

(define (query-type->message query-type)
   (define message-hash
     (hash
      "Game Title" "Enter Title: "
      "Year" "Enter date-range [{year1}-{year2}]: "
      "Publisher" "Enter publisher: "
      "Region" "Enter Region [North-America,Europe,Japan,Rest of World,Global]: "
      "Genre" "Enter Genre: "
      "Skip" "Skipping search parameter. Input anything here,  it will be ignored"))
   (hash-ref message-hash query-type "INVALID QUERY TYPE!"))


(define (get-match-function query-type)
  (define match-function-hash
    (hash
     "Game Title" name-match?
     "Year" date-match?
     "Publisher" publisher-match?
     "Region" region-match?
     "Genre" genre-match?))
    (hash-ref match-function-hash query-type))

;checks equality of name 1 and name 2, ignores case
(define (name-match? name1 name2)
  (string-ci=? name1 name2))


;Precondition:  date-range must be a string of the format "[year1]-[year2]"
;date must be a number
;Postcondition: returns true if date is containted in the range, false otherwise

(define (date-match? date date-range-input)
   (let ([date-range (sort (map string->number (string-split date-range-input "-")) <)])
     (if (and (> date (first date-range)) (< date (second date-range)))
         #t
         #f
)))

;TODO: figure out what region matching even is lmao
(define (region-match?  region-val threshold)
  #t)

;checks equality of genre 1 and genre 2, ignores case
(define (genre-match? genre1 genre2)
  (string-ci=? genre1 genre2))

;Always returns true regardless of parameters
;Has 2 parameters so it can be used in filter
(define (auto-match)
  #t)

;Precondition: Seperate words in publisher name are seperated by spaces
;Postcondition: returns a match if words matches any word in publisher. Otherwise returns false
(define (publisher-match? publisher words)
(member words (string-split publisher " ") string-ci=?))

;Precondition: takes in a header list and data-line a list where each element corresponds to an entry in the header list.
; query type must be a valid query type (one of the specific strings checked for in get-match-function).
;Target must be a string that matches the expected formating of the match function that corresponds to querry type
;Note: This function is curried elseswhere in the code, so do not change the order of these parameters
;Postcondition:  Returns a boolean corresponding to whether a match for the query was found in the data-lien
(define (match?  header query-type target data-line)
  (if (equal? query-type "Skip")
      (auto-match)
  ((get-match-function query-type) (find-attribute data-line header query-type) target)))

;Precondition:  query type must be a valid query type (one of the specific strings checked for in get-match-function).
; Query body must be a string that matches the expected formating of the match function that corresponds to querry type
;The data must be a list of lists where each list containtains an element   for  each corresponding  element in the header list

;Postcondition: Returns a list of all lists which match the query
(define (make-query query-type query-body data header)
  (filter
   (curry match? header query-type query-body) data))

;Precondition: Each query paramater must be a list containing a type and body.  These types an bodies must follow the preconditions of make-query
;Postcondition:  Calls make-querry three times, each time using the result of the previous call as the data parameter for the next call
(define (triple-query-fold header query1 query2 query3 data)
  (make-query (first query3) (second query3)
               (make-query (first query2) (second query2)
                            (make-query (first query1) (second query1) data header) header)
               header)
)

;Precondition:   attribute must exist in header and each list in data should have a valid entry in the corresponding spot. {same position where attribute exists in header}
;Attribute must be numerical
;Postcondition: returns data, ordered by attribute (ascending)
(define (sort-by attribute data header)
  (sort data (make-search-comparison-function attribute header)))

;Precondition:  attribute must exist in header
;Postcondition: Returns a < function which compares two datalines by attribute.
;(The datalines must have their entries correspond to the entries in header)
(define (make-search-comparison-function attribute header)
  (lambda (line1 line2)
      (< (find-attribute line1 header attribute) (find-attribute line2 header attribute))))

;Precondition: user must enter a valid choice.  [TODO: make this function repeat until a valid sorting style is chosen]
;Postcondition:  prints a message to the user, prompting them to enter a sorting style

(define (prompt-sort-style)
  (println "Enter sorting style [Rank/Review/Year/North America/Europe/Japan/Rest of World/Global]")
  (read-line))