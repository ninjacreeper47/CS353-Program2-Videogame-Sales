#lang racket

(require csv-reading)  ;https://docs.racket-lang.org/csv-reading/index.html


(define (assignment-input-file)
  "Video Games Sales.csv")

;Precondition:   File must be a csv file, which matches the  default csv-reader specs
;Postcondition: returns a list of lists,  where each line in the file is a list containing an element for each data attribute.
;Numeric values are stored as numeric types everything else is strings
(define (load-data [filename assignment-input-file])
  (map list<string>->list<number/string>
 (csv->list (open-input-file filename))))

;test function
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
;Returns a list containing a valid query type and additionial user input
(define (prompt-query)
 (print "Enter Querry Type [Name, Date, Publisher, Region, Genre,Skip]: ")
 (prompt-for-specifics-of-query(string-titlecase  (read-line))))

;Sub function for prompt-query
;Postcondition:  Prints a message relevant to the query type.  Then prompts the user for input,  and returns a list containing query type and user input
;If query type is invalid, this function will call the parent prompt-query function again. This means that the recursion will not terminate until a valid query type is entered
(define (prompt-for-specifics-of-query query-type)
  (print (query-type->message query-type))
  (if (equal? (query-type->message query-type) "INVALID QUERY TYPE!")
      (prompt-query)
      (list query-type (read-line))))

(define (query-type->message query-type)
   (define message-hash
     (hash
      "Name" "Enter Title: "
      "Date" "Enter date-range [{year1}-{year2}]: "
      "Publisher" "Enter publisher: "
      "Region" "Enter Region [North-America,Europe,Japan,Rest of World,Global]: "
      "Genre" "Enter Genre: "
      "Skip" "Skipping search parameter. Input anything here,  it will be ignored"))
   (hash-ref message-hash query-type "INVALID QUERY TYPE!"))