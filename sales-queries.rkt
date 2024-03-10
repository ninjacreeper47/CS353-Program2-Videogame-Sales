#lang racket

(require csv-reading)  ;https://docs.racket-lang.org/csv-reading/index.html

;test function
(define my-input
  "Video Games Sales.csv")

;Precondition:   File must be a csv file, which matches the  default csv-reader specs
;Postcondition: returns a list of lists,  where each line in the file is a list containing an element for each data attribute.
;Numeric values are stored as numeric types everything else is strings
(define (load-data filename)
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