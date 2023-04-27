#lang racket

(define openFDA-url "https://api.fda.gov/drug/label.json")

(require
 browser/external
 with-cache
 net/url
 json
 web-server/templates)

(define (cache-init)
  (when (not (directory-exists? "cache"))
    (make-directory "cache")))

(define (get-json url)
  (call/input-url (string->url url)
                  get-pure-port
                  (compose string->jsexpr
                           port->string)))

; search-db: String, String [Integer] -> Jsexpr

(define (search-db search-field search-value [limits #f])
  (define url (~a openFDA-url "?search="
                  search-field ":"  search-value
                  (if limits (~a "&limit=" limits) "")))
  
  (println url)
  (get-json url))

(define (list-results jsexp)
  (let* ([R (hash-ref jsexp 'results)]
         [results (for/list ([r R])
                    (list `("ID" . ,(hash-ref r 'id))
                          `("GENERIC" . ,(hash-ref (hash-ref r 'openfda "") 'generic_name ""))))])

    (with-output-to-file "cache/results.html"
      (λ () (display (include-template "DF-results-list.html")))
      #:exists 'replace
      #:mode 'text)

    ))

(define (extract-drug-information jsexp)
  (let* ([drug-data (first (hash-ref jsexp 'results))]
         [sections (sort (hash-keys drug-data) symbol<?)])
    (with-output-to-file "cache/view.html"
      (λ () (display (include-template "DF-drug-view.html")))
      #:exists 'replace
      #:mode 'text
      )))

(define (view-page path)
  (send-url (~a "file://" (current-directory) "cache/" path)))

(define (search-and-view-drug drug-name)
  (extract-drug-information
   (search-db 'openfda.substance_name drug-name))
  (view-page "view.html"))

(define (search-and-view-results drug-name)
  (list-results
   (search-db 'openfda.substance_name drug-name))
  (view-page "results.html"))
