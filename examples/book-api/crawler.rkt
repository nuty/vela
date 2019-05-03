#lang racket
 
#|| A web crawler. ||#
 
(provide parts
         href?
         url-representative url-equal?
         crawl)
 
(require net/url html xml racket/sandbox)
 
#| Representative of url u somewhat uniquely/canonically identifying the page.
    A list of four elements: the scheme, the host, non-empty path elements
    [ignoring path parameters], and query. |#
(define (url-representative u)
    (list
     (url-scheme u)
     (url-host u)
     (remove "" (map path/param-path (url-path u)))
     (url-query u)))
 
#| Whether urls u1 and u2 are equal, according to their representatives.
    Used to distinguish/identify keys in a url-dict. |#
(define (url-equal? u1 u2)
   (equal? (url-representative u1) (url-representative u2)))
 
#| New empty mutable dictionary for url keys looked up according to url-representative. |#
(define (make-url-dict)
  #| Non-unique hash code for url to facilitate faster lookup. |#
  (define (url-hash url) (equal-hash-code (url-representative url)))
  (make-custom-hash url-equal? url-hash))
 
#| Allow many malformed web pages. |#
(permissive-xexprs #t)
 
#| HTML page at url u, for urls of file, http or https scheme.
   As list of xexprs, or #f if not one of those types or problem fetching or parsing. |#
(define (url->xexprs u time-limit space-limit verbose?)
  ; When verbose?: display url-representative of u, and information about failures.
  (define (failure-result reason) (when verbose? (displayln reason)) #f)
  (when verbose? (displayln (url-representative u)))
  (and (member (url-scheme u) '("file" "http" "https"))
       (with-handlers ([exn:fail? failure-result])
         ; Limits attempt to taking time-limit seconds and using space-limit megabytes.
         (with-limits time-limit space-limit
                      (map xml->xexpr
                           (call/input-url u get-pure-port read-html-as-xml))))))
 
#| List of s, elements of s, elements of elements of s, etc recursively. |#
;this will actually put all of s in a list by keeping the lists containing no other lists intact
(define (parts s)
  (if (null? s)
      null
      (if (list? (first s))
          (if (andmap (lambda (x) (not (list? x))) (first s))
              (append (list (first s)) (parts (rest s)))
              (append (parts (first s)) (parts (rest s))))
          (cons (first s) (parts (rest s))))))
 
#| Whether part is an xexpr for an href element. |#
(define (href? part)
  (and (list? part) (>= (length part) 1) (equal? (first part) 'href) (string? (second part))))
 
#| Crawl of pages up to and including depth links away from url u, as a dictionary
     mapping urls to pages as lists of xexprs.
   The given url is considered zero links away from itself.
   Up to page-links number of links on each page are attempted to be fetched.
   Each attempt is limited to taking page-time seconds and page-space megabytes of memory.
   When same-host?: crawl is only into pages with the same host as u.
   When verbose?: displays url->xexprs verbose information for each attempted page fetch.
   If crawled is given, the crawl is added to it instead of making a new dictionary.
 
   Get & add page if not already crawled, crawl linked pages.
   Useful url functions: string->url, combine-url/relative. |#
 
(define (crawl u depth page-links page-time page-space [same-host? #t]
               [verbose? #t]
               [crawled (make-url-dict)])
  ;register the url
  (dict-set! crawled u (url->xexprs u page-time page-space verbose?))
  (if (or (zero? depth) (zero? page-links))
      ;doesnt enter the code if either the depth or the number of links per page is zero
      null
      (let ([i page-links] [templst (parts (dict-ref crawled u #f))])
        ;loop through page as long as nb of links processed is less than page-links
        (for ([j (in-range 0 (length templst))] #:unless (= i 0))
          ;if link is found, check if it isnt in dictionnary yet and if it has to be filtered out because of same-host?
          (if (and (href? (list-ref templst j))
                   (not (dict-has-key? crawled (combine-url/relative u (second (list-ref templst j)))))
                   (or (false? same-host?) (equal? (url-host u) (url-host (combine-url/relative u (second (list-ref templst j)))))))
                   (begin
                     ;recurse with depth decreased
                     (crawl (combine-url/relative u (second (list-ref templst j))) (sub1 depth)
                            page-links page-time page-space same-host? verbose? crawled)
                     ;decrease the max nb of links left to find
                     (set! i (sub1 i)))
              null))))
  crawled)



