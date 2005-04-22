#!/usr/bin/scsh -s
-o srfi-13
!#

;;srfi-13: string operations

(define topurl
  "http://www.roskilde-festival.dk/object.php?obj=88000c&Letter=alle&code=1")
(define httpclient '(wget -O -))
(define bandheading "class=\"heading2\"")
(define shortdescr "<td colspan=2 valign=top>")
(define soundsamples "hoejtaler.gif")
(define websitelinks "link.gif")
(define photo "PR photo")
(define text "hvidtekst")
(define feature "Read more")

(define (get-url-strings url)
  (run/strings ,(append httpclient (list url))))

(define (get-url-string url)
  (run/string ,(append httpclient (list url))))

(define (display-list lst)
  (for-each (lambda (x)
              (newline)
              (write x)
              (newline))
            lst))

(define split-tags (infix-splitter (rx (| "<" ">"))))
(define split-strings (infix-splitter "\""))
(define split-slash (infix-splitter "/"))
(define split-braces (infix-splitter (rx (| "(" ")"))))
(define split-cells (infix-splitter (rx "<td")))

(define (band-url-name-country-shortdesc headline
                                         tagline)
  (let* ((headtags (split-tags headline))
         (url (list-ref (split-strings (list-ref headtags
                                                 5))
                        3))
         (namecountry (split-braces (list-ref headtags
                                              6)))
         (name (car namecountry))
         (country (split-slash (cadr namecountry)))
         (shortdesc (list-ref (split-tags tagline)
                              2)))
    (list url
          name
          country
          shortdesc)))

(define (walk-lines lines patternhead patterntext)
  (let collect ((rest lines)
                (collected '()))
    (if (null? rest)
        (reverse collected)
        (let ((line (car rest)))
          (if (or (string-contains-ci line
                                      patternhead)
                  (string-contains-ci line
                                      patterntext))
              (collect (cdr rest)
                       (cons line collected))
              (collect (cdr rest)
                       collected))))))
          

(define (get-first-level)
  (let ((html-lines (walk-lines (get-url-strings topurl)
                                bandheading
                                shortdescr)))
    (let collect ((rest html-lines)
                  (collected '()))
      (if (null? rest)
          (reverse collected)
          (collect (cddr rest)
                   (cons (band-url-name-country-shortdesc (car rest)
                                                          (cadr rest))
                         collected))))))

;;(display-list (get-first-level))
