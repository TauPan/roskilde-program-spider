#!/bin/sh
IFS=" "
exec scsh -s -lel htmlprag/load.scm -o htmlprag -o handle -o conditions -o srfi-13 -o srfi-28 -o threads "$0" "$@"
!#

;;; $Id: program-spider.scm,v 1.3 2005/04/24 01:23:22 friedel Exp friedel $

;;;srfi-1: list library
;;;srfi-13: string operations
;;;srfi-42:
;;;handle: error handling
;;;conditions: condition checking
;;;htmlprag: parse html

(define topurl
;;  "http://www.roskilde-festival.dk/object.php?obj=88000c&Letter=alle&code=1") ;; 2005
    "http://www.roskilde-festival.dk/object.php?obj=539000c&Letter=alle&code=1")
(define httpclient '(wget -O -))
(define convert '(iconv -f iso8859-1 -t utf-8))
;;; Some addresses for find-branch-address
(define bandlisttable   '(td table tr td (1 . table)))
(define bandname        '((1 . td) a))
(define bandinfourl     '(@ href))
(define shortdesc       '((1 . td)))
(define photo           '(td table tr td (1 . table)
                             tr (1 . td) img @ src))
(define soundsamples    '(td table tr td (1 . table)
                             tr (2 . td) table tr td a @ href))
(define homepage        '(td table tr td (1 . table)
                             tr (2 . td) table (1 . tr) td a @ href))
(define longdescription '(td table (2 . tr) (1 . td) p))
(define feature         '(td table (2 . tr) (1 . td) (1 . p) b a @ href))

(define faktboxstart (rx "faktabox start"))
(define faktboxend (rx "Faktabox slut"))

(define donkey-top-url "http://localhost:4080/submit?q=")

;;; some indexes

(define idx-bandname 0)
(define idx-countries 1)
(define idx-shortdesc 2)
(define idx-longdesc 3)
(define idx-img 4)
(define idx-www 5)
(define idx-snd 6)
(define idx-feature 7)

;;; Formats and feature lists:
(define band-row-format-html
  (string-append "<tr>~%"
                 "<td><table>~%"
                 "<tr><td><h3>~a</h3></td></tr>~%" ;; bandname
                 "<tr><td>~a</td></tr>~%" ;; countries
                 "<tr><td><img src=\"~a\"></td></tr>~%" ;; img
                 "<tr><td>~a</td></tr>~%" ;; short description
                 "<tr><td><table><tr>~%"
                 "<td><a href=\"~a\">www</a></td>~%" ;; www
                 "<td><a href=\"~a\">snd</a></td>~%" ;; snd
                 "<td><a href=\"~a\">feature</a></td>~%"
                 ;; feature
                 "<td><a href=\"~a\">donkey</a></td>~%"
                 ;; donkey
                 "</tr></table></td></tr>"
                 "</table></td>~%"
                 "<td>~a</td>~%" ;; long description
                 "</tr>~%"))

(define bandlist-page-format-html
  (string-append "<html>~%"
                 "<body>~%"
                 "<table border=\"1\" rules=\"rows\">~%"
                 "~a~%"
                 "</table>~%"
                 "</body>~%"
                 "</html>~%"))

(define (band-row-featurelist bandlist-item)
  (append (map (lambda (x) (list-ref bandlist-item x))
               (list idx-bandname
                     idx-countries
                     idx-img
                     idx-shortdesc
                     idx-www
                     idx-snd
                     idx-feature))
          (list (donkey-search-url (url-encode (car bandlist-item)))
                (list-ref bandlist-item idx-longdesc))))

;;; -------------------------------------------------------------
(define (url-body url)
  (cdr (find-branch-address '(html body)
                            (html->shtml
                             (run/string ,(append httpclient
                                                  (list url)))))))

(define (get-url-string url)
  (run/string (| ,(append httpclient (list url))
                 ,convert)))

(define (car-is? el x)
  (and (list? x)
       (equal? el
               (car x))))

(define (find-car el lis)
  (find (lambda (x)
          (car-is? el x))
        lis))

(define (filter-car el lis)
  (filter (lambda (x)
            (car-is? el x))
          lis))

(define split-slash (infix-splitter "/"))

(define split-parens (infix-splitter (rx (| "(" ")" ))))

(define (elements-between pred1
                          pred2
                          lst)
  (if (not (list? lst))
      #f
      (if (null? lst)
          #f
          (if (pred1 (car lst))
              (take-while (lambda (x) (not (pred2 x)))
                          (cdr lst))
              (or (elements-between pred1
                                    pred2
                                    (car lst))
                  (elements-between pred1
                                    pred2
                                    (cdr lst)))))))

(define (comment-match pr re)
  (and (list? pr)
       (eq? (car pr)
            shtml-comment-symbol)
       (regexp-search? re (cadr pr))))

(define (find-branch-address seq tree)
  (if (null? seq)
      tree
      (let ((look (car seq)))
        (if (pair? look)
            (find-branch-address (cdr seq)
                                 (list-ref (filter-car (cdr look)
                                                       tree)
                                           (car look)))
            (find-branch-address (cdr seq)
                                 (find-car look
                                           tree))))))

(define (error-or-f? val)
  (or (error? val)
      (not val)))

(define (call+calc-or-fail thunk successcalc failthunk)
  (let ((val (ignore-errors thunk)))
    (if (error-or-f? val)
        (failthunk)
        (successcalc val))))

(define-syntax with-success-or-fail
  (syntax-rules ()
    ((with-success-or-fail expr successcalc failexpr)
     (call+calc-or-fail (lambda () expr)
                        successcalc
                        (lambda () failexpr)))))

(define-syntax do-or-f
  (syntax-rules ()
    ((do-or-f . body)
     (with-success-or-fail (begin . body) (lambda (x) x) #f))))

(define (faktabox tree)
  (elements-between (lambda (x)
                      (comment-match x faktboxstart))
                    (lambda (x)
                      (comment-match x faktboxend))
                    tree))

(define (bandlist-table tree)
  (filter-car
   'tr
   (find-branch-address
    bandlisttable
    (faktabox tree))))

(define (get-html-branch address tree)
  (with-success-or-fail
   (shtml->html (cdr (find-branch-address address
                                          tree)))
   (lambda (x) x)
   ""))

(define (get-bandinfo url)
  (let ((fbox (faktabox (url-body url))))
    (list (get-html-branch longdescription fbox)
          (get-html-branch photo fbox)
          (get-html-branch homepage fbox)
          (get-html-branch soundsamples fbox)
          (get-html-branch feature fbox))))

(define (url-name-country rest collected)
  (if (null? rest)
      (reverse collected)
      (let* ((el (car rest))
             (newel
              (with-success-or-fail
               (find-branch-address bandname el)
               (lambda (el)
                 (do-or-f
                  (cons (second (find-branch-address bandinfourl
                                                     el))
                        (let ((y (split-parens (third el))))
                          (list (string-trim-both (car y))
                                (split-slash (second y)))))))
               #f)))
        (if newel
            (short-description (cdr rest)
                               collected
                               newel)
            (url-name-country (cdr rest)
                              collected)))))

(define (short-description rest collected passover)
  (if (null? rest)
      (reverse collected)
      (let* ((el (car rest))
             (newel (do-or-f
                     (let ((strlist
                            (filter string?
                                    (drop (find-branch-address
                                           shortdesc
                                           el)
                                          2))))
                       (if (not (null? strlist))
                           (reduce string-append
                                   ""
                                   strlist)
                           #f)))))
        (if newel
            (url-name-country (cdr rest)
                              (cons (append (cdr passover)
                                            (list newel)
                                            (get-bandinfo (car passover)))
                                    collected))
            (short-description (cdr rest)
                               collected
                               passover)))))

(define (spider-bandlist tree)
  (url-name-country tree '()))

(define (get-bandlist)
  (spider-bandlist (bandlist-table (url-body topurl))))


;;; just for debugging convenience, I could use format, but I don't wanna!
(define (display-list lst)
  (for-each (lambda (x)
              (newline)
              (write x)
              (newline))
            lst))

(define (bl2csv entry)
  (let collect ((str "")
                (rest entry))
    (if (null? rest)
        str
        (let ((a (car rest)))
          (collect (string-append str
                                  (format "~s" a)
                                  (if (not (null? (cdr rest)))
                                      ","
                                      ""))
                   (cdr rest))))))

(define (donkey-search-url actname)
  (string-append "http://localhost:4080/submit?q=s+"
                             actname))

(define (char->hex c)
  (string-upcase (number->string (char->ascii c)
                                 16)))

(define (html-encode str)
  (string-fold-right (lambda (c s)
                       (string-append
                        (format "&#x~a;"
                                (char->hex c))
                        s))
                     ""
                     str))

(define (url-encode str)
  (string-fold-right (lambda (c s)
                       (string-append
                        (format "%~a"
                                (char->hex c))
                        s))
                     ""
                     str))

(define (donkey-search-line actname)
  ;; simply throw out plain html... I could use shtml but I'm too
  ;; lazy to read the manual right now :-}
  (format "<p><a href=\"~a\">~a</a></p>~%"
          (donkey-search-url (url-encode actname))
          (html-encode actname)))

(define (donkey-search-webpage bandlist filename)
  (with-output-to-file filename
    (lambda ()
      (display (format "<html>~%<body>~%~a~%</body>~%</html>~%"
                       (map donkey-search-line
                            (map car
                                 bandlist)))))))

(define (band-row-html bandlist-item)
  (apply format
         band-row-format-html
         (band-row-featurelist bandlist-item)))

(define (my-bandlist-page-html bandlist filename)
  (with-output-to-file filename
    (lambda ()
      (display (format bandlist-page-format-html
                       (map band-row-html bandlist))))))


;;(display-list (get-first-level))

;;; Local Variables:
;;; mode: scheme
;;; End:


;;; $Log: program-spider.scm,v $
;;; Revision 1.3  2005/04/24 01:23:22  friedel
;;; Improve with-success-or-fail and do-or-f to be slightly more general
;;; (to have a better excuse for keeping them)
;;;
;;; Revision 1.2  2005/04/23 23:52:42  friedel
;;; Program is correctly parsed into a list of lists :-}
;;;

#!
,exec ,load /usr/local/lib/scsh/modules/0.6/htmlprag-0.13/load.scm
,open threads
,open handle
,open srfi-13
,open conditions
,open srfi-28
y
,open htmlprag
y

!#
