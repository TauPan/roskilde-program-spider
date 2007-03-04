#!/bin/sh
IFS=" "
cd `dirname $0`
exec /usr/lib/scsh-0.6/scshvm -o /usr/lib/scsh-0.6/scshvm -h 8000000 -i /usr/lib/scsh-0.6/scsh.image -lel /usr/local/lib/scsh/modules/0.6/htmlprag-0.13/load.scm -o htmlprag -o handle -o conditions -o srfi-13 -o srfi-28 -o threads -l $0 $@
!#

;;; $Id: program-spider.scm,v 1.7 2006/01/30 11:51:29 friedel Exp friedel $

;;;srfi-1: list library
;;;srfi-13: string operations
;;;srfi-28: basic format strings
;;;handle: error handling
;;;conditions: condition checking
;;;threads: needed by conditions
;;;htmlprag: parse html

(define program-topurl
; "http://www.roskilde-festival.dk/object.php?obj=88000c&Letter=alle&code=1")  ;; 2005
; "http://www.roskilde-festival.dk/object.php?obj=539000c&Letter=alle&code=1")   ;; 2006
"http://www.roskilde-festival.dk/object.php?obj=823000c&code=1")
(define forum-topurl
  "http://www.roskilde-festival.dk/community/index.php?page=300")

(define httpclient '(wget -O -))
(define sleeptime 5000)
(define convert '(iconv -f iso8859-1 -t utf-8))
;;; Some addresses for find-branch-address
;; for the bandlist
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

;; for the forum
(define forumlisttable
  '(table tr td table (7 . tr) td table (1 . tr) (1 . td) (1 . table)))

(define threadlisttable
  forumlisttable) ;; same format, yay

(define postlisttable
  '(table tr td table (7 . tr) td table (1 . tr) (1 . td) table)) ;; almost the same


'( tr td  table)

(define topiclink
  '((4 . td) table tr (1 . td) (1 . small) a @ class ))

(define donkey-top-url "http://localhost:4080/submit?q=")
(define allmusic-search-format
  "http://www.allmusic.com/cg/amg.dll?P=amg&sql=~a&x=0&y=0&opt1=1&sourceid=mozilla-search")
(define lastfm-top-url "http://www.last.fm/music/")

;;; some indexes

(define idx-bandname 0)
(define idx-countries 1)
(define idx-shortdesc 2)
(define idx-longdesc 3)
(define idx-img 4)
(define idx-www 5)
(define idx-snd 6)
(define idx-feature 7)

;;; Formats:
(define band-row-format-html
  (string-append "<tr>~%"
                 "<td width=\"250px\">~%"
                 "<p><h3>~a</h3></p>~%" ;; bandname
                 "<p>~a<p>~%" ;; countries
                 "<p><img src=\"~a\"><p>~%" ;; img
                 "<p>~a</p>~%" ;; short description
                 "<p>~%"
                 "<small>~%"
                 "<a href=\"~a\">www</a>&nbsp;~%" ;; www
                 "<a href=\"~a\">snd</a>&nbsp;~%" ;; snd
                 "<a href=\"~a\">feature</a>&nbsp;~%"
                 ;; feature
                 "<a href=\"~a\">allmusic</a>&nbsp;~%"
                 ;; allmusic
                 "<a href=\"~a\">last.fm</a>&nbsp;~%"
                 ;; last.fm
                 "<a href=\"~a\">donkey</a>&nbsp;~%"
                 ;; donkey
                 "</small>~%"
                 "</p></td>"
                 "<td>~a</td>~%" ;; long description
                 "</tr>~%"))

(define (band-row-featurelist bandlist-item)
  (append (map (lambda (x) (list-ref bandlist-item x))
               (list idx-bandname
                     idx-countries
                     idx-img
                     idx-shortdesc
                     idx-www
                     idx-snd
                     idx-feature))
          (let* ((bandname (car bandlist-item))
                 (bandurlspace (space-encode bandname))
                 (bandurlenc (url-encode bandname)))
            (list (allmusic-search-url bandurlspace)
                  (lastfm-search-url bandurlspace)
                  (donkey-search-url (url-encode (car bandlist-item)))
                (list-ref bandlist-item idx-longdesc)))))

(define bandlist-page-format-html
  (string-append "<html>~%"
                 "<body>~%"
                 "<table border=\"1\" rules=\"rows\">~%"
                 "~a~%"
                 "</table>~%"
                 "</body>~%"
                 "</html>~%"))

(define forum-summary-page-format-html
  (string-append "<html>~%"
                 "<style type=\"text/css\">~%"
                 "table { width:100%; table-layout: fixed;}~%"
                 "td {  padding: 8px; }~%"
                 "</style>~%"
                 "<body>~%"
                 "<h1>Forum summary for posts since ~a, generated on ~a.</h1>~%"
                 "<table border=\"1\" rules=\"rows\">~%"
                 "<colgroup>~%"
                 "<col width=\"20%\">~%"
                 "<col width=\"65%\">~%"
                 "<col width=\"15%\" align=\"right\">~%"
                 "</colgroup>~%"
                 "~a~%"
                 "</table>~%"
                 "</body>~%"
                 "</html>~%"))

(define forum-row-format-html
  (string-append "<tr style=\"border-style:hidden;\">~%"
                 "<td colspan=\"3\"><h2>Forum: <a href=\"~a\">~a</a></h2>~%"
                 "</td>~%"
                 "</tr>~%"
                 "~a~%"))

(define (forum-row-featurelist forum-item)
  (append (map (lambda (x) (list-ref forum-item x))
               (list 1
                     0))
          (list (apply string-append
                       (map thread-row-html
                            (cdddr forum-item))))))

(define thread-row-format-html
  (string-append "<tr style=\"border-style:double;\">~%"
                 "<td colspan=\"3\"><h3>Thread: <a href=\"~a\">~a</a></h3>~%"
                 "</td>~%"
                 "</tr>~%"
                 "~a~%"))

(define (thread-row-featurelist thread-item)
  (append (map (lambda (x) (list-ref thread-item x))
               (list 1
                     0))
          (list (apply string-append
                       (map post-row-html
                            (cdddr thread-item))))))


(define post-row-format-html
  (string-append "<tr style=\"border-bottom-style:hidden;\">~%"
                 "<td><p>~a</p>~%"
                 "</td>~%"
                 "<td><h4>~a</h4></td>~%"
                 "<td><small><p>~a</p></small></td>~%"
                 "</tr>~%"
                 "<tr>~%"
                 "<td colspan=\"3\">~%"
                 "<p>"
                 "~a~%"
                 "</p>"
                 "</td>~%"
                 "</tr>~%"))

(define (post-row-featurelist post-item)
  (map (lambda (x) (list-ref post-item x))
               (list 0
                     1
                     2
                     3)))

;;; -------------------------------------------------------------
(define (url-body url)
  (sleep sleeptime)
  (cdr (find-branch-address '(html body)
                            (html->shtml
                             (run/string ,(append httpclient
                                                  (list url)))))))

(define (get-url-string url)
  (sleep sleeptime)
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

(define split-space (infix-splitter " "))

(define split-dash (infix-splitter "-"))

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

(define (identity x)
  x)

(define (filter-branch-address seq tree)
  (filter identity
          (map (lambda (x)
                 (do-or-f (find-branch-address seq
                                               x)))
          tree)))

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

(define (forumlist-table tree)
  (filter-branch-address '((1 . td))
                         (find-branch-address
                          forumlisttable
                          tree)))

(define (threadlist-table tree)
  (find-branch-address threadlisttable tree))

(define (postlist-table tree)
  (filter (lambda (x)
            (do-or-f (find-branch-address topiclink
                                          x)))
          (filter-car 'tr (find-branch-address postlisttable tree))))



(define (filter-and-append-strings seq tree)
  (map (lambda (x) (apply string-append (cdr x)))
       (filter-branch-address seq
                              tree)))

(define (filter-and-convert-strings seq tree)
  (map (lambda (x) (shtml->html (cdr x)))
       (filter-branch-address seq
                              tree)))

(define (posts-posters tree)
  (map (lambda (x) (shtml->html (cddr x)))
       (filter-branch-address '((1 . td))
                              tree)))

(define (posts-subjects tree)
  (filter-and-convert-strings '((4 . td) table tr td b)
                              tree))

(define (posts-dates tree)
  (map mangle-date
       (filter-and-convert-strings '((4 . td) table tr (1 . td) small)
                                   tree)))

(define (posts-texts tree)
  (map (lambda (x) (shtml->html (drop x 6)))
       (filter-branch-address '((4 . td))
                              tree)))

(define (posts-replylinks tree)
  (filter-and-append-strings '((4 . td) table tr (1 . td)  (1 . small) a @ href)
                             tree))

(define (posts-list tree)
  (map list
       (posts-posters tree)
       (posts-subjects tree)
       (posts-dates tree)
       (posts-texts tree)
       (posts-replylinks tree)))

(define (thread-titles tree)
  (filter-and-convert-strings '((1 . td) a b) tree))

(define (thread-links tree)
  (filter-and-append-strings '((1 . td) a @ href) tree))

(define (drop-uneven tree accum)
  (if (null? tree)
      (reverse accum)
      (drop-uneven (cddr tree)
                   (cons (cadr tree) accum))))

(define (thread-dates tree)
  (map (lambda (x) (let ((ls (cddr x)))
                     (mangle-date
                      (string-append (first ls)
                                     " "
                                     (third ls)))))
       (drop-uneven (filter-branch-address '((5 . td)) tree)
                    '())))

(define (thread-list tree)
  (map list
       (thread-titles tree)
       (thread-links tree)
       (thread-dates tree)))

(define (forumlist-titles tree)
  (filter-and-convert-strings '(a b)
                              tree))

(define (forumlist-links tree)
  (filter-and-append-strings '(a @ href)
                             tree))

(define (forumlist-descriptions tree)
  (map (lambda (x) (shtml->html (cddr x)))
       (filter (lambda (x) (string? (list-ref x 2)))
               tree)))

(define (forumlist-list tree)
  (map list
       (forumlist-titles tree)
       (forumlist-links tree)
       (forumlist-descriptions tree)))

(define (mangle-date str)
  "mangle a date string from the site such that it can be
compared alphabetically, i.e. from '19-10-2005 13:20' to
'2005-10-19 13:20'"
  (let* ((date-clock (split-space str))
         (day-mon-year (split-dash (first date-clock))))
    (string-append (third day-mon-year)
                   "-"
                   (second day-mon-year)
                   "-"
                   (first day-mon-year)
                   " "
                   (second date-clock))))

(define (laterthan firstdate seconddate)
  (or (string> firstdate seconddate)
      (string= firstdate seconddate)))

(define (posts-in-thread-since thread-url date)
  "date: YYYY-MM-DD HH:MM"
  (let ((allposts (posts-list (postlist-table (url-body thread-url)))))
    (filter (lambda (x) (laterthan (third x) date))
            allposts)))

(define (posts-in-forum-since forum-url date)
  (let* ((laterdate (lambda (x) (laterthan (third x) date)))
         (threads (filter laterdate (thread-list (threadlist-table (url-body forum-url))))))
    (if (not (null? threads))
        (map (lambda (thread)
               (append thread
                       (posts-in-thread-since (second thread) date)))
             threads)
         threads)))

(define (posts-global-since date)
  (let* ((forums (forumlist-list (forumlist-table (url-body forum-topurl)))))
    (if (not (null? forums))
        (map (lambda (forum)
               (let ((posts (posts-in-forum-since (second forum) date)))
                 (if (null? posts)
                     #f
                     (append forum posts))))
             forums)
        forums)))

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
                        (let ((y (split-parens (shtml->html (drop el 2)))))
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
  (spider-bandlist (bandlist-table (url-body program-topurl))))


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
  (string-append donkey-top-url "s+"
                             actname))

(define (allmusic-search-url actname)
  (format allmusic-search-format actname))

(define (lastfm-search-url actname)
  (string-append lastfm-top-url actname))

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

(define (space-encode str)
  (string-map (lambda (c)
                (if (eqv? c #\space)
                    #\+
                    c))
              str))

(define (url-encode str)
  (string-fold-right (lambda (c s)
                       (string-append
                        (format "%~a"
                                (char->hex c))
                        s))
                     ""
                     str))

(define (band-row-html bandlist-item)
  (apply format
         band-row-format-html
         (band-row-featurelist bandlist-item)))

  ;; simply throw out plain html... I could use shtml but I'm too
  ;; lazy to read the manual right now :-}
(define (my-bandlist-page-html bandlist filename)
  (with-output-to-file filename
    (lambda ()
      (display (format bandlist-page-format-html
                       (apply string-append
                              (map band-row-html bandlist)))))))

;;(with-output-to-file "global-posts-list-2006-Jan-1-29.scm" (lambda () (display (format "~s" global-posts-list))))
(define (dump-bandlist-to-file bandlist filename)
  (with-output-to-file filename (lambda () (display (format "~s" bandlist)))))

(define (post-row-html post-entry)
  (apply format post-row-format-html
         (post-row-featurelist post-entry)))

(define (thread-row-html thread-entry)
  (apply format thread-row-format-html
         (thread-row-featurelist thread-entry)))

(define (forum-row-html forum-entry)
  (apply format forum-row-format-html
         (forum-row-featurelist forum-entry)))

(define (now)
  (run/string (date "+%Y-%m-%d %H:%M")))

(define (my-forum-summary-page global-posts-list date filename)
  (with-output-to-file filename
    (lambda ()
      (display (format forum-summary-page-format-html
                       date
                       (now)
                       (apply string-append
                              (map forum-row-html
                                   (filter identity
                                           global-posts-list))))))))

(define (dashdate date)
  (let* ((date-clock (split-space date)))
    (string-append (car date-clock) "-" (cadr date-clock))))

(define (do-forum-summary since)
  (let* ((until (string-drop-right (now) 1))
         (until-dashed (dashdate until))
         (since-dashed (dashdate since))
         (global-posts-list (posts-global-since since))
         (outfile (string-append "forum-summary-"
                                 since-dashed
                                 "--"
                                 until-dashed
                                 ".html")))
    (display (format "Writing forum summary from ~s to ~s to file ~s~%"
                     since
                     until
                     outfile))
    (my-forum-summary-page global-posts-list since outfile)
    (display (format "Opening summary page (~s) in browser.~%"
                     outfile))
    (run (wwwtf ,outfile))))

(define (do-bandlist)
  (let* ((bandlist (get-bandlist))
         (today (car (split-space (now))))
         (dumpfile (string-append "my-bandlist-"
                                  today
                                  ".scm"))
         (htmlfile (string-append "my-bandlist-2006"
                                  ".html")))
    (display (format "Dumping bandlist to file ~s.~%"
                     dumpfile))
    (dump-bandlist-to-file bandlist dumpfile)
    (display (format "Rendering bandlist to file ~s~%"
                     htmlfile))
    (my-bandlist-page-html bandlist htmlfile)
    (display (format "Copying bandlist page to http://dudelab.org/~~taupan/~s~%"
                     htmlfile))
    (run (scp ,htmlfile "dudelab:public_html/"))
    (display (format "Opening bandlist page in browser.~%"))
    (run (wwwt ,(string-append "http://dudelab.org/~taupan/" htmlfile)))))

;;(display-list (get-first-level))

;;; Local Variables:
;;; mode: scheme
;;; End:


;;; $Log: program-spider.scm,v $
;;; Revision 1.7  2006/01/30 11:51:29  friedel
;;; parsing and writing html for the forum threads works nicely... hell is
;;; this code a mess!
;;;
;;; Revision 1.6  2006/01/29 21:46:34  friedel
;;; spidering the forums works, now for the output...
;;;
;;; Revision 1.5  2006/01/27 21:28:13  friedel
;;; moving out superfluous functions, trying to get rid of the () in the output
;;;
;;; Revision 1.4  2006/01/27 19:00:16  friedel
;;; This already worked fine for the 2005 program, spiking it up for RSS
;;; generation and forum spidering now
;;;
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

;;;Local variables:
;;;scheme-program-name: /home/friedel/roskilde/roskilde.scm
;;;End:
