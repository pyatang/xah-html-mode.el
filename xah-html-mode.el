;;; xah-html-mode.el --- Major mode for editing html. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2021, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 11.10.20210412115720
;; Created: 12 May 2012
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, html, web
;; License: GPL v3
;; Homepage: http://ergoemacs.org/emacs/xah-html-mode.html

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Major mode for editing HTML files.
;; home page: http://ergoemacs.org/emacs/xah-html-mode.html

;; part of emacs
(require 'ido)
(require 'sgml-mode)
(require 'newcomment)
(require 'browse-url)
(require 'url-util)
(require 'thingatpt)
(require 'seq)
(require 'subr-x)
;; (require 'mhtml-mode)

(require 'xah-replace-pairs) ; http://ergoemacs.org/emacs/elisp_replace_string_region.html
(require 'xah-get-thing) ; http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html
(require 'xah-css-mode) ; http://ergoemacs.org/emacs/xah-css-mode.html
(require 'htmlize) ; https://github.com/hniksic/emacs-htmlize
(require 'xeu_elisp_util)

;; (load "html-util.el" )

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")

(defvar xah-html-webroot-list nil "a list of web root full paths.
this is used by `xah-html-get-webroot', and commands that create link with relative path.")

(setq
 xah-html-webroot-list
 '(

   "/Users/xah/web/ergoemacs_org/"
   "/Users/xah/web/wordyenglish_com/"
   "/Users/xah/web/xaharts_org/"
   "/Users/xah/web/xahlee_info/"
   "/Users/xah/web/xahlee_org/"
   "/Users/xah/web/xahmusic_org/"
   "/Users/xah/web/xahsl_org/"
   ))

(defun xah-html-get-webroot (@path)
  "Return the webroot of @path.
Webroot dir are defined in `xah-html-webroot-list'.
All path should be full path here.
Version 2018-05-08"
  (interactive)
  (seq-find
   (lambda (x)
     (string-match
      (concat "^" x)
      @path ))
   xah-html-webroot-list
   nil
   ))

(defun xah-html-local-link-p (@href-value)
  "Return true if it's a local file link, else false.
None local link may start with these:
 http://
 https://
 mailto:
 irc:
 ftp:
 javascript:
 //
Version 2020-07-16"
(cond
   ((string-match-p "^\.\./" @href-value) t)
   ((string-match-p "^\./" @href-value) t)
   ((string-match-p "^//" @href-value) nil)
   ((string-match-p "^http://" @href-value) nil)
   ((string-match-p "^https://" @href-value) nil)
   ((string-match-p "^mailto:" @href-value) nil)
   ((string-match-p "^irc:" @href-value) nil)
   ((string-match-p "^ftp:" @href-value) nil)
   ((string-match-p "^javascript:" @href-value) nil)
   (t t)
))

(defun xah-html-local-url-to-file-path (@local-file-url)
  "Turn a localhost file URL such as file:///C:/Users/xah/cat.html to file path.
@local-file-url must be a full path.

basically, remove the prefix of
file:///
file://localhost
file://

For example, the following string shown in browser URL field:
; On Windows Vista 2009-06
 [C:\\Users\\jj\\index.html]  IE
 [file:///C:/Users/jj/index.html]  Firefox, Google Chrome, Safari
 [file://localhost/C:/Users/jj/index.html]  Opera
 becomes
 [C:/Users/jj/index.html]

 On Mac 2009-06
 [file:///Users/jj/index.html]  Safari, Firefox
 [file://localhost/Users/jj/index.html]  Opera
 becomes
 [/Users/jj/index.html]

 On Ubuntu Linux, 2011-05
 [file:///media/HP/Users/jj/index.html] firefox
 becomes
 [/media/HP/Users/jj/index.html]
Version 2009-06-01 2021-01-12"
  (let ((case-fold-search nil))
    (xah-replace-regexp-pairs-in-string
     @local-file-url
     [
      ["\\`file://localhost" ""]
      ["\\`file://" ""]
      ["\\`/\\([A-Za-z]\\):" "\\1:"] ; Windows C:\\
      ["\\`C:" "c:"] ; need because a bug in `file-relative-name', it doesn't work when path C: is cap
      ["\\\\" "/"]   ; Windows \ → /
      ]
     "FIXEDCASE"
     )))

(defun xah-html-get-relative-path-to-webroot (@path)
  "Return the relative path of @path with respect to its webroot.
 @path should be full path here.
Webroot dir are defined in `xah-html-webroot-list'.
If @path is not there, return relative path to parent dir.
Version 2018-05-08"
  (interactive)
  (let (($webroot (xah-html-get-webroot @path))
        ($current_dir (file-name-directory (or (buffer-file-name) default-directory))))
    (if $webroot
        (progn
          (concat (file-relative-name $webroot $current_dir)
                  (file-relative-name @path $webroot)))
      (file-relative-name @path $current_dir))))

(defun xah-html--display-hr-as-line ()
  "Display hr tag as a line.
Version 2020-09-05"
  (interactive)
  (let (p1 p2)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "<hr>" nil "NOERROR")
        (setq p1 (match-beginning 0))
        (setq p2 (match-end 0))
        (put-text-property
         p1 p2 'display "__________________________________________________"))
      (goto-char (point-min))
      (while (search-forward "<hr />" nil "NOERROR")
        (setq p1 (match-beginning 0))
        (setq p2 (match-end 0))
        (put-text-property
         p1 p2 'display "__________________________________________________")))))

(defun xah-html-display-page-break-as-line ()
  "Display the formfeed ^L char as line.
Version 2018-08-17"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

;; HHH___________________________________________________________________

(defun xah-html--get-tag-name (&optional left<)
  "Return the tag name.
This function assumes your cursor is inside a tag, eg <…▮…>"
  (let ( $p1 $p2 )
    (when (not left<)
      (setq left< (search-backward "<")))
    (goto-char left<)
    (forward-char 1)
    (when (looking-at "/" )
      (forward-char 1))
    (setq $p1 (point))
    (re-search-forward " \\|>")
    (backward-char 1)
    (setq $p2 (point))
    (buffer-substring-no-properties $p1 $p2)))

(defun xah-html--cursor-in-link-p ()
  "Return true if curser is inside a string of src or href.
Version 2020-01-15"
  (interactive)
  (let ((in-string-q (nth 3 (syntax-ppss))))
    (if in-string-q
        (save-excursion
          (skip-chars-backward "^\"")
          (backward-char)
          (if (string-match "=" (char-to-string (char-before)))
              (progn
                (backward-char 1)
                (if (or
                     (string-match "href" (current-word))
                     (string-match "src" (current-word))
                     (string-match "poster" (current-word))
                     (string-match "content" (current-word)))
                    (progn t)
                  (progn nil)))
            nil))
      nil
      )))

;; (defun xah-html--cursor-in-link-p ()
;;   "Return true if curser is inside a string of src or href.
;; Version 2018-02-21"
;;   (interactive)
;;   (let (
;;         (p0 (point))
;;         (in-string-q
;;          (save-excursion
;;            (beginning-of-line)
;;            (re-search-forward "src=\"\\|href=\"" (line-end-position) t))))
;;     (save-excursion
;;       (skip-chars-backward "^\"")
;;       (backward-char)
;;       (if (string-match "=" (char-to-string (char-before)))
;;           (progn
;;             (backward-char 1)
;;             (if (or
;;                  (string-match "href" (current-word))
;;                  (string-match "src" (current-word))
;;                  (string-match "content" (current-word)))
;;                 (progn t)
;;               (progn nil)))
;;         nil))))

(defun xah-html--tag-self-closing-p (@tag-name)
  "Return true if the tag is a self-closing tag, <br> or <br />"
  (interactive)
  (member @tag-name xah-html-html5-self-close-tags))

(defun xah-html-delete-tag ()
  "Delete the tag under cursor.
This function assume cursor is inside the tag <…▮…>.
Version 2021-01-03"
  (interactive)
  (let (p1 p2)
    (save-excursion
      (search-backward "<" )
      (setq p1 (point))
      (search-forward ">")
      (setq p2 (point))
      (delete-region p1 p2))))

(defun xah-html-delete-tag-pair ()
  "Remove the current tag(s) under cursor.
For example, if you have <p>some</p> and cursor is inside either the beginning or ending tag, it'll remove both. But if cursor is inside a self-closing tag such as <br />, just remove that.
This function assumes cursor is inside a tag <…▮…>.
This function  self-closing tags ends in />.
Version 2021-01-03 2021-02-04"
  (interactive)
  (save-excursion
    (let (p0 inEndTag-p p1 p2 openTag-p1 openTag-p2 selfCloseTag-p closingTag-p1 closingTag-p2 openTagStr closeTagStr)
      (setq p0 (point))
      (search-backward "<")
      (setq p1 (point))
      (forward-char 1)
      (setq inEndTag-p (char-equal (char-after ) ?/))
      (search-forward ">")
      (setq p2 (point))
      (if inEndTag-p
          (progn
            (setq closingTag-p1 p1)
            (setq closingTag-p2 p2)
            (goto-char p0)
            (sgml-skip-tag-backward 1)
            (setq openTag-p1 (point))
            (search-forward ">" )
            (setq openTag-p2 (point))
            (setq closeTagStr (buffer-substring closingTag-p1 closingTag-p2))
            (setq openTagStr (buffer-substring openTag-p1 openTag-p2))
            (message "Deleted: %s%s" openTagStr closeTagStr)
            (delete-region closingTag-p1 closingTag-p2)
            (delete-region openTag-p1 openTag-p2))
        (progn
          (setq selfCloseTag-p (char-equal (char-after (- p2 2)) ?/))
          (if selfCloseTag-p
              (progn
                (setq openTagStr (buffer-substring p1 p2))
                (message "Deleted:\n%s" openTagStr )
                (delete-region p1 p2))
            (progn
              (setq openTag-p1 p1)
              (setq openTag-p2 p2)
              (goto-char p0)
              (sgml-skip-tag-forward 1)
              (setq closingTag-p2 (point))
              (search-backward "<" )
              (setq closingTag-p1 (point))
              (setq closeTagStr (buffer-substring closingTag-p1 closingTag-p2))
              (setq openTagStr (buffer-substring openTag-p1 openTag-p2))
              (message "Deleted:%s%s" openTagStr closeTagStr)
              (delete-region closingTag-p1 closingTag-p2)
              (delete-region openTag-p1 openTag-p2))))))))

(defun xah-html--get-bracket-positions ()
  "Returns HTML angle bracket positions.
Returns a vector [ $pPrevL $pPrevR $pNextL $pNextR ]
 $pPrevL is the position of < nearest to cursor on the left side
 $pPrevR is the position of > nearest to cursor on the left side
 similar for $pNextL and $pNextR for the right side.
If any of these are not found, nil is the value.
Here, a char's position is the point immediately to the left of the char.

Version 2016-10-18"
  (let (
        ($pos (point))
        $pPrevL ; position of first < char to the left of cursor
        $pPrevR
        $pNextL
        $pNextR
        )
    (save-excursion
      (goto-char $pos)
      (setq $pPrevL (search-backward "<" nil "move"))
      (goto-char $pos)
      (setq $pPrevR (search-backward ">" nil "move"))
      (goto-char $pos)
      (setq $pNextL
            (if (search-forward "<" nil "move")
                (- (point) 1)
              nil
              ))
      (goto-char $pos)
      (setq $pNextR
            (if (search-forward ">" nil "move")
                (- (point) 1)
              nil
              ))
      (vector $pPrevL $pPrevR $pNextL $pNextR))))

(defun xah-html--cursor-in-tag (&optional @bracketPositions)
  "Return t if cursor is between angle brackets like this: 「<…▮…>」, where the … is any char except angle brackets.
More precisely: on the left side of cursor, there exist a <, and there's no > between < and cursor.
And, on the right side of cursor, there exist a >, and there's no < between > and cursor.
 @bracketPositions is optional. If nil, then `xah-html--get-bracket-positions' is called to get it.
Version 2016-10-18 2021-01-03"
  (interactive)
  (let (($bracketPos
         (if @bracketPositions
             @bracketPositions
           (xah-html--get-bracket-positions)))
        $pPrevL $pPrevR $pNextR $pNextL )
    (progn
      (setq $pPrevL (elt $bracketPos 0))
      (setq $pPrevR (elt $bracketPos 1))
      (setq $pNextL (elt $bracketPos 2))
      (setq $pNextR (elt $bracketPos 3)))
    (if (and
         $pPrevL
         $pNextR
         (if $pPrevR
             (< $pPrevR $pPrevL)
           t
           )
         (if $pNextL
             (< $pNextR $pNextL)
           t
           ))
        nil)))

;; HHH___________________________________________________________________

;; (defvar xah-html--month-full-names '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") "list of English month full names.")

;; (defvar xah-html--month-abbrev-names (mapcar (lambda (x) (substring x 0 3)) xah-html--month-full-names) "list of English month 3-letter abbrev names.")

;; (defvar xah-html--weekday-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") "list of English weekday full names.")

(defun xah-html--is-datetimestamp-p (@input-string)
  "Return t if @input-string is a date/time stamp, else nil.
This is based on heuristic, so it's not 100% correct.
If the string contains any month names, weekday names, or of the form dddd-dd-dd, dddd-dd-dddd, dddd-dd-dd, or using slash, then it's considered a date.

2015-09-27 issue: if a sentence “You May Do So”, it's be thought as date. Similar for containing word “March”.
Version 2017-11-22"
  (let* ((case-fold-search t)
         ($someMonthNames '("January" "February" "April" "June" "July" "August" "September" "October" "November" "December"))
         ($someMonthAbbrevs (mapcar (lambda (x) (substring x 0 3)) $someMonthNames))
         ($weekDayNames '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
    (cond
     ((string-match (regexp-opt (append $someMonthNames $someMonthAbbrevs $weekDayNames) 'words) @input-string) t)
     ;; mm/dd/yyyy
     ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]\\b" @input-string) t)
     ;; yyyy/mm/dd
     ((string-match "\\b[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" @input-string) t)
     ;; mm/dd/yy
     ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" @input-string) t)
     ;; mm-dd-yyyy
     ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]\\b" @input-string) t)
     ;; yyyy-mm-dd
     ((string-match "\\b[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" @input-string) t)
     ;; mm-dd-yy
     ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" @input-string) t)
     ((string-match "\\b[0-9][0-9][0-9][0-9]\\b" @input-string) t)
     (t nil))))

;; HHH___________________________________________________________________

;; (defun xah-html--trim-string (string)
;;   "Remove white spaces in beginning and ending of string.
;; White space here is any of: space, tab, emacs newline (line feed, ASCII 10).
;; Note: Emacs 24.4 and later, use
;;  (require 'subr-x)
;;  (string-trim ‹str›)
;; "
;;   (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;; HHH___________________________________________________________________
(defcustom xah-html-html5-tag-names nil
  "A alist of HTML5 tag names.
For each element, the key is tag name, value is a vector of one element of string: “w” means word, “l” means line, “b” means block, “s” means self-closing tag such as br, others are placeholder for unknown. The purpose of the value is to indicate the default way to wrap the tag around cursor. "
; todo: need to go the the list and look at the type carefully. Right now it's just quickly done. lots are “z”, for unkown. Also, some are self closing tags, current has mark of “n”.
:group 'xah-html-mode
)
(setq xah-html-html5-tag-names
      '(
        ;; most frequently used should be on top. todo: reorder based on use. do a cache or such, look at ido-switch-buffer

        ("div" . ["b"])
        ("span" . ["w"])
        ("br" . ["s"])

        ("b" . ["w"])
        ("strong" . ["w"])
        ("em" . ["w"])
        ("i" . ["w"])
        ("mark" . ["w"])
        ("s" . ["w"])

        ("var" . ["w"])
        ("sub" . ["z"])
        ("sup" . ["z"])
        ("small" . ["w"])
        ("time" . ["w"])
        ("kbd" . ["w"])
        ("li" . ["l"])

        ("ol" . ["b"])
        ("p" . ["b"])
        ("pre" . ["b"])

        ("h1" . ["l"])
        ("h2" . ["l"])
        ("h3" . ["l"])
        ("h4" . ["l"])
        ("h5" . ["l"])
        ("h6" . ["l"])
        ("dl" . ["b"])
        ("dt" . ["l"])
        ("marquee" . ["l"])

        ("img" . ["l"])
        ("input" . ["l"])
        ("ins" . ["z"])
        ("hr" . ["s"])

        ("a" . ["l"])
        ("blockquote" . ["b"])
        ("cite" . ["l"])
        ("code" . ["l"])

        ("nav" . ["b"])

        ("rp" . ["l"])
        ("rt" . ["w"])
        ("ruby" . ["l"])
        ("abbr" . ["w"])

        ("article" . ["b"])

        ("applet" . ["l"])
        ("main" . ["b"])
        ("address" . ["w"])
        ("aside" . ["b"])
        ("audio" . ["l"])
        ("area" . ["l"])
        ("base" . ["l"])
        ("basefont" . ["l"])
        ("bdi" . ["w"])
        ("bdo" . ["w"])
        ("body" . ["b"])
        ("button" . ["w"])
        ("canvas" . ["b"])
        ("caption" . ["l"])
        ("col" . ["n"])
        ("colgroup" . ["l"])
        ("command" . ["z"])
        ("datalist" . ["z"])
        ("dd" . ["z"])
        ("del" . ["z"])
        ("details" . ["z"])
        ("dfn" . ["z"])
        ("embed" . ["l"])
        ("fieldset" . ["z"])
        ("figure" . ["b"])
        ("figcaption" . ["l"])
        ("footer" . ["b"])
        ("form" . ["l"])
        ("head" . ["b"])
        ("header" . ["b"])
        ("html" . ["b"])
        ("iframe" . ["z"])
        ("keygen" . ["z"])
        ("label" . ["z"])
        ("legend" . ["z"])
        ("link" . ["z"])
        ("map" . ["z"])
        ("menu" . ["z"])
        ("menuitem" . ["z"])
        ("meta" . ["z"])
        ("meter" . ["z"])
        ("noscript" . ["l"])
        ("object" . ["z"])
        ("optgroup" . ["z"])
        ("option" . ["z"])
        ("output" . ["z"])
        ("param" . ["z"])
        ("picture" . ["z"])
        ("progress" . ["z"])
        ("q" . ["w"])
        ("samp" . ["z"])
        ("script" . ["b"])
        ("section" . ["b"])
        ("select" . ["z"])
        ("source" . ["z"])
        ("style" . ["b"])
        ("summary" . ["z"])
        ("table" . ["b"])
        ("tbody" . ["z"])
        ("td" . ["l"])
        ("textarea" . ["z"])
        ("tfoot" . ["z"])
        ("th" . ["z"])
        ("thead" . ["z"])
        ("title" . ["l"])
        ("tr" . ["b"])
        ("track" . ["b"])
        ("u" . ["w"])
        ("ul" . ["b"])
        ("video" . ["l"])
        ("wbr" . ["z"])))

(defvar xah-html-html5-tag-list nil
  "list of html tag names.
Extracted from `xah-html-html5-tag-names'
Version 2018-11-02")
(setq xah-html-html5-tag-list (mapcar (lambda (x) (car x)) xah-html-html5-tag-names))

(defcustom xah-html-attribute-names nil
  "HTML attribute names."
:group 'xah-html-mode)
(setq xah-html-attribute-names '(

"action"
"alt"
"charset"
"class"
"cols"
"content"
"enctype"
"for"
"height"
"href"
"http-equiv"
"id"
"lang"
"list"
"max"
"maxlength"
"media"
"method"
"min"
"name"
"poster"
"preload"
"rel"
"rows"
"size"
"src"
"srcset"
"step"
"style"
"target"
"title"
"type"
"value"
"width"
 ))

(defcustom xah-html-boolean-attribute-names nil
  "HTML boolean attribute names."
:group 'xah-html-mode)
(setq
 xah-html-boolean-attribute-names
 '(
  "controls"
   "autoplay"
   "loop"
   "async"
   "defer"
   "checked"
   "multiple"
   "selected"
   ))

(defcustom xah-html-html5-self-close-tags nil
  "List of HTML5 self-closing tag name. "
  :group 'xah-html-mode )
(setq xah-html-html5-self-close-tags '( "area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "track" "wbr"))

;; HHH___________________________________________________________________

(defun xah-html--get-tag-type (tag-name)
  "Return the wrap-type info of tag-name in `xah-html-html5-tag-names'
Version 2018-11-02"
  (elt
   (cdr
    (assoc tag-name xah-html-html5-tag-names)
    ) 0))

(defvar xah-html-lang-name-map nil "a alist that maps lang name. Each element has this form
 (‹lang code› . [‹emacs major mode name› ‹file extension›])
For example:
 (\"emacs-lisp\" . [\"xah-elisp-mode\" \"el\"])
Version 2019-04-08
")

(setq xah-html-lang-name-map
      '(
        ("ahk" . ["ahk-mode" "ahk"])

        ("code" . ["fundamental-mode" "txt"])
        ("output" . ["fundamental-mode" "txt"])

        ("bash" . ["sh-mode" "sh"])
        ("bash-output" . ["fundamental-mode" "txt"])
        ("unix-config" . ["conf-space-mode" "conf"])
        ("cmd" . ["dos-mode" "bat"])

        ("bbcode" . ["xbbcode-mode" "bbcode"])
        ("markdown" . ["markdown-mode" "md"])
        ("c" . ["c-mode" "c"])
        ("cpp" . ["c++-mode" "cpp"])
        ("common-lisp" . ["lisp-mode" "lisp"])

        ("org-mode" . ["org-mode" "org"])

        ;; ("clojure" . ["clojure-mode" "clj"])
        ("clojure" . ["xah-clojure-mode" "clj"])
        ("typescript" . ["xah-js-mode" "ts"])
        ("css" . ["xah-css-mode" "css"])
        ("emacs-lisp" . ["xah-elisp-mode" "el"])
        ("dart" . ["dart-mode" "dart"])

        ("haskell" . ["haskell-mode" "hs"])
        ("erlang" . ["erlang-mode" "erl"])
        ("golang" . ["go-mode" "go"])
        ("html" . ["xah-html-mode" "html"])
        ("mysql" . ["sql-mode" "sql"])
        ("xml" . ["sgml-mode" "xml"])
        ("html6" . ["xah-html6-mode" "html6"])
        ("java" . ["java-mode" "java"])
        ("csharp" . ["csharp-mode" "cs"])
        ("js" . ["xah-js-mode" "js"])
        ("nodejs" . ["xah-js-mode" "js"])
        ("lsl" . ["xlsl-mode" "lsl"])
        ("latex" . ["latex-mode" "txt"])
        ("ocaml" . ["tuareg-mode" "ml"])
        ("perl" . ["perl-mode" "pl"])
        ("php" . ["xah-php-mode" "php"])
        ("povray" . ["pov-mode" "pov"])
        ("powershell" . ["powershell-mode" "ps1"])
        ("python" . ["python-mode" "py"])
        ("python3" . ["python-mode" "py3"])
        ("qi" . ["shen-mode" "qi"])
        ("ruby" . ["ruby-mode" "rb"])
        ("scala" . ["scala-mode" "scala"])
        ("apl" . ["gnu-apl-mode" "apl"])
        ("scheme" . ["scheme-mode" "scm"])
        ("racket" . ["racket-mode" "rkt"])
        ("prolog" . ["prolog-mode" "prolog"])
        ("yasnippet" . ["snippet-mode" "yasnippet"])
        ("vbs" . ["visual-basic-mode" "vbs"])
        ("visualbasic" . ["visual-basic-mode" "vbs"])
        ("mathematica" . ["fundamental-mode" "m"])
        ("math" . ["fundamental-mode" "txt"])
        ("slim" . ["slim-mode" "slim"])
        ("yaml" . ["yaml-mode" "yaml"])
        ("haml" . ["haml-mode" "haml"])
        ("sass" . ["sass-mode" "sass"])
        ("scss" . ["xah-css-mode" "css"])

        ("poem_xl" . ["text-mode" "txt"])
        ("text" . ["text-mode" "txt"])
        ("lyrics_xl" . ["text-mode" "txt"])

        ("vimrc" . ["vimrc-mode" "vim"])))

(defvar xah-html-lang-name-list nil "List of langcode.")
(setq xah-html-lang-name-list (mapcar 'car xah-html-lang-name-map))

(defvar xah-html-lang-mode-list nil "List of supported language mode names.")
(setq xah-html-lang-mode-list (mapcar (lambda (x) (aref (cdr x) 0)) xah-html-lang-name-map))

(defun xah-html-htmlized-p (@begin @end)
  "Return true if region @BEGIN @END is htmlized code.
“htmlized” means the text contains &gt; or &lt; or <span class=\"...\">.
WARNING: it just check if it contains cortain span tags.
Version 2020-08-05 2021-03-12"
  (save-excursion
    (cond
     ( (progn (goto-char @begin) (search-forward "&gt;" @end t)) t )
     ( (progn (goto-char @begin) (search-forward "&lt;" @end t)) t )
     ( (progn (goto-char @begin) (re-search-forward "<span class=\"comment\">\\|<span class=\"comment-delimiter\">\\|<span class=\"function-name\">\\|<span class=\"string\">\\|<span class=\"variable-name\">\\|<span class=\"keyword\">\\|<span class=\"bold\">\\|<span class=\"builtin\">\\|<span class=\"constant\">\\|<span class=\"doc\">\\|<span class=\"preprocessor\">\\|<span class=\"type\">\\|<span class=\"underline\">\\|<span class=\"warning\">" @end t)) t
       )
     (t nil))))

(defun xah-html-get-langcode ()
  "Get the langCode and position boundary of current HTML pre block.
A pre block is text of this form
 <pre class=\"‹langCode›\">…▮…</pre>.
Your cursor must be between the tags.

Returns a vector [langCode pos1 pos2], where pos1 pos2 are the boundary of the text content.
Version 2018-09-28"
  (interactive)
  (let ($langCode
        ( $p0 (point))
        $p1 $p2
        (pre> nil)
        (code> nil)
        (pre< nil)
        (code< nil)
        (p5 nil)
        $preClose<
        $preClose>
        $codeClose<
        $codeClose>
        )
    (progn
      (goto-char $p0)
      (when (re-search-backward "<pre *[^>]*>" nil t)
        (setq pre< (point))
        (search-forward ">")
        (setq pre> (point))
        (backward-char 1)
        (xah-html-skip-tag-forward)
        (setq $preClose> (point))
        (search-backward "<")
        (setq $preClose< (point)))

      (goto-char $p0)
      (when (re-search-backward "<code *[^>]*>" nil t)
        (setq code< (point))
        (search-forward ">")
        (setq code> (point))
        (backward-char 1)
        (xah-html-skip-tag-forward)
        (setq $codeClose> (point))
        (search-backward "<")
        (setq $codeClose< (point)))

      (if (and (eq pre> nil) (eq code> nil))
          (error "no <pre> nor <code> found" )
        (if (eq pre> nil)
            (setq p5 code>)
          (if (eq code> nil)
              (setq p5 pre>)
            (if (> pre> code>)
                (progn (setq p5 pre>))
              (progn (setq p5 code>))))))

      (if (eq p5 pre>)
          (progn
            (goto-char pre<)
            (re-search-forward "class=\"\\([-A-Za-z0-9]+\\)\"")
            (setq $langCode (match-string 1))
            (goto-char p5)
            (vector $langCode pre> $preClose<))
        (progn
          (goto-char code<)
          (re-search-forward "class=\"\\([-A-Za-z0-9]+\\)\"")
          (setq $langCode (match-string 1))
          (goto-char p5)
          (vector $langCode code> $codeClose<)))
      ;;
      )))

(defun xah-html-get-precode-make-new-file (lang-name-map)
  "Create a new file in current dir with content from text inside pre code block.
For example, if the cursor is somewhere between the tags:
<pre class=\"ruby\">print 7</pre>

after calling, a new file of name
xxtemp.201508234945.8292.rb
  is created in current dir, with content “print 7”.
The numbers in the file name are datestamp and a random integer.
If file already exist, emacs will ask to overwrite.

If there's a text selection, use that region as content.
Version 2019-04-29"
  (interactive (list xah-html-lang-name-map))
  (let* (
         ($langcodeinfo (xah-html-get-langcode))
         ($langCode (elt $langcodeinfo 0))
         ($p1 (elt $langcodeinfo 1))
         ($p2 (elt $langcodeinfo 2))
         ($textContent (buffer-substring-no-properties $p1 $p2))
         ($fileSuffix (elt (cdr (assoc $langCode lang-name-map)) 1))
         ($majorMode (elt (cdr (assoc $langCode lang-name-map)) 0))
         ($majorModeSym (intern-soft $majorMode))
         ($fnTemp (format "%s.%s.%x.%s"
                          "xxtemp"
                          (format-time-string "%Y%m%d")
                          (random 999)
                          $fileSuffix))
         $fname
         ($buf (generate-new-buffer "untitled")))
    (when (null $majorMode)
      (message "no major mode found for class 「%s」." $langCode))
    (progn
      (split-window-below)
      (delete-region $p1 $p2 )
      (switch-to-buffer $buf)
      (if (fboundp $majorModeSym)
          (funcall $majorModeSym)
        (fundamental-mode))
      (setq buffer-offer-save t)
      (insert $textContent)
      (when (xah-html-htmlized-p (point-min) (point-max))
        (xah-html-remove-span-tag-region (point-min) (point-max)))
      (xah-html-decode-ampersand-region (point-min) (point-max)))
    (setq $fname
          (if (equal $fileSuffix "java")
              (progn
                (goto-char (point-min))
                (if (re-search-forward "class \\([A-Za-z0-9]+\\)[\n ]*{" nil t)
                    (progn
                      (format "%s.java" (match-string 1)))
                  (if (re-search-forward "class \\([A-Za-z0-9]+\\)[\n ]*{" nil t)
                      (progn
                        (format "%s.java" (match-string 1)))
                    $fnTemp)))
            $fnTemp))
    (write-file $fname "CONFIRM")
    (goto-char (point-min))))

;; HHH___________________________________________________________________

(defun xah-html-htmlize-string (@input-str @major-mode-name)
  "Take @input-str and return a htmlized version using @major-mode-name.
The purpose is to syntax color source code in HTML.

If @major-mode-name is string. It'll be converted to symbol and if is not in `obarray', `fundamental-mode' is used.

This function requires the `htmlize-buffer' from htmlize.el by Hrvoje Niksic.

Version 2018-09-28, 2020-09-27"
  (let ($output-buff
        $resultStr
        ($majorModeSym (intern-soft @major-mode-name)))
    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert @input-str)
      (if (fboundp $majorModeSym)
          (funcall $majorModeSym)
        (fundamental-mode))
      (font-lock-ensure)
      (setq $output-buff (htmlize-buffer)))
    ;; extract the fontified source code in htmlize output
    (with-current-buffer $output-buff
      (let ($p1 $p2 )
        (setq $p1 (search-forward "<pre>"))
        (setq $p2 (search-forward "</pre>"))
        (setq $resultStr (buffer-substring-no-properties (+ $p1 1) (- $p2 6)))))
    (kill-buffer $output-buff)
    $resultStr ))

(defun xah-html-htmlize-region (@p1 @p2 @mode-name )
  "Htmlized region @p1 @p2 using `major-mode' @mode-name.
Version 2016-12-18 2021-02-19"
  (interactive
   (list (region-beginning)
         (region-end)
         (ido-completing-read "Chose mode for coloring:" xah-html-lang-mode-list)))
  (let* (
         ($inputStr (buffer-substring-no-properties @p1 @p2))
         ($outStr (string-trim-right (xah-html-htmlize-string $inputStr @mode-name))))
    (if (string-equal $inputStr $outStr)
        nil
      (progn
        (delete-region @p1 @p2)
        (insert $outStr)))))

(defun xah-html-langcode-to-mode-name (@lang-code @lang-code-map)
  "get the `major-mode' name associated with @lang-code.
return major-mode name as string. If none found, return nil.
Version 2017-01-10"
  (interactive)
  (elt (cdr (assoc @lang-code @lang-code-map)) 0))

(defun xah-html-htmlize-pre47 (@lang-code-map)
  "Replace text enclosed by “pre” tag to htmlized code.

For example, if the cursor is inside the pre tags <pre class=\"‹langCode›\">…▮…</pre>, then after calling, the text inside the pre tag will be htmlized. That is, wrapped with many span tags for syntax coloring.

The opening tag must be of the form <pre class=\"‹langCode›\">.  The ‹langCode› determines what emacs mode is used to colorize the text. See `xah-html-lang-name-map' for possible ‹langCode›.

Cursor will end up right before </pre>.

See also: `xah-html-dehtml-711', `xah-html-toggle-syntax-coloring-markup'.
This function requires the `htmlize-buffer' from htmlize.el by Hrvoje Niksic.
Version 2018-09-28"
  (interactive (list xah-html-lang-name-map))
  (let* (
         ($precodeData (xah-html-get-langcode))
         ($langCode (elt $precodeData 0))
         ($p1 (elt $precodeData 1))
         ($p2 (elt $precodeData 2))
         ($modeName (xah-html-langcode-to-mode-name $langCode @lang-code-map)))
    (xah-html-htmlize-region $p1 $p2 $modeName)))

(defun xah-html-dehtml-711 (@begin @end)
  "Delete span tags between pre tags.
Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.
This command does the inverse of `xah-html-htmlize-pre47'.
Version 2018-10-08"
  (interactive
   (let* ( ($xx (xah-html-get-langcode)))
     (list (elt $xx 1) (elt $xx 2))))
  (save-restriction
    (narrow-to-region @begin @end)
    (xah-html-remove-span-tag-region (point-min) (point-max))
    (xah-html-decode-ampersand-region (point-min) (point-max))
    (xah-html-code-tag-to-brackets (point-min) (point-max))))

(defun xah-html-dehtmlize-pre-code-buffer ()
  "Remove htmlized text inside any code block in current page.
More specifically, any text inside

<pre class=\"‹langCode›\">…<pre>

or

<pre class=\"prettyprint\">
<code class=\"‹langCode›\">…<code>
<pre>

Returns 0 if nothing is done. Else a positive integer of the count of <pre class=lang>.
Version 2018-10-11"
  (interactive)
  (let (($count 0))
    (setq $count (+
                 (xah-html-dehtmlize-code-buffer)
                 (xah-html-dehtmlize-pre-buffer)))
    (message "dehtmlized %s code blocks" $count)
    $count
    ))

(defun xah-html-dehtmlize-pre-buffer ()
  "Remove htmlized text inside any <pre class=\"‹langCode›\">…<pre> in current page.
Returns 0 if nothing is done. Else a positive integer of the count of <pre class=lang>.
Version 2018-10-03"
  (interactive)
  (let ($langCode $p1 $p2 ($count 0)
                  $majorModeNameStr
                  )
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward "<pre class=\"\\([-A-Za-z0-9]+\\)\">" nil "move")
        (setq $langCode (match-string 1))
        (setq $majorModeNameStr (xah-html-langcode-to-mode-name $langCode xah-html-lang-name-map))
        (when $majorModeNameStr
          (progn
            (setq $p1 (point))
            (backward-char 1)
            (xah-html-skip-tag-forward)
            (search-backward "</pre>")
            (setq $p2 (point))
            (save-restriction
              (narrow-to-region $p1 $p2)
              (xah-html-dehtml-711 (point-min) (point-max))
              (setq $count (1+ $count)))))))
    $count
    ))

(defun xah-html-dehtmlize-code-buffer ()
  "Remove htmlized text inside any <code class=\"‹langCode›\">…<code> in current page.
Returns 0 if nothing is done. Else a positive integer of the count of <pre class=lang>.
Version 2018-10-03"
  (interactive)
  (let ($langCode $p1 $p2 ($count 0)
                  $majorModeNameStr
                  )
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward "<code class=\"\\([-A-Za-z0-9]+\\)\">" nil "move")
        (setq $langCode (match-string 1))
        (setq $majorModeNameStr (xah-html-langcode-to-mode-name $langCode xah-html-lang-name-map))
        (when $majorModeNameStr
          (progn
            (setq $p1 (point))
            (backward-char 1)
            (xah-html-skip-tag-forward)
            (search-backward "</code>")
            (setq $p2 (point))
            (save-restriction
              (narrow-to-region $p1 $p2)
              (xah-html-dehtml-711 (point-min) (point-max))
              (setq $count (1+ $count)))))))
    $count
    ))

(defun xah-html-toggle-syntax-coloring-markup (lang-name-map)
  "Call `xah-html-htmlize-pre47' or `xah-html-dehtml-711'.
Version 2019-06-13"
  (interactive (list xah-html-lang-name-map))
  (let* (
         ($tmp (xah-html-get-langcode))
         ($p1 (elt $tmp 1))
         ($p2 (elt $tmp 2)))
    (if (xah-html-htmlized-p $p1 $p2)
        (xah-html-dehtml-711 $p1 $p2)
      (xah-html-htmlize-pre47 lang-name-map))))

(defun xah-html-redo-syntax-coloring-file ( @file-path )
  "redo all pre lang code syntax coloring in current HTML page.
Returns 0 if nothing is done. Else a positive integer of the count of <pre class=lang>.
Version 2017-01-11"
  (interactive (read-file-name "file path:" nil nil t))
  (let ( ($result 0))
    (with-temp-buffer
      (insert-file-contents @file-path)
      (goto-char 1)
      (xah-html-mode)
      (setq $result (xah-html-redo-syntax-coloring-buffer))
      (when (> $result 0)
        (let (($backup-path
               (concat @file-path "~htmlize" (format-time-string "%Y%m%dT%H%M%S") "~")))
          (copy-file @file-path $backup-path t))
        (write-region (point-min) (point-max) @file-path)))
    $result
    ))

(defun xah-html-redo-syntax-coloring-buffer ()
  "Redo all <pre class=\"...\"> syntax coloring in current HTML page.
Returns 0 if nothing is done. Else a positive integer of the count of <pre class=lang>.
Version 2019-06-13"
  (interactive)
  (let ($langCode $p1 $p2 ($count 0)
                  $majorModeNameStr
                  )
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward "<pre class=\"\\([-A-Za-z0-9]+\\)\">" nil "move")
        (setq $langCode (match-string 1))
        (setq $majorModeNameStr (xah-html-langcode-to-mode-name $langCode xah-html-lang-name-map))
        (when $majorModeNameStr
          (progn
            (setq $p1 (point))
            (backward-char 1)
            (xah-html-skip-tag-forward)
            (search-backward "</pre>")
            (setq $p2 (point))
            (save-restriction
              (narrow-to-region $p1 $p2)
              (when (xah-html-htmlized-p (point-min) (point-max))
                (xah-html-dehtml-711 (point-min) (point-max)))
              (xah-html-htmlize-region (point-min) (point-max) $majorModeNameStr )
              (setq $count (1+ $count)))))))
    (message "xah-html-redo-syntax-coloring-buffer %s redone" $count)
    $count
    ))

(defun xah-html-open-local-link ()
  "Open the link under cursor or insert newline.
If cursor is on a src=… or href=…, then if it a file path, open file, if http, open in browser.
Else call `newline'.
To force insert a newline, you can always press C-q C-RET.
Version 2019-05-30"
  (interactive)
  (if (xah-html--cursor-in-link-p)
      (let (($srcStr  (xah-html-remove-uri-fragment (xah-get-thing-at-point 'filepath ))))
        (if (string-match "^http:\\|^https:" $srcStr)
            (browse-url $srcStr)
          (if (file-exists-p $srcStr)
              (let ; open f.ts instead of f.js
                  ( ($ext (file-name-extension $srcStr))
                    ($fnamecore (file-name-sans-extension $srcStr)))
                (if (and (string-equal $ext "js")
                         (file-exists-p (concat $fnamecore ".ts")))
                    (find-file (concat $fnamecore ".ts"))
                  (progn (find-file $srcStr))))
            (when (y-or-n-p (format "file no exist at 「%s」. Create new?" $srcStr))
              (find-file $srcStr)))))
    (newline)))

;; HHH___________________________________________________________________

(defun xah-html-skip-tag-forward ()
  "Move cursor to the closing tag."
  (interactive)
  (sgml-skip-tag-forward 1)
  )

(defun xah-html-skip-tag-backward ()
  "Move cursor to the beginning tag."
  (interactive)
  (sgml-skip-tag-backward 1)
  )

(defun xah-html-change-current-tag ()
  "change the tag name of current tag, and class name if there's one. WARNING:
this is a quick 1 min hackjob, works only when there's no nesting.
version 2016-12-18"
  (interactive)
  (let ($p1 $p2 $oldTagName $newTagName $newClassName)
    (search-backward "<" )
    (forward-char 1)
    (setq $p1 (point))
    (setq $oldTagName (xah-html--get-tag-name))
    (setq $newTagName (ido-completing-read "HTML tag:" xah-html-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "span"))
    (goto-char $p1)
    (delete-char (length $oldTagName))
    (insert $newTagName)
    (search-forward (concat "</" $oldTagName))
    (delete-char (- (length $oldTagName)))
    (insert $newTagName)
    (progn
      (goto-char $p1)
      (search-forward ">")
      (setq $p2  (point))
      (goto-char $p1)
      (when
          (re-search-forward "class[ \n]*=[ \n]*\"" $p2 "move")
  ;(string-match "class[ \n]*=[ \n]*\"" (buffer-substring-no-properties $p1 $p2))
        (let ($p3 $p4)
          (setq $p3 (point))
          (search-forward "\"")
          (setq $p4 (- (point) 1))
          (setq $newClassName (read-string "new class name:"))
          (if (string-equal $newClassName "")
              (progn ; todo need to clean this up. don't use bunch of user functions
                (delete-region $p3 $p4 )
                (kill-word -1)
                (delete-char -1))
            (progn (delete-region $p3 $p4 )
                   (goto-char $p3)
                   (insert $newClassName))))))))

;; (defun xah-html-split-tag ()
;;   "split a HTML element into 2 elements of the same tag, at cursor point.

;; for example:
;;  <p>some▮thing</p>
;; becomes
;;  <p>some</p>
;;  ▮
;;  <p>thing</p>
;; "
;;   (interactive)
;;   )

(defun xah-html-encode-ampersand-region (begin end)
  "Replace HTML chars & < > to HTML on current selection.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2018-10-08"
  (interactive "r")
  (xah-replace-pairs-region begin end '( ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] )))

(defun xah-html-decode-ampersand-region (begin end)
  "Replace HTML chars & < > to HTML on current selection.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2018-10-08"
  (interactive "r")
  (xah-replace-pairs-region begin end '( ["&lt;" "<"] ["&gt;" ">"] ["&amp;" "&"])))

(defun xah-html-escape-char-to-entity (@begin @end &optional @entity-to-char-p)
  "Replace HTML chars & < > to HTML entities on current text block or selection.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

Highlight changed places.
If `universal-argument' is called first, the replacement direction is reversed.

When called in lisp code, @begin @end are region begin/end positions. If @entity-to-char-p is true, reverse change direction.

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2020-08-30 2021-03-25"
  (interactive
   (save-excursion
     (list
      (if (region-active-p)
          (region-beginning)
        (progn
          (when (re-search-backward "\n[ \t]*\n" nil "move")
            (re-search-forward "\n[ \t]*\n" nil ))
          (point)))
      (if (use-region-p)
          (region-end)
        (progn
          (when (re-search-forward "\n[ \t]*\n" nil "move")
            (re-search-backward "\n[ \t]*\n" nil "move"))
          (point)))
      (if current-prefix-arg t nil))))
  (let (($changedItems '())
        ($findReplaceMap
         (if @entity-to-char-p
             ;; this to prevent creating a replacement sequence out of blue
             [
              ["&amp;" "螽⛫1"] ["&lt;" "螽⛫2"] ["&gt;" "螽⛫3"]
              ["螽⛫1" "&"] ["螽⛫2" "<"] ["螽⛫3" ">"]
              ]
           [ ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ]
           )))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (let ( (case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while (search-forward (elt $x 0) nil t)
               (push (format "%s %s" (point) $x) $changedItems)
               (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")
               (overlay-put (make-overlay (- (point) (length (elt $x 1))) (point)) 'font-lock-face '(:foreground "red"))))
           $findReplaceMap))))))

(defun xah-html-escape-char-to-unicode (@begin @end &optional @fullwidth-to-ascii-p)
  "Replace chars < > & to fullwidth version ＜ ＞ ＆ in current text block or selection.

Highlight changed places.
If `universal-argument' is called first, the replacement direction is reversed.

When called in lisp code, @begin @end are region begin/end positions. If @fullwidth-to-ascii-p is true, reverse change direction.

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2020-08-30 2021-03-25"
  (interactive
   (list
    (if (region-active-p)
        (region-beginning)
      (progn
        (when (re-search-backward "\n[ \t]*\n" nil "move")
          (re-search-forward "\n[ \t]*\n" nil ))
        (point)))
    (if (use-region-p)
        (region-end)
      (progn
        (when (re-search-forward "\n[ \t]*\n" nil "move")
          (re-search-backward "\n[ \t]*\n" nil "move"))
        (point)))
    (if current-prefix-arg t nil)))
  (let (($findReplaceMap
         (if @fullwidth-to-ascii-p
             [ ["＆" "&"] [ "＜" "<"] [ "＞" ">"] ]
           [ ["&" "＆"] ["<" "＜"] [">" "＞"] ]
           )))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (mapc
         (lambda ($x)
           (goto-char (point-min))
           (while (search-forward (elt $x 0) nil t)
             (replace-match (elt $x 1))
             (overlay-put (make-overlay (- (point) (length (elt $x 1))) (point)) 'font-lock-face '(:foreground "red"))))
         $findReplaceMap)))))

(defun xah-html-named-entity-to-char (@begin @end)
  "Replace HTML named entity to Unicode character in current text block or selection.
Changed places are highlighted.
For example, “&copy;” becomes “©”.
The following HTML Entities are not replaced: &amp; &lt; &gt;

When called in lisp code, @begin @end are region begin/end positions.

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2020-08-30"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (list
        (progn
          (search-backward "\n\n" nil "move" )
          (search-forward "\n\n" nil "move")
          (point))
        (progn
          (search-forward "\n\n" nil "move")
          (search-backward "\n\n" nil "move" )
          (point))))))
  (let (
        ($replaceMap
         [
          ["&nbsp;" " "] ["&ensp;" " "] ["&emsp;" " "] ["&thinsp;" " "]
          ["&rlm;" "‏"] ["&lrm;" "‎"] ["&zwj;" "‍"] ["&zwnj;" "‌"]

          ["&iexcl;" "¡"] ["&cent;" "¢"] ["&pound;" "£"] ["&curren;" "¤"] ["&yen;" "¥"]
          ["&brvbar;" "¦"] ["&sect;" "§"] ["&uml;" "¨"] ["&copy;" "©"] ["&ordf;" "ª"]
          ["&laquo;" "«"] ["&not;" "¬"] ["&shy;" "­"] ["&reg;" "®"] ["&macr;" "¯"]
          ["&deg;" "°"] ["&plusmn;" "±"] ["&sup2;" "²"] ["&sup3;" "³"] ["&acute;" "´"]
          ["&micro;" "µ"] ["&para;" "¶"] ["&middot;" "·"] ["&cedil;" "¸"] ["&sup1;" "¹"]
          ["&ordm;" "º"] ["&raquo;" "»"] ["&frac14;" "¼"] ["&frac12;" "½"]
          ["&frac34;" "¾"] ["&iquest;" "¿"]

          ["&Agrave;" "À"] ["&Aacute;" "Á"] ["&Acirc;" "Â"] ["&Atilde;" "Ã"] ["&Auml;" "Ä"]
          ["&Aring;" "Å"] ["&AElig;" "Æ"] ["&Ccedil;" "Ç"] ["&Egrave;" "È"] ["&Eacute;" "É"]
          ["&Ecirc;" "Ê"] ["&Euml;" "Ë"] ["&Igrave;" "Ì"] ["&Iacute;" "Í"] ["&Icirc;" "Î"]
          ["&Iuml;" "Ï"] ["&ETH;" "Ð"] ["&Ntilde;" "Ñ"] ["&Ograve;" "Ò"] ["&Oacute;" "Ó"]
          ["&Ocirc;" "Ô"] ["&Otilde;" "Õ"] ["&Ouml;" "Ö"] ["&times;" "×"] ["&Oslash;" "Ø"]
          ["&Ugrave;" "Ù"] ["&Uacute;" "Ú"] ["&Ucirc;" "Û"] ["&Uuml;" "Ü"] ["&Yacute;" "Ý"]
          ["&THORN;" "Þ"] ["&szlig;" "ß"] ["&agrave;" "à"] ["&aacute;" "á"] ["&acirc;" "â"]
          ["&atilde;" "ã"] ["&auml;" "ä"] ["&aring;" "å"] ["&aelig;" "æ"] ["&ccedil;" "ç"]
          ["&egrave;" "è"] ["&eacute;" "é"] ["&ecirc;" "ê"] ["&euml;" "ë"] ["&igrave;" "ì"]
          ["&iacute;" "í"] ["&icirc;" "î"] ["&iuml;" "ï"] ["&eth;" "ð"] ["&ntilde;" "ñ"]
          ["&ograve;" "ò"] ["&oacute;" "ó"] ["&ocirc;" "ô"] ["&otilde;" "õ"] ["&ouml;" "ö"]

          ["&divide;" "÷"] ["&oslash;" "ø"] ["&ugrave;" "ù"] ["&uacute;" "ú"] ["&ucirc;" "û"]
          ["&uuml;" "ü"] ["&yacute;" "ý"] ["&thorn;" "þ"] ["&yuml;" "ÿ"] ["&fnof;" "ƒ"]

          ["&Alpha;" "Α"] ["&Beta;" "Β"] ["&Gamma;" "Γ"] ["&Delta;" "Δ"] ["&Epsilon;" "Ε"]
          ["&Zeta;" "Ζ"] ["&Eta;" "Η"] ["&Theta;" "Θ"] ["&Iota;" "Ι"] ["&Kappa;" "Κ"]
          ["&Lambda;" "Λ"] ["&Mu;" "Μ"] ["&Nu;" "Ν"] ["&Xi;" "Ξ"] ["&Omicron;" "Ο"]
          ["&Pi;" "Π"] ["&Rho;" "Ρ"] ["&Sigma;" "Σ"] ["&Tau;" "Τ"] ["&Upsilon;" "Υ"]
          ["&Phi;" "Φ"] ["&Chi;" "Χ"] ["&Psi;" "Ψ"] ["&Omega;" "Ω"] ["&alpha;" "α"]
          ["&beta;" "β"] ["&gamma;" "γ"] ["&delta;" "δ"] ["&epsilon;" "ε"] ["&zeta;" "ζ"]
          ["&eta;" "η"] ["&theta;" "θ"] ["&iota;" "ι"] ["&kappa;" "κ"] ["&lambda;" "λ"]
          ["&mu;" "μ"] ["&nu;" "ν"] ["&xi;" "ξ"] ["&omicron;" "ο"] ["&pi;" "π"]
          ["&rho;" "ρ"] ["&sigmaf;" "ς"] ["&sigma;" "σ"] ["&tau;" "τ"] ["&upsilon;" "υ"]
          ["&phi;" "φ"] ["&chi;" "χ"] ["&psi;" "ψ"] ["&omega;" "ω"] ["&thetasym;" "ϑ"]
          ["&upsih;" "ϒ"] ["&piv;" "ϖ"]

          ["&bull;" "•"] ["&hellip;" "…"] ["&prime;" "′"] ["&Prime;" "″"] ["&oline;" "‾"]
          ["&frasl;" "⁄"] ["&weierp;" "℘"] ["&image;" "ℑ"] ["&real;" "ℜ"] ["&trade;" "™"]
          ["&alefsym;" "ℵ"] ["&larr;" "←"] ["&uarr;" "↑"] ["&rarr;" "→"] ["&darr;" "↓"]
          ["&harr;" "↔"] ["&crarr;" "↵"] ["&lArr;" "⇐"] ["&uArr;" "⇑"] ["&rArr;" "⇒"]
          ["&dArr;" "⇓"] ["&hArr;" "⇔"] ["&forall;" "∀"] ["&part;" "∂"] ["&exist;" "∃"]
          ["&empty;" "∅"] ["&nabla;" "∇"] ["&isin;" "∈"] ["&notin;" "∉"] ["&ni;" "∋"]
          ["&prod;" "∏"] ["&sum;" "∑"] ["&minus;" "−"] ["&lowast;" "∗"] ["&radic;" "√"]
          ["&prop;" "∝"] ["&infin;" "∞"] ["&ang;" "∠"] ["&and;" "∧"] ["&or;" "∨"]
          ["&cap;" "∩"] ["&cup;" "∪"] ["&int;" "∫"] ["&there4;" "∴"] ["&sim;" "∼"]
          ["&cong;" "≅"] ["&asymp;" "≈"] ["&ne;" "≠"] ["&equiv;" "≡"] ["&le;" "≤"]
          ["&ge;" "≥"] ["&sub;" "⊂"] ["&sup;" "⊃"] ["&nsub;" "⊄"] ["&sube;" "⊆"]
          ["&supe;" "⊇"] ["&oplus;" "⊕"] ["&otimes;" "⊗"] ["&perp;" "⊥"] ["&sdot;" "⋅"]
          ["&lceil;" "⌈"] ["&rceil;" "⌉"] ["&lfloor;" "⌊"] ["&rfloor;" "⌋"] ["&lang;" "〈"]
          ["&rang;" "〉"] ["&loz;" "◊"] ["&spades;" "♠"] ["&clubs;" "♣"] ["&hearts;" "♥"]
          ["&diams;" "♦"] ["&quot;" "\""] ["&OElig;" "Œ"] ["&oelig;" "œ"] ["&Scaron;" "Š"]
          ["&scaron;" "š"] ["&Yuml;" "Ÿ"] ["&circ;" "ˆ"] ["&tilde;" "˜"] ["&ndash;" "–"]
          ["&mdash;" "—"] ["&lsquo;" "‘"] ["&rsquo;" "’"] ["&sbquo;" "‚"] ["&ldquo;" "“"]
          ["&rdquo;" "”"] ["&bdquo;" "„"] ["&dagger;" "†"] ["&Dagger;" "‡"] ["&permil;" "‰"]
          ["&lsaquo;" "‹"] ["&rsaquo;" "›"] ["&euro;" "€"]
          ]))
    (save-restriction
      (narrow-to-region @begin @end)
      (let ( (case-fold-search nil))
        (mapc
         (lambda ($x)
           (goto-char (point-min))
           (while (search-forward (elt $x 0) nil t)
             (replace-match (elt $x 1))
             (overlay-put (make-overlay (- (point) (length (elt $x 1))) (point)) 'font-lock-face '(:foreground "red"))))
         $replaceMap)))))

(defun xah-html-get-html-file-title (fname &optional no-error-p)
  "Return fname <title> tag's text.
Assumes that the file contains the string “<title>…</title>”. If not, and if no-error-p is true, then return empty string.

Version 2018-06-06"
  (with-temp-buffer
    (insert-file-contents fname nil nil nil t)
    (goto-char 1)
    (if (search-forward "<title>" nil no-error-p)
        (buffer-substring-no-properties
         (point)
         (- (search-forward "</title>") 8))
      ""
      )))

(defun xah-html-lines-to-list ()
  "Make the current block of lines into a HTML list.

If `universal-argument' is called first, use ordered list <ol> instead of <ul>.

Example:
If your cursor is in the following block of text:

cat
dog

becomes:

<ul>
<li>cat</li>
<li>dog</li>
</ul>

Version 2019-03-15"
  (interactive)
  (let ($bds $p1 $p2 $input-str $resultStr )
    (setq $bds (xah-get-bounds-of-thing 'block))
    (setq $p1 (car $bds))
    (setq $p2 (cdr $bds))
    (setq $input-str (buffer-substring-no-properties $p1  $p2))
    (save-excursion
      (setq $resultStr
            (with-temp-buffer
              (insert $input-str)
              (goto-char (point-max))
              (insert "\n")
              (progn
                (goto-char (point-min))
                (while
                    (re-search-forward  "\.html$" nil t)
                  (backward-char 1)
                  (xah-html-any-linkify)))
              (goto-char (point-min))
              (while
                  (not (equal (line-end-position) (point-max)))
                (beginning-of-line)
                (when (looking-at "• ")
                  (delete-char 2))
                (when (looking-at "* ")
                  (delete-char 2))
                (when (looking-at "- ")
                  (delete-char 2))
                (when (looking-at "⓪①②③④⑤⑥⑦⑧⑨⑩")
                  (delete-char 1))
                (while (looking-at " ")
                  (delete-char 1))
                (insert "<li>")
                (end-of-line) (insert "</li>")
                (forward-line 1 ))
              (if current-prefix-arg
                  (progn
                    (goto-char (point-min))
                    (insert "<ol>\n")
                    (goto-char (point-max))
                    (insert "</ol>"))
                (progn
                  (goto-char (point-min))
                  (insert "<ul>\n")
                  (goto-char (point-max))
                  (insert "</ul>")))
              (buffer-string))))
    (delete-region $p1 $p2)
    (insert $resultStr)))

(defun xah-html-lines-to-dl ()
  "Make the current block of lines into a HTML dl list.
e.g.

cat . 4 legs
bird . has wings
becomes

<dl>
<dt>cat</dt><dd>4 legs</dd>
<dt>bird</dt><dd>has wings</dd>
</dl>

First occurence of “. ” in each line is used to separate dt and dd. If none found, it's an error. Note, must have space after the period.

If `universal-argument' is called first, ask user to enter a separater marker for dt and dd.
For example, if the input is

cat → 4 legs
bird → has wings

Version 2018-10-11 2020-12-24 2021-01-12"
  (interactive)
  (let ($bds $p1 $p2 $input-str $resultStr $endpos)
    (setq $bds (xah-get-bounds-of-thing 'block))
    (setq $p1 (car $bds))
    (setq $p2 (cdr $bds))
    (setq $input-str (buffer-substring-no-properties $p1  $p2))
    (if current-prefix-arg
        (progn
          (setq $sep (read-string "separator char between dt dd:" )))
      (setq $sep "\\. +" ))
    (save-excursion
      (setq $resultStr
            (with-temp-buffer
              (insert $input-str)
              (goto-char (point-max))
              (insert "\n")
              (goto-char (point-min))
              (while (not (equal (point) (point-max)))
                (beginning-of-line) (insert "<dt>")
                (setq $endpos (line-end-position))
                (if (re-search-forward $sep $endpos )
                    (progn
                     (delete-region (match-beginning 0) (match-end 0))
                     (insert "</dt><dd>")
                     (end-of-line)
                     (insert "</dd>")
                     (forward-line 1 ))
                  (user-error "cannot find period in line. Try call it with universal-argument.")))
              (goto-char (point-min))
              (insert "<dl>\n")
              (goto-char (point-max))
              (insert "</dl>")
              (buffer-string))))
    (delete-region $p1 $p2)
    (insert $resultStr)))

(defun xah-html-dl-to-table ()
  "Change html dl to table.
Cursor must be inside dl tags.
Currently, assume there are only 2 columns.
Version 2019-05-26"
  (interactive )
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<dl>" )
        (setq $p1 (point))
        (search-forward "</dl>")
        (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)

      (goto-char (point-min))
      (re-search-forward "<dl>")
      (replace-match "<table>" t t )

      (goto-char (point-min))
      (search-forward "</dl>")
      (replace-match "</table>" t t )

      (goto-char (point-min))
      (while (search-forward "<dt>" nil t)
        (replace-match "<tr><td>" t t ))

      (goto-char (point-min))
      (while (search-forward "</dt>" nil t)
        (replace-match "</td>" t t ))

      (goto-char (point-min))
      (while (search-forward "<dd>" nil t)
        (replace-match "<td>" t t ))
      (goto-char (point-max))

      (goto-char (point-min))
      (while (search-forward "</dd>" nil t)
        (replace-match "</td></tr>" t t ))
      (goto-char (point-max))

      ;;
      )))

(defun xah-html-table-to-dl ()
  "Change html table to dl.
Cursor must be inside table tags.
 <caption> is removed.
 <th> are also removed.
Currently, assume there are only 2 columns.

Version 2019-09-24"
  (interactive )
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<table" )
        (setq $p1 (point))
        (search-forward "</table>")
        (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)

      (goto-char (point-min))

      (re-search-forward "<table *\\([^>]+?\\)>")
      (replace-match "<dl>" t t )

      (goto-char (point-min))
      (search-forward "</table>")
      (replace-match "</dl>" t t )

      (goto-char (point-min))
      (when
          (search-forward "<caption>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (when
          (search-forward "<th>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (while (re-search-forward "<tr>\n* *<td>" nil t)
        (replace-match "<dt>" t t ))

      (goto-char (point-min))
      (while (re-search-forward "</td>\n* *<td>" nil t)
        (replace-match "</dt>\n<dd>" t t ))

      (goto-char (point-min))
      (while (re-search-forward "</td>\n* *</tr>" nil t)
        (replace-match "</dd>" t t ))

      ;; (goto-char (point-min))
      ;; (while (re-search-forward "</dt><dd>" nil t)
      ;;   (replace-match "</dt>\n<dd>" t t ))

      (goto-char (point-max))

      ;;
      )))

(defun xah-html-table-to-ul ()
  "Change html table to ul
Cursor must be inside table tags.
 <caption> is removed.
 <th> are also removed.
Currently, assume there are only 2 columns.
“ → ” is used to separate columns.

If `universal-argument' is called first, prompt for separator string.

Version 2019-12-10"
  (interactive )
  (let (
        $p1 $p2
        ($sep (if current-prefix-arg
                  (read-string "Seperator:" "→")
                " → "
                )))
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<table" )
        (setq $p1 (point))
        (search-forward "</table>")
        (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)

      (goto-char (point-min))
      (when
          (search-forward "<caption>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (when
          (search-forward "<th>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (re-search-forward "<table *\\([^>]+?\\)>")
      (replace-match "<ul>" t t )

      (goto-char (point-min))
      (search-forward "</table>")
      (replace-match "</ul>" t t )

      (goto-char (point-min))
      (while (search-forward "<tr><td>" nil t)
        (replace-match "<li>" t t ))

      (goto-char (point-min))
      (while (search-forward "</td><td>" nil t)
        (replace-match $sep t t ))

      (goto-char (point-min))
      (while (search-forward "</td></tr>" nil t)
        (replace-match "</li>" t t ))

      (goto-char (point-max))
      ;;
      )))

(defun xah-html-ul-to-dl (@begin @end @sep @keep-sep-p)
  "Change html unordered list to definition list.
Cursor must be inside <ul></ul> tags.

Prompt for separator string and whether to keep.
else, add empty <dt></dt> in the beginning. @keep-sep-p if true, keep it in result.

Version 2020-09-05 2020-12-24"
  (interactive
   (list
    (if (use-region-p) (region-beginning))
    (if (use-region-p) (region-end))
    (read-string "Seperator:" )
    (yes-or-no-p "Keep Seperator:")))

  (let (($p1 (if @begin @begin (save-excursion (search-forward ">" ) (search-backward "<ul>" ) (point))))
        ($p2 (if @end @end (progn (search-backward "<") (search-forward "</ul>") (point)))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min)) (search-forward "<ul>") (replace-match "<dl>" t t )
      (goto-char (point-min)) (search-forward "</ul>") (replace-match "</dl>" t t )
      (goto-char (point-min)) (while (search-forward "</li>" nil "move") (replace-match "</dd>" t t ))
      (if (or (string-equal @sep "") (eq @sep nil))
          (progn
            (goto-char (point-min)) (while (search-forward "<li>" nil "move") (replace-match "<dt></dt><dd>" t t )))
        (progn
          (goto-char (point-min)) (while (search-forward "<li>" nil "move") (replace-match "<dt>" t t ))
          (goto-char (point-min))
          (while (search-forward @sep nil t)
            (replace-match (if @keep-sep-p (concat @sep "</dt><dd>\n") "</dt><dd>\n" )  t t )
            (search-forward "</dd>" nil "move" )))))))

(defun xah-html-dl-to-ul ()
  "Change html dl to ul.
Cursor must be inside dl tags.

If `universal-argument' is called first, prompt for separator string.

Version 2020-03-09"
  (interactive )
  (let (
        $p1 $p2
        ($sep (if current-prefix-arg
                  (read-string "Seperator:" " → ")
                " → "
                )))
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<dl>" )
        (setq $p1 (point))
        (search-forward "</dl>")
        (setq $p2 (point))))

    (save-restriction
      (narrow-to-region $p1 $p2)

      (goto-char (point-min))
      (search-forward "<dl>")
      (replace-match "<ul>" t t )

      (goto-char (point-min))
      (search-forward "</dl>")
      (replace-match "</ul>" t t )

      (goto-char (point-min))
      (while (search-forward "<dt>" nil t)
        (replace-match "<li>" t t ))

      (goto-char (point-min))
      (while (search-forward "</dd>" nil t)
        (replace-match "</li>" t t ))

      (goto-char (point-min))
      (while (re-search-forward "</dt> *\n*<dd>" nil t)
        (replace-match $sep t t ))
      (goto-char (point-max))
      ;;
      )))

(defun xah-html-lines-to-table ()
  "Transform the current text block or selection into a HTML table.

If there's a text selection, use the selection as input.
Otherwise, used current text block delimited by empty lines.

@SEPARATOR is a string used as a delimitor for columns.

For example:

a.b.c
1.2.3

with “.” as separator, becomes

<table class=\"nrm\">
<tr><td>a</td><td>b</td><td>c</td></tr>
<tr><td>1</td><td>2</td><td>3</td></tr>
</table>

URL `http://ergoemacs.org/emacs/elisp_make-html-table.html'
Version 2019-06-07"
  (interactive)
  (let ($bds
        $p1 $p2
        ($sep (read-string "String for column separation:" ","))
        ($i 0)
        ($j 0))
    (setq $bds (xah-get-bounds-of-thing-or-region 'block))
    (setq $p1 (car $bds))
    (setq $p2 (cdr $bds))

    (when (equal (length $sep) 0) (user-error "separator cannot be empty."))

    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ((case-fold-search nil))

          (goto-char (point-max))
          (insert "\n")

          (goto-char (point-min))
          (while (and
                  (search-forward $sep nil "move")
                  (< $i 2000))
            (replace-match "</td><td>")
            (1+ $i))

          (goto-char (point-min))
          (while (and
                  (search-forward "\n" nil "move")
                  (< $j 2000))
            (replace-match "</td></tr>
<tr><td>")
            (1+ $j))

          (goto-char (point-max))
          (beginning-of-line)
          (delete-char 8)

          (goto-char (point-min))
          (insert "<table class=\"nrm\">
<tr><td>")

          (goto-char (point-max))
          (insert "</table>")
          ;;
          )))))

(defun xah-html-table-to-lines ()
  "inverse of `xah-html-lines-to-table'.
Version 2016-12-18 2021-01-07"
  (interactive)
  (let ( $p1 $p2)
    (search-backward "<table")
    (setq $p1 (point))
    (search-forward "</table>")
    (setq $p2 (point))
    (xah-replace-regexp-pairs-region
     $p1 $p2
     [
      ["<table \\([^>]+?\\)>" ""]
      ["</table>" ""]
      ["<th>" "🖸"]
      ["</th>" "🖸"]
      ["<td>" "🖸"]
      ["</td>" "🖸"]
      ["<tr>" ""]
      ["</tr>" ""]
      ]
     "FIXEDCASE" "LITERAL"
     )
    (xah-replace-regexp-pairs-region
     $p1 $p2
     [
      ["^🖸" ""]
      ["🖸$" ""]
      ["🖸🖸" "|"]
      ]
     "FIXEDCASE" "LITERAL"
     )))

(defun xah-html-word-to-wikipedia-linkify ()
  "Make the current word or text selection into a Wikipedia link.
For Example:
 Emacs
becomes
 <a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>

URL `http://ergoemacs.org/emacs/elisp_html_word_to_wikipedia_linkify.html'
Version 2015-07-27"
  (interactive)
  (let ($p0 $p1 $p2 $linkText)
    (if (region-active-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (setq $p0 (point))
        (skip-chars-backward "^ \t\n")
        (setq $p1 (point))
        (goto-char $p0)
        (skip-chars-forward "^ \t\n")
        (setq $p2 (point))))
    (setq $linkText
          (replace-regexp-in-string "_" " " (buffer-substring-no-properties $p1 $p2)))
    (delete-region $p1 $p2)
    (insert (concat "<a href=\"http://en.wikipedia.org/wiki/"
                    (replace-regexp-in-string " " "_" $linkText)
                    "\">" $linkText "</a>"))))

(defun xah-html-remove-paragraph-tags ()
  "Remove paragraph <p></p> tags.
In text selection or current text block.
Version 2020-08-17 2020-09-08"
  (interactive)
  (let ($p1 $p2 )
    (save-excursion
      (if (region-active-p)
          (progn
            (setq $p1 (region-beginning))
            (setq $p2 (region-end)))
        (progn
          (when (re-search-backward "\n[ \t]*\n" nil "move")
            (re-search-forward "\n[ \t]*\n"))
          (setq $p1 (point))
          (re-search-forward "\n[ \t]*\n" nil "move")
          (re-search-backward "\n[ \t]*\n" )
          (setq $p2 (point)))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (search-forward "<p>" nil t) (replace-match "" ))
      (goto-char (point-min))
      (while (search-forward "</p>" nil t) (replace-match "" )))))

(defun xah-html-remove-list-tags ()
  "Remove HTML ul ol li list tags.
In text selection or current text block.
Version 2020-07-15"
  (interactive)
  (let ($p1 $p2)
    (save-excursion
      (if (region-active-p)
          (progn
            (setq $p1 (region-beginning))
            (setq $p2 (region-end)))
        (progn
          (skip-chars-forward " \n\t")
          (when (re-search-backward "\n[ \t]*\n" nil "move")
            (re-search-forward "\n[ \t]*\n"))
          (setq $p1 (point))
          (re-search-forward "\n[ \t]*\n" nil "move")
          (re-search-backward "\n[ \t]*\n" )
          (setq $p2 (point)))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (search-forward "<ul>" nil t) (replace-match "" ))
      (goto-char (point-min))
      (while (search-forward "</ul>" nil t) (replace-match "" ))
      (goto-char (point-min))
      (while (search-forward "<ol>" nil t) (replace-match "" ))
      (goto-char (point-min))
      (while (search-forward "</ol>" nil t) (replace-match "" ))
      (goto-char (point-min))
      (while (search-forward "<li>" nil t) (replace-match "" ))
      (goto-char (point-min))
      (while (search-forward "</li>" nil t) (replace-match "" )))))

(defun xah-html-remove-span-tag-region (@begin @end)
  "Delete HTML “span” tags in region.
Only span tags of the form <span class=\"…\"> and </span> are deleted.

When done, the cursor is placed at @end.
Version 2018-10-08"
  (interactive "r")
  (save-restriction
    (narrow-to-region @begin @end)
    (xah-replace-regexp-pairs-region (point-min) (point-max) '(["<span class=\"[^\"]+\">" ""]))
    (xah-replace-pairs-region (point-min) (point-max) '( ["</span>" ""] ))
    (goto-char (point-max))))

(defun xah-html-code-tag-to-brackets (@begin @end &optional @change-entity-p)
  "Change HTML code tags to brackets in text selection or current text block.

 <code>…</code> and <code class=\"…\">…</code>
are changed to 「…」

<var class=\"…\">…</var>
 is changed to
 ‹…›

The HTML entities &amp; &lt; &gt; are changed to & < >.

if `universal-argument' is called first, don't convert the HTML entities.

When done, the cursor is placed at @end.

when called in lisp program,
@begin @end are region begin/end.
If @change-entity-p is true, convert HTML entities to char.
Version 2018-10-08"
  (interactive
   (let (($bds (xah-get-bounds-of-thing 'block)))
     (list (car $bds) (cdr $bds) (if current-prefix-arg nil t))))
  (save-restriction
    (narrow-to-region @begin @end)
    (xah-replace-regexp-pairs-region (point-min) (point-max) '(["<code class=\"[^\"]+\">" "「"] ["<var class=\"[^\"]+\">" "‹"]))
    (xah-replace-pairs-region
     (point-min) (point-max)
     '(
       ["<code>" "「"]
       ["</code>" "」"]
       ["<var>" "‹"]
       ["</var>" "›"] ))
    (when @change-entity-p
      (xah-html-decode-ampersand-region (point-min) (point-max)))
    (goto-char (point-max))))

;; '(
;; ["<!doctype html>" ""]
;; ["<meta charset=\"utf-8\" />" ""]
;; [" class=\"[-_a-z0-9]+\" *"  " "]
;; [" id=\"[-_a-z0-9]+\" *"  " "]
;; [" title=\"\\([^\"]+?\\)\" *"  " "]
;; [" data-accessed=\"[-0-9]+\" *"  " "]
;; [" width=\"[0-9]+%?\" *"  " "]
;; [" height=\"[0-9]+%?\" *"  " "]

;; ["<link rel=\"stylesheet\" href=\"\\([^\"]+?\\)\" />" ""]
;; ["<a +href=\"\\([^\"]+?\\)\" *>\\([^<]+?\\)</a>" "\\2"]
;; ["<img +src=\"\\([^\"]+?\\)\" +alt=\"\\([^\"]+?\\)\" */?>" ""]

;; ["<[a-z0-9]+ */?>" ""]
;; ["</[a-z0-9]+>" ""]
;; ["&amp;" "&"]
;; ["&lt;" "<"]
;; ["&gt;" ">"]
;; )

(defun xah-html-remove-html-tags (&optional @begin @end)
  "Delete HTML tags in in current text block or text selection.
A “text block” is text between blank lines

Use `xah-html-html-to-text' if you want the html link URL to remain.

 WARNING: this command does not cover all HTML tags or convert all HTML entities. For robust solution you might use the terminal command “lynx” or other.
Version 2018-11-27 2021-01-12"
  (interactive)
  (let ($p1 $p2 $input-str $output-str)
    (if @begin
        (progn
          (setq $p1 @begin)
          (setq $p2 @end))
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (let (($bds (xah-get-bounds-of-thing 'block)))
          (setq $p1 (car $bds) $p2 (cdr $bds)))))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (setq $output-str
          (let ((case-fold-search t) ($tempStr $input-str))
            (setq $tempStr
                  (xah-replace-regexp-pairs-in-string
                   $tempStr
                   '(
                     ["<kbd>" " "]
                     ["</kbd>" " "]
                     ["<script>\\([^\\<]+?\\)</script>" ""]
                     ["<[^>]+?>" ""]
                     ["</[a-z0-9]+>" ""]
                     ["&amp;" "&"]
                     ["&lt;" "<"]
                     ["&gt;" ">"]
                     )))
            $tempStr
            ))
    (delete-region $p1 $p2)
    (insert $output-str))
  (skip-chars-forward "\n" ))

(defun xah-html-link-to-text (@begin @end)
  "Convert html link <a …>…</a> to plain text form.
If the href value and link text is the same, then result is:
 [ ‹URL› ]
else
 〈link text〉 [ ‹URL› ]
Version 2020-12-02"
  (interactive
   (let ($p1 $p2)
     (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n+" nil "move")
             (progn (re-search-forward "\n[ \t]*\n+")
                    (setq $p1 (point)))
           (setq $p1 (point)))
         (re-search-forward "\n[ \t]*\n" nil "move")
         (setq $p2 (point))))
     (list $p1 $p2)))
  (let ( ($p1 @begin ) ($p2 @end))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (re-search-forward "<a .*href=\"\\([^\"]+?\\)\".*>\\([^<]+?\\)</a>" nil t)
        (let* (
               ($hrefVal (match-string 1))
               ($linkText (match-string 2))
               ($matchData (match-data 0))
               ($tagBegin (nth 0 $matchData))
               ($tagEnd (nth 1 $matchData))
               ($url (if (xah-html-local-link-p $hrefVal)
                         (if (fboundp 'xahsite-web-path-to-filepath)
                             (let ((xahFPath (xahsite-web-path-to-filepath $hrefVal)))
                               (if (xahsite-file-path-is-xahsite-p xahFPath)
                                   (xahsite-filepath-to-url xahFPath )
                                 xahFPath
                                 ))
                           $hrefVal
                           )
                       $hrefVal
                       )))
          (delete-region $tagBegin $tagEnd)
          (if (string-equal $url $linkText)
              (insert (format " [ %s ] " $url))
            (insert (format " 〈%s〉 [ %s ] " $linkText $url ))))))))

(defun xah-html-html-to-text ()
  "Convert HTML to plain text on current text block or text selection.
Version 2019-04-12 2021-04-10"
  (interactive)
  (let ( $p1 $p2 $input-str $output-str)
    (let ($bds)
      (setq $bds (xah-get-bounds-of-thing-or-region 'block))
      (setq $p1 (car $bds) $p2 (cdr $bds)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (setq
     $output-str
     (with-temp-buffer
       (insert $input-str)

       (xah-html-link-to-text (point-min) (point-max))

       (goto-char (point-min))
       (while (re-search-forward "src=\"\\([^\"]+?\\)\""  nil t)
         (let (($matchString (match-string 1)))
           (search-forward ">" )
           (search-forward ">" )
           (insert (format " [ %s ] " $matchString))))

       (goto-char (point-min))
       (let ((case-fold-search nil))
         (xah-replace-regexp-pairs-region
          (point-min)
          (point-max)
          [
           [" class=\"\\([A-Za-z0-9]+\\)\" " " "]
           ["<var class=\"d\">\\([^<]+?\\)</var>" "‹\\1›"]
           ["<script>\\([^\\<]+?\\)</script>" ""]

           ;; ["<a +href=\"\\([^\"]+?\\)\" *>\\([^<]+?\\)</a>" "\\2"]
           ;; ["<a +href=\"\\([^\"]+?\\)\" *>\\([^<]+?\\)</a>" "\\2 [ \\1 ]"]
           ;; ["href=\"\\([^\"]+?\\)\"" " \\1 "]
           ;; [">\\([^<]+?\\)</a>" ">\\1\n"]

           ["<img +src=\"\\([^\"]+?\\)\" +alt=\"\\([^\"]+?\\)\" +width=\"[0-9]+\" +height=\"[0-9]+\" */?>" "[IMAGE “\\2” \\1 ]"]
           ]
          "FIXEDCASE" )
         (xah-replace-pairs-region
          (point-min)
          (point-max)
          '(

            ;; todo. something wrong here. not supposed to be regex
            ["<li>" "<li>• " ]
            ["</li>" "" ]

            ["<code>" "「" ]
            ["<code class=\"elisp_f_3d841\">" "「" ]
            ["<code class=\"path_xl\">" "「" ]
            ["</code>" "」" ]

            ["<cite>" "〈" ]
            ["<cite class=\"book\">" "〈" ]
            ["</cite>" "〉" ]

            ["<kbd>" "" ]
            ["</kbd>" "" ]

            ["<h2>" "────────── ────────── ────────── ────────── ──────────\n" ]
            ["</h2>" "" ]
            ["<h3>" "────────── ────────── ──────────\n" ]
            ["</h3>" "" ]
            ["<h4>" "────────── ──────────\n" ]
            ["</h4>" "" ]
            ["<h5>" "──────────\n" ]
            ["</h5>" "" ]
            ["<h6>" "──────────\n" ]
            ["</h6>" "" ]
            ["</td><td>" " | " ]
            ["</th><th>" " | " ]

            )))
       (xah-html-remove-html-tags (point-min) (point-max))
       (buffer-substring (point-min) (point-max))))
    (delete-region $p1 $p2 )
    (insert $output-str)))

(defun xah-html-youtube-to-text ()
  "Remove embedded YouTube html block to url and caption.
Works on text block or region.

Example, this:

<figure>
<iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/Vn7U7S0-5CQ\" allowfullscreen></iframe>
<figcaption>
xah talk show 2019-08-19 Deseret/Shavian Alphabets, IPA, font size, fugue, elisp coding youtube html
</figcaption>
</figure>

becomes:

https://www.youtube.com/watch?v=Vn7U7S0-5CQ
xah talk show 2019-08-19 Deseret/Shavian Alphabets, IPA, font size, fugue, elisp coding youtube html

Version 2020-08-27"
  (interactive)
  (let ( bds p1 p2 p3 p4 figCapText $url $id $timeStamp)
    (setq bds (xah-get-bounds-of-thing-or-region 'block))
    (setq p1 (car bds) p2 (cdr bds))
    (save-restriction
      (narrow-to-region p1 p2)
      (goto-char (point-min))
      (re-search-forward "src=\"\\([^\"]+\\)\"" )
      (setq $url (match-string 1 ))
      (setq $id (xah-html-get-youtube-id $url))
      (when (not $id)
        (error "Cannot find the youtube video id in url $url" ))
      (goto-char (point-min))
      ;; https://www.youtube.com/embed/gjiAjtGzC64?start=2358
      (setq $timeStamp
            (if (string-match "\\?start=\\([0-9]+\\)" $url )
                (match-string 1 $url)
              nil))
      (let ((case-fold-search t))
        (goto-char (point-min))
        (search-forward "<figcaption>" )
        (setq p3 (point))
        (goto-char (point-min))
        (search-forward "</figcaption>" )
        (search-backward "</figcaption>")
        (setq p4 (point))
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region p3 p4)
            (set-mark (point-min))
            (goto-char (point-max))
            (xah-html-html-to-text)
            (setq figCapText (buffer-substring-no-properties (point-min) (point-max)))))
        (delete-region (point-min) (point-max))
        (progn
          ;; https://youtu.be/gjiAjtGzC64?t=2358
          (insert "https://youtu.be/" $id)
          (when $timeStamp
            (insert (format "?t=%s" $timeStamp)))
          (end-of-line )
          (insert "\n" figCapText "\n\n"))))))

(defun xah-call-ImageMagick ( @argsStr @pathOld @pathNew  )
  "Wrapper to ImageMagick' “convert” shell command.
@argsStr is argument string passed to ImageMagick's “convert” command.
@pathOld is image file full path.
@pathNew is new image file full path.
Version 2021-01-11"
  (let ( $cmdStr )
    ;; relative paths used to get around Windows/Cygwin path remapping problem
    (setq $cmdStr
          (format
           "%s %s \"%s\" \"%s\""
           (if (string-equal system-type "windows-nt") "magick.exe convert" "convert" )
           @argsStr
           (if (file-name-absolute-p @pathOld ) (file-relative-name @pathOld) @pathOld )
           (if (file-name-absolute-p @pathNew ) (file-relative-name @pathNew) @pathNew )))
    (shell-command $cmdStr)
    (message "Called:「%s」" $cmdStr)))

(defun xah-html-resize-img ()
  "Create a new resized image of image path under cursor.
In a html file, put cursor on a image file path, call the command,
a thumbnail will be created in the same dir, and path of the newly created file will be inserted before the img tag.
If `universal-argument' is called first, ask for jpeg quality (default is 90) and whether to sharpen. (default is sharpen 1.)
Version 2020-11-13 2021-03-27"
  (interactive)
  (let* (
         ($bounds (bounds-of-thing-at-point 'filename))
         ($p1 (car $bounds))
         ($p2 (cdr $bounds))
         ($inputPath (buffer-substring-no-properties $p1 $p2))
         ($fPath1 (expand-file-name $inputPath ))
         ($dir (file-name-directory $fPath1))
         ($fname (file-name-nondirectory $fPath1))
         ($coreName (file-name-sans-extension $fname))
         ($ext (file-name-extension $fname))
         ($sideLength (string-to-number (read-from-minibuffer "~width:" "250" )))
         ($thumbnailSizeArea (* $sideLength $sideLength))
         ($size (xah-get-image-dimensions $fPath1))
         ($w (aref $size 0))
         ($h (aref $size 1))
         ($fnameNew (format "%s-s%d.%s" $coreName $sideLength $ext ))
         ($fPathNew (concat $dir $fnameNew))
         ($isLossy-p (or (string-equal (downcase $ext)  "jpg")
                         (string-equal (downcase $ext)  "jpeg")))
         $args $doIt
         )
    (when (not (file-exists-p $inputPath))
      (user-error "File not exist: %s" $inputPath))
    (setq $args
          (format " -scale %s%% %s %s "
                  (round (* (sqrt (/ (float $thumbnailSizeArea) (float (* $w $h)))) 100))
                  (if $isLossy-p
                      (if current-prefix-arg
                          (format "-quality %s%%" (read-string "quality:" "90"))
                        "-quality 90%")
                    " "
                    )
                  (if current-prefix-arg
                      (if (y-or-n-p "sharpen?")
                          " -sharpen 1 "
                        ""
                        )
                    " -sharpen 1 ")))
    (if (file-exists-p $fPathNew)
        (setq $doIt (y-or-n-p (format "File exist 「%s」. Replace it?" $fPathNew)))
      (setq $doIt t ))
    (when $doIt (xah-call-ImageMagick $args $fPath1 $fPathNew ))
    ;; (message "%s" $args)
    (search-backward "<" )
    (insert $fPathNew "\n")
    (backward-word )))

(defun xah-html-convert-to-jpg ()
  "Convert the image file under cursor in a html file, from jpg, then, linkify it in html. Do not delete the original
If `universal-argument' is called first, ask to delete png.
Version 2019-12-17 2021-01-11"
  (interactive)
  (let* ( ( $inputPath (thing-at-point 'filename))
          ($newName (format "%s.jpg" (file-name-sans-extension $inputPath))))
    (xah-call-ImageMagick " " $inputPath $newName)
    (search-backward "<" )
    (insert $newName )
    (insert "\n")
    (backward-char 2)
    (xah-html-image-linkify)
    ;;
    ))

(defun xah-html-rename-source-file-path ()
  "Rename HTML source file path.
Place cursor anywhere inside a HTML source local file path, e.g.
<img src=\"img/▮cats.jpg\" >
or
<a href=\"js_▮canvas_tutorial.html\">
Call this command, it prompt for a new path/name with one line for dir path and line for file name. The separate lines are for ease of editing.
When done editing, newline characters in path are removed, and comma or space replaced by _. The file name is renamed, and link also updated.
This command is for interactive use only.
Version 2019-10-05 2021-02-10"
  (interactive)
  (let* (
         ($p0 (point))
         ($bounds (bounds-of-thing-at-point 'filename))
         ($p1 (car $bounds))
         ($p2 (cdr $bounds))
         ($input (buffer-substring-no-properties $p1 $p2))
         ($currentDir (file-name-directory (or (buffer-file-name) default-directory )))
         ($oldPath (expand-file-name $input $currentDir))
         ($promptPath (concat (file-name-directory $oldPath) "\n" (file-name-nondirectory $oldPath)))
         ($userInputPath (read-string "New name: " $promptPath nil $promptPath ))
         ($doit-p nil))
    (setq $newPath
          (replace-regexp-in-string
           " \\|\n\\|," "_"
           (replace-regexp-in-string "\n" "" $userInputPath)))
    (setq $doit-p
          (if (file-exists-p $newPath)
              (y-or-n-p "File exist. Replace?")
            t))
    (message "old path: %s\n new path: %s" $oldPath $newPath)
    (when $doit-p
      (progn
        (rename-file $oldPath $newPath t)
        (goto-char $p0)
        (delete-region $p1 $p2)
        (insert
         (file-relative-name $newPath))))))

(defun xah-html-extract-url (@begin @end &optional @not-full-path-p)
  "Extract URLs in current block or region to `kill-ring'.
When called interactively, copy result to `kill-ring'. Each URL in a line.

If the URL is a local file relative path, convert it to full path.

If `universal-argument' is called first, don't convert relative URL to full path.

This command extracts all text of the forms
 <‹letter› … href=‹path› …>
 <‹letter› … src=‹path› …>
The quote for ‹path› may be double or single quote.

When called in lisp code, @begin @end are region begin/end positions.
Returns a list of strings.

URL `http://ergoemacs.org/emacs/elisp_extract_url_command.html'
Version 2021-02-20 2021-03-22"
  (interactive
   (let ($p1 $p2)
     ;; set region boundary $p1 $p2
     (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "move")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq $p1 (point)))
           (setq $p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "move")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq $p2 (point)))
           (setq $p2 (point)))))
     (list $p1 $p2 (not current-prefix-arg))))
  (let (($regionText (buffer-substring-no-properties @begin @end))
        ($urlList (list)))
    (with-temp-buffer
      (insert $regionText)
      (goto-char (point-min))
      (while (re-search-forward "<" nil t)
        (replace-match "\n<" "FIXEDCASE" "LITERAL"))
      (goto-char (point-min))
      (while (re-search-forward
              "<[A-Za-z]+.+?\\(href\\|src\\)[[:blank:]]*?=[[:blank:]]*?\\([\"']\\)\\([^\"']+?\\)\\2" nil t)
        (push (match-string 3) $urlList)))
    (setq $urlList (reverse $urlList))
    (when @not-full-path-p
      (setq $urlList
            (mapcar
             (lambda ($x)
               (if (string-match "^http:\\|^https:" $x )
                   $x
                 (expand-file-name
                  $x
                  (file-name-directory
                   (if (buffer-file-name)
                       (buffer-file-name)
                     default-directory
                     )))))
             $urlList)))
    (when (called-interactively-p 'any)
      (let (($printedResult (concat (mapconcat 'identity $urlList "\n") "\n" )))
        (kill-new $printedResult)
        (message "%s" $printedResult)))
    $urlList ))

(defun xah-html-local-links-to-fullpath ()
  "Change all local links to fullpaths, in region or current text block.
Version 2021-04-09"
  (interactive)
  (require 'xah-get-thing)
  (let ( (bounds (xah-get-bounds-of-thing-or-region 'block)))
    (save-restriction
      (narrow-to-region (car bounds) (cdr bounds))
      (goto-char (point-min))
      (while (search-forward "<a href=\"" nil t)
        (let* ((lBounds (bounds-of-thing-at-point 'filename))
               (hrefVal (buffer-substring-no-properties (car lBounds) (cdr lBounds))))
          (when (not (string-match "^http" hrefVal ))
            (delete-region (car lBounds) (cdr lBounds))
            (insert (concat "file:///" (expand-file-name hrefVal)))))))))

(defun xah-html-local-links-to-relative-path ()
  "Change all local links to relative paths, in region or current text block.
Version 2021-04-09"
  (interactive)
  (require 'xah-get-thing)
  (require 'subr-x)
  (let ( (bounds (xah-get-bounds-of-thing-or-region 'block)))
    (save-restriction
      (narrow-to-region (car bounds) (cdr bounds))
      (goto-char (point-min))
      (while (search-forward "<a href=\"" nil t)
        (let* ((lBounds (bounds-of-thing-at-point 'filename))
               (hrefVal (buffer-substring-no-properties (car lBounds) (cdr lBounds))))
          (when (string-match "file:///" hrefVal )
            (delete-region (car lBounds) (cdr lBounds))
            (insert (file-relative-name (string-remove-prefix "file:///" hrefVal)))))))))

(defun xah-html-update-title (@title)
  "Update the <title>…</title> and first <h1>…</h1> of current buffer.
When called in elisp code, @title is new title, a string.
URL `http://ergoemacs.org/emacs/elisp_update-html-title.html'
Version 2019-01-11 2021-02-09"
  (interactive
   (let ($oldTitle)
     (save-excursion
       (goto-char (point-min))
       (re-search-forward "<title>\\([^<]+?\\)</title>")
       (setq $oldTitle (match-string 1 )))
     (list (read-string "New title:" $oldTitle nil $oldTitle "INHERIT-INPUT-METHOD"))))
  (let ($p1 $p2)
    (save-excursion
      (goto-char (point-min))
      (progn (search-forward "<title>")
             (setq $p1 (point))
             (search-forward "</title>")
             (setq $p2 (- (point) 8))
             (delete-region $p1 $p2 )
             (goto-char $p1)
             (insert @title ))
      (xah-html-update-first-h1 @title))))

(defun xah-html-insert-date-section ()
  "Insert a section tag with date tag inside.
Like this:
<section>
<div class=\"date_xl\"><time>2021-01-11</time></div>
</section>
Version 2020-11-16"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (point))
             (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (insert "\n\n<section>\n\n")
      (insert (format "<div class=\"date_xl\"><time>%s</time></div>\n\n" (format-time-string "%Y-%m-%d")))
      (goto-char (point-max))
      (insert "\n\n</section>\n\n")
      (search-backward "\n\n</section>" ))))

(defun xah-html-insert-date-tag ()
  "Insert a date tag.
Version 2020-11-16"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (point))
             (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (insert (concat "<div class=\"date_xl\"><time>" (format-time-string "%Y-%m-%d") "</time></div>\n\n\n" )))
    (backward-char 1)))

(defun xah-html-update-first-h1 (@h1Text)
  "Update the first <h1>…</h1> of current buffer.
When called in elisp code, @h1Text is new title, a string.
URL `http://ergoemacs.org/emacs/elisp_update-html-title.html'
Version 2019-01-11 2021-02-09"
  (interactive
   (let ($oldTitle)
     (save-excursion
       (goto-char (point-min))
       (re-search-forward "<h1>\\([^<]+?\\)</h1>")
       (setq $oldTitle (match-string 1 )))
     (list (read-string "New title:" $oldTitle nil $oldTitle "INHERIT-INPUT-METHOD"))))
  (let ($p1 $p2)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "<h1>")
          (progn
            (setq $p1 (point))
            (search-forward "</h1>")
            (search-backward "<")
            (setq $p2 (point))
            (delete-region $p1 $p2 )
            (goto-char $p1)
            (insert @h1Text ))
        (progn
          (message "<h1> tag not found."))))))

(defun xah-html-make-citation ()
  "Reformat current text block or selection into a canonical citation format.
For example, place cursor somewhere in the following block:

Why Utopian Communities Fail
https://areomagazine.com/2018/03/08/why-utopian-communities-fail/
2018-03-08
by Ewan Morrison

becomes

 [<cite>Why Utopian Communities Fail</cite> <time>2018-03-08</time> By Ewan Morrison. At <a class=\"sorc\" rel=\"noopener\" target=\"_blank\" href=\"https://areomagazine.com/2018/03/08/why-utopian-communities-fail/\" data-accessed=\"2018-03-24\">https://areomagazine.com/2018/03/08/why-utopian-communities-fail/</a> ]

If there's a text selection, use it for input, otherwise the input is a text block between blank lines.

The order of lines for {title, author, date/time, url} needs not be in that order. Author should start with “by”.

URL `http://ergoemacs.org/emacs/elisp_make-citation.html'
Version 2020-07-15 2021-02-11"
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing 'block))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputText (buffer-substring-no-properties $p1 $p2))
         ;; ($inputText (replace-regexp-in-string "^[[:space:]]*" "" (elt $bds 0))) ; remove white space in front
         ;; ($lines (split-string $inputText "[ \t]*\n[ \t]*" t "[[:space:]]*"))
         ($lines (split-string $inputText "\n" t " *"))
         $title $author $date $url )
    ;; set title, date, url, author,
    (let ($x (case-fold-search t))
      ;; the whole thing here is not optimal implementation. data structure should be hash or so. easier... basically, we have n items, and we need to identify them into n things. that is, pairing them up. Now, some items are easily recognized with 100% certainty. We pair those first. Then, in the end, we'll have 2 or so items that we need to identify, but by then, the items are few, and we can easily distinguish them. So, for this, we need a data structure such that we can easily remove item for those we already identified.
      (while (> (length $lines) 0)
        (setq $x (pop $lines))
        (cond
         ((string-match "https?://" $x) (setq $url $x))
         ((xah-html--is-datetimestamp-p $x) (setq $date $x))
         ((string-match "^ *[bB]y:* " $x) (setq $author $x))
         (t (setq $title $x)))))
    (when (not $url) (error "I can't find “url” %s" $url))
    (when (not $date) (error "error 74188 I can't find “date” %s" $date))
    (when (not $title) (error "I can't find “title” %s" $title))
    (when (not $author) (error "I can't find “author” %s" $author))
    (setq $title (string-trim $title))
    (setq $title (replace-regexp-in-string "^\"\\(.+\\)\"$" "\\1" $title))
    (setq $title (xah-replace-pairs-in-string $title '(["’" "'"] ["&" "＆"] )))
    (setq $author (string-trim $author))
    (setq $author (replace-regexp-in-string "\\. " " " $author)) ; remove period in Initals
    (setq $author (replace-regexp-in-string "^ *[Bb]y:* +" "" $author))
    (setq $author (upcase-initials (downcase $author)))
    (setq $date (string-trim $date))
    (setq $date (xah-fix-datetime-string $date))
    (setq $url (string-trim $url))
    (setq $url (with-temp-buffer (insert $url) (xah-html-source-url-linkify 1) (buffer-string)))
    (delete-region $p1 $p2 )
    (insert (concat "[<cite>" $title "</cite> ")
            "<time>" $date "</time>"
            " By " $author
            ". At " $url
            " ]")))

(defun xah-html-make-link-defunct ()
  "Make the HTML link under cursor to a defunct form.
Example:
If cursor is inside this tag
 <a href=\"‹url›\">…</a>
or
 <a href=\"‹url›\" data-accessed=\"‹access_date›\">…</a>
where ‹access_date› is of this format 2019-04-02

It becomes:
 <s data-accessed=\"‹access_date›\" data-defunct-date=\"‹now_date›\">‹url›</s>

URL `http://ergoemacs.org/emacs/elisp_html_dead_link.html'
Version 2019-04-02"
  (interactive)
  (let ($p1 $p2 $wholeLinkStr $newLinkStr $url $accessedDate)
    (save-excursion
      ;; get the boundary of opening tag
      (forward-char 3)
      (search-backward "<a " ) (setq $p1 (point))
      (search-forward "</a>") (setq $p2 (point))
      (setq $wholeLinkStr (buffer-substring-no-properties $p1 $p2))
      (with-temp-buffer
        ;; generate replacement text
        (insert $wholeLinkStr)
        (goto-char (point-min))
        (re-search-forward  "href=\"\\([^\"]+?\\)\"")
        (setq $url (match-string 1))
        (setq $accessedDate
              (if (re-search-forward
                   "data-accessed=\"\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)\"" $p2 t)
                  (match-string 1)
                ""
                ))
        (setq $newLinkStr
              (format "<s data-accessed=\"%s\" data-defunct-date=\"%s\">%s</s>" $accessedDate (format-time-string "%Y-%m-%d") $url ))))
    (delete-region $p1 $p2)
    (insert $newLinkStr)))

(defun xah-html-video-file-suffix-p (@path)
  "Returns t if @path has video file suffix e.g. mp4, else nil.
Version 2018-11-14"
  (let ((case-fold-search t))
    (string-match-p
     "\\.mp4\\'\\|\\.mov\\'\\|\\.mkv\\'\\|\\.webm\\'\\|\\.m1v\\'\\|\\.m4v\\'" @path)))

(defun xah-html-audio-file-suffix-p (@path)
  "Returns t if @path has audio file suffix e.g. m4a, else nil.
Version 2018-11-14"
  (let ((case-fold-search t))
    (string-match-p "\\.mp3\\'\\|\\.m4a\\'\\|\\.ogg\\'\\|\\.opus\\'\\|\\.oga\\'" @path)))

(defun xah-html-image-file-suffix-p (@path)
  "Returns t if @path ends in .jpg .png .gif .svg, else nil.
Version 2017-08-11"
  (let ((case-fold-search t))
    (string-match-p "\.jpg\\'\\|\.png\\'\\|\.gif\\'\\|\.svg\\'" @path)))

(defun xah-html-image-linkify ()
  "Replace image file path under cursor to HTML img inline link.
Example:
 img/my_cats.jpg
become

 <img src=\"img/my_cats.jpg\" alt=\"my cats\" width=\"470\" height=\"456\" />

If `univers-argument' is called before, don't width and height attribute.

Returns the string used in the alt attribute.

URL `http://ergoemacs.org/emacs/elisp_image_tag.html'
Version 2018-06-14 2021-01-12"
  (interactive)
  (let ( $p1 $p2 $imgPath $hrefValue $altText $imgWH $width $height)
    (save-excursion
      ;; get image file path begin end pos
      (let ($p0)
        (setq $p0 (point))
        ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
        (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
        (setq $p1 (point))
        (goto-char $p0)
        (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
        (setq $p2 (point))
        (goto-char $p0)))
    (setq $imgPath
          (if (fboundp 'xahsite-web-path-to-filepath)
              (xahsite-web-path-to-filepath
               (xah-html-local-url-to-file-path
                (buffer-substring-no-properties $p1 $p2 )))
            (buffer-substring-no-properties $p1 $p2 )))
    (when (not (file-exists-p $imgPath))
      (user-error "file not exist at %s"  $imgPath))
    (setq $hrefValue
          (file-relative-name
           $imgPath
           (file-name-directory (or (buffer-file-name) default-directory))))
    (setq $altText
          (replace-regexp-in-string
             "_" " "
             (replace-regexp-in-string
              "\\.[A-Za-z]\\{3,4\\}$" "" (file-name-nondirectory $imgPath) t t) t t))
    (if current-prefix-arg
        (progn
          (delete-region $p1 $p2)
          (insert
           (concat
            "<img src=\""
            $hrefValue
            "\"" " " "alt=\"" $altText "\"" " />")))
      (progn
        (setq $imgWH (xah-get-image-dimensions $imgPath))
        (setq $width (number-to-string (elt $imgWH 0)))
        (setq $height (number-to-string (elt $imgWH 1)))

        (delete-region $p1 $p2)
        (insert
         (if (or (equal $width "0") (equal $height "0"))
             (concat
              "<img src=\""
              $hrefValue
              "\"" " " "alt=\"" $altText "\"" " />")
           (concat
            "<img src=\""
            $hrefValue
            "\"" " " "alt=\"" $altText "\""
            " width=\"" $width "\""
            " height=\"" $height "\" />")))))
    $altText
    ))

(defun xah-html-image-to-link (&optional @begin @end)
  "Make image file path at cursor point into a img link.
Example:
i/cat.jpg
becomes
<a class=\"bigImg\" href=\"i/cat.jpg\">4176×2366</a>
If there's a text selection, use that region as file name.
Version 2020-06-24"
  (interactive)
  (let
      ($p0 $p1 $p2 $input $imgPath $dimension $width $height $resultStr)
    (progn ; sets $p1 $p2
      (if @begin
          (progn
            (setq $p1 @begin)
            (setq $p2 @end))
        (if (use-region-p)
            (setq $p1 (region-beginning) $p2 (region-end))
          (save-excursion
            (setq $p0 (point))
            ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
            (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
            (setq $p1 (point))
            (goto-char $p0)
            (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
            (setq $p2 (point))))))
    (setq $input (buffer-substring-no-properties $p1 $p2))
    (setq $imgPath (xah-html-local-url-to-file-path $input))
    (setq $dimension (xah-get-image-dimensions $imgPath))
    (setq $width (number-to-string (elt $dimension 0)))
    (setq $height (number-to-string (elt $dimension 1)))
    (setq $resultStr
          (concat "<a class=\"bigImg\" href=\"" (file-relative-name $imgPath) "\">" $width "×" $height "</a>"))
    (delete-region $p1 $p2)
    (insert $resultStr)))

(defun xah-html-wrap-figure-tag (&optional @begin @end @figcaption)
  "Wrap <figure> tag around @begin @end, also add <figcaption>.
If no @begin @end are given, use current line.

<figure>
<img src=\"cat.png\" alt=\"cat\" width=\"707\" height=\"517\" />
<figcaption>▮</figcaption>
</figure>

If there's a text selection, use that as image path.

Version 2019-11-15"
  (interactive)
  (let ($p1 $p2
            ($figcapStr (if @figcaption
                       @figcaption
                     ""
                     )))

    (if (and @begin @end)
        (progn (setq $p1 @begin $p2 @end))
      (progn
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))

    (goto-char $p2)
    (insert "\n<figcaption>\n")
    (insert $figcapStr "\n</figcaption>\n</figure>\n\n")

    (goto-char $p1)
    (insert "\n<figure>\n")

    (search-forward "</figcaption>" nil t)
    (search-backward "<")
    (backward-char )
    ;;
    ))

(defun xah-html-image-figure-linkify ()
  "Replace a image file's path under cursor with a HTML img tag,
and wrap it with “figure” and “figcaption” tags.
Example, if cursor is on the word “i/cat.png”, then it will became
<figure>
<img src=\"cat.png\" alt=\"cat\" width=\"707\" height=\"517\" />
<figcaption>▮</figcaption>
</figure>
If there's a text selection, use that as image path.
This function calls `xah-html-image-linkify'.
Version 2019-11-15"
  (interactive)
  (let ($p1 $p2 $altStr)
    (setq $altStr (xah-html-image-linkify))
    (search-backward "<img ")
    (insert "\n<figure>\n")
    (search-forward ">")
    (insert "\n<figcaption>\n")
    (insert $altStr "\n</figcaption>\n</figure>\n\n")
    ;; (search-backward "</figcaption>")
    ;; (backward-char )
    ;;
    ))

(defun xah-html-css-linkify ()
  "Make the path under cursor into a HTML link.
 e.g. ~/web/xahlee_org/lit.css
becomes
<link rel=\"stylesheet\" href=\"../lit.css\" />
Version 2018-01-30"
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing-or-region 'filepath))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputStr (buffer-substring-no-properties $p1 $p2))
         ($src (if (string-match "^http" $inputStr ) $inputStr (file-relative-name $inputStr))))
    (delete-region $p1 $p2)
    (insert (format "<link rel=\"stylesheet\" href=\"%s\" />" $src))))

(defun xah-html-javascript-linkify ()
  "Make the path under cursor into a HTML link.
 eg <script src=\"xyz.js\"></script>
Version 2016-10-31"
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing-or-region 'filepath))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputStr (buffer-substring-no-properties $p1 $p2))
         ($src
          (if (string-match "^http" $inputStr )
              $inputStr
            (file-relative-name $inputStr))))
    (delete-region $p1 $p2)
    (insert (format "<script defer src=\"%s\"></script>" $src))))

(defun xah-html-pdf-embed-linkify ()
  "Make the path under cursor into a embedded pdf.
e.g. math.pdf
becomes
<embed src=\"%s\" type=\"application/pdf\" style=\"width:100%;height:95vh\" />

Version 2019-07-29"
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing-or-region 'filepath))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputStr (buffer-substring-no-properties $p1 $p2))
         ($src
          (if (string-match "^http" $inputStr )
              $inputStr
            (file-relative-name $inputStr))))
    (delete-region $p1 $p2)
    (insert
     (format "<embed src=\"%s\" type=\"application/pdf\" style=\"width:100%%;height:1000px\" />" $src))))

(defun xah-html-pdf-linkify ()
  "Make the pdf file path under cursor into a link.
e.g.
 doc/cat.pdf
becomes
 <a href=\"doc/cat.pdf\">cat.pdf</a>

Version 2017-09-13"
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing-or-region 'filepath))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputStr (buffer-substring-no-properties $p1 $p2))
         ($fname (file-name-nondirectory $inputStr))
         ($src
          (if (string-match "^http" $inputStr )
              $inputStr
            (file-relative-name $inputStr))))
    (delete-region $p1 $p2)
    (insert (format "<a href=\"%s\">%s</a>" $src $fname))))

;; (defun xah-html-audio-file-linkify ()
;;   "Make the path under cursor into a HTML link.
;; e.g. xyz.mp3
;; becomes
;; <audio src=\"xyz.mp3\"></audio>
;; Version 2018-09-19"
;;   (interactive)
;;   (let* (
;;          ($bds (xah-get-bounds-of-thing-or-region 'filepath))
;;          ($p1 (car $bds))
;;          ($p2 (cdr $bds))
;;          ($inputStr (buffer-substring-no-properties $p1 $p2))
;;          ($src
;;           (if (string-match "^http" $inputStr )
;;               $inputStr
;;             (file-relative-name $inputStr))))
;;     (delete-region $p1 $p2)
;;     (insert (format "<audio src=\"%s\" controls loop></audio>" $src))))

(defun xah-html-audio-file-linkify ( &optional @figure)
  "Make the path under cursor into a HTML audio tag link.
e.g. xyz.mp4
becomes
<audio src=\"i/xyz.m4a\" controls loop></audio>

if
@add-figure is not nil, wrap figure and figcaption tags around it.

Returns a alt string based on the file name.

Version 2020-01-19"
  (interactive)
  (let* (
         ($bds (bounds-of-thing-at-point 'filename ))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputStr (buffer-substring-no-properties $p1 $p2 ))
         ($src
          (if (string-match "^http" $inputStr )
              $inputStr
            (if (file-exists-p $inputStr)
                (file-relative-name $inputStr)
              (user-error "file not found: 「%s」" $inputStr)))))
    (delete-region $p1 $p2)
    (if @figure
        (progn
          (goto-char $p1)
          (insert "\n<figure>\n")
          (insert (format "<audio src=\"%s\" controls loop></audio>\n" $src))
          (insert "<figcaption>\n")
          (insert (replace-regexp-in-string "_" " " (file-name-sans-extension (file-name-nondirectory $src))))
          (insert "\n</figcaption>\n</figure>\n\n")
          (search-backward "</figcaption>" ))
      (progn
        (insert (format "<audio src=\"%s\" controls loop></audio>" $src))))))

(defun xah-html-video-file-linkify ( &optional @figure)
  "Make the path under cursor into a HTML video tag link.
e.g. xyz.mp4
becomes
<video src=\"i/xyz.mp4\" controls loop></video>

if
@add-figure is not nil, wrap figure and figcaption tags around it.

Returns a alt string based on the file name.

Version 2020-01-19"
  (interactive)
  (let* (
         ($bds (bounds-of-thing-at-point 'filename ))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputStr (buffer-substring-no-properties $p1 $p2 ))
         ($src
          (if (string-match "^http" $inputStr )
              $inputStr
            (if (file-exists-p $inputStr)
                (file-relative-name $inputStr)
              (user-error "file not found: 「%s」" $inputStr)))))
    (delete-region $p1 $p2)
    (if @figure
        (progn
          (goto-char $p1)
          (insert "\n<figure>\n")
          (insert (format "<video src=\"%s\" controls loop></video>\n" $src))
          (insert "<figcaption>\n")
          (insert (replace-regexp-in-string "_" " " (file-name-sans-extension (file-name-nondirectory $src))))
          (insert "\n</figcaption>\n</figure>\n\n")
          (search-backward "</figcaption>" ))
      (progn
        (insert (format "<video src=\"%s\" controls loop></video>" $src))))))

(defun xah-html-amazon-linkify (&optional @tracking-id)
  "Make the current amazon URL or selection into a link.

Examples of amazon product URL formats
http://www.amazon.com/Cyborg-R-T-Gaming-Mouse/dp/B003CP0BHM/ref=pd_sim_e_1
http://www.amazon.com/gp/product/B003CP0BHM
http://www.amazon.com/exec/obidos/ASIN/B003CP0BHM/xahh-20
http://www.amazon.com/exec/obidos/tg/detail/-/B003CP0BHM/
http://www.amazon.com/dp/B003CP0BHM?tag=xahhome-20
http://amzn.to/1F5M1hA
https://alexa.design/2okfMcj

Example output:
<a class=\"amz\" target=\"_blank\" href=\"http://www.amazon.com/dp/B003CP0BHM/?tag=xahh-20\" title=\"Cyborg R T Gaming Mouse\">amazon</a>

ASIN is a 10 character string that's a product id.

URL `http://ergoemacs.org/emacs/elisp_amazon-linkify.html'
Version 2020-01-15"
  (interactive)
  (let (($bds (bounds-of-thing-at-point 'url))
        $p1 $p2 $url $asin $thingName
        ($trackId (if @tracking-id @tracking-id "xahh-20" )))
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (car $bds) $p2 (cdr $bds)))
    (setq $url (buffer-substring-no-properties $p1 $p2))
    (if (or (string-match "//amzn.to/" $url)
            (string-match "//alexa.design/" $url))
        (progn (delete-region $p1 $p2)
               (insert (format "<a class=\"amz_search\" target=\"_blank\" href=\"%s\">amazon</a>" $url)))
      (progn
        (setq $asin
              (cond
               ((string-match "/dp/\\([[:alnum:]]\\{10\\}\\)/?" $url) (match-string 1 $url))
               ((string-match "/dp/\\([[:alnum:]]\\{10\\}\\)\\?tag=" $url) (match-string 1 $url))
               ((string-match "/gp/product/\\([[:alnum:]]\\{10\\}\\)" $url) (match-string 1 $url))
               ((string-match "/ASIN/\\([[:alnum:]]\\{10\\}\\)" $url) (match-string 1 $url))
               ((string-match "/tg/detail/-/\\([[:alnum:]]\\{10\\}\\)/" $url) (match-string 1 $url))
               ((string-match "/\\([[:alnum:]]\\{10\\}\\)/" $url) (match-string 1 $url))
               ((and
                 (equal 10 (length $url ))
                 (string-match "\\`\\([[:alnum:]]\\{10\\}\\)\\'" $url))
                $url)
               (t (error "no amazon ASIN found"))))
        (setq
         $thingName
         (replace-regexp-in-string
          "-" " "
          (if (string-match "amazon\.com/\\([^/]+?\\)/dp/" $url)
              (progn (match-string 1 $url))
            (progn
              (message "no product name found" ))
            ""
            )))
        (delete-region $p1 $p2)
        (insert

         (format "<a class=\"amz\" target=\"_blank\" href=\"http://www.amazon.com/dp/%s/?tag=%s\" title=\"%s\">Buy at amazon</a>" $asin $trackId $thingName))
        (search-backward "\">")))))

(defun xah-html-get-youtube-id (@url)
  "Return the ID string inside a YouTube @url string.
If cannot find, return nil.
Version 2020-08-27"
  (interactive)
  (let ($id)
    (cond
     ;; RhYNu6i_uY4
     ((eq 11 (length @url))
      @url)
     ;; https://www.youtube.com/watch?v=RhYNu6i_uY4
     ((string-match "v=\\(.\\{11\\}\\)" @url)
      (match-string 1 @url))
     ;; https://youtu.be/RhYNu6i_uY4
     ((string-match "youtu\\.be/\\(.\\{11\\}\\)" @url)
      (match-string 1 @url))
     ;; https://www.youtube.com/embed/RhYNu6i_uY4
     ((string-match "youtube.com/embed/\\(.\\{11\\}\\)" @url)
      (match-string 1 @url))
     (t nil))))

(defun xah-html-youtube-linkify ()
  "Make the current line of youtube url into a embeded video.

The line can be any of

 https://www.youtube.com/watch?v=RhYNu6i_uY4
 https://www.youtube.com/watch?v=RhYNu6i_uY4?t=198
 https://youtu.be/RhYNu6i_uY4
 https://www.youtube.com/embed/RhYNu6i_uY4
 RhYNu6i_uY4

Here's sample result:

<figure>
<iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/RhYNu6i_uY4\" allowfullscreen></iframe>
<figcaption>
</figcaption>
</figure>

Version 2020-08-27 2021-04-08"
  (interactive)
  (let ( $p1 $p2 $inputStr $id $timeStamp )
    (re-search-backward "[ \n]")
    (forward-char )
    (setq $p1 (point))
    (re-search-forward "[ \n]" )
    (setq $p2 (point))
    (setq $inputStr (buffer-substring-no-properties $p1 $p2))
    (setq $timeStamp
          (if (or
               (string-match "t=\\([0-9]+\\)" $inputStr )
               (string-match "time_continue=\\([0-9]+\\)" $inputStr ))
              (match-string 1 $inputStr)
            ""))
    (setq $id (xah-html-get-youtube-id $inputStr))
    (delete-region $p1 $p2)
    (insert
     (format "

<figure>
<iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/%s%s\" allowfullscreen></iframe>
<figcaption>
</figcaption>
</figure>

"
             $id
             (if (string-equal $timeStamp "")
                 ""
               (concat "?start=" $timeStamp))))
    (search-backward "</figcaption>" )
    (backward-char 1)))

(defun xah-html-file-linkify (&optional @begin @end)
  "Make the path under cursor into a HTML link for xah site.

For Example, if you cursor is on the text “../emacs/emacs.html”,
then it'll become:
“<a href=\"../emacs/emacs.html\">Emacs Tutorial</a>”.
The link text is pulled from the file's <title> tag if exists.

If there is text selection, use it as file path.

The file path can also be a full path or URL.
Version 2017-12-20 2021-02-14"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (let ($p0 $p1 $p2)
         (setq $p0 (point))
         ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
         (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
         (setq $p1 (point))
         (goto-char $p0)
         (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
         (setq $p2 (point))
         (list $p1 $p2)))))
  (when (or (null @begin) (null @end))
    (setq @begin (line-beginning-position) @end (line-end-position)))
  (let* (
         ($inputStr
          (xah-html-local-url-to-file-path (buffer-substring-no-properties @begin @end)))
         ($inputStParts (xah-html-split-uri-hashmark $inputStr))
         ($pt1 (aref $inputStParts 0))
         ($fragPart (aref $inputStParts 1))
         ($fPath (expand-file-name $pt1 default-directory)))
    (if (file-exists-p $fPath)
        (let* (
               ($rPath (file-relative-name $fPath (file-name-directory (or (buffer-file-name) default-directory))))
               ($title
                (if (string-match-p ".+html\\'" $fPath)
                    (concat (xah-html-get-html-file-title $fPath t) $fragPart)
                  (file-name-nondirectory $fPath)))
               ($resultStr
                (format "<a href=\"%s\">%s</a>"
                        (concat $rPath $fragPart)
                        (if (string-equal $title "") $rPath $title ))))
          (delete-region @begin @end)
          (insert $resultStr))
      (progn (message "Cannot locate the file: 「%s」" $fPath)))))

(defun xah-html-any-linkify ()
  "Make the text under cursor into a HTML link.

Exactly what tag is used depends on the file name suffix. this command calls one of the following:
`xah-html-source-url-linkify'
`xah-html-wikipedia-url-linkify'
`xah-html-css-linkify'
`xah-html-pdf-linkify'
`xah-html-javascript-linkify'
`xah-html-audio-file-linkify'
`xah-html-video-file-linkify'
`xah-html-youtube-linkify'
`xah-html-amazon-linkify'
`xah-html-source-url-linkify'
`xah-html-image-figure-linkify'
`xah-html-url-linkify'

If region is active, use it as input.
Version 2020-08-07 2021-02-26"
  (interactive)
  (let ( $p1 $p2 $input)
    ;; (if (string-match "%" $input )
    ;;     (decode-coding-string (url-unhex-string "https://mysticsiva.wordpress.com/2016/11/04/%E3%82%AD%E3%83%BC%E3%82%AD%E3%83%A3%E3%83%83%E3%83%97%E4%BA%A4%E6%8F%9B3/") 'utf-8)
    ;;   $input)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (let ($p0)
          (setq $p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|[]{}<>〔〕“”〈〉《》【】〖〗〘〙〘〙«»‹›·。\\`")
          (setq $p1 (point))
          (goto-char $p0)
          (skip-chars-forward "^  \"\t\n'|[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
          (setq $p2 (point)))))
    (setq $input (xah-html-local-url-to-file-path (buffer-substring-no-properties $p1 $p2)))
    (message "%s" $input)
    (cond
     ((or (string-match-p "wikipedia.org/" $input)
          (string-match-p "wiktionary.org/" $input)
          (string-match-p "wikimedia.org/" $input))
      (if (xah-html-image-file-suffix-p $input)
          (progn
            (message "Call %s" "xah-html-source-url-linkify")
            (xah-html-source-url-linkify 3))
        (progn
          (message "Call %s" "xah-html-wikipedia-url-linkify")
          (xah-html-wikipedia-url-linkify ))))
     ((string-match-p "\\.css\\'" $input) (xah-html-css-linkify))
     ((string-match-p "\\.pdf" $input) (xah-html-pdf-linkify))
     ((string-match-p "\\.js\\'\\|\\.ts\\'" $input) (xah-html-javascript-linkify))
     ((xah-html-audio-file-suffix-p $input)
      (progn
        (message "Call %s" "xah-html-audio-file-linkify")
        (xah-html-audio-file-linkify t)))
     ((xah-html-video-file-suffix-p $input)
      (progn
        (message "Call %s" "xah-html-video-file-linkify")
        (xah-html-video-file-linkify t)))
     ((string-match-p "youtube\.com/" $input)
      (progn
        (message "Call %s" "xah-html-youtube-linkify")
        (xah-html-youtube-linkify)))
     ((string-match-p "youtu\.be/" $input)
      (progn
        (message "Call %s" "xah-html-youtube-linkify")
        (xah-html-youtube-linkify)))
     ((string-match-p "www\.amazon\.com/\\|//amzn\.to/" $input)
      (progn
        (message "Call %s" "xah-html-amazon-linkify")
        (xah-html-amazon-linkify)))
     ((string-match-p "\\`https?://" $input)
      (progn
        (if (fboundp 'xahsite-url-is-xah-website-p)
            (if (xahsite-url-is-xah-website-p $input)
                (progn
                  (xah-file-linkify $p1 $p2))
              (progn
                (message "Call %s" "xah-html-source-url-linkify")
                (xah-html-source-url-linkify 0)))
          (progn
            (message "Call %s" "xah-html-source-url-linkify")
            (xah-html-source-url-linkify 0)))))
     ((xah-html-image-file-suffix-p $input) (xah-html-image-figure-linkify))
     ((string-match
       (concat "^" (expand-file-name "~/" ) "web/")
       (or (buffer-file-name) default-directory))
      ;; (if (fboundp 'xah-all-linkify) (progn (xah-all-linkify)) (xah-html-url-linkify))
      (progn
        (message "Call %s" "xah-html-url-linkify")
        (xah-html-file-linkify $p1 $p2)))
     ((file-exists-p $input)
      (progn
        (message "Call %s" "xah-html-file-linkify")
        (xah-html-file-linkify $p1 $p2)))
     (t (progn
          (message "Call %s" "xah-html-url-linkify")
          (xah-html-url-linkify))))
    ;;
    ))

(defun xah-html-source-url-linkify (@prefixArg)
  "Change URL under cursor into a HTML link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
<a class=\"sorc\" rel=\"noopener\" target=\"_blank\" href=\"http://example.com/xyz.htm\" data-accessed=\"2008-12-25\">http://example.com/xyz.htm</a>

If `universal-argument' is called first,
The anchor text may be of 4 possibilities:

1 → 「‹full url›」
2 or 4 → 「‹domain›…」
3 → 「img src」
0 or any → smartly decide.

URL `http://ergoemacs.org/emacs/elisp_html-linkify.html'
Version 2020-07-15 2021-02-11"
  (interactive "P")
  (let ( $p1 $p2 $input $url $domainName $linkText )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (let (($bds (bounds-of-thing-at-point 'url)))
        (setq $p1 (car $bds) $p2 (cdr $bds))))
    (when (eq $p1 nil)
      (user-error "Text under cursor probably not a url." ))
    (setq $input (buffer-substring-no-properties $p1 $p2))
    (setq $url (replace-regexp-in-string "&amp;" "&" $input nil "LITERAL"))
    ;; in case it's already encoded. TODO this is only 99% correct.
    (setq $domainName
          (progn
            (string-match "://\\([^\/]+?\\)/?" $url)
            (match-string 1 $url)))
    (when (eq 0 (length $domainName))
      (user-error "cannot find domain name. Got %s" $domainName))
    (setq $linkText
          (cond
           ((equal @prefixArg 1) $url)
           ((or (equal @prefixArg 2) (equal @prefixArg 4) (equal @prefixArg '(4))) (concat $domainName "…"))
           ((equal @prefixArg 3) "image source")
           (t (if
                  (let ((case-fold-search t))
                    (string-match "\\(wikimedia\\.org\\|wikipedia\\.org\\).+\\(jpg\\|png\\|svg\\)$" $url))
                  "image source"
                $url
                ))))
    (setq $url (replace-regexp-in-string "&" "&amp;" $url))
    ;; delete URL and insert the link
    (delete-region $p1 $p2)
    (insert (format
             "<a class=\"sorc\" rel=\"noopener\" target=\"_blank\" href=\"%s\" data-accessed=\"%s\">%s</a>"
             $url (format-time-string "%Y-%m-%d") $linkText
             ))))

(defun xah-html-wikipedia-url-linkify ()
  "Change Wikipedia URL under cursor into a HTML link.
If there is a text selection, use that as input.

Example:
http://en.wikipedia.org/wiki/Emacs
becomes
<a class=\"wikipedia_92d5m\" rel=\"noopener\" target=\"_blank\" href=\"http://en.wikipedia.org/wiki/Emacs\" data-accessed=\"2015-09-14\">Emacs</a>.

Works the same way for links to wiktionary, e.g. https://en.wiktionary.org/wiki/%E4%BA%86

URL `http://ergoemacs.org/emacs/elisp_html_wikipedia_url_linkify.html'
Version 2020-03-24"
  (interactive)
  (let (
        $p1 $p2
        $input-str
        $link-text
        $output-str
        )
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (let ($p0)
          (progn
            (setq $p0 (point))
            (skip-chars-backward "^ \t\n<>[]")
            (setq $p1 (point))
            (goto-char $p0)
            (skip-chars-forward "^ \t\n<>[]")
            (setq $p2 (point))))))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $link-text
          (replace-regexp-in-string
           "_" " "
           (decode-coding-string (url-unhex-string (file-name-nondirectory $input-str)) 'utf-8)))
    (setq $output-str
          (format
           "<a class=\"wikipedia_92d5m\" rel=\"noopener\" target=\"_blank\" href=\"%s\" data-accessed=\"%s\">%s</a>"
           (url-encode-url $input-str)
           (format-time-string "%Y-%m-%d")
           $link-text
           ))
    (progn
      (delete-region $p1 $p2)
      (insert $output-str))))

(defun xah-html-url-linkify ()
  "Make the URL at cursor point into a HTML link.
Work on current non-whitespace char sequence or text selection.

For example
http://example.org/cat.html
becomes
<a href=\"http://example.org/cat.html\">http://example.org/cat.html</a>

URL `http://ergoemacs.org/emacs/wrap-url.html'
Version 2020-02-19"
  (interactive)
  (let ( $p1 $p2 $input $newStr )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (let ($p0)
          (setq $p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
          (setq $p1 (point))
          (goto-char $p0)
          (skip-chars-forward "^  \"\t\n'|[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
          (setq $p2 (point)))))
    (setq $input (buffer-substring-no-properties $p1 $p2))
    (setq $newStr
          (if (string-match "^http" $input )
              $input
            (progn (file-relative-name (xah-html-local-url-to-file-path $input)))))
    (delete-region $p1 $p2)
    (insert (concat "<a href=\"" (url-encode-url $newStr) "\">" $newStr "</a>" ))))

(defun xah-html-wrap-p-tag ()
  "Add <p>…</p> tag to current block or text selection.
If there's a text selection, wrap p around each text block.
A text block is separated by blank lines.

URL `http://ergoemacs.org/emacs/emacs_html_wrap_tags.html'

Version 2019-06-21"
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing-or-region 'block))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputText (buffer-substring-no-properties $p1 $p2)))
    (delete-region $p1 $p2 )
    (insert
     "<p>\n"
     (replace-regexp-in-string "\n\n+" "</p>\n\n<p>" (string-trim $inputText))
     "\n</p>")
    (skip-chars-forward "\n" )))

(defun xah-html-insert-br-tag ()
  "Insert a html <br /> tag.
If there's a text selection, add to end of all lines.
Version 2020-11-22"
  (interactive)
  (let (p1 p2)
    (if (use-region-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          (save-restriction
            (narrow-to-region p1 p2)
            (goto-char (point-min))
            (while (search-forward "\n" nil "NOERROR" )
              (replace-match "<br />\n"))))
      (insert "<br />\n"))))

(defun xah-html-emacs-to-windows-kbd-notation (@begin @end)
  "Change emacs key notation to Windows's notation on text selection or current line.

For example:
 C-h f → Ctrl+h f
 M-a → Alt+a
 <f9> <f8> → F9 F8
And a special W for Window.
 W-a → Win+a

This command will do most emacs syntax correctly, but not 100% correct, especially on notations like <C-M-down>. But works if it's written as C-M-<down>

When called in lisp code, @begin @end are region begin/end positions.
Version 2017-09-30 2021-02-03"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-restriction
    (narrow-to-region @begin @end)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (mapc
       (lambda ($x)
         (goto-char (point-min))
         (while
             (search-forward (aref $x 0) nil t)
           (replace-match (aref $x 1) "FIXEDCASE")))
       [
        ["<prior>" "PageUp"]
        ["<next>" "PageDown"]
        ["<home>" "Home"]
        ["<end>" "End"]
        ["<f1>" "F1"] ["<f2>" "F2"] ["<f3>" "F3"] ["<f4>" "F4"] ["<f5>" "F5"] ["<f6>" "F6"] ["<f7>" "F7"] ["<f8>" "F8"] ["<f9>" "F9"] ["<f10>" "F10"] ["<f11>" "F11"] ["<f12>" "F12"]
        ["<return>" "Return"]
        ["<tab>" "Tab"]
        ["<escape>" "Escape"]
        ["<right>" "→"]
        ["<left>" "←"]
        ["<up>" "↑"]
        ["<down>" "↓"]
        ["<insert>" "Insert"]
        ["<delete>" "Delete"]
        ["<backspace>" "Backspace"]
        ["<menu>" "Menu"]
        ]))
    (let ((case-fold-search nil))
      (mapc
       (lambda ($x)
         (goto-char (point-min))
         (while
             (re-search-forward (aref $x 0) nil t)
           (replace-match (aref $x 1) "FIXEDCASE")))
       [
        ["<C-\\(.\\)>" "C-<\\1>"]
        ["<C-s-\\(.\\)>" "C-s-<\\1>"]
        ["<M-\\(.\\)>" "M-<\\1>"]
        ["<M-s-\\(.\\)>" "M-s-<\\1>"]
        ["\\bC-\\(.\\)" "Ctrl+\\1"]
        ["\\bM-\\(.\\)" "Alt+\\1"]
        ["\\bA-\\(.\\)" "Alt+\\1"]
        ["\\bS-\\(.\\)" "Shift+\\1"]
        ["\\bs-\\(.\\)" "Super+\\1"]
        ["\\bH-\\(.\\)" "Hyper+\\1"]
        ["\\bW-\\(.\\)" "Win+\\1"]
        ["\\bRET\\b" "Enter"]
        ["\\bSPC\\b" "Space"]
        ["\\bTAB\\b" "Tab"]
        ["\\bESC\\b" "Escape"]
        ["\\bDEL\\b" "Delete"]
        ]))))

(defun xah-html-htmlize-elisp-keywords (@begin @end)
  "Replace 「square-bracketed」 elisp names to HTML markup, in current line or text selection.

Example:
 「sort-lines」
    becomes
  <code class=\"elisp_f_3d841\">sort-lines</code>

When called in lisp code, @begin @end are region begin/end positions.

Note: a word is changed only if all of the following are true:

• The symbol string is tightly enclosed in double curly quotes, e.g. 「sort-lines」 but not 「use sort-lines」.
• `fboundp' or `boundp' returns true.
• symbol string's char contains only alphanumeric or hyphen, even though elisp identifier allows many other chars. e.g. `yas/reload-all', `color-cie-ε'.

This command also makes a report of changed items.

Some issues:

• Some words are common in other lang, e.g. “while”, “print”, “string”, unix “find”, “grep”, HTML's “kbd” tag, etc. But they are also built-in elisp symbols. This command will tag them, but you may not want that.

• Some function/variable are from 3rd party libs, and some are not bundled with GNU emacs , e.g. 「'htmlize」. They may or may not be tagged depending whether they've been loaded.
Version 2017-03-17 2021-01-14"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let (($changedItems '()) $mStr)
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward
                  "「\\([:-A-Za-z0-9]+\\)」"
                  (point-max) t)
            (setq $mStr (match-string 1))
            (cond
             ((and (intern-soft $mStr) (fboundp (intern-soft $mStr)))
              (progn
                (push (format "f %s" $mStr) $changedItems)
                (replace-match (concat "<code class=\"elisp_f_3d841\">" $mStr "</code>") t t)
                (let ($p1 $p2)
                  (search-backward "</code>" )
                  (setq $p2 (point))
                  (search-backward "<code class=\"elisp_f_3d841\">" )
                  (search-forward "<code class=\"elisp_f_3d841\">")
                  (setq $p1 (point))
                  (overlay-put (make-overlay $p1 $p2) 'face (list :background "yellow"))
                  (search-forward "</code>"))))
             ((and (intern-soft $mStr) (boundp (intern-soft $mStr)))
              (progn
                (push (format "υ %s" $mStr) $changedItems)
                (replace-match (concat "<var class=\"elisp\">" $mStr "</var>") t t)
                (let ($p1 $p2)
                  (search-backward "</var>" )
                  (setq $p2 (point))
                  (search-backward "<var class=\"elisp\">" )
                  (search-forward "<var class=\"elisp\">")
                  (setq $p1 (point))
                  (overlay-put (make-overlay $p1 $p2) 'face (list :background "green"))
                  (search-forward "</var>"))))
             (t "do nothing"))))))
    (mapcar
     (lambda ($x)
       (princ $x)
       (terpri))
     (reverse $changedItems))))

(defun xah-html-brackets-to-html (@begin @end)
  "Replace bracketed text to HTML markup in current line or text selection.

• 「emacs-lisp-function-name」 → <code class=\"elisp_f_3d841\">emacs-lisp-function-name</code> if current file path contains “emacs”.
• 「…」 → <code>…</code>
• 〈…〉 → <cite>…</cite>
• 《…》 → <cite>…</cite>
• 〔…〕 → <code class=\"path_xl\">\\1</code>
•  ‹…› → <var class=\"d\">…</var>
• [<a href=] → [see <a href=]

Changes are reported to message buffer with char position.

When called in lisp code, @begin @end are region begin/end positions.
Version 2019-09-11"
  (interactive
   (let (($bds (xah-get-bounds-of-thing-or-region 'block)))
     (list (car $bds) (cdr $bds))))
  (let (($changedItems '()))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (when (and (buffer-file-name) (string-match "emacs" (buffer-file-name)))
          (xah-html-htmlize-elisp-keywords @begin @end))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "「\\([^」]+?\\)」" nil t)
            (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) $changedItems)
            (save-restriction
              (narrow-to-region (match-beginning 0) (match-end 0))
              (goto-char (point-min))
              (delete-char 1)
              (goto-char (point-max))
              (delete-char -1)
              (xah-html-encode-ampersand-region (point-min) (point-max))
              (goto-char (point-min))
              (insert "<code>")
              (overlay-put (make-overlay (point) (point-max)) 'face 'highlight)
              (goto-char (point-max))
              (insert "</code>"))))
        (goto-char (point-min))
        (while (re-search-forward "〈\\([^〉]+?\\)〉" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) $changedItems)
          (replace-match "<cite>\\1</cite>" t)
          (let ($p1 $p2)
            (search-backward "</cite>" )
            (setq $p2 (point))
            (search-backward "<cite>" )
            (search-forward "<cite>" )
            (setq $p1 (point))
            (overlay-put (make-overlay $p1 $p2) 'face 'highlight)
            (search-forward "</cite>" )))
        (goto-char (point-min))
        (while (re-search-forward "《\\([^》]+?\\)》" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) $changedItems)
          (replace-match "<cite>\\1</cite>" t)
          (let ($p1 $p2)
            (search-backward "</cite>" )
            (setq $p2 (point))
            (search-backward "<cite>" )
            (search-forward "<cite>" )
            (setq $p1 (point))
            (overlay-put (make-overlay $p1 $p2) 'face 'highlight)
            (search-forward "</cite>" )))
        (goto-char (point-min))
        (while (re-search-forward "‹\\([^›]+?\\)›" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) $changedItems)
          (replace-match "<var class=\"d\">\\1</var>" t)
          (let ($p1 $p2)
            (search-backward "</var>" )
            (setq $p2 (point))
            (search-backward "<var class=\"d\">" )
            (search-forward "<var class=\"d\">" )
            (setq $p1 (point))
            (overlay-put (make-overlay $p1 $p2) 'face 'highlight)
            (search-forward "</var>" )))
        (goto-char (point-min))
        (while (search-forward "[<a href=" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) $changedItems)
          (replace-match "[see <a href=" t)
          (let ($p1 $p2)
            (search-backward "[see <a href=" )
            (setq $p1 (point))
            (search-forward "[see <a href=" )
            (setq $p2 (point))
            (overlay-put (make-overlay $p1 $p2) 'face 'highlight)))
        (goto-char (point-min))
        (while (re-search-forward "〔\\([ -_/\\:~.A-Za-z0-9%]+?\\)〕" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) $changedItems)
          (replace-match "<code class=\"path_xl\">\\1</code>" t)
          (let ($p1 $p2)
            (search-backward "</code>" )
            (setq $p2 (point))
            (search-backward "<code class=\"path_xl\">" )
            (search-forward "<code class=\"path_xl\">")
            (setq $p1 (point))
            (overlay-put (make-overlay $p1 $p2) 'face 'highlight)
            (search-forward "</code>")))
        ;; (progn
        ;;   (goto-char (point-min))
        ;;   (while (re-search-forward "\\.\\.\\." nil t)
        ;;     (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) $changedItems)
        ;;     (replace-match "…" t)
        ;;     (let ($p1 $p2)
        ;;       (search-backward "…" )
        ;;       (setq $p1 (point))
        ;;       (search-forward "…")
        ;;       (setq $p2 (point))
        ;;       (overlay-put (make-overlay $p1 $p2) 'face 'highlight))))
        ))
    (mapcar
     (lambda ($x)
       (princ $x)
       (terpri))
     (reverse $changedItems))))

(defun xah-html-htmlize-keyboard-shortcut-notation (@begin @end)
  "Markup keyboard shortcut notation in HTML tag, on text selection or current line.
Example:
 C-w ⇒ <kbd>Ctrl</kbd>+<kbd>w</kbd>
 ctrl+w ⇒ <kbd>Ctrl</kbd>+<kbd>w</kbd>

When called in lisp code, @begin @end are region begin/end positions.

Version 2020-12-18"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let* (
         ($replaceList
          [
           ;; case must match
           ["Ctrl" "<kbd>Ctrl</kbd>"]
           ["Control" "<kbd>Ctrl</kbd>"]
           ["AltGr" "<kbd>AltGraph</kbd>"]
           ["AltGraph" "<kbd>AltGraph</kbd>"]
           ["Compose" "<kbd>Compose</kbd>"]
           ["Alt" "<kbd>Alt</kbd>"]
           ["Shift" "<kbd>Shift</kbd>"]
           ["Cmd" "<kbd>⌘ command</kbd>"]
           ["command" "<kbd>⌘ command</kbd>"]
           ["Option" "<kbd>⌥ option</kbd>"]
           ["Opt" "<kbd>⌥ option</kbd>"]
           ["Win" "<kbd>❖ Window</kbd>"]
           ["Menu" "<kbd>▤ Menu</kbd>"]
           ["Meta" "<kbd>Meta</kbd>"]
           ["Super" "<kbd>Super</kbd>"]
           ["Hyper" "<kbd>Hyper</kbd>"]

           ["Return" "<kbd>Return</kbd>"]
           ["Enter" "<kbd>Enter</kbd>"]
           ["Backspace" "<kbd>Backspace</kbd>"]
           ["bs" "<kbd>Backspace</kbd>"]
           ["Delete" "<kbd>Delete ⌦</kbd>"]
           ["DEL" "<kbd>Delete ⌦</kbd>"]
           ["Space" "<kbd>Space</kbd>"]
           ["Caps Lock" "<kbd>CapsLock</kbd>"]
           ["capslock" "<kbd>CapsLock</kbd>"]
           ["f lock" "<kbd>F Lock</kbd>"]
           ["numlock" "<kbd>NumLock</kbd>"]
           ["Number Lock" "<kbd>NumLock</kbd>"]

           ["Help" "<kbd>Help</kbd>"]
           ["Power" "<kbd>Power</kbd>"]
           ["Tab" "<kbd>Tab</kbd>"]
           ["Esc" "<kbd>Escape</kbd>"]
           ["escape" "<kbd>Escape</kbd>"]
           ["Home" "<kbd>Home</kbd>"]
           ["End" "<kbd>End</kbd>"]
           ["PgUp" "<kbd>PageUp</kbd>"]
           ["PgDn" "<kbd>PageDown</kbd>"]
           ["Insert" "<kbd>Insert</kbd>"]
           ["INS" "<kbd>Insert</kbd>"]
           ["Pause" "<kbd>Pause</kbd>"]
           ["Break" "<kbd>Break</kbd>"]
           ["PrtScn" "<kbd>PrintScreen</kbd>"]
           ["ps" "<kbd>PrintScreen</kbd>"]
           ["sysrq" "<kbd>SysRq</kbd>"]
           ["scrlk" "<kbd>ScrollLock</kbd>"]
           ["scrolllock" "<kbd>ScrollLock</kbd>"]
           ["Fn" "<kbd>Fn</kbd>"]

           ["Copy" "<kbd>Copy</kbd>"]
           ["Cut" "<kbd>Cut</kbd>"]
           ["Paste" "<kbd>Paste</kbd>"]
           ["Undo" "<kbd>Undo</kbd>"]
           ["Redo" "<kbd>Redo</kbd>"]

           ["F10" "<kbd>F10</kbd>"] ["F11" "<kbd>F11</kbd>"] ["F12" "<kbd>F12</kbd>"] ["F13" "<kbd>F13</kbd>"] ["F14" "<kbd>F14</kbd>"] ["F15" "<kbd>F15</kbd>"] ["F16" "<kbd>F16</kbd>"] ["F17" "<kbd>F17</kbd>"] ["F18" "<kbd>F18</kbd>"] ["F19" "<kbd>F19</kbd>"] ["F20" "<kbd>F20</kbd>"] ["F21" "<kbd>F21</kbd>"] ["F22" "<kbd>F22</kbd>"] ["F23" "<kbd>F23</kbd>"] ["F24" "<kbd>F24</kbd>"]

           ["F1" "<kbd>F1</kbd>"] ["F2" "<kbd>F2</kbd>"] ["F3" "<kbd>F3</kbd>"] ["F4" "<kbd>F4</kbd>"] ["F5" "<kbd>F5</kbd>"] ["F6" "<kbd>F6</kbd>"] ["F7" "<kbd>F7</kbd>"] ["F8" "<kbd>F8</kbd>"] ["F9" "<kbd>F9</kbd>"]

           ["kp0" "<kbd>Keypad 0</kbd>"] ["kp1" "<kbd>Keypad 1</kbd>"] ["kp2" "<kbd>Keypad 2</kbd>"] ["kp3" "<kbd>Keypad 3</kbd>"] ["kp4" "<kbd>Keypad 4</kbd>"] ["kp5" "<kbd>Keypad 5</kbd>"] ["kp6" "<kbd>Keypad 6</kbd>"] ["kp7" "<kbd>Keypad 7</kbd>"] ["kp8" "<kbd>Keypad 8</kbd>"] ["kp9" "<kbd>Keypad 9</kbd>"]

           ["kp+" "<kbd>Keypad +</kbd>"]
           ["kp-" "<kbd>Keypad -</kbd>"]
           ["kp*" "<kbd>Keypad *</kbd>"]
           ["kp/" "<kbd>Keypad /</kbd>"]

           ["Left" "<kbd>←</kbd>"]
           ["Right" "<kbd>→</kbd>"]
           ["Up" "<kbd>↑</kbd>"]
           ["Down" "<kbd>↓</kbd>"]

           ["←" "<kbd>←</kbd>"]
           ["→" "<kbd>→</kbd>"]
           ["↑" "<kbd>↑</kbd>"]
           ["↓" "<kbd>↓</kbd>"]

           ["‹key›" "<kbd>‹key›</kbd>"]

           ["  " "+"]
           ]))

    (save-restriction
      (narrow-to-region @begin @end)
      (xah-html-emacs-to-windows-kbd-notation (point-min) (point-max))

      (xah-replace-pairs-region (point-min) (point-max) $replaceList 'REPORT 'HILIGHT)

      (let ((case-fold-search nil))
        (mapc
         (lambda ($x)
           (goto-char (point-min))
           (while
               (re-search-forward (aref $x 0) nil t)
             (replace-match (aref $x 1) "FIXEDCASE")))
         [
          ["\+\\([^<]\\) \\(.\\) \\(.\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd> <kbd>\\3</kbd>"]
          ["\+\\([^<]\\) \\([A-Za-z0-0]\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd>"]

          ["\+\\([^<]\\)" "+<kbd>\\1</kbd>"]
          ["\+\\(.\\)】" "+<kbd>\\1</kbd>】"]

          [" \\(.\\) " " <kbd>\\1</kbd> "]
          [" \\(.\\)】" " <kbd>\\1</kbd>】"]
          ]))))

  ;; test cases
  ;; 【Ctrl+x a】
  ;; 【Ctrl+x a b】
  ;; 【Ctrl+x Ctrl+j】
  )

(defvar xah-html-html-tag-input-history nil "for input history of `xah-html-wrap-html-tag'")
(setq xah-html-tag-input-history (list))

(defvar xah-html-class-input-history nil "for input history of `xah-html-wrap-html-tag'")
(setq xah-html-class-input-history (list))

(defun xah-html-insert-open-close-tags (@tag @p1 @p2 &optional @class @id )
  "Add HTML open/close tags around region boundary @p1 @p2.
@tag is tag name. @class is class value string. @id is id value string.
version 2019-06-29"
  (let (($isSelfClose (xah-html--tag-self-closing-p @tag)))
    (save-restriction
      (narrow-to-region @p1 @p2)
      (goto-char (point-min))
      (progn
        (insert "<" @tag)
        (when @id (insert " " (format "id=\"%s\"" @id)))
        (when @class (insert " " (format "class=\"%s\"" @class))))
      (if $isSelfClose
          (insert " />" )
        (progn
          (insert ">")
          (goto-char (point-max))
          (insert "</" @tag ">" ))))))

(defun xah-html-wrap-html-tag (@tag &optional @class @id)
  "Insert HTML open/close tags.

Wrap around text selection.
If there's no text selection, the tag will be wrapped around current {word, line, text-block}, depending on the tag used.
If current line or word is empty, then insert open/end tags and place cursor between them.
If `universal-argument' is called first, then also prompt for a “class” attribute and “class”. Empty value means don't add the attribute.
Version 2020-11-11"
  (interactive
   (list
    (ido-completing-read "HTML tag:" xah-html-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "div")
    (when current-prefix-arg (read-string "class:" nil xah-html-class-input-history ))
    (when current-prefix-arg (read-string "id:" "auto" ))))
  (when (string-equal @id "auto") (setq @id (format "id_%05x" (random (1- (expt 16 5))))))
  (let* (
         ($wrap-type (xah-html--get-tag-type @tag))
         ($bds
          (if (use-region-p)
              (cons (region-beginning) (region-end))
            (cond
             ((equal $wrap-type "w") (bounds-of-thing-at-point 'word ))
             ((equal $wrap-type "l") (cons (line-beginning-position) (line-end-position)))
             ((equal $wrap-type "b") (xah-get-bounds-of-thing 'block))
             ((equal $wrap-type "s") (cons (point) (point)))
             (t (cons (point) (point))))))
         $p1 $p2
         )
    (if $bds
        (setq $p1 (car $bds) $p2 (cdr $bds))
      (setq $p1 (point) $p2 (point)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      ;; trim whitespace
      (when
          (and
           (not (use-region-p))
           (or
            (equal $wrap-type "l")
            (equal $wrap-type "b"))
           (not (or
                 (string-equal @tag "pre")
                 (string-equal @tag "code"))))
        (progn
          (goto-char (point-min))
          (delete-horizontal-space)
          (goto-char (point-max))
          (delete-horizontal-space)))
      ;; add blank at start/end
      (when (equal $wrap-type "b")
        (progn
          (goto-char (point-min))
          (insert "\n")
          (goto-char (point-max))
          (insert "\n")))
      (xah-html-insert-open-close-tags
       @tag (point-min) (point-max)
       (if (eq (length @class) 0) nil @class )
       (if (eq (length @id) 0) nil @id )))
    ;; move cursor
    (if (= $p1 $p2)
        (if (xah-html--tag-self-closing-p @tag)
            (skip-chars-forward " \t\n" 100)
          (search-backward "</" ))
      (skip-chars-forward " \t\n"))))

(defun xah-html-insert-wrap-source-code (&optional @lang-code)
  "Insert or wrap a <pre class=\"@lang-code\">…</pre> tags
 to text selection or current text block.
Version 2020-10-22"
  (interactive
   (list
    (ido-completing-read
     "lang code:" (mapcar (lambda (x) (car x)) xah-html-lang-name-map)
     "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "code")))
  (let* (($bds (xah-get-bounds-of-thing-or-region 'block))
         ($p1 (car $bds))
         ($p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (insert (format "<pre class=\"%s\">\n" @lang-code ))
      (goto-char (point-max))
      (insert "\n</pre>")
      (backward-char 7))))

(defun xah-html-mark-unicode (@pos)
  "Wrap a special <mark> tag around the character before cursor.
like this:
 <mark class=\"unicode\" title=\"U+3B1: GREEK SMALL LETTER ALPHA\">α</mark>

If the char is any of 「&」 「<」 「>」, then replace them with 「&amp;」「&lt;」「&gt;」.
If the unicode name contains < or >, such as <CJK IDEOGRAPH-7684>, they are removed.

When called in elisp program, wrap the tag around char before position @pos.

Version 2018-01-12 2020-12-20"
  (interactive (list (point)))
  (let* (
         ($codepoint (string-to-char (buffer-substring-no-properties (- @pos 1) @pos )))
         ($name (if (and
                     (boundp 'describe-char-unicodedata-file)
                     describe-char-unicodedata-file)
                    (progn
                      (require 'descr-text)
                      (car (cdr (car (describe-char-unicode-data  $codepoint)))))
                  (get-char-code-property $codepoint 'name)))
         ($char (buffer-substring-no-properties (- @pos 1) @pos)))
    (goto-char (- @pos 1))
    (insert (format "<mark class=\"unicode\" title=\"U+%X: %s\">" $codepoint (replace-regexp-in-string "<\\|>" "" $name  t t )))
    (right-char 1)
    (insert (format "</mark>"))

    (cond
     ((string-equal $char "&") (search-backward "<" ) (insert "amp;"))
     ((string-equal $char "<") (search-backward "<" ) (delete-char -1) (insert "&lt;"))
     ((string-equal $char ">") (search-backward "<" ) (delete-char -1) (insert "&gt;")))))

(defun xah-html-markup-ruby (&optional @begin @end)
  "Wrap HTML ruby annotation tag on current line or selection.
Chars inside paren are wrapped with “rt” tag.
For example
 abc (xyz)
becomes
 <ruby class=\"ruby88\">abc <rt>xyz</rt></ruby>

When called in lisp code, @begin @end are region begin/end positions.

URL `http://ergoemacs.org/emacs/elisp_html-ruby-annotation-markup.html'
Version 2017-01-11"
  (interactive)
  (progn
    (if (not @begin)
        (progn
          (if (use-region-p)
              (progn
                (setq @begin (region-beginning))
                (setq @end (region-end)))
            (progn
              (setq @begin (line-beginning-position))
              (setq @end (line-end-position)))))
      (progn
        (setq @begin (line-beginning-position))
        (setq @end (line-end-position))))
    (save-restriction
      (narrow-to-region @begin @end)
      (progn
        (goto-char (point-min))
        (while (search-forward "(" nil "move")
          (replace-match "<rt>")))
      (progn
        (goto-char (point-min))
        (while (search-forward ")" nil "move")
          (replace-match "</rt>")))
      (goto-char (point-min))
      (insert "<ruby class=\"ruby88\">")
      (goto-char (point-max))
      (insert "</ruby>"))))

(defun xah-html-clean-whitespace ()
  "Delete redundant whitespace in HTML file.
Work on text selection or whole buffer.
This is heuristic based, does not remove ALL possible redundant whitespace."
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing 'buffer))
         ($p1 (car $bds))
         ($p2 (cdr $bds)))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward " *<p>\n+" nil "move")
            (replace-match "<p>")))))))

(defun xah-html-remove-wikipedia-link ()
  "Delet wikipedia link under cursor or after cursor position.
The link text remains, and is colored red.
Entire link is printed to messages buffer.

Returns the entire link text.

Version 2020-08-22"
  (interactive)
  (let ( $p1 $p2 $wholeLink $p3 $p4 $p5 $p6 $linkTextBegin $linkTextEnd)
    (when (search-forward "</a>" nil "move")
      (progn
        (setq $p2 (point))
        (re-search-backward "<a .*href=")
        (setq $p1 (point))
        (setq $wholeLink (buffer-substring-no-properties $p1 $p2))
        (when (search-forward ".wikipedia.org/wiki/"  $p2 "move")
          (progn
            ;; delete the beginning link tag
            (search-backward "<" )
            (setq $p3 (point))
            (search-forward ">")
            (setq $p4 (point))
            (delete-region $p3 $p4)
            (setq $linkTextBegin (point))
            ;; delete the end link tag
            (search-forward ">")
            (setq $p5 (point))
            (search-backward "<" )
            (setq $p6 (point))
            (delete-region $p5 $p6)
            (setq $linkTextEnd (point))
            (overlay-put (make-overlay $linkTextBegin $linkTextEnd)'font-lock-face '(:foreground "red"))
            (overlay-put (make-overlay $linkTextBegin $linkTextEnd)'face 'bold)
            $wholeLink
            ))))))

(defun xah-html-remove-all-wikipedia-link ()
  "Remove all wikipedia links in buffer of HTML.
Highlight the link text that remains.
Return and print the list of url removed.
Version 2020-08-22"
  (interactive)
  (save-excursion
    (let ($url ($removedTextList '()))
      (goto-char (point-min))
      ;; remove url like this https://en.wikipedia.org/wiki/United_States
      ;; but not https://en.wikipedia.org/wiki/File:QWERTY-home-keys-position.svg
      (while (search-forward "wikipedia.org/wiki" nil "move")
        (setq $url (thing-at-point 'url))
        (when (and (not (string-match "File:" $url ))
                   (not (string-match "Image:" $url )))
          (xah-html-remove-wikipedia-link)
          (push $url $removedTextList)))
      (mapc (lambda (x) (princ x) (terpri)) $removedTextList)
      $removedTextList
      )))

(defun xah-html-remove-uri-fragment (@href-value)
  "remove URL @href-value fragment, anything after first 「#」 char, including the #.
See also `xah-html-split-uri-hashmark'
Version 2017-08-15"
  ;; test
  ;; (xah-html-remove-uri-fragment "a#b") ; "a"
  ;; (xah-html-remove-uri-fragment "#3") ; ""
  ;; (xah-html-remove-uri-fragment "4") ; "4"
  ;; (xah-html-remove-uri-fragment "#") ; ""
  ;; (xah-html-remove-uri-fragment "") ; ""
  ;; (if (string-match "#" xx )
  ;;     (substring xx 0 (match-beginning 0))
  ;;   xx )
  (let (($x (string-match-p "#" @href-value )))
    (if $x
        (substring @href-value 0 $x)
      @href-value )))

(defun xah-html-split-uri-hashmark (@href-value)
  "Split a URL @href-value by 「#」 char, return a vector.
 e.g. \"y.html#z\" ⇒ [\"y.html\", \"#z\"]

Examples:
 「a#b」 ⇒ 「a」 「#b」
 「#」 ⇒ 「」 「#」
 「#3」 ⇒ 「」 「#3」
 「3#」 ⇒ 「3」 「#」
 「4」 ⇒  「4」 「」
 「」 ⇒  「」 「」

See also: `xah-remove-uri-fragment'
Version 2017-08-15"
  ;; test
  ;; (xah-html-split-uri-hashmark "a#b") ; ["a" "#b"]
  ;; (xah-html-split-uri-hashmark "#3") ; ["" "#3"]
  ;; (xah-html-split-uri-hashmark "#") ; ["" "#"]
  ;; (xah-html-split-uri-hashmark "4") ; ["4" ""]
  ;; (xah-html-split-uri-hashmark "") ; ["" ""]
  (let (($x (string-match-p "#" @href-value )))
    (if $x
        (vector (substring @href-value 0 $x) (substring @href-value $x))
      (vector @href-value "" ))))

(defun xah-html-url-percent-decode-string (string)
  "Returns string URL percent decoded.
Example:

http://example.org/%28D%C3%BCrer%29
↓
http://example.org/(Dürer)

http://example.org/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
↓
http://example.org/文本编辑器

URL `http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html'
Version 2018-10-26 2021-01-14"
  (require 'url-util)
  (decode-coding-string (url-unhex-string string) 'utf-8))

(defun xah-html-decode-percent-encoded-url ()
  "Decode percent encoded URL of current line or selection.

Example:
 %28D%C3%BCrer%29
becomes
 (Dürer)

Example:
 %E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
 文本编辑器

URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'
Version 2018-10-26 2021-01-14"
  (interactive)
  (let ( $p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-unhex-string $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "percent-decode no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert (decode-coding-string $newStr 'utf-8))))))

(defun xah-html-browse-url-of-buffer ()
  "Like `browse-url-of-buffer' but save file first.

Then, if `universal-argument' is called, visit the corresponding xahsite URL.
For example, if current buffer is of this file:
 ~/web/xahlee_info/index.html
then after calling this function,
default browser will be launched and opening this URL:
 http://xahlee.info/index.html
Version 2020-06-26"
  (interactive)
  (let (($url
         (if current-prefix-arg
             (when (fboundp 'xahsite-filepath-to-url) (xahsite-filepath-to-url (buffer-file-name)))
           (buffer-file-name))))
    (when (buffer-modified-p )
      (when (fboundp 'xah-clean-whitespace) (xah-clean-whitespace))
      (save-buffer))
    (browse-url $url )))

(defun xah-html-open-in-chrome-browser ()
  "Open the current file or `dired' marked files in Google Chrome browser.
Work in Windows, macOS, linux.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-10"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (format "open -a /Applications/Google\\ Chrome.app \"%s\"" $fpath)))
         $file-list))
       ((string-equal system-type "windows-nt")
        ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
        (let ((process-connection-type nil))
          (mapc
           (lambda ($fpath)
             (start-process "" nil "powershell" "start-process" "chrome" $fpath ))
           $file-list)))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath)
           (shell-command (format "google-chrome-stable \"%s\"" $fpath)))
         $file-list))))))

(defun xah-html-open-link-in-chrome ()
  "Open url under cursor in Google Chrome.
Work in Windows, macOS, linux.
Version 2019-11-10"
  (interactive)
  (let* (($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗〘〙«»‹›❮❯❬❭〘〙·。\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a Google\\ Chrome.app \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
      (let ((process-connection-type nil))
        (start-process "" nil "powershell" "start-process" "chrome" $path )))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "google-chrome-stable \"%s\"" $path))))))

(defun xah-html-open-in-chrome ()
  "Open current buffer or link under cursor in Google Chrome browser.
If cursor position is link/url, open that link. Else, open current buffer.
Version 2020-08-04"
  (interactive)
  (let ((filePath (thing-at-point 'filename )))
    (if (and filePath (string-match "^http\\|html$" filePath ))
        (xah-html-open-link-in-chrome)
      (xah-html-open-in-chrome-browser))))

(defun xah-html-open-in-brave ()
  "Open the current file or `dired' marked files in Brave browser.
If the file is not saved, save it first.
Version 2019-11-10"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command (format "open -a 'Brave Browser.app' \"%s\"" $fpath)))
         $file-list))
       ((string-equal system-type "windows-nt")
        ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
        (let ((process-connection-type nil))
          (mapc
           (lambda ($fpath)
             (start-process "" nil "powershell" "start-process" "brave" $fpath ))
           $file-list)))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath)
           (shell-command (format "brave \"%s\"" $fpath)))
         $file-list))))))

(defun xah-html-open-link-in-brave (&optional @fullpath)
  "open url under cursor in Brave browser.
Work in Mac OS only
Version 2019-02-17"
  (interactive)
  (let ($path)
    (if @fullpath
        (progn (setq $path @fullpath))
      (let (($inputStr
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let ($p0 $p1 $p2
                         ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗〘〙«»‹›❮❯❬❭〘〙·。\\"))
                 (setq $p0 (point))
                 (skip-chars-backward $pathStops)
                 (setq $p1 (point))
                 (goto-char $p0)
                 (skip-chars-forward $pathStops)
                 (setq $p2 (point))
                 (goto-char $p0)
                 (buffer-substring-no-properties $p1 $p2)))))
        (setq $path (replace-regexp-in-string
                     "^file:///" "/"
                     (replace-regexp-in-string
                      ":\\'" "" $inputStr)))))
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a 'Brave Browser.app' \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
      (let ((process-connection-type nil))
        (start-process "" nil "powershell" "start-process" "brave" $path )))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "brave \"%s\"" $path))))))

(defun xah-html-open-link-in-firefox (&optional @fullpath)
  "open url under cursor in Firefox browser.
Work in Windows, macOS. 2019-11-09 linux not yet.
Version 2019-11-09"
  (interactive)
  (let ($path)
    (if @fullpath
        (progn (setq $path @fullpath))
      (let (($inputStr
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let ($p0 $p1 $p2
                         ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗〘〙«»‹›❮❯❬❭〘〙·。\\"))
                 (setq $p0 (point))
                 (skip-chars-backward $pathStops)
                 (setq $p1 (point))
                 (goto-char $p0)
                 (skip-chars-forward $pathStops)
                 (setq $p2 (point))
                 (goto-char $p0)
                 (buffer-substring-no-properties $p1 $p2)))))
        (setq $path (replace-regexp-in-string
                     "^file:///" "/"
                     (replace-regexp-in-string
                      ":\\'" "" $inputStr)))))
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a 'Firefox.app' \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
      (let ((process-connection-type nil))
        (start-process "" nil "powershell" "start-process" "firefox" $path )))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "firefox \"%s\"" $path))))))

(defun xah-html-open-in-firefox ()
  "Open current link or buffer or marked files in Firefox browser.
If cursor position is link/url, open that link. Else, open current buffer. But if in dired, open marked files or current file.
Version 2020-11-30"
  (interactive)
  (if (string-equal major-mode "dired-mode")
      (let (($file-list (dired-get-marked-files)))
        (cond
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (when (buffer-modified-p )
               (save-buffer))
             (shell-command
              (format "open -a firefox.app \"%s\"" $fpath))) $file-list)))
        ;;
        )
    (let (($linkPath (thing-at-point 'filename )))
      (if (and $linkPath (string-match "^http\\|html$" $linkPath ))
          (xah-html-open-link-in-firefox)
        (let (($path (buffer-file-name)))
          (when (buffer-modified-p ) (save-buffer))
          (cond
           ((string-equal system-type "darwin")
            (shell-command (format "open -a 'Firefox.app' \"%s\"" $path)))
           ((string-equal system-type "windows-nt")
            ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
            (let ((process-connection-type nil))
              (start-process "" nil "powershell" "start-process" "firefox" (concat "file:///" $path))))
           ((string-equal system-type "gnu/linux")
            (shell-command (format "firefox \"%s\"" $path)))))))))

(defun xah-html-open-in-safari ()
  "Open the current file or `dired' marked files in Mac's Safari browser.
If the file is not saved, save it first.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2018-02-26"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (when (buffer-modified-p )
             (save-buffer))
           (shell-command
            (format "open -a Safari.app \"%s\"" $fpath))) $file-list))))))

(defun xah-html-open-link-in-safari ()
  "open url under cursor in Safari.
Work in Mac OS only
Version 2019-02-09"
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗〘〙«»‹›❮❯❬❭〘〙·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (shell-command
     (format "open -a Safari.app \"%s\"" $path))))

(defun xah-html-encode-percent-encoded-url ()
  "Percent encode URL in current line or selection.

Example:
    http://example.org/(Dürer)
becomes
    http://example.org/(D%C3%BCrer)

Example:
    http://example.org/文本编辑器
becomes
    http://example.org/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8

URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'
Version 2018-10-26"
  (interactive)
  (let ($p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-encode-url $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert $newStr)))))

;; HHH___________________________________________________________________

(defun xah-html-abbrev-enable-function ()
  "Return t if not in string or comment. Else nil.
This is for abbrev table property `:enable-function'.
Version 2016-10-24"
  (let (($syntax-state (syntax-ppss)))
    (not (or (nth 3 $syntax-state) (nth 4 $syntax-state)))))

;; (defun xah-html-abbrev-enable-function ()
;;   "Return t, always.
;; This is for abbrev table property `:enable-function'.
;; Version 2019-05-29"
;;   t)

(defun xah-html-expand-abbrev ()
  "Expand the symbol before cursor,
if cursor is not in string or comment.
Returns the abbrev symbol if there's a expansion, else nil.
Version 2017-01-13"
  (interactive)
  (when (xah-html-abbrev-enable-function) ; abbrev property :enable-function doesn't seem to work, so check here instead
    (let ( ($p0 (point))
           $p1 $p2
           $abrStr
           $abrSymbol
           )
      (save-excursion
        (forward-symbol -1)
        (setq $p1 (point))
        (goto-char $p0)
        (setq $p2 $p0))
      (setq $abrStr (buffer-substring-no-properties $p1 $p2))
      (setq $abrSymbol (abbrev-symbol $abrStr))
      (if $abrSymbol
          (progn
            (abbrev-insert $abrSymbol $abrStr $p1 $p2 )
            (xah-html--abbrev-position-cursor $p1)
            $abrSymbol)
        nil))))

(defun xah-html--abbrev-position-cursor (&optional @pos)
  "Move cursor back to ▮ if exist, else put at end.
Return true if found, else false.
Version 2016-10-24"
  (interactive)
  (let (($found-p (search-backward "▮" (if @pos @pos (max (point-min) (- (point) 100))) t )))
    (when $found-p (delete-char 1))
    $found-p
    ))

(defun xah-html--ahf ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “ahf” stand for abbrev hook function.
Version 2016-10-24"
  t)

(put 'xah-html--ahf 'no-self-insert t)

(setq xah-html-mode-abbrev-table nil)
(define-abbrev-table 'xah-html-mode-abbrev-table
  '(

    ("cdata" "<![CDATA[▮]]>" xah-html--ahf)

    ("cl" "class=\"▮\"" xah-html--ahf)
    ("idh" "id=\"▮\"" xah-html--ahf)
    ("posterh" "poster=\"▮\"" xah-html--ahf)

    ("wid" "width" xah-html--ahf)
    ("hei" "height" xah-html--ahf)
    ("bgc" "background-color" xah-html--ahf)
    ("bgc" "background-color" xah-html--ahf)
    ("br" "<br />\n" xah-html--ahf)
    ("hr" "<hr />\n\n" xah-html--ahf)
    ("divh" "<div id=\"x\" class=\"x\">\n▮</div>\n" xah-html--ahf)
    ("spanh" "<span id=\"x\" class=\"x\">▮</span>\n" xah-html--ahf)
    ("ph" "<p class=\"x\">▮</p>\n\n" xah-html--ahf)
    ("buttonh" "<button id=\"button18646\" class=\"x\" type=\"button\">Click Me</button>\n\n" xah-html--ahf)

    ("cssh" "<link rel=\"stylesheet\" href=\"lbasic.css\" />")
    ("styleh" "<style type=\"text/css\">\np {line-height:130%}\n</style>")
    ("targeth" "target=\"_blank\"")
    ("iframe" "<iframe src=\"some.html\" width=\"200\" height=\"300\"></iframe>")

    ("og" "<meta property=\"og:image\" content=\"http://ergoemacs.org/emacs/i/geek_vs_non_geek_repetitive_tasks.png\" />" xah-html--ahf)

    ("html4s" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" xah-html--ahf)
    ("html4t" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" xah-html--ahf)
    ("xhtmlh" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" xah-html--ahf)
    ("htmlh" "<!doctype html><html><head><meta charset=\"utf-8\" />
<meta name=viewport content=\"width=device-width, initial-scale=1\">

<title>ttt</title>
</head>
<body>

<h1>ttt</h1>

</body>
</html>" xah-html--ahf))

  "abbrev table for `xah-html-mode'"
  )

(abbrev-table-put xah-html-mode-abbrev-table :case-fixed t)
(abbrev-table-put xah-html-mode-abbrev-table :system t)
(abbrev-table-put xah-html-mode-abbrev-table :enable-function 'xah-html-abbrev-enable-function)

;; HHH___________________________________________________________________
;; keybinding

(defvar xah-html-mode-map nil "Keybinding for `xah-html-mode'")
(progn
  (setq xah-html-mode-map (make-sparse-keymap))
  (define-key xah-html-mode-map (kbd "RET") 'xah-html-open-local-link)
  (define-key xah-html-mode-map (kbd "TAB") 'xah-html-wrap-html-tag)
  (define-key xah-html-mode-map (kbd "<C-return>") 'xah-html-insert-br-tag)
  (define-prefix-command 'xah-html-leader-map)
  (define-key xah-html-leader-map (kbd "<backspace>") 'xah-html-remove-html-tags)
  (define-key xah-html-leader-map (kbd "<right>") 'xah-html-skip-tag-forward)
  (define-key xah-html-leader-map (kbd "<left>") 'xah-html-skip-tag-backward)
  (define-key xah-html-leader-map (kbd "'") 'xah-html-encode-percent-encoded-url)
  (define-key xah-html-leader-map (kbd ",") 'xah-html-decode-percent-encoded-url)
  (define-key xah-html-leader-map (kbd ".") 'xah-html-escape-char-to-entity)
  (define-key xah-html-leader-map (kbd ";") 'xah-html-emacs-to-windows-kbd-notation)
  (define-key xah-html-leader-map (kbd "1") 'xah-html-get-precode-make-new-file)
  (define-key xah-html-leader-map (kbd "2") 'xah-html-toggle-syntax-coloring-markup)
  (define-key xah-html-leader-map (kbd "4") 'xah-html-markup-ruby)
  (define-key xah-html-leader-map (kbd "5") 'xah-html-mark-unicode)
  (define-key xah-html-leader-map (kbd "6") 'xah-html-html-to-text)
  (define-key xah-html-leader-map (kbd "9") 'xah-html-redo-syntax-coloring-buffer)
  (define-key xah-html-leader-map (kbd "0") 'xah-html-dehtmlize-pre-code-buffer)
  ;; a
  (define-key xah-html-leader-map (kbd "b") 'xah-html-rename-source-file-path)
  (define-key xah-html-leader-map (kbd "c") 'xah-html-lines-to-list)
  (define-key xah-html-leader-map (kbd "d") 'xah-html-extract-url)
  (define-key xah-html-leader-map (kbd "e") 'xah-html-source-url-linkify)
  (define-key xah-html-leader-map (kbd "f") 'xah-html-image-linkify)
  (define-key xah-html-leader-map (kbd "g") 'xah-html-brackets-to-html)
  (define-key xah-html-leader-map (kbd "h") 'xah-html-any-linkify)
  (define-key xah-html-leader-map (kbd "i") 'nil)
  (define-key xah-html-leader-map (kbd "i c") 'xah-html-resize-img)
  (define-key xah-html-leader-map (kbd "i t") 'xah-html-convert-to-jpg)
  (define-key xah-html-leader-map (kbd "i h") 'xah-html-image-to-link)
  (define-key xah-html-leader-map (kbd "j") 'xah-html-url-linkify)
  (define-key xah-html-leader-map (kbd "k") 'xah-html-htmlize-keyboard-shortcut-notation)
  (define-key xah-html-leader-map (kbd "l") 'xah-html-image-figure-linkify)
  (define-key xah-html-leader-map (kbd "m") 'xah-html-insert-wrap-source-code)
  (define-key xah-html-leader-map (kbd "n") 'xah-html-update-title)
  (define-key xah-html-leader-map (kbd "o") 'nil)
  (define-key xah-html-leader-map (kbd "p") 'xah-html-browse-url-of-buffer)
  (define-key xah-html-leader-map (kbd "q") 'xah-html-make-link-defunct)
  (define-key xah-html-leader-map (kbd "r") 'nil)
  (define-key xah-html-leader-map (kbd "s") 'xah-html-lines-to-dl)
  (define-key xah-html-leader-map (kbd "t") 'xah-html-wrap-p-tag)
  (define-key xah-html-leader-map (kbd "u") 'xah-html-delete-tag-pair)
  (define-key xah-html-leader-map (kbd "v") 'xah-html-lines-to-table)
  (define-key xah-html-leader-map (kbd "w") 'nil)
  (define-key xah-html-leader-map (kbd "w w") 'xah-html-named-entity-to-char)
  (define-key xah-html-leader-map (kbd "w c") 'xah-html-open-in-chrome)
  (define-key xah-html-leader-map (kbd "w f") 'xah-html-open-in-firefox)
  (define-key xah-html-leader-map (kbd "w s") 'xah-html-open-in-safari)
  (define-key xah-html-leader-map (kbd "w b") 'xah-html-open-in-brave)
  (define-key xah-html-leader-map (kbd "x") 'xah-html-escape-char-to-unicode)
  (define-key xah-html-leader-map (kbd "y") 'xah-html-make-citation)
  (define-key xah-html-leader-map (kbd "z") 'xah-html-table-to-lines)
  (define-key xah-html-leader-map (kbd "SPC") 'nil)
  (define-key xah-html-leader-map (kbd "SPC SPC") 'xah-html-insert-date-section)
  ;; define separate, so that user can override the lead key
  (define-key xah-html-mode-map (kbd "C-c C-c") xah-html-leader-map))

;; HHH___________________________________________________________________

(defvar xah-html-mode-syntax-table nil "Syntax table for `xah-html-mode'.")

(setq xah-html-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?! "." synTable)
        (modify-syntax-entry ?# "." synTable)

        (modify-syntax-entry ?$ "." synTable)

        (modify-syntax-entry ?% "." synTable)
        (modify-syntax-entry ?& "." synTable)
        (modify-syntax-entry ?' "." synTable)
        (modify-syntax-entry ?* "." synTable)
        (modify-syntax-entry ?+ "." synTable)
        (modify-syntax-entry ?, "." synTable)
        (modify-syntax-entry ?- "_" synTable)
        (modify-syntax-entry ?. "." synTable)
        (modify-syntax-entry ?/ "." synTable)
        (modify-syntax-entry ?: "." synTable)
        (modify-syntax-entry ?\; "." synTable)
        (modify-syntax-entry ?< "." synTable)
        (modify-syntax-entry ?= "." synTable)
        (modify-syntax-entry ?> "." synTable)
        (modify-syntax-entry ?? "." synTable)
        (modify-syntax-entry ?@ "." synTable)

        (modify-syntax-entry ?\" "\"" synTable)
        ;; (modify-syntax-entry ?\" "." synTable)

        (modify-syntax-entry ?\\ "\\" synTable)

        (modify-syntax-entry ?^ "." synTable)
        (modify-syntax-entry ?_ "_" synTable)
        (modify-syntax-entry ?` "." synTable)
        (modify-syntax-entry ?| "." synTable)
        (modify-syntax-entry ?~ "." synTable)

        (modify-syntax-entry ?{ "(}" synTable)
        (modify-syntax-entry ?} "){" synTable)

        (modify-syntax-entry ?\( "()" synTable)
        (modify-syntax-entry ?\) ")(" synTable)

        (modify-syntax-entry ?\[ "(]" synTable)
        (modify-syntax-entry ?\] ")[" synTable)

        (modify-syntax-entry ?‹ "(›" synTable)
        (modify-syntax-entry ?› ")‹" synTable)

        (modify-syntax-entry ?« "(»" synTable)
        (modify-syntax-entry ?» ")«" synTable)

        (modify-syntax-entry ?“ "(”" synTable)
        (modify-syntax-entry ?” ")“" synTable)

        (modify-syntax-entry ?‘ "(’" synTable)
        (modify-syntax-entry ?’ ")‘" synTable)

        synTable)
)

(defface xah-html-double-curly-quote-f
  '((t :foreground "black"
       :background "aquamarine"
       ))
  "Face used for curly quoted text."
  :group 'xah-html-mode )

(face-spec-set
 'xah-html-double-curly-quote-f
 '((t :foreground "black"
      :background "aquamarine"
      ))
 'face-defface-spec
 )

(defface xah-html-single-curly-quote-f
  '((t :foreground "red"
       :background "aquamarine"
       ))
  "Face used for curly quoted text."
  :group 'xah-html-mode )

(face-spec-set
 'xah-html-single-curly-quote-f
 '((t :foreground "red"
      :background "aquamarine"
      ))
 'face-defface-spec
 )

(defface xah-html-french-quote-f
  '((t :foreground "DarkMagenta"
       :background "aquamarine"
       ))
  "Face used for « French quoted » text."
  :group 'xah-html-mode )

(face-spec-set
 'xah-html-french-quote-f
 '((t :foreground "DarkMagenta"
      :background "aquamarine"
      ))
 'face-defface-spec
 )

(defface xah-html-span-f
  '(
    (t :background "pink"))
  "face for span tag content."
  :group 'xah-html-mode )

;; temp for debugging
(face-spec-set
 'xah-html-span-f
 '(
   (t :background "pink"))
 'face-defface-spec
 )

(defface xah-html-mark-f
  '(
    (t :background "yellow"))
  "face for mark tag content."
  :group 'xah-html-mode )

;; temp for debugging
(face-spec-set
 'xah-html-mark-f
 '(
   (t :background "yellow"))
 'face-defface-spec
 )

(setq xah-html-font-lock-keywords
      (let (
            (htmlElementNamesRegex (regexp-opt xah-html-html5-tag-list 'words))
            (htmlAttributeNamesRegexp (regexp-opt xah-html-attribute-names 'words))
            (htmlBooleanAttributeNamesRegexp (regexp-opt xah-html-boolean-attribute-names 'words))
            (cssPropertieNames (regexp-opt xah-css-property-names 'words))
            (cssValueNames (regexp-opt xah-css-value-kwds 'words))
            (cssColorNames (regexp-opt xah-css-color-names 'words))
            (cssUnitNames (regexp-opt xah-css-unit-names 'words))

  ;              (attriRegex " *= *\"\\([ -_a-z]*?\\)\"")
  ;              (attriRegex " +\\(?:[ =\"-_a-z]*?\\)") ; one or more attributes
            (attriRegex " +\\(?:[^<>]*?\\)") ; one or more attributes

  ;              (textNodeRegex "\\([ [:graph:]]+?\\)")
            (textNodeRegex "\\([^<]+?\\)") ; ← hack, to avoid multi-line

            )
        `(

          ("<!--\\|-->" . font-lock-comment-delimiter-face)

          ;; todo these multiline regex are very slow when there are long lines.
          (,(format "<!--%s-->" textNodeRegex) . (1 font-lock-comment-face))
          (,(format "<h\\([1-6]\\)>%s</h\\1>" textNodeRegex) . (2 'bold))

          (,(format "<title>%s</title>" textNodeRegex) . (1 'bold))
          (,(format "<span>%s</span>" textNodeRegex) . (1 'xah-html-span-f))
          (,(format "<span%s>%s</span>" attriRegex textNodeRegex) . (1 'xah-html-span-f))
          (,(format "<mark>%s</mark>" textNodeRegex) . (1 'xah-html-mark-f))
          (,(format "<mark%s>%s</mark>" attriRegex textNodeRegex) . (1 'xah-html-mark-f))
          (,(format "<b>%s</b>" textNodeRegex) . (1 'bold))
          (,(format "<b%s>%s</b>" attriRegex textNodeRegex) . (1 'bold))

          (,(format "“%s”" textNodeRegex) . 'xah-html-double-curly-quote-f)

          ;; (,"=\"\\([-_/.A-Za-z0-9]+?\\)\"" . (1 'font-lock-string-face))

          ;; ("<[^>]+?>" . font-lock-function-name-face)
          ;; ("</?[A-Za-z0-9]+>" . font-lock-function-name-face)

          ;; (,(concat "</\\(" htmlElementNamesRegex "\\) *>") . (1 font-lock-function-name-face))
          ;; (,(concat "<\\(" htmlElementNamesRegex "\\).*?>") . (1 font-lock-function-name-face))

          (,htmlElementNamesRegex . font-lock-function-name-face)

          ;; (,(concat " +\\(" htmlAttributeNamesRegexp "\\) *= *['\"]") . (1 font-lock-variable-name-face))
          (,htmlAttributeNamesRegexp . font-lock-variable-name-face)

          (,htmlBooleanAttributeNamesRegexp . font-lock-constant-face)

          (,cssPropertieNames . font-lock-type-face)
          (,cssValueNames . font-lock-keyword-face)
          (,cssColorNames . font-lock-preprocessor-face)
          (,cssUnitNames . font-lock-reference-face))))

;; HHH___________________________________________________________________

;;;###autoload
(define-derived-mode
  xah-html-mode
  fundamental-mode
  "∑html"
  "A simple major mode for HTML5.
HTML5 keywords are colored.

URL `http://ergoemacs.org/emacs/xah-html-mode.html'

\\{xah-html-mode-map}"

  (setq font-lock-defaults '((xah-html-font-lock-keywords)))

  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")

  (make-local-variable 'abbrev-expand-function)
  (if (version< emacs-version "24.4")
      (add-hook 'abbrev-expand-functions 'xah-html-expand-abbrev nil t)
    (setq abbrev-expand-function 'xah-html-expand-abbrev))

  (xah-html-display-page-break-as-line)
  (abbrev-mode 1)

  ;; (xah-html--display-hr-as-line)

  :group 'xah-html-mode
  )

(add-to-list 'auto-mode-alist '("\\.html\\'" . xah-html-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . xah-html-mode))

(provide 'xah-html-mode)

;;; xah-html-mode.el ends here
