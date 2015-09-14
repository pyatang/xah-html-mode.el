;;; xah-html-mode.el --- Major mode for editing pure html5.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.0.4
;; Created: 12 May 2012
;; Keywords: languages, html, web
;; Homepage: http://ergoemacs.org/emacs/xah-html-mode.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:
;; Major mode for editing pure HTML5 files.
;; home page: http://ergoemacs.org/emacs/xah-html-mode.html

(require 'xah-replace-pairs)
(require 'htmlize)

(progn
  ;; part of emacs
  (require 'ido)
  (require 'sgml-mode)
  (require 'browse-url)
  (require 'url-util)
  (require 'hi-lock) ; uses its face definitions
  )

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")



(defvar xah-html--month-full-names '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") "list of English month full names.")

(defvar xah-html--month-abbrev-names (mapcar (lambda (x) (substring x 0 3)) xah-html--month-full-names) "list of English month 3-letter abbrev names.")

(defvar xah-html--weekday-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") "list of English weekday full names.")

(defun xah-html--is-datetimestamp-p (φinput-string)
  "Return t if φinput-string is a date/time stamp, else nil.
This is based on heuristic, so it's not 100% correct.
If the string contains any month names, weekday names, or of the form dddd-dd-dd, dddd-dd-dddd, dddd-dd-dd, or using slash, then it's considered a date.
"
  (cond
         ((string-match (regexp-opt (append xah-html--month-full-names xah-html--month-abbrev-names xah-html--weekday-names) 'words) φinput-string) t)
         ;; mm/dd/yyyy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]\\b" φinput-string) t)
         ;; yyyy/mm/dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" φinput-string) t)
         ;; mm/dd/yy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" φinput-string) t)
         ;; mm-dd-yyyy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]\\b" φinput-string) t)
         ;; yyyy-mm-dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" φinput-string) t)
         ;; mm-dd-yy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" φinput-string) t)
         (t nil) ))



(defun xah-html--trim-string (φstring)
  "Remove white spaces in beginning and ending of φstring.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).

Note: in emacs GNU Emacs 24.4+ and later, there's `string-trim' function. You need to (require 'subr-x).
"
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" φstring)))

(defun xah-html--get-thing-at-cursor (φunit)
  "Return the string and boundary of ΦUNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

ΦUNIT can be:

• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.

• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, anything doesn't have white spaces in it.

• 'line — delimited by “\\n”. (captured text does not include “\\n”.)

• 'block — delimited by empty lines or beginning/end of buffer. Lines with just spaces or tabs are also considered empty line. (captured text does not include a ending “\\n”.)

• 'buffer — whole buffer. (respects `narrow-to-region')

• 'filepath — delimited by chars that's USUALLY not part of filepath.

• 'url — delimited by chars that's USUALLY not part of URL.

• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
 (setq bds (xah-html--get-thing-at-cursor 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:

• This function returns the text and the 2 boundaries as a vector in one shot.

• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.

• This function's behavior does not depend on syntax table. e.g. for units 「'word」, 「'block」, etc."
  (let (p1 p2)
    (save-excursion
      (cond
       ( (eq φunit 'word)
         (let ((wordcharset "-A-Za-z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
           (skip-chars-backward wordcharset)
           (setq p1 (point))
           (skip-chars-forward wordcharset)
           (setq p2 (point))))

       ( (eq φunit 'glyphs)
         (progn
           (skip-chars-backward "[:graph:]")
           (setq p1 (point))
           (skip-chars-forward "[:graph:]")
           (setq p2 (point))))

       ((eq φunit 'buffer)
        (progn
          (setq p1 (point-min))
          (setq p2 (point-max))))

       ((eq φunit 'line)
        (progn
          (setq p1 (line-beginning-position))
          (setq p2 (line-end-position))))
       ((eq φunit 'block)
        (progn
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq p1 (point)))
            (setq p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq p2 (point)))
            (setq p2 (point)))))

       ((eq φunit 'filepath)
        (let (p0)
          (setq p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
          (setq p2 (point))))

       ((eq φunit 'url)
        (let (p0
              ;; (ξdelimitors "^ \t\n,()[]{}<>〔〕“”\"`'!$^*|\;")
              (ξdelimitors "!\"#$%&'*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"))
          (setq p0 (point))
          (skip-chars-backward ξdelimitors) ;"^ \t\n,([{<>〔“\""
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward ξdelimitors) ;"^ \t\n,)]}<>〕\"”"
          (setq p2 (point))))

       ((vectorp φunit)
        (let (p0)
          (setq p0 (point))
          (skip-chars-backward (elt φunit 0))
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward (elt φunit 1))
          (setq p2 (point))))))

    (vector (buffer-substring-no-properties p1 p2) p1 p2 )))

(defun xah-html--get-thing-or-selection (φunit)
  "Return the string and boundary of text selection or ΦUNIT under cursor.

If `use-region-p' is true, then the region is the φunit.  Else,
it depends on the ΦUNIT. See `xah-html--get-thing-at-cursor' for detail about
ΦUNIT.

Returns a vector [text a b], where text is the string and a and b
are its boundary.

Example usage:
 (setq bds (xah-html--get-thing-or-selection 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )"
  (interactive)
  (if (use-region-p)
      (let ((p1 (region-beginning)) (p2 (region-end)))
        (vector (buffer-substring-no-properties p1 p2) p1 p2 ))
    (xah-html--get-thing-at-cursor φunit)))


(defcustom xah-html-html5-tag-names nil
  "A alist of HTML5 tag names. For each element, the key is tag name, value is a vector of one element of string: “w” means word, “l” means line, “b” means block, others are placeholder for unknown. The purpose of the value is to indicate the default way to wrap the tag around cursor. "
; todo: need to go the the list and look at the type carefully. Right now it's just quickly done. lots are “z”, for unkown. Also, some are self closing tags, current has mark of “n”.
:group 'xah-html-mode
)
(setq xah-html-html5-tag-names
'(
  ;; most frequently used should be on top. todo: reorder based on use. do a cache or such, look at ido-switch-buffer

("div" . ["b"])
("span" . ["w"])
("br" . ["n"])

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
("dl" . ["l"])
("dt" . ["l"])

("img" . ["l"])
("input" . ["l"])
("ins" . ["z"])
("hr" . ["n"])

("a" . ["l"])
("blockquote" . ["b"])
("cite" . ["l"])
("code" . ["l"])

("nav" . ["l"])

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
("figcaption" . ["l"])
("figure" . ["b"])
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
("meta" . ["z"])
("meter" . ["z"])
("noscript" . ["l"])
("object" . ["z"])
("optgroup" . ["z"])
("option" . ["z"])
("output" . ["z"])
("param" . ["z"])
("progress" . ["z"])
("q" . ["w"])
("samp" . ["z"])
("script" . ["b"])
("section" . ["b"])
("select" . ["z"])
("source" . ["z"])
("style" . ["l"])
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
("u" . ["w"])
("ul" . ["b"])
("video" . ["l"])
("wbr" . ["z"])

)
 )

(defvar xah-html-html5-tag-list nil "list version of `xah-html-html5-tag-names'")
(setq xah-html-html5-tag-list (mapcar (lambda (x) (car x)) xah-html-html5-tag-names))

(defcustom xah-html-attribute-names nil
  "HTML5 attribute names."
:group 'xah-html-mode)
(setq xah-html-attribute-names '( "id" "class" "style" "title" "href" "type" "rel" "http-equiv" "content" "charset" "alt" "src" "width" "height" "controls" "autoplay" "preload" "name" "value" "size" "async" "defer" ))

(defcustom xah-html-html5-self-close-tags nil
  "List of HTML5 self-closing tag name. "
  :group 'xah-html-mode )
(setq xah-html-html5-self-close-tags '( "area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "track" "wbr"))

(defvar xah-html-css-color-names nil "List of CSS color names.")
(setq xah-html-css-color-names
'("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet" "brown" "burlywood" "cadetblue" "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk" "crimson" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray" "darkgreen" "darkgrey" "darkkhaki" "darkmagenta" "darkolivegreen" "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen" "darkslateblue" "darkslategray" "darkslategrey" "darkturquoise" "darkviolet" "deeppink" "deepskyblue" "dimgray" "dimgrey" "dodgerblue" "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro" "ghostwhite" "gold" "goldenrod" "gray" "green" "greenyellow" "grey" "honeydew" "hotpink" "indianred" "indigo" "ivory" "khaki" "lavender" "lavenderblush" "lawngreen" "lemonchiffon" "lightblue" "lightcoral" "lightcyan" "lightgoldenrodyellow" "lightgray" "lightgreen" "lightgrey" "lightpink" "lightsalmon" "lightseagreen" "lightskyblue" "lightslategray" "lightslategrey" "lightsteelblue" "lightyellow" "lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise" "mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin" "navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange" "orangered" "orchid" "palegoldenrod" "palegreen" "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue" "purple" "red" "rosybrown" "royalblue" "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell" "sienna" "silver" "skyblue" "slateblue" "slategray" "slategrey" "snow" "springgreen" "steelblue" "tan" "teal" "thistle" "tomato" "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow" "yellowgreen")
 )

(defvar xah-html-css-property-names nil "List of CSS property names.")
(setq xah-html-css-property-names
      '(
        "background" "background-color" "background-image" "background-position" "background-repeat" "border" "border-bottom" "border-collapse" "border-color" "border-left" "border-radius" "border-top" "box-shadow" "clear" "color" "content" "cursor" "direction" "display" "filter" "float" "font-family" "font-size" "font-style" "font-weight" "height" "line-height" "list-style" "list-style-image" "list-style-type" "margin" "margin-bottom" "margin-left" "margin-right" "margin-top" "max-width" "min-width" "opacity" "orphans" "overflow" "padding" "padding-left" "padding-right" "padding-top" "page-break-after" "page-break-inside" "position" "pre-wrap" "table" "table-cell" "text-align" "text-decoration" "unicode-bidi" "vertical-align" "white-space" "widows" "width" "word-wrap" "z-index"
        ))

(defvar xah-html-css-unit-names nil "List of CSS unite names.")
(setq xah-html-css-unit-names '("px" "pt" "pc" "cm" "mm" "in" "em" "ex" "%"))

(defvar xah-html-css-value-kwds nil "List of CSS value names")
(setq xah-html-css-value-kwds
      '(
        "!important" "absolute" "alpha" "auto" "avoid" "block" "bold" "both" "bottom" "break-word" "center" "collapse" "dashed" "dotted" "embed" "fixed" "help" "hidden" "hsl" "hsla" "inherit" "inline" "inline-block" "italic" "large" "left" "ltr" "middle" "monospace" "no-repeat" "none" "normal" "nowrap" "pointer" "relative" "rgb" "rgba" "right" "rtl" "sans-serif" "serif" "small" "smaller" "solid" "square" "static" "thin" "top" "transparent" "underline" "url" "x-large" "xx-large"
        ))



(defun xah-html-get-tag-type (φtag-name)
  "Return the wrap-type info of φtag-name in `xah-html-html5-tag-names'"
  (elt
   (cdr
    (assoc φtag-name xah-html-html5-tag-names)
    ) 0))

(defvar xah-html-lang-name-map nil "a alist that maps lang name. Each element has this form 「(‹lang code› . [‹emacs major mode name› ‹file extension›])」")
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

        ("clojure" . ["clojure-mode" "clj"])
        ("css" . ["css-mode" "css"])
        ("emacs-lisp" . ["xah-elisp-mode" "el"])
        ("haskell" . ["haskell-mode" "hs"])
        ("html" . ["html-mode" "html"])
        ("mysql" . ["sql-mode" "sql"])
        ("xml" . ["sgml-mode" "xml"])
        ("html6" . ["xah-html6-mode" "html6"])
        ("java" . ["java-mode" "java"])
        ("js" . ["xah-js-mode" "js"])
        ("nodejs" . ["xah-js-mode" "js"])
        ("lsl" . ["xlsl-mode" "lsl"])
        ("latex" . ["latex-mode" "txt"])
        ("ocaml" . ["tuareg-mode" "ocaml"])
        ("perl" . ["cperl-mode" "pl"])
        ("php" . ["php-mode" "php"])
        ("povray" . ["pov-mode" "pov"])
        ("powershell" . ["powershell-mode" "ps1"])
        ("python" . ["python-mode" "py"])
        ("python3" . ["python-mode" "py3"])
        ("qi" . ["shen-mode" "qi"])
        ("ruby" . ["ruby-mode" "rb"])
        ("scala" . ["scala-mode" "scala"])
        ("apl" . ["gnu-apl-mode" "apl"])
        ("scheme" . ["scheme-mode" "scm"])
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
        ("scss" . ["css-mode" "css"])

        ("vimrc" . ["vimrc-mode" "vim"])))

(defvar xah-html-lang-name-list nil "List of langcode.")
(setq xah-html-lang-name-list (mapcar 'car xah-html-lang-name-map))

(defun xah-html-precode-htmlized-p (φp1 φp2)
  "Return true if region φp1 φp2 is htmlized code.
WARNING: it just losely check if it contains span tag."
  (progn
    (goto-char φp1)
    (re-search-forward "<span class=" φp2 "NOERROR")))

(defun xah-html-get-precode-langCode ()
  "Get the langCode and position boundary of current HTML pre block.
A pre block is text of this form
 <pre class=\"‹langCode›\">…▮…</pre>.
Your cursor must be between the tags.

Returns a vector [langCode pos1 pos2], where pos1 pos2 are the boundary of the text content."
  (interactive)
  (let (ξlangCode ξp1 ξp2)
    (save-excursion
      (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
      (setq ξlangCode (match-string 1))
      (setq ξp1 (search-forward ">")) ; text content begin
      (backward-char 1)
      (xah-html-skip-tag-forward)
      (setq ξp2 (search-backward "</pre>")) ; text content end
      (vector ξlangCode ξp1 ξp2))))

(defun xah-html-get-precode-make-new-file (φlang-name-map)
  "Create a new file in current dir with content from text inside pre code block.
For example, if the cursor is somewhere between the tags:
<pre class=\"ruby\">print 7</pre>

after calling, a new file of name
xxtemp.201508234945.8292.rb
  is created in current dir, with content “print 7”.
The numbers in the file name are datestamp and a random integer.
If file already exist, emacs will ask to overwrite.

If there's a text selection, use that region as content.
Version 2015-08-23"
  (interactive (list xah-html-lang-name-map))
  (let* (
         (ξlangcodeinfo (xah-html-get-precode-langCode))
         (ξlangCode (elt ξlangcodeinfo 0))
         (ξp1 (elt ξlangcodeinfo 1))
         (ξp2 (elt ξlangcodeinfo 2))
         (ξtextContent (buffer-substring-no-properties ξp1 ξp2))
         (ξfileSuffix (elt (cdr (assoc ξlangCode φlang-name-map)) 1))
         (ξmajorMode (elt (cdr (assoc ξlangCode φlang-name-map)) 0))
         ξfname
         (ξbuf (generate-new-buffer "untitled")))

    (progn
      (split-window-below)
      (delete-region ξp1 ξp2 )
      (switch-to-buffer ξbuf)
      (funcall (intern ξmajorMode))
      (setq buffer-offer-save t)
      (insert ξtextContent)
      (when (xah-html-precode-htmlized-p (point-min) (point-max))
        (xah-html-remove-span-tag-region (point-min) (point-max))))

    (if (equal ξfileSuffix "java")
        (progn
          (goto-char (point-min))
          (if (search-forward-regexp "public class \\([A-Za-z0-9]+\\) [\n ]*{" nil 'NOERROR)
              (progn
                (setq ξfname
                      (format "%s.java" (match-string 1))))
            (progn (setq ξfname
                         (format "%s.%s.%d.%s"
                                 "xxtemp"
                                 (format-time-string "%Y%m%d%M%S")
                                 (random 99999)
                                 ξfileSuffix)))))
      (progn
        (setq ξfname
              (format "%s.%s.%d.%s"
                      "xxtemp"
                      (format-time-string "%Y%m%d%M%S")
                      (random 99999)
                      ξfileSuffix))))
    (write-file ξfname "CONFIRM")
    (goto-char (point-min))))



(defun xah-html-htmlize-string (φsource-code-str φmajor-mode-name)
  "Take φsource-code-str and return a htmlized version using major mode φmajor-mode-name.
The purpose is to syntax color source code in HTML.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive)
  (let (htmlizeOutputBuffer ξresultStr)
    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert φsource-code-str)
      (funcall (intern φmajor-mode-name))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuffer (htmlize-buffer)))
    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuffer
      (let (ξp1 ξp2 )
        (setq ξp1 (search-forward "<pre>"))
        (setq ξp2 (search-forward "</pre>"))
        (setq ξresultStr (buffer-substring-no-properties (+ ξp1 1) (- ξp2 6)))))
    (kill-buffer htmlizeOutputBuffer)
    ξresultStr ))

(defun xah-html-langcode-to-major-mode-name (φlang-code φlang-code-map)
  "get the `major-mode' name associated with φlang-code."
  (interactive)
  (elt (cdr (assoc φlang-code φlang-code-map)) 0))

(defun xah-html-htmlize-precode (φlang-code-map)
  "Replace text enclosed by “pre” tag to htmlized code.

For example, if the cursor is inside the pre tags <pre class=\"‹langCode›\">…▮…</pre>, then after calling, the text inside the pre tag will be htmlized. That is, wrapped with many span tags for syntax coloring.

The opening tag must be of the form <pre class=\"‹langCode›\">.  The ‹langCode› determines what emacs mode is used to colorize the text. See `xah-html-lang-name-map' for possible ‹langCode›.

Cursor will end up right before </pre>.

See also: `xah-html-dehtmlize-precode', `xah-html-toggle-syntax-coloring-markup'.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive (list xah-html-lang-name-map))
  (let (ξlangCode ξp1 ξp2 ξmodeName )
    (let* (
           (t78730 (xah-html-get-precode-langCode))
           (ξlangCode (elt t78730 0))
           (ξp1 (elt t78730 1))
           (ξp2 (elt t78730 2))
           ;; (ξmodeName (elt (cdr (assoc ξlangCode φlang-code-map)) 0))
           (ξmodeName (xah-html-langcode-to-major-mode-name ξlangCode φlang-code-map)))
      (xah-html-htmlize-region ξp1 ξp2 ξmodeName t))))

(defun xah-html-htmlize-region (φp1 φp2 φmode-name &optional φtrim-whitespace-boundary?)
  "Htmlized region φp1 φp2 using `major-mode' φmode-name.

This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive
   (list (region-beginning)
         (region-end)
         (ido-completing-read "Chose mode for coloring:" (mapcar 'cdr auto-mode-alist))))
  (let* (
         (ξinput-str (buffer-substring-no-properties φp1 φp2))
         (ξout-str
          (xah-html-htmlize-string (if φtrim-whitespace-boundary?
                                       (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" ξinput-str))
                                     ξinput-str
                                     ) φmode-name)))

    (if (string= ξinput-str ξout-str)
        nil
      (progn
        (delete-region φp1 φp2)
        (insert ξout-str)))))

(defun xah-html-dehtmlize-precode (φp1 φp2)
  "Delete span tags between pre tags.
Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.
This command does the inverse of `xah-html-htmlize-precode'."
  (interactive
   (let* ( (t55238 (xah-html-get-precode-langCode)))
     (list (elt t55238 1) (elt t55238 2))))
  (save-restriction
    (narrow-to-region φp1 φp2)
    (xah-html-remove-span-tag-region (point-min) (point-max))
    (xah-html-code-tag-to-brackets (point-min) (point-max))))

(defun xah-html-toggle-syntax-coloring-markup (φlang-name-map)
  "Call `xah-html-htmlize-precode' or `xah-html-dehtmlize-precode'."
  (interactive (list xah-html-lang-name-map))
  (let* (
         (ξt34342 (xah-html-get-precode-langCode))
         (ξp1 (elt ξt34342 1))
         (ξp2 (elt ξt34342 2)))
    (if (xah-html-precode-htmlized-p ξp1 ξp2)
        (progn
          (xah-html-dehtmlize-precode ξp1 ξp2))
      (progn
        (xah-html-htmlize-precode φlang-name-map)))))

(defun xah-html-redo-syntax-coloring-buffer (&optional φlang-code)
  "redo all pre lang code syntax coloring in current html page."
  (interactive)
  (let (ξlangCode ξp1 ξp2 (ξi 0))

    ;; (ξsearch-string
    ;;  (if φlang-code
    ;;      (progn (format "<pre class=\"\\([-A-Za-z0-9]+\\)\">" φlang-code))
    ;;    (progn "<pre class=\"\\([-A-Za-z0-9]+\\)\">")))

    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward "<pre class=\"\\([-A-Za-z0-9]+\\)\">" nil "NOERROR")
        (setq ξlangCode (match-string 1))
        (setq ξp1 (point))
        (backward-char 1)
        (xah-html-skip-tag-forward)
        (search-backward "</pre>")
        (setq ξp2 (point))
        (save-restriction
          (narrow-to-region ξp1 ξp2)
          (xah-html-dehtmlize-precode (point-min) (point-max))
          (xah-html-htmlize-region (point-min) (point-max) (xah-html-langcode-to-major-mode-name ξlangCode xah-html-lang-name-map) t)
          (setq ξi (1+ ξi)))))
    (message "xah-html-redo-syntax-coloring-buffer %s redone" ξi)))


;; syntax table
(defvar xah-html-syntax-table nil "Syntax table for `xah-html-mode'.")

(setq xah-html-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\n "> b" synTable)
        (modify-syntax-entry ?\! "." synTable)
        (modify-syntax-entry ?\" "\"" synTable)
        (modify-syntax-entry ?\# "." synTable)
        (modify-syntax-entry ?\$ "." synTable)
        (modify-syntax-entry ?\% "." synTable)
        (modify-syntax-entry ?\& "." synTable)
        (modify-syntax-entry ?\' "." synTable)
        (modify-syntax-entry ?\( "()" synTable)
        (modify-syntax-entry ?\) ")(" synTable)
        (modify-syntax-entry ?\* "." synTable)
        (modify-syntax-entry ?\+ "." synTable)
        (modify-syntax-entry ?\, "." synTable)
        (modify-syntax-entry ?\- "_" synTable)
        (modify-syntax-entry ?\. "." synTable)
        (modify-syntax-entry ?\/ "." synTable)
        (modify-syntax-entry '(?0 . ?9) "w" synTable)
        (modify-syntax-entry ?\: "." synTable)
        (modify-syntax-entry ?\; "." synTable)
        (modify-syntax-entry ?\< "." synTable)
        (modify-syntax-entry ?\= "." synTable)
        (modify-syntax-entry ?\> "." synTable)
        (modify-syntax-entry ?\? "." synTable)
        (modify-syntax-entry ?\@ "." synTable)
        (modify-syntax-entry '(?A . ?Z) "w" synTable)
        (modify-syntax-entry ?\[ "(]" synTable)
        (modify-syntax-entry ?\\ "\\" synTable)
        (modify-syntax-entry ?\] ")[" synTable)
        (modify-syntax-entry ?^ "." synTable) ; can't use blackslash, because it became control
        (modify-syntax-entry ?\_ "_" synTable)
        (modify-syntax-entry ?\` "." synTable)
        (modify-syntax-entry '(?a . ?z) "w" synTable)
        (modify-syntax-entry ?\{ "(}" synTable)
        (modify-syntax-entry ?\| "." synTable)
        (modify-syntax-entry ?\} "){" synTable)
        (modify-syntax-entry ?\~ "." synTable)

        (modify-syntax-entry ?“ "(”" synTable)
        (modify-syntax-entry ?” ")“" synTable)

        (modify-syntax-entry ?‘ "(’" synTable)
        (modify-syntax-entry ?’ ")‘" synTable)

        synTable)
)



(defface xah-html-curly“”-quoted-text-face
  '((((class color) (min-colors 88) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)

(defface xah-html-curly‘’-quoted-text-face
  '((((class color) (min-colors 88) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)



(defun xah-html-tag-self-closing-p (φtag-name)
  "Return true if the tag is a self-closing tag, ⁖ br."
  (interactive)
  (member φtag-name  xah-html-html5-self-close-tags))

(defun xah-html-cursor-in-tag-markup? (&optional φbracketPositions)
  "Return true if cursor is inside a tag markup.
For example,
 <p class=\"…\">…</p>
 If cursor is between the beginning p or ending p markup.
 φbracketPositions is optional. If nil, then
 `xah-html-get-bracket-positions' is called to get it."
  (interactive)
  (let ( pl< pl> pr> pr< )
    (when (not φbracketPositions)
      (progn
        (setq φbracketPositions (xah-html-get-bracket-positions))
        (setq pl< (elt φbracketPositions 0))
        (setq pl> (elt φbracketPositions 1))
        (setq pr< (elt φbracketPositions 2))
        (setq pr> (elt φbracketPositions 3))))
    (if (and (< pl> pl<) (< pr> pr<))
        (progn (message "%s" "yes") t)
      (progn (message "%s" "no") nil))))

(defun xah-html-end-tag? (&optional φbracketPositions)
  "Return t if cursor is inside a begin tag, else nil.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
 It simply check if the left brack is followed by a slash or not.

φbracketPositions is optional. If nil, then
 `xah-html-get-bracket-positions' is called to get it.
"
  (let ( pl< pl> pr> pr< )
    (when (not φbracketPositions)
      (progn
        (setq φbracketPositions (xah-html-get-bracket-positions))
        (setq pl< (elt φbracketPositions 0))
        (setq pl> (elt φbracketPositions 1))
        (setq pr< (elt φbracketPositions 2))
        (setq pr> (elt φbracketPositions 3))))
    (goto-char pl<)
    (forward-char 1)
    (looking-at "/" )))

(defun xah-html-get-tag-name (&optional φleft<)
  "Return the tag name.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
"
  (let ( ξp1 ξp2 )
    (when (not φleft<)
      (setq φleft< (search-backward "<")))
    (goto-char φleft<)
    (forward-char 1)
    (when (looking-at "/" )
      (forward-char 1))
    (setq ξp1 (point))
    (search-forward-regexp " \\|>")
    (backward-char 1)
    (setq ξp2 (point))
    (buffer-substring-no-properties ξp1 ξp2)))

(defun xah-html-get-bracket-positions ()
  "Returns html angle bracket positions.
Returns a vector [ pl< pl> pr< pr> ]
 pl< is the position of < nearest to cursor on the left side
 pl> is the position of > nearest to cursor on the left side
 similar for pr< and pr> for the right side.

this command does not `save-excursion'. You need to call that.
"
  ;; search for current tag.
  ;; find left nearest >, and right nearest <
  ;; or left nearest <, and right nearest >
  ;; determine if it's <…> or >…<.
  (let (
        (p-current (point))
        pl< ; position of first < char to the left of cursor
        pl>
        pr<
        pr>
        )
    (progn
      (goto-char p-current)
      (setq pl< (search-backward "<" nil "NOERROR"))
      (goto-char p-current)
      (setq pl> (search-backward ">" nil "NOERROR"))
      (goto-char p-current)
      (setq pr< (search-forward "<" nil "NOERROR"))
      (goto-char p-current)
      (setq pr> (search-forward ">" nil "NOERROR"))
      (vector pl< pl> pr< pr>))))

(defun xah-html-delete-tag ()
  "work in progress. do nothing.
Delete the tag under cursor.
Also delete the matching beginning/ending tag."
  (interactive)
  (save-excursion
    ;; determine if it's inside the tag. ⁖ <…>
    ;; if so, good. else abort.
    ;; now, determine if it's opening tag or closing. ⁖ closing tag start with </
    ;; if it's opening tag, need to delete the matching one to the right
    ;; else, need to delete the matching one to the left
    ;; let's assume it's the opening.
    ;; now, determine if there's nested element. ⁖ <p>…<b>…</b>…</p>
    ;;    to do this, first determine the name of the tag. ⁖ the “p” in  <p …>, then search the matching tag.
    ;; if so, O shit, it's complex. Need to determine if one of the nested has the same tag name. and and …
    ;; if not, then we can proceed. Just find the closing tag and delete it. Also the beginning.
    (let ( )
      (if (xah-html-cursor-in-tag-markup?)
          (progn
            (if (xah-html-end-tag?)
                (progn (message "end %s" (xah-html-get-tag-name)))
              (progn (message "begin %s" (xah-html-get-tag-name)))))
        (progn (message "%s" "cursor needs to be inside a tag."))))))

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
this is a quick 1 min hackjob, works only when there's no nesting."
  (interactive)
  (let (ξp1 ξp2 oldTagName newTagName oldClassName newClassName)
    (search-backward "<" )
    (forward-char 1)
    (setq ξp1 (point))
    (setq oldTagName (xah-html-get-tag-name))
    (setq newTagName (ido-completing-read "HTML tag:" xah-html-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "span"))
    (goto-char ξp1)
    (delete-char (length oldTagName))
    (insert newTagName)
    (search-forward (concat "</" oldTagName))
    (delete-char (- (length oldTagName)))
    (insert newTagName)

    (progn
      (goto-char ξp1)
      (search-forward ">")
      (setq ξp2  (point))
      (goto-char ξp1)
      (when
          (search-forward-regexp "class[ \n]*=[ \n]*\"" ξp2 "NOERROR")
  ;(string-match "class[ \n]*=[ \n]*\"" (buffer-substring-no-properties ξp1 ξp2))
        (progn
          (setq ξp1 (point))
          (search-forward "\"")
          (setq ξp2 (- (point) 1))
          (setq oldClassName (buffer-substring-no-properties ξp1 ξp2))
          (setq newClassName (read-string "new class name:"))
          (if (string-equal newClassName "")
              (progn ; todo need to clean this up. don't use bunch of user functions
                (delete-region ξp1 ξp2 )
                (backward-kill-word 1)
                (delete-char -1))
            (progn (delete-region ξp1 ξp2 )
                   (goto-char ξp1)
                   (insert newClassName))))))))

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

(defun xah-html-replace-html-chars-to-entities (φp1 φp2 &optional φentity-to-char-p)
  "Replace HTML chars & < > to HTML entities on current line or selection.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

If `universal-argument' is called, the replacement direction is reversed. That is &amp; ⇒ & etc.

When called in lisp code, φp1 φp2 are region begin/end positions.
If φentity-to-char-p is true, change entities to chars instead.

See also: `xah-html-replace-html-named-entities', `xah-html-replace-html-chars-to-unicode'

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2015-04-23"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) (if current-prefix-arg t nil))
     (list (line-beginning-position) (line-end-position) (if current-prefix-arg t nil))))
  (if φentity-to-char-p
      (xah-replace-pairs-region φp1 φp2 '( ["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">"] ))
    (xah-replace-pairs-region φp1 φp2 '( ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ))))

(defun xah-html-replace-html-chars-to-unicode (φp1 φp2)
  "Replace HTML < > & to Unicode chars 〈 〉 ＆ on the current line or text selection.

When called in lisp code, φp1 φp2 are region begin/end positions.
If φentity-to-char-p is true, change entities to chars instead.

See also:
`xah-html-replace-html-named-entities'
`xah-html-replace-html-chars-to-entities'

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2015-04-23"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-restriction
    (narrow-to-region φp1 φp2)
    (goto-char (point-min))
    (while (search-forward "&" nil t) (replace-match "＆" nil t))
    (goto-char (point-min))
    (while (search-forward "<" nil t) (replace-match "〈" nil t))
    (goto-char (point-min))
    (while (search-forward ">" nil t) (replace-match "〉" nil t))))

(defun xah-html-replace-html-named-entities (φp1 φp2)
  "Replace HTML entities to Unicode character in current line or selection.
For example, “&copy;” becomes “©”.

The following HTML Entities are not replaced:
 &amp; &
 &lt; <
 &gt; >

When called in lisp code, φp1 φp2 are region begin/end positions.

See also:
`xah-html-replace-html-chars-to-entities'
`xah-html-replace-html-chars-to-unicode'

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2015-04-23"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let (
        (ξreplaceMap
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
      (narrow-to-region φp1 φp2)
      (let ( (case-fold-search nil))
        (mapc
         (lambda (ξx)
           (goto-char (point-min))
           (while (search-forward (elt ξx 0) nil t)
             (replace-match (elt ξx 1) 'FIXEDCASE 'LITERAL)))
         ξreplaceMap)))))

(defun xah-html-get-html-file-title (φfname &optional φno-error-p)
  "Return φfname <title> tag's text.
Assumes that the file contains the string “<title>…</title>”. If not, and if φno-error-p is true, then return nil."
  (with-temp-buffer
    (insert-file-contents φfname nil nil nil t)
    (goto-char 1)
    (if (search-forward "<title>" nil φno-error-p)
        (buffer-substring-no-properties
         (point)
         (- (search-forward "</title>") 8))
      nil
      )))

(defun xah-html-lines-to-html-list ()
  "Make the current block of lines into a HTML list.
Any URL in the line will be turned into links.

Example:
If your cursor is in the following block of text:

Castratos are castrated males made for singing: http://en.wikipedia.org/wiki/Castrato , record of the last castrato: http://www.archive.org/details/AlessandroMoreschi
human vocal range: http://en.wikipedia.org/wiki/Vocal_range

It will become:
<ul>
<li>Castratos are castrated males made for singing: <a href=\"http://en.wikipedia.org/wiki/Castrato\">Castrato</a> , record of the last castrato: <a href=\"http://www.archive.org/details/AlessandroMoreschi\">http://www.archive.org/details/AlessandroMoreschi</a></li>
<li>human vocal range: <a href=\"http://en.wikipedia.org/wiki/Vocal_range\">Vocal range</a></li>
</ul>"
  (interactive)
  (let (ξbds ξp1 ξp2 ξinput-str ξresultStr)
    (setq ξbds (xah-html--get-thing-or-selection 'block))
    (setq ξinput-str (elt ξbds 0) ξp1 (elt ξbds 1) ξp2 (elt ξbds 2))
    (save-excursion
      (setq ξresultStr
            (with-temp-buffer
              (insert ξinput-str)
              (delete-trailing-whitespace)
              (goto-char 1)
              (while
                  (search-forward-regexp  "\.html$" nil t)
                (backward-char 1)
                (xah-all-linkify))

              (goto-char 1)
              (while
                  (not (equal (line-end-position) (point-max)))
                (beginning-of-line) (insert "<li>")
                (end-of-line) (insert "</li>")
                (forward-line 1 ))

              (beginning-of-line) (insert "<li>")
              (end-of-line) (insert "</li>")

              (goto-char 1)
              (insert "<ul>\n")
              (goto-char (point-max))
              (insert "\n</ul>")

              (buffer-string))))
    (delete-region ξp1 ξp2)
    (insert ξresultStr)))

(defun xah-html-make-html-table-string (φtextBlock φdelimiter)
  "Transform the string TEXTBLOCK into a HTML marked up table.

 “\\n” is used as delimiter of rows. Extra newlines at the end is discarded.
The argument φdelimiter is a char used as the delimiter for columns.

 See the parent function `xah-html-make-html-table'."
(let ((txtbk φtextBlock))
    (setq txtbk (replace-regexp-in-string "\n+$" "\n" (concat txtbk "\n"))) ; make sure ending is just one newline char
    (setq txtbk (replace-regexp-in-string φdelimiter "</td><td>" txtbk))
    (setq txtbk (replace-regexp-in-string "\n" "</td></tr>\n<tr><td>" txtbk))
    (setq txtbk (substring txtbk 0 -8)) ; delete the beginning “<tr><td>” in last line
    (concat "<table class=\"nrm\">\n<tr><td>" txtbk "</table>")
))

(defun xah-html-make-html-table (φsep)
  "Transform the current text block or selection into a HTML table.

If there's a text selection, use the selection as input.
Otherwise, used current text block delimited by empty lines.

ΦSEP is a string used as a delimitor for columns.

For example:

a*b*c
1*2*3
this*and*that

with “*” as separator, becomes

<table class=\"nrm\">
<tr><td>a</td><td>b</td><td>c</td></tr>
<tr><td>1</td><td>2</td><td>3</td></tr>
<tr><td>this</td><td>and</td><td>that</td></tr>
</table>"
  (interactive "sEnter string pattern for column separation:")
  (let (ξbds ξp1 ξp2 ξstr)

    (setq ξbds (xah-html--get-thing-or-selection 'block))
    (setq ξstr (elt ξbds 0))
    (setq ξp1 (elt ξbds 1))
    (setq ξp2 (elt ξbds 2))
    (delete-region ξp1 ξp2)
    (insert (xah-html-make-html-table-string ξstr φsep) "\n")))

(defun xah-html-make-html-table-undo ()
  "inverse of `xah-html-make-html-table'."
  (interactive)
  (let ( ξp1 ξp2 ξstr)
    (search-backward "<table")
    (setq ξp1 (point))
    (search-forward "</table>")
    (setq ξp2 (point))

    (xah-replace-regexp-pairs-region ξp1 ξp2 [
                                        ["<table \\([^>]+?\\)>" ""]
                                        ["</th><th>" "•"]
                                        ["</td><td>" "•"]
                                        ["<tr>" ""]
                                        ["</tr>" ""]
                                        ["</table>" ""]
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
  (let (ξp0 ξp1 ξp2 ξlinkText)
    (if (region-active-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (progn
        (setq ξp0 (point))
        (skip-chars-backward "^ \t\n")
        (setq ξp1 (point))
        (goto-char ξp0)
        (skip-chars-forward "^ \t\n")
        (setq ξp2 (point))))
    (setq ξlinkText
          (replace-regexp-in-string "_" " " (buffer-substring-no-properties ξp1 ξp2)))
    (delete-region ξp1 ξp2)
    (insert (concat "<a href=\"http://en.wikipedia.org/wiki/"
                    (replace-regexp-in-string " " "_" ξlinkText)
                    "\">" ξlinkText "</a>"))))

(defun xah-html-remove-span-tag-region (φp1 φp2)
  "Delete HTML “span” tags in region.
And the following HTML entities are changed:
 &amp; ⇒ &
 &lt; ⇒ <
 &gt; ⇒ >

Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.

When done, the cursor is placed at φp2."
  (interactive "r")
  (save-restriction
    (narrow-to-region φp1 φp2)
    (xah-replace-regexp-pairs-region (point-min) (point-max) '(["<span class=\"[^\"]+\">" ""]))
    (xah-replace-pairs-region (point-min) (point-max) '( ["</span>" ""] ))
    (xah-html-replace-html-chars-to-entities (point-min) (point-max) "ΦENTITY-TO-CHAR-P")
    (goto-char (point-max))))

(defun xah-html-code-tag-to-brackets (φp1 φp2 &optional φchange-entity-p)
  "Change HTML code tags to brackets in text selection or current text block.

 <code>…</code>
and
 <code class=\"…\">…</code>
are changed to
「…」

<var class=\"…\">…</var>
 is changed to
 ‹…›

The html entities &amp; &lt; &gt; are changed to & < >.

if `universal-argument' is called first, don't convert the html entities.

When done, the cursor is placed at φp2.

when called in lisp program,
φp1 φp2 are region begin/end.
If φchange-entity-p is true, convert html entities to char.
"
  (interactive
   (let ((ξbds (xah-html--get-thing-or-selection 'block)))
     (list (elt ξbds 1) (elt ξbds 2) (if current-prefix-arg nil t))))

  (save-restriction
    (narrow-to-region φp1 φp2)
    (xah-replace-regexp-pairs-region (point-min) (point-max) '(["<code class=\"[^\"]+\">" "「"] ["<var class=\"[^\"]+\">" "‹"]))
    (xah-replace-pairs-region
     (point-min) (point-max)
     '(
       ["<code>" "「"]
       ["</code>" "」"]
       ["<var>" "‹"]
       ["</var>" "›"] ))
    (when φchange-entity-p (xah-html-replace-html-chars-to-entities (point-min) (point-max) "ΦENTITY-TO-CHAR-P"))
    (goto-char (point-max))))

(defun xah-html-remove-html-tags (φstring &optional φfrom φto)
  "Delete HTML tags in string or region.
Work on current text block or text selection. (a “text block” is text between blank lines)

When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions φfrom φto.

WARNING: this command does not cover all HTML tags or convert all HTML entities. For robust solution you might use: 「lynx -dump -display_charset=utf-8 URL」."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((ξbds (xah-html--get-thing-or-selection 'block)))
       (list nil (elt ξbds 1) (elt ξbds 2)))))

  (let (ξwork-on-string-p ξinput-str ξoutput-str)
    (setq ξwork-on-string-p (if φstring t nil))
    (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))
    (setq ξoutput-str
          (let ((case-fold-search t) (tempStr ξinput-str))

            (setq tempStr
                  (xah-replace-regexp-pairs-in-string
                   tempStr
                   '(
                     ["<script>\\([^\\<]+?\\)</script>" ""]
                     ["<[^>]+?>" ""]
                     ["</[a-z0-9]+>" ""]
                     ["&amp;" "&"]
                     ["&lt;" "<"]
                     ["&gt;" ">"]
                     )

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

                   ))

            tempStr
            ))

    (if ξwork-on-string-p
        ξoutput-str
      (save-excursion
        (delete-region φfrom φto)
        (goto-char φfrom)
        (insert ξoutput-str)))))

;; (defun xah-html-html-to-text (φstring &optional φfrom φto)
;; "Convert html to plain text on text selection or current text block."
;;   (interactive
;;    (if (region-active-p)
;;        (list nil (region-beginning) (region-end))
;;      (let ((ξbds (xah-html--get-thing-or-selection 'block)) )
;;        (list nil (elt ξbds 1) (elt ξbds 2))) ) )

;;   (let (ξwork-on-string-p ξinput-str ξoutput-str)
;;     (setq ξwork-on-string-p (if φstring t nil))
;;     (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))
;;     (setq ξoutput-str
;;           (let ((case-fold-search t) (tempStr ξinput-str))
;; (setq tempStr (xah-replace-regexp-pairs-in-string tempStr '(
;; ["<script>\\([^\\<]+?\\)</script>" ""]
;; ["<li>" "<li>• " ]
;; ["<h2>" "────────── ────────── ────────── ────────── ──────────
;; <h2>" ]
;; ["<h3>" "────────── ────────── ──────────
;; <h3>" ]
;; ["<h4>" "────────── ──────────
;; <h4>" ]
;; ["<a +href=\"\\([^\"]+?\\)\" *>\\([^<]+?\\)</a>" "\\2 〔 \\1 〕"]
;; ["<img +src=\"\\([^\"]+?\\)\" +alt=\"\\([^\"]+?\\)\" +width=\"[0-9]+\" +height=\"[0-9]+\" */?>" "〔IMAGE “\\2” \\1 〕"]
;; )))
;; tempStr
;;  ) )

;; (setq ξoutput-str (xah-html-remove-html-tags ξoutput-str) )

;;     (if ξwork-on-string-p
;;         ξoutput-str
;;       (save-excursion
;;         (delete-region φfrom φto)
;;         (goto-char φfrom)
;;         (insert ξoutput-str) )) ) )

(defun xah-html-html-to-text (φstring &optional φfrom φto)
  "Convert html to plain text on text selection or current text block."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((ξbds (xah-html--get-thing-or-selection 'block)))
       (list nil (elt ξbds 1) (elt ξbds 2)))))

  (let (ξwork-on-string-p ξinput-str ξoutput-str)
    (setq ξwork-on-string-p (if φstring t nil))
    (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))

    (setq ξoutput-str
          (with-temp-buffer
            (insert ξinput-str)
            (goto-char 1)
            (let ((case-fold-search nil))
              (xah-replace-pairs-region 1 (point-max)
                                    '(
                                      ["<script>\\([^\\<]+?\\)</script>" ""]
                                      ["<li>" "<li>• " ]
                                      ["<h2>" "────────── ────────── ────────── ────────── ──────────
<h2>" ]
                                      ["<h3>" "────────── ────────── ──────────
<h3>" ]
                                      ["<h4>" "────────── ──────────
<h4>" ]
                                      ["<a +href=\"\\([^\"]+?\\)\" *>\\([^<]+?\\)</a>" "\\2 〔 \\1 〕"]
                                      ["<img +src=\"\\([^\"]+?\\)\" +alt=\"\\([^\"]+?\\)\" +width=\"[0-9]+\" +height=\"[0-9]+\" */?>" "〔IMAGE “\\2” \\1 〕"]
                                      )))
            (buffer-substring 1 (point-max))))

    (setq ξoutput-str (xah-html-remove-html-tags ξoutput-str))

    (if ξwork-on-string-p
        ξoutput-str
      (save-excursion
        (delete-region φfrom φto)
        (goto-char φfrom)
        (insert ξoutput-str)))))

(defun xah-html-extract-url (φp1 φp2 &optional φconvert-relative-url-p)
  "Extract URLs in current block or region to `kill-ring'.

If `universal-argument' is called first, convert relative URL to full path.

This command extracts all text of the forms
 <‹letter› … href=\"…\" …>
 <‹letter› … src=\"…\" …>
that is on a a single line, by regex. The quote may be single quote.

When called in lisp code, φp1 φp2 are region begin/end positions.
Returns a list.

URL `http://ergoemacs.org/emacs/elisp_extract_url_command.html'
Version 2015-03-20"
  (interactive
   (let (ξp1 ξp2)
     ;; set region boundary ξp1 ξp2
     (if (use-region-p)
         (progn (setq ξp1 (region-beginning))
                (setq ξp2 (region-end)))
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq ξp1 (point)))
           (setq ξp1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq ξp2 (point)))
           (setq ξp2 (point)))))
     (list ξp1 ξp2 current-prefix-arg)))

  (let ((ξregionText (buffer-substring-no-properties φp1 φp2))
        (ξurlList (list)))
    (with-temp-buffer
      (insert ξregionText)

      (goto-char 1)
      (while (re-search-forward "<" nil t)
        (replace-match "\n<" "FIXEDCASE" "LITERAL"))

      (goto-char 1)
      (while (re-search-forward
              "<[A-Za-z]+.+?\\(href\\|src\\)[[:blank:]]*?=[[:blank:]]*?\\([\"']\\)\\([^\"']+?\\)\\2" nil t)
        (push (match-string 3) ξurlList)))
    (setq ξurlList (reverse ξurlList))

    (when φconvert-relative-url-p
      (setq ξurlList
            (mapcar
             (lambda (ξx)
               (if (string-match "^http" ξx )
                   (progn ξx)
                 (progn
                   (expand-file-name ξx (file-name-directory (buffer-file-name))))))
             ξurlList)))

    (when (called-interactively-p 'any)
      (let ((ξprintedResult (mapconcat 'identity ξurlList "\n")))
        (kill-new ξprintedResult)
        (message "%s" ξprintedResult)))
    ξurlList ))

(defun xah-html-update-title ( φnewTitle)
  "Update a HTML article's title and h1 tags.
Update the <title>…</title> and <h1>…</h1> of current buffer."
  (interactive
   (let (oldTitle)
     (save-excursion
       (goto-char 1)
       (search-forward-regexp "<title>\\([^<]+?\\)</title>")
       (setq oldTitle (match-string 1 )))
     (list (read-string "New title:" oldTitle nil oldTitle "INHERIT-INPUT-METHOD"))))
  (let (ξp1 ξp2)
    (save-excursion
      (goto-char 1)

      (progn (search-forward "<title>")
             (setq ξp1 (point))
             (search-forward "</title>")
             (search-backward "<")
             (setq ξp2 (point))
             (delete-region ξp1 ξp2 )
             (goto-char ξp1)
             (insert φnewTitle ))

      (if (search-forward "<h1>")
          (progn
            (setq ξp1 (point))
            (search-forward "</h1>")
            (search-backward "<")
            (setq ξp2 (point))
            (delete-region ξp1 ξp2 )
            (goto-char ξp1)
            (insert φnewTitle ))
        (progn
          (message "<h1> tag not found. adding"))))))

(defun xah-html-make-citation ()
  "Reformat current text block or selection into a canonical citation format.

For example, place cursor somewhere in the following block:

Circus Maximalist
By PAUL GRAY
Monday, Sep. 12, 1994
http://www.time.com/time/magazine/article/0,9171,981408,00.html

After execution, the lines will become

<cite>Circus Maximalist</cite> <time>1994-09-12</time> By Paul Gray. @ <a href=\"http://www.time.com/time/magazine/article/0,9171,981408,00.html\">Source www.time.com</a>

If there's a text selection, use it for input, otherwise the input is a text block between blank lines.

The order of lines for {title, author, date/time, url} needs not be in that order. Author should start with “by”."
  (interactive)
  (let* (
         (ξbds (xah-html--get-thing-or-selection 'block))
         (inputText (elt ξbds 0))
         (ξp1 (elt ξbds 1))
         (ξp2 (elt ξbds 2))
         myList ξtitle ξauthor ξdate ξurl )

    (setq inputText (replace-regexp-in-string "^[[:space:]]*" "" inputText)) ; remove white space in front

    (setq myList (split-string inputText "[[:space:]]*\n[[:space:]]*" t))

    (let (ξx (case-fold-search t))
      (while (> (length myList) 0)
        (setq ξx (pop myList))
        (cond
         ((string-match "^https?://" ξx ) (setq ξurl ξx))
         ((string-match "^ *[bB]y " ξx ) (setq ξauthor ξx))
         ((string-match "^ *author[: ]" ξx ) (setq ξauthor ξx))
         ((xah-html--is-datetimestamp-p ξx) (setq ξdate ξx))
         (t (setq ξtitle ξx)))))

    (message "title:「%s」\n author:「%s」\n date:「%s」\n url:「%s」" ξtitle ξauthor ξdate ξurl)

    (when (null ξtitle) (error "I can't find “title”"))
    (when (null ξauthor) (error "I can't find “author”"))
    (when (null ξdate) (error "I can't find “date”"))
    (when (null ξurl) (error "I can't find “url”"))

    (setq ξtitle (xah-html--trim-string ξtitle))
    (setq ξtitle (replace-regexp-in-string "^\"\\(.+\\)\"$" "\\1" ξtitle))
    (setq ξtitle (xah-replace-pairs-in-string ξtitle '(["’" "'"] ["&" "＆"] )))

    (setq ξauthor (xah-html--trim-string ξauthor))
    (setq ξauthor (replace-regexp-in-string "\\. " " " ξauthor)) ; remove period in Initals
    (setq ξauthor (replace-regexp-in-string "[Bb]y +" "" ξauthor))
    (setq ξauthor (upcase-initials (downcase ξauthor)))

    (setq ξdate (xah-html--trim-string ξdate))
    (setq ξdate (xah-fix-datetime-stamp ξdate))

    (setq ξurl (xah-html--trim-string ξurl))
    (setq ξurl (with-temp-buffer (insert ξurl) (xah-html-source-url-linkify 1) (buffer-string)))

    (delete-region ξp1 ξp2 )
    (insert (concat "〔<cite>" ξtitle "</cite> ")
            "<time>" ξdate "</time>"
            " By " ξauthor
            ". @ " ξurl
            "〕")))

(defun xah-html-make-link-defunct ()
  "Make the html link under cursor to a defunct form.
Example:
If cursor is inside this tag
 <a class=\"sorc\" href=\"http://example.com/\" data-accessed=\"2008-12-26\">…</a>
 (and inside the opening tag.)

It becomes:

 <s data-accessed=\"2006-03-11\" data-defunct-date=\"2014-01-11\">http://www.math.ca/cgi/kabol/search.pl</s>

URL `http://ergoemacs.org/emacs/elisp_html-linkify.html'
Version 2015-09-12"
  (interactive)
  (let (ξp1 ξp2 ξwholeLinkStr ξnewLinkStr ξurl ξaccessedDate)
    (save-excursion
      ;; get the boundary of opening tag
      (forward-char 3)
      (search-backward "<a " ) (setq ξp1 (point))
      (search-forward "</a>") (setq ξp2 (point))

      ;; get ξwholeLinkStr
      (setq ξwholeLinkStr (buffer-substring-no-properties ξp1 ξp2))

      ;; generate replacement text
      (with-temp-buffer
        (insert ξwholeLinkStr)

        (goto-char 1)
        (search-forward-regexp  "href=\"\\([^\"]+?\\)\"")
        (setq ξurl (match-string 1))

        (search-forward-regexp  "data-accessed=\"\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)\"")
        (setq ξaccessedDate (match-string 1))

        (setq ξnewLinkStr (format "<s data-accessed=\"%s\" data-defunct-date=\"%s\">%s</s>" ξaccessedDate (format-time-string "%Y-%m-%d") ξurl ))))

    (delete-region ξp1 ξp2)
    (insert ξnewLinkStr)))

(defun xah-html-source-url-linkify (φprefixArg)
  "Make URL at cursor point into a html link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
<a class=\"sorc\" href=\"http://example.com/xyz.htm\" data-accessed=\"2008-12-25\">example.com…</a>

The anchor text may be of 4 possibilities, depending on value of `universal-argument'.

1 → 「‹full url›」
2 or 4 → 「‹domain›…」
3 → 「img src」
0 or any → smartly decide.

URL `http://ergoemacs.org/emacs/elisp_html-linkify.html'
Version 2015-09-12"
  (interactive "P")
  (let (
         ξboundaries
         ξp1-input
         ξp2-input
         ξinput-str
         ξp1-url ξp2-url ξp1-tag ξp2-tag
         ξurl ξdomainName ξlinkText )

    (if (use-region-p)
        (progn
          (setq ξp1-input (region-beginning))
          (setq ξp2-input (region-end))
          (setq ξinput-str (buffer-substring-no-properties ξp1-input ξp2-input)))
      (progn
        (setq ξboundaries (bounds-of-thing-at-point 'url))
        (setq ξp1-input (car ξboundaries))
        (setq ξp2-input (cdr ξboundaries))
        (setq ξinput-str (buffer-substring-no-properties ξp1-input ξp2-input))))

    ;; check if it's just plain URL or already in linked form 「<a href=…>…</a>」
    ;; If latter, you need to get the boundaries for the entire link too.
    (if (string-match "href=\"" ξinput-str)
        (save-excursion
          (search-backward "href=" (- (point) 104)) ; search boundary as extra guard for error
          (forward-char 6)
          (setq ξp1-url (point))
          (search-forward "\"" (+ ξp1-url 104))
          (setq ξp2-url (- (point) 1))

          (goto-char ξp1-url)
          (search-backward "<a" (- ξp1-url 30))
          (setq ξp1-tag (point))
          (goto-char ξp2-url)
          (search-forward "</a>" (+ ξp2-url 140))
          (setq ξp2-tag (point)))
      (progn
        (setq ξp1-url ξp1-input)
        (setq ξp2-url ξp2-input)
        (setq ξp1-tag ξp1-input)
        (setq ξp2-tag ξp2-input)))

    (setq ξurl (replace-regexp-in-string "&amp;" "&" (buffer-substring-no-properties ξp1-url ξp2-url) nil "LITERAL")) ; in case it's already encoded. TODO this is only 99% correct.

    ;; get the domain name
    (setq ξdomainName
          (progn
            (string-match "://\\([^\/]+?\\)/" ξurl)
            (match-string 1 ξurl)))

    (setq ξlinkText
          (cond
           ((equal φprefixArg 1) ξurl) ; full url
           ((or (equal φprefixArg 2) (equal φprefixArg 4) (equal φprefixArg '(4))) (concat ξdomainName "…")) ; ‹domain›…
           ((equal φprefixArg 3) "img src") ; img src
           (t (if
                  (or
                   (string-match "wikipedia\\.org.+jpg$" ξurl)
                   (string-match "wikipedia\\.org.+JPG$" ξurl)
                   (string-match "wikipedia\\.org.+png$" ξurl)
                   (string-match "wikipedia\\.org.+PNG$" ξurl)
                   (string-match "wikipedia\\.org.+svg$" ξurl)
                   (string-match "wikipedia\\.org.+SVG$" ξurl))
                  "img src"
                ξurl
                )) ; smart
           ))

    (setq ξurl (replace-regexp-in-string "&" "&amp;" ξurl))

    ;; delete URL and insert the link
    (delete-region ξp1-tag ξp2-tag)
    (insert (format
             "<a class=\"sorc\" href=\"%s\" data-accessed=\"%s\">%s</a>"
             ξurl (format-time-string "%Y-%m-%d") ξlinkText
             ))))

(defun xah-html-wikipedia-url-linkify ()
  "Change Wikipedia URL under cursor into a HTML link.
If there is a text selection, use that as input.

Example:
http://en.wikipedia.org/wiki/Emacs
⇒
<a class=\"wikipedia-69128\" href=\"http://en.wikipedia.org/wiki/Emacs\" data-accessed=\"2015-09-14\">Emacs</a>.

Version 2015-09-14."
  (interactive)
  (let (ξboundaries
        ξp1 ξp2
        ξinput-str
        ξlinkText
        ξoutput-str
        )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp1 (region-end)))
      (progn
        (let (ξp0)
          (progn
            (setq ξp0 (point))
            (skip-chars-backward "^ \t\n<>[]")
            (setq ξp1 (point))
            (goto-char ξp0)
            (skip-chars-forward "^ \t\n<>[]")
            (setq ξp2 (point))))

        ;; (setq ξboundaries (bounds-of-thing-at-point 'url))
        ;; (setq ξp1 (car ξboundaries))
        ;; (setq ξp2 (cdr ξboundaries))
        ))
    (setq ξinput-str (buffer-substring-no-properties ξp1 ξp2))
    (require 'url-util)
    (setq ξlinkText
          (replace-regexp-in-string
           "_" " "
           (decode-coding-string (url-unhex-string (file-name-nondirectory ξinput-str)) 'utf-8)))
    (setq ξoutput-str
          (format
           "<a class=\"wikipedia-69128\" href=\"%s\" data-accessed=\"%s\">%s</a>"
           (url-encode-url ξinput-str)
           (format-time-string "%Y-%m-%d")
           ξlinkText
           ))
    (progn
      (delete-region ξp1 ξp2)
      (insert ξoutput-str))))

;; (defun xah-html-wikipedia-url-linkify-old (φstring &optional φfrom-to-pair)
;;   "Make the URL at cursor point into a html link.

;; If there is a text selection, use that as input.

;; Example:
;; http://en.wikipedia.org/wiki/Emacs
;; ⇒
;; <a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>.

;; When called interactively, work on current URL or text selection (of a URL).

;; When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions in sequence φfrom-to-pair."

;;   (interactive
;;    (if (region-active-p)
;;        (list nil (vector (region-beginning) (region-end)))
;;      (let (ξp0 ξp1 ξp2)
;;        (progn
;;          (setq ξp0 (point))
;;          (skip-chars-backward "^ \t\n<>[]")
;;          (setq ξp1 (point))
;;          (goto-char ξp0)
;;          (skip-chars-forward "^ \t\n<>[]")
;;          (setq ξp2 (point))
;;          (list nil (vector ξp1 ξp2))))))
;;   (let (ξwork-on-string-p ξinput-str ξoutput-str
;;                           (ξfrom (elt φfrom-to-pair 0))
;;                           (ξto (elt φfrom-to-pair 1)))
;;     (setq ξwork-on-string-p (if () t nil))
;;     (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties ξfrom ξto)))

;;     (setq ξoutput-str
;;           (format "<a href=\"%s\">%s</a>" (url-encode-url ξinput-str)
;;                   (replace-regexp-in-string "_" " "
;;                                             (xah-html-url-percent-decode-string (file-name-nondirectory ξinput-str)))))

;;     (if ξwork-on-string-p
;;         ξoutput-str
;;       (progn
;;         (delete-region ξfrom ξto)
;;         (goto-char ξfrom)
;;         (insert ξoutput-str)))))

(defun xah-html-wrap-url (φstring &optional φfrom φto)
  "Make the URL at cursor point into a html link.

When called interactively, work on current glyph sequence or text selection.

When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions φfrom φto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((ξbds (xah-html--get-thing-at-cursor 'glyphs)))
       (list nil (elt ξbds 1) (elt ξbds 2)))))

  (let (ξwork-on-string-p ξinput-str ξoutput-str)
    (setq ξwork-on-string-p (if φstring t nil))
    (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))
    (setq ξoutput-str (concat "<a href=\"" (url-encode-url ξinput-str) "\">" ξinput-str "</a>" ))

    (if ξwork-on-string-p
        ξoutput-str
      (save-excursion
        (delete-region φfrom φto)
        (goto-char φfrom)
        (insert ξoutput-str)))))

(defun xah-html-wrap-p-tag ()
  "Add <p>…</p> tag to current text block or text selection.
If there's a text selection, wrap p around each text block (separated by 2 newline chars.)"
  (interactive)
  (let (ξbds ξp1 ξp2 inputText)

    (setq ξbds (xah-html--get-thing-or-selection 'block))
    (setq inputText (elt ξbds 0))
    (setq ξp1 (elt ξbds 1))
    (setq ξp2 (elt ξbds 2))

    (delete-region ξp1 ξp2 )
    (insert "<p>" (replace-regexp-in-string "\n\n+" "</p>\n\n<p>" (xah-html--trim-string inputText)) "</p>")))

(defun xah-html-emacs-to-windows-kbd-notation (φp1 φp2)
  "Change emacs key notation to Windows's notation on text selection or current line.

For example:
 「C-h f」⇒ 「Ctrl+h f」
 「M-a」⇒ 「Alt+a」
 「<f9> <f8>」 ⇒ 「F9 F8」

This command will do most emacs syntax correctly, but not 100% correct, especially on notations like <C-M-down>. But works if it's written as C-M-<down>

When called in lisp code, φp1 φp2 are region begin/end positions.
Version 2015-04-08."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-restriction
    (narrow-to-region φp1 φp2)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (mapc
       (lambda (ξx)
         (goto-char (point-min))
         (while
             (search-forward (aref ξx 0) nil t)
           (replace-match (aref ξx 1) 'FIXEDCASE)))
       [
        ["<prior>" "PageUp"]
        ["<next>" "PageDown"]
        ["<home>" "Home"]
        ["<end>" "End"]
        ["<f1>" "F1"] ["<f2>" "F2"] ["<f3>" "F3"] ["<f4>" "F4"] ["<f5>" "F5"] ["<f6>" "F6"] ["<f7>" "F7"] ["<f8>" "F8"] ["<f9>" "F9"] ["<f10>" "F10"] ["<f11>" "F11"] ["<f12>" "F12"]
        ["<return>" "Return"]
        ["<tab>" "Tab"]
        ["<escape>" "Esc"]
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
       (lambda (ξx)
         (goto-char (point-min))
         (while
             (search-forward-regexp (aref ξx 0) nil t)
           (replace-match (aref ξx 1) 'FIXEDCASE)))
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
        ["\\bRET\\b" "Enter"]
        ["\\bSPC\\b" "Space"]
        ["\\bTAB\\b" "Tab"]
        ["\\bESC\\b" "Esc"]
        ["\\bDEL\\b" "Delete"]
        ]))))

(defun xah-html-htmlize-elisp-keywords (φp1 φp2)
  "Replace 「square-bracketed」 elisp names to HTML markup, in current line or text selection.

Example:
 「sort-lines」
    becomes
  <code class=\"elisp-ƒ\">sort-lines</code>

When called in lisp code, φp1 φp2 are region begin/end positions.

Note: a word is changed only if all of the following are true:

• The symbol string is tightly enclosed in double curly quotes, e.g. 「sort-lines」 but not 「use sort-lines」.
• `fboundp' or `boundp' returns true.
• symbol string's char contains only alphanumeric or hyphen, even though elisp identifier allows many other chars. e.g. `yas/reload-all', `color-cie-ε'.

This command also makes a report of changed items.

Some issues:

• Some words are common in other lang, e.g. “while”, “print”, “string”, unix “find”, “grep”, HTML's “kbd” tag, etc. But they are also built-in elisp symbols. This command will tag them, but you may not want that.

• Some function/variable are from 3rd party libs, and some are not bundled with GNU emacs , e.g. 「'htmlize」. They may or may not be tagged depending whether they've been loaded."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((ξchangedItems '()) ξmStr)
    (save-excursion
      (save-restriction
        (narrow-to-region φp1 φp2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp
                  "「\\([:-A-Za-z0-9]+\\)」"
                  (point-max) t)
            (setq ξmStr (match-string 1))
            (cond
             ((fboundp (intern ξmStr))
              (progn
                (push (format "ƒ %s" ξmStr) ξchangedItems)
                (replace-match (concat "<code class=\"elisp-ƒ\">" ξmStr "</code>") t t)))
             ((boundp (intern ξmStr))
              (progn
                (push (format "υ %s" ξmStr) ξchangedItems)
                (replace-match (concat "<var class=\"elisp\">" ξmStr "</var>") t t)))
             (t "do nothing"))))))
    (mapcar
     (lambda (ξx)
       (princ ξx)
       (terpri))
     (reverse ξchangedItems))))

(defun xah-html-brackets-to-html (φp1 φp2)
  "Replace bracketed text to HTML markup in current line or text selection.

• 「…」 → <code>…</code>
• 〈…〉 → <cite>…</cite>
• 《…》 → <cite class=\"book\">…</cite>
• 〔…〕 → <code class=\"path-α\">\\1</code>
•  ‹…› → <var class=\"d\">…</var>
• 〔<…〕 → 〔➤ <…〕

When called in lisp code, φp1 φp2 are region begin/end positions.
"
  (interactive
   (let ((bds (xah-html--get-thing-or-selection 'block)))
     (list (elt bds 1) (elt bds 2))))
  (let ((ξchangedItems '()))
    (save-excursion
      (save-restriction
        (narrow-to-region φp1 φp2)

        ;; (goto-char (point-min))
        ;; (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
        ;;   (xah-html-htmlize-elisp-keywords (point-min) (point-max)))

        (goto-char (point-min))
        (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
          (push (match-string-no-properties 1) ξchangedItems)
          (replace-match "<code>\\1</code>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
          (push (match-string-no-properties 1) ξchangedItems)
          (replace-match "<cite>\\1</cite>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
          (push (match-string-no-properties 1) ξchangedItems)
          (replace-match "<cite class=\"book\">\\1</cite>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "‹\\([^›]+?\\)›" nil t)
          (push (match-string-no-properties 1) ξchangedItems)
          (replace-match "<var class=\"d\">\\1</var>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "〔<a href=" nil t)
          (push (match-string-no-properties 1) ξchangedItems)
          (replace-match "〔➤ <a href=" t))

        (goto-char (point-min))
        (while (search-forward-regexp "〔\\([ -_/\\:~.A-Za-z0-9%]+?\\)〕" nil t)
          (push (match-string-no-properties 1) ξchangedItems)
          (replace-match "<code class=\"path-α\">\\1</code>" t))))

    (mapcar
     (lambda (ξx)
       (princ ξx)
       (terpri))
     (reverse ξchangedItems))))

(defun xah-html-htmlize-keyboard-shortcut-notation (φp1 φp2)
  "Markup keyboard shortcut notation in HTML tag, on text selection or current line.
Example:
 C-w ⇒ <kbd>Ctrl</kbd>+<kbd>w</kbd>
 ctrl+w ⇒ <kbd>Ctrl</kbd>+<kbd>w</kbd>

When called in lisp code, φp1 φp2 are region begin/end positions.

Version 2015-04-08"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let* (
         (ξreplaceList [
                        ;; case must match
                        ["Ctrl" "<kbd>Ctrl</kbd>"]
                        ["Control" "<kbd>Ctrl</kbd>"]
                        ["AltGr" "<kbd>AltGr</kbd>"]
                        ["Compose" "<kbd>⎄ Compose</kbd>"]
                        ["Alt" "<kbd>Alt</kbd>"]
                        ["Shift" "<kbd>⇧ Shift</kbd>"]
                        ["Cmd" "<kbd>⌘ Cmd</kbd>"]
                        ["Option" "<kbd>⌥ Opt</kbd>"]
                        ["Opt" "<kbd>⌥ Opt</kbd>"]
                        ["Win" "<kbd>❖ Win</kbd>"]
                        ["Menu" "<kbd>▤ Menu</kbd>"]
                        ["Meta" "<kbd>◆ Meta</kbd>"]
                        ["Super" "<kbd>❖ Super</kbd>"]
                        ["Hyper" "<kbd>Hyper</kbd>"]

                        ["Return" "<kbd>Return ↩</kbd>"]
                        ["Enter" "<kbd>Enter ↵</kbd>"]
                        ["Backspace" "<kbd>⌫ Backspace</kbd>"]
                        ["Delete" "<kbd>⌦ Delete</kbd>"]
                        ["DEL" "<kbd>⌦ Delete</kbd>"]
                        ["Space" "<kbd>Space</kbd>"]
                        ["Caps Lock" "<kbd>Caps Lock</kbd>"]
                        ["capslock" "<kbd>Caps Lock</kbd>"]
                        ["f-lock" "<kbd>F Lock</kbd>"]
                        ["f lock" "<kbd>F Lock</kbd>"]
                        ["numlock" "<kbd>Num Lock</kbd>"]
                        ["Num Lock" "<kbd>Num Lock</kbd>"]

                        ["Tab" "<kbd>Tab ↹</kbd>"]
                        ["Esc" "<kbd>Esc</kbd>"]
                        ["Home" "<kbd>↖ Home</kbd>"]
                        ["End" "<kbd>↘ End</kbd>"]
                        ["Page Up" "<kbd>⇞ Page △</kbd>"]
                        ["pageup" "<kbd>⇞ Page △</kbd>"]
                        ["PgUp" "<kbd>⇞ Page △</kbd>"]
                        ["Page Down" "<kbd>⇟ Page ▽</kbd>"]
                        ["pagedown" "<kbd>⇟ Page ▽</kbd>"]
                        ["PgDn" "<kbd>⇟ Page ▽</kbd>"]
                        ["Insert" "<kbd>Insert</kbd>"]
                        ["INS" "<kbd>Insert</kbd>"]
                        ["Pause" "<kbd>Pause</kbd>"]
                        ["Break" "<kbd>Pause</kbd>"]
                        ["PrtScn" "<kbd>PrtScn</kbd>"]
                        ["printscreen" "<kbd>PrtScn</kbd>"]
                        ["scrlk" "<kbd>Scroll Lock</kbd>"]
                        ["ScrLk" "<kbd>Scroll Lock</kbd>"]
                        ["scrolllock" "<kbd>Scroll Lock</kbd>"]
                        ["Fn" "<kbd>Fn</kbd>"]

                        ["Copy" "<kbd>Copy</kbd>"]
                        ["Cut" "<kbd>✂ Cut</kbd>"]
                        ["Paste" "<kbd>Paste</kbd>"]
                        ["Undo" "<kbd>⎌ Undo</kbd>"]
                        ["Redo" "<kbd>↷</kbd>"]

                        ["F10" "<kbd>F10</kbd>"]
                        ["F11" "<kbd>F11</kbd>"]
                        ["F12" "<kbd>F12</kbd>"]
                        ["F13" "<kbd>F13</kbd>"]
                        ["F14" "<kbd>F14</kbd>"]
                        ["F15" "<kbd>F15</kbd>"]
                        ["F16" "<kbd>F16</kbd>"]
                        ["F17" "<kbd>F17</kbd>"]
                        ["F18" "<kbd>F18</kbd>"]
                        ["F19" "<kbd>F19</kbd>"]
                        ["F20" "<kbd>F20</kbd>"]
                        ["F21" "<kbd>F21</kbd>"]
                        ["F22" "<kbd>F22</kbd>"]
                        ["F23" "<kbd>F23</kbd>"]
                        ["F24" "<kbd>F24</kbd>"]

                        ["F1" "<kbd>F1</kbd>"]
                        ["F2" "<kbd>F2</kbd>"]
                        ["F3" "<kbd>F3</kbd>"]
                        ["F4" "<kbd>F4</kbd>"]
                        ["F5" "<kbd>F5</kbd>"]
                        ["F6" "<kbd>F6</kbd>"]
                        ["F7" "<kbd>F7</kbd>"]
                        ["F8" "<kbd>F8</kbd>"]
                        ["F9" "<kbd>F9</kbd>"]

                        ["kp0" "<kbd>Keypad 0</kbd>"]
                        ["kp1" "<kbd>Keypad 1</kbd>"]
                        ["kp2" "<kbd>Keypad 2</kbd>"]
                        ["kp3" "<kbd>Keypad 3</kbd>"]
                        ["kp4" "<kbd>Keypad 4</kbd>"]
                        ["kp5" "<kbd>Keypad 5</kbd>"]
                        ["kp6" "<kbd>Keypad 6</kbd>"]
                        ["kp7" "<kbd>Keypad 7</kbd>"]
                        ["kp8" "<kbd>Keypad 8</kbd>"]
                        ["kp9" "<kbd>Keypad 9</kbd>"]

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

                        ]))

    (save-restriction
      (narrow-to-region φp1 φp2)
      (xah-html-emacs-to-windows-kbd-notation (point-min) (point-max))

    (xah-replace-pairs-region (point-min) (point-max) ξreplaceList)

      (let ((case-fold-search nil))

        (mapc
         (lambda (ξx)
           (goto-char (point-min))
           (while
               (search-forward-regexp (aref ξx 0) nil t)
             (replace-match (aref ξx 1) 'FIXEDCASE)))
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

(defvar xah-html-class-input-history nil "for input history of `xah-html-wrap-html-tag'")
(setq xah-html-class-input-history (list))

(defun xah-html-add-open-close-tags (φtag-name φclass-name φp1 φp2)
  "Add HTML open/close tags around region boundary φp1 φp2.
This function does not `save-excursion'."
  (let* (
         (ξclass-str (if (or (null φclass-name) (string= φclass-name "")) "" (format " class=\"%s\"" φclass-name)))
         (ξstr-left (format "<%s%s>" φtag-name ξclass-str))
         (ξstr-right (format "</%s>" φtag-name )))

    (goto-char φp1)

    (if (xah-html-tag-self-closing-p φtag-name)
        (progn (insert (format "<%s%s />" φtag-name ξclass-str)))
      (progn
        (insert ξstr-left )
        (goto-char (+ φp2 (length ξstr-left)))
        (insert ξstr-right )))))

(defun xah-html-wrap-html-tag (φtag-name &optional φclass-name)
  "Insert HTML open/close tags to current text unit or text selection.
When there's no text selection, the tag will be wrapped around current {word, line, text-block}, depending on the tag used.

If current line or word is empty, then insert open/end tags and place cursor between them.
If `universal-argument' is called first, then also prompt for a “class” attribute. Empty value means don't add the attribute."
  (interactive
   (list
    (ido-completing-read "HTML tag:" xah-html-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "div")
    (if current-prefix-arg
        (read-string "class:" nil xah-html-class-input-history "")
      nil )))
  (let (ξbds ξp1 ξp2
            lineWordBlock
            )
    (progn
      (setq lineWordBlock (xah-html-get-tag-type φtag-name))
      (setq ξbds
            (cond
             ((equal lineWordBlock "w") (xah-html--get-thing-or-selection 'word))
             ((equal lineWordBlock "l") (xah-html--get-thing-or-selection 'line))
             ((equal lineWordBlock "b") (xah-html--get-thing-or-selection 'block))
             (t (xah-html--get-thing-or-selection 'word))))
      (setq ξp1 (elt ξbds 1))
      (setq ξp2 (elt ξbds 2))
      (xah-html-add-open-close-tags φtag-name φclass-name ξp1 ξp2)

      (when ; put cursor between when input text is empty
          (and (equal ξp1 ξp2) (not (xah-html-tag-self-closing-p φtag-name)))
        (progn (search-backward "</" ))))))

(defun xah-html-insert-wrap-source-code (&optional φlang-code)
  "Insert/wrap a <pre class=\"‹φlang-code›\"> tags to text selection or current text block."
  (interactive
   (list
    (ido-completing-read "lang code:" (mapcar (lambda (x) (car x)) xah-html-lang-name-map) "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "code")))
  (let (ξbds ξp1 ξp2)
    (setq ξbds (xah-html--get-thing-or-selection 'block))
    (setq ξp1 (elt ξbds 1))
    (setq ξp2 (elt ξbds 2))
    (xah-html-add-open-close-tags "pre" φlang-code ξp1 ξp2)))

(defun xah-html-mark-unicode (φp1)
  "Wrap a special <mark> tag around the character before cursor.
like this:
 <mark class=\"unicode\" title=\"U+3B1: GREEK SMALL LETTER ALPHA\">α</mark>

If the char is any of 「&」 「<」 「>」, then replace them with 「&amp;」「&lt;」「&gt;」.

When called in elisp program, wrap the tag around charbefore position φp1."
  (interactive (list (point)))
  (let* (
         (ξcodepoint (string-to-char (buffer-substring-no-properties (- φp1 1) φp1 )))
         (ξname (get-char-code-property ξcodepoint 'name))
         (ξchar (buffer-substring-no-properties (- φp1 1) φp1)))
    (goto-char (- φp1 1))
    (insert (format "<mark class=\"unicode\" title=\"U+%X: %s\">" ξcodepoint ξname))
    (right-char 1)
    (insert (format "</mark>"))

    (cond
     ((string-equal ξchar "&") (search-backward "<" ) (insert "amp;"))
     ((string-equal ξchar "<") (search-backward "<" ) (delete-char -1) (insert "&lt;"))
     ((string-equal ξchar ">") (search-backward "<" ) (delete-char -1) (insert "&gt;")))))

(defun xah-html-markup-ruby (&optional φbegin φend)
  "Wrap HTML ruby annotation tag on current line or selection.
Chars inside paren are wrapped with “rt” tag.
For example
 abc (xyz)
becomes
 <ruby class=\"ruby88\">abc <rt>xyz</rt></ruby>

When called in lisp code, φbegin φend are region begin/end positions.

URL `http://ergoemacs.org/emacs/elisp_html-ruby-annotation-markup.html'
Version 2015-07-27"
  (interactive)
  (progn
    (if (null φbegin)
        (progn
          (if (use-region-p)
              (progn
                (setq φbegin (region-beginning))
                (setq φend (region-end)))
            (progn
              (setq φbegin (line-beginning-position))
              (setq φend (line-end-position)))))
      (progn
        (setq φbegin (line-beginning-position))
        (setq φend (line-end-position))))

    (save-restriction
      (narrow-to-region φbegin φend)
      (progn
        (goto-char (point-min))
        (while (search-forward "(" nil 'NOERROR)
          (replace-match "<rt>")))
      (progn
        (goto-char (point-min))
        (while (search-forward ")" nil 'NOERROR)
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
         (ξbds (xah-html--get-thing-or-selection 'buffer))
         (ξp1 (elt ξbds 1))
         (ξp2 (elt ξbds 2)))
    (save-excursion
      (save-restriction
        (narrow-to-region ξp1 ξp2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "[ \t]+\n" nil "noerror")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp " *<p>\n+" nil "noerror")
            (replace-match "<p>")))))))

(defun xah-html-url-percent-decode-string (φstring)
  "Returns φstring URL percent-encoded

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_%28D%C3%BCrer%29
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)

Example:
    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
    http://zh.wikipedia.org/wiki/文本编辑器

To encode, use `url-encode-url' in url-util.el.

URL `http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html'
Version 2015-09-14"
  (require 'url-util)
  (decode-coding-string (url-unhex-string φstring) 'utf-8))

(defun xah-html-decode-percent-encoded-url ()
  "Decode percent encoded URI of URI under cursor or selection.

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_%28D%C3%BCrer%29
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)

Example:
    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
    http://zh.wikipedia.org/wiki/文本编辑器

For string version, see `xah-html-url-percent-decode-string'.
To encode, see `xah-html-encode-percent-encoded-url'.
URL `http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html'
Version 2015-09-14."
  (interactive)
  (let (ξboundaries ξp1 ξp2 ξinput-str)
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (progn
        (setq ξboundaries (bounds-of-thing-at-point 'url))
        (setq ξp1 (car ξboundaries))
        (setq ξp2 (cdr ξboundaries))))
    (setq ξinput-str (buffer-substring-no-properties ξp1 ξp2))
    (require 'url-util)
    (delete-region ξp1 ξp2)
    (insert (decode-coding-string (url-unhex-string ξinput-str) 'utf-8))))

(defun xah-html-encode-percent-encoded-url ()
  "Percent encode URL under cursor or selection.

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(D%C3%BCrer)

Example:
    http://zh.wikipedia.org/wiki/文本编辑器
becomes
    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8

URL `http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html'
Version 2015-09-14."
  (interactive)
  (let (ξboundaries ξp1 ξp2 ξinput-str)
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (progn
        (setq ξboundaries (bounds-of-thing-at-point 'url))
        (setq ξp1 (car ξboundaries))
        (setq ξp2 (cdr ξboundaries))))
    (setq ξinput-str (buffer-substring-no-properties ξp1 ξp2))
    (require 'url-util)
    (delete-region ξp1 ξp2)
    (insert (url-encode-url ξinput-str))))



(defun xah-html-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
;; (let ((ξsyntax-state (syntax-ppss)))
;;     (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
;;         (progn nil)
;;       t))
t
)

(setq xah-html-abbrev-table nil)

(define-abbrev-table 'xah-html-abbrev-table
  '(

    ("cdata" "<![CDATA[▮]]>" nil :system t)
    ("hr" "<hr />")
    ("br" "<br />")
    ("cl" "class=\"\"" nil :system t)

    ("w" "width" nil :system t)
    ("h" "height" nil :system t)
    ("bgc" "background-color" nil :system t)

    ("1css" "<link rel=\"stylesheet\" href=\"lbasic.css\" />")
    ("1style" "<style type=\"text/css\">\np {line-height:130%}\n</style>")
    ("1refresh" "<meta http-equiv=\"refresh\" content=\"0; url=http://example.com/\">")

    ("iframe" "<iframe src=\"some.html\" width=\"200\" height=\"300\"></iframe>")

    ;; todo
;; http://xahlee.info/js/css_colors.html
;; http://xahlee.info/js/css_color_names.html
    ("1white" "#ffffff" nil :system t)
    ("1silver" "#c0c0c0" nil :system t)
    ("1gray" "#808080" nil :system t)
    ("1black" "#000000" nil :system t)
    ("1red" "#ff0000" nil :system t)
    ("1maroon" "#800000" nil :system t)
    ("1yellow" "#ffff00" nil :system t)
    ("1olive" "#808000" nil :system t)
    ("1lime" "#00ff00" nil :system t)
    ("1green" "#008000" nil :system t)
    ("1aqua" "#00ffff" nil :system t)
    ("1teal" "#008080" nil :system t)
    ("1blue" "#0000ff" nil :system t)
    ("1navy" "#000080" nil :system t)
    ("1fuchsia" "#ff00ff" nil :system t)
    ("1purple" "#800080" nil :system t)
    ("1orange" "#ffa500" nil :system t)
    ("hsl" "hsl(0,100%,50%)" nil :system t)

    ("1html5" "<!DOCTYPE html>" nil :system t)
    ("html4s" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" nil :system t)
    ("html4t" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" nil :system t)
    ("xhtml" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" nil :system t)
    ("1html" "<!doctype html><html><head><meta charset=\"utf-8\" />
<title>ttt</title>
</head>
<body>

</body>
</html>" nil :system t)

    )

  "abbrev table for `xah-html-mode'"
  ;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  ;; :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t

  )

;; (symbol-plist 'xah-html-abbrev-table)
;; (get 'xah-html-abbrev-table 'case-fixed)
;; (abbrev-table-get xah-html-abbrev-table :case-fixed)
;; (abbrev-table-get xah-html-abbrev-table :enable-function)
;; (abbrev-table-get xah-html-abbrev-table :parents)

  ;; :enable-function 'xah-html-abbrev-enable-function


;; keybinding

(defvar xah-html-keymap nil "Keybinding for `xah-html-mode'")
(progn
  (setq xah-html-keymap (make-sparse-keymap))
  (define-key xah-html-keymap (kbd "<C-right>") 'xah-html-skip-tag-forward)
  (define-key xah-html-keymap (kbd "<C-left>") 'xah-html-skip-tag-backward)

  (define-key xah-html-keymap (kbd "TAB") 'xah-html-wrap-html-tag)

  (define-prefix-command 'xah-html-single-keys-keymap)

  (define-key xah-html-single-keys-keymap (kbd "c") 'xah-html-lines-to-html-list)
  (define-key xah-html-single-keys-keymap (kbd "g") 'browse-url-of-buffer)
  (define-key xah-html-single-keys-keymap (kbd "k") 'xah-html-htmlize-keyboard-shortcut-notation)
  (define-key xah-html-single-keys-keymap (kbd "m") 'xah-html-insert-wrap-source-code)

  (define-key xah-html-single-keys-keymap (kbd "DEL") 'xah-html-remove-html-tags)
  (define-key xah-html-single-keys-keymap (kbd ".") 'xah-html-decode-percent-encoded-url)
  (define-key xah-html-single-keys-keymap (kbd "3") 'xah-html-update-title)
  (define-key xah-html-single-keys-keymap (kbd "4") 'xah-html-markup-ruby)
  (define-key xah-html-single-keys-keymap (kbd "5") 'xah-html-mark-unicode)
  (define-key xah-html-single-keys-keymap (kbd "6") 'xah-html-html-to-text)
  (define-key xah-html-single-keys-keymap (kbd "7") 'xah-html-toggle-syntax-coloring-markup)
  (define-key xah-html-single-keys-keymap (kbd "8") 'xah-html-get-precode-make-new-file)
  (define-key xah-html-single-keys-keymap (kbd "9") 'xah-html-redo-syntax-coloring-buffer)
  (define-key xah-html-single-keys-keymap (kbd "l 3") 'xah-html-source-url-linkify)
  (define-key xah-html-single-keys-keymap (kbd "l s") 'xah-html-make-link-defunct)
  (define-key xah-html-single-keys-keymap (kbd "l w") 'xah-html-word-to-wikipedia-linkify)
  (define-key xah-html-single-keys-keymap (kbd "l g") 'xah-html-wikipedia-url-linkify)
  (define-key xah-html-single-keys-keymap (kbd "h") 'xah-html-wrap-url)
  (define-key xah-html-single-keys-keymap (kbd "r u") 'xah-html-replace-html-chars-to-unicode)
  (define-key xah-html-single-keys-keymap (kbd "r e") 'xah-html-replace-html-chars-to-entities)
  (define-key xah-html-single-keys-keymap (kbd "r p") 'xah-html-replace-html-named-entities)
  (define-key xah-html-single-keys-keymap (kbd "r .") 'xah-html-htmlize-elisp-keywords)
  (define-key xah-html-single-keys-keymap (kbd "r t") 'xah-html-brackets-to-html)
  (define-key xah-html-single-keys-keymap (kbd "r j") 'xah-html-emacs-to-windows-kbd-notation)
  (define-key xah-html-single-keys-keymap (kbd "r m") 'xah-html-make-html-table)
  (define-key xah-html-single-keys-keymap (kbd "r v") 'xah-html-make-html-table-undo)
  (define-key xah-html-single-keys-keymap (kbd "t") 'xah-html-wrap-p-tag)
  (define-key xah-html-single-keys-keymap (kbd "n") nil)
  (define-key xah-html-single-keys-keymap (kbd "n u") 'xah-html-extract-url)
  (define-key xah-html-single-keys-keymap (kbd "y") 'xah-html-make-citation))



(setq xah-html-font-lock-keywords
      (let (
            (htmlElementNamesRegex (regexp-opt xah-html-html5-tag-list))
            (htmlAttributeNamesRegexp (regexp-opt xah-html-attribute-names))
            (cssPropertieNames (regexp-opt xah-html-css-property-names 'words))
            (cssValueNames (regexp-opt xah-html-css-value-kwds 'words))
            (cssColorNames (regexp-opt xah-html-css-color-names 'words))
            (cssUnitNames (regexp-opt xah-html-css-unit-names 'words))

  ;              (attriRegex " *= *\"\\([ -_a-z]*?\\)\"")
  ;              (attriRegex " +\\(?:[ =\"-_a-z]*?\\)") ; one or more attributes
            (attriRegex " +\\(?:[^\n<>]*?\\)") ; one or more attributes
  ;              (textNodeRegex "\\([ -_A-Za-z]+?\\)")
  ;              (textNodeRegex "\\([ [:graph:]]+?\\)")
            (textNodeRegex "\\([^\n<]+?\\)") ; ← hack, to avoid multi-line
            )
        `(

          ;; todo these multiline regex are bad. see elisp manual
          ("<!--\\|-->" . font-lock-comment-delimiter-face)
          (,(format "<!--%s-->" textNodeRegex) . (1 font-lock-comment-face))
          (,(format "<h\\([1-6]\\)>%s</h\\1>" textNodeRegex) . (2 "bold"))
          (,(format "“%s”" textNodeRegex) . (1 'xah-html-curly“”-quoted-text-face))
          (,(format "‘%s’" textNodeRegex) . (1 'xah-html-curly‘’-quoted-text-face))
          (,(format "<title>%s</title>" textNodeRegex) . (1 "bold"))
          (,(format "<span%s>%s</span>" attriRegex textNodeRegex) . (1 "hi-pink"))
          (,(format "<mark>%s</mark>" textNodeRegex) . (1 "hi-yellow"))
          (,(format "<mark%s>%s</mark>" attriRegex textNodeRegex) . (1 "hi-yellow"))
          (,(format "<b%s>%s</b>" attriRegex textNodeRegex) . (1 "bold"))

          (,(concat "</\\(" htmlElementNamesRegex "\\) *>") . (1 font-lock-function-name-face))
          (,(concat "<\\(" htmlElementNamesRegex "\\).*?>") . (1 font-lock-function-name-face))

          (,(concat " +\\(" htmlAttributeNamesRegexp "\\) *= *['\"]") . (1 font-lock-variable-name-face))

          (,cssPropertieNames . font-lock-type-face)
          ;; (,(concat ":\\(" cssValueNames " *\\)+") . (1 font-lock-keyword-face))
          ;; (,(concat ": *\\(" cssValueNames "\\)") . (1 font-lock-keyword-face))
          ;; (,(concat "\\(" cssValueNames "\\).*?;") . (1 font-lock-keyword-face))
          ;; (,(concat ":.*?\\(" cssValueNames "\\).*?;") . (1 font-lock-keyword-face))
          (,cssValueNames . font-lock-keyword-face)
          (,cssColorNames . font-lock-preprocessor-face)
          (,cssUnitNames . font-lock-reference-face))))



;;;autoload
(define-derived-mode xah-html-mode prog-mode
  "∑html"
  "A simple major mode for HTML5.
HTML5 keywords are colored.

\\{xah-html-keymap}"

  (setq font-lock-defaults '((xah-html-font-lock-keywords)))
  (setq local-abbrev-table xah-html-abbrev-table)

  (set-syntax-table xah-html-syntax-table)

  (if (or
       (not (boundp 'xfk-major-mode-lead-key))
       (null 'xfk-major-mode-lead-key))
      (define-key xah-html-keymap (kbd "<menu> e") xah-html-single-keys-keymap)
    (define-key xah-html-keymap xfk-major-mode-lead-key xah-html-single-keys-keymap))
  (use-local-map xah-html-keymap)

  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")

  ;;  (setq mode-name "xah-html")
  (run-mode-hooks 'xah-html-mode-hook)
  )

(provide 'xah-html-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-html-mode.el ends here
