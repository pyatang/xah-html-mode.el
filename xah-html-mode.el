;;; xah-html-mode.el --- Major mode for editing pure html5.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 4.5.0
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
  (require 'newcomment)
  (require 'browse-url)
  (require 'url-util)
  )

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")



(defvar xah-html--month-full-names '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") "list of English month full names.")

(defvar xah-html--month-abbrev-names (mapcar (lambda (x) (substring x 0 3)) xah-html--month-full-names) "list of English month 3-letter abbrev names.")

(defvar xah-html--weekday-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") "list of English weekday full names.")

;; 'ada-mode 'antlr-mode 'archive-mode 'asm-mode 'autoconf-mode 'awk-mode 'bat-mode 'bibtex-mode 'bibtex-style-mode 'bovine-grammar-mode 'c++-mode 'c-mode 'change-log-mode 'clojure-mode 'clojurec-mode 'clojurescript-mode 'clojurex-mode 'compilation-mode 'conf-colon-mode 'conf-javaprop-mode 'conf-mode 'conf-mode-maybe 'conf-ppd-mode 'conf-space-mode 'conf-unix-mode 'conf-xdefaults-mode 'css-mode 'dcl-mode 'delphi-mode 'diff-mode 'dns-mode 'doc-view-mode-maybe 'doctex-mode 'dsssl-mode 'ebrowse-tree-mode 'elisp-byte-code-mode 'emacs-lisp-mode 'f90-mode 'fortran-mode 'fundamental-mode 'gdb-script-mode 'git-rebase-mode 'go-mode 'html-mode 'icon-mode 'idl-mode 'idlwave-mode 'image-mode 'java-mode 'javascript-mode 'latex-mode 'ld-script-mode 'lisp-mode 'm2-mode 'm4-mode 'mail-mode 'makefile-automake-mode 'makefile-gmake-mode 'makefile-imake-mode 'makefile-makepp-mode 'markdown-mode 'metafont-mode 'metapost-mode 'mixal-mode 'nroff-mode 'objc-mode 'org-mode 'pascal-mode 'perl-mode 'pike-mode 'prolog-mode 'ps-mode 'python-mode 'rst-mode 'ruby-mode 'scheme-mode 'scribe-mode 'scss-mode 'ses-mode 'sgml-mode 'sh-mode 'shell-script-mode 'sieve-mode 'simula-mode 'snmp-mode 'snmpv2-mode 'sql-mode 'srecode-template-mode 'tar-mode 'tcl-mode 'tex-mode 'texinfo-mode 'text-mode 'tuareg-mode 'vera-mode 'verilog-mode 'vhdl-mode 'vimrc-mode 'wisent-grammar-mode 'xah-clojure-mode 'xah-css-mode 'xah-elisp-mode 'xah-html-mode 'xah-js-mode 'xah-php-mode 'xml-mode

(defun xah-html--is-datetimestamp-p (input-string)
  "Return t if input-string is a date/time stamp, else nil.
This is based on heuristic, so it's not 100% correct.
If the string contains any month names, weekday names, or of the form dddd-dd-dd, dddd-dd-dddd, dddd-dd-dd, or using slash, then it's considered a date.

2015-09-27 issue: if a sentence “You May Do So”, it's be thought as date. Similar for containing word “March”."
  (cond
         ((string-match (regexp-opt (append xah-html--month-full-names xah-html--month-abbrev-names xah-html--weekday-names) 'words) input-string) t)
         ;; mm/dd/yyyy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]\\b" input-string) t)
         ;; yyyy/mm/dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" input-string) t)
         ;; mm/dd/yy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" input-string) t)
         ;; mm-dd-yyyy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]\\b" input-string) t)
         ;; yyyy-mm-dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" input-string) t)
         ;; mm-dd-yy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" input-string) t)
         (t nil) ))



(defun xah-html--trim-string (string)
  "Remove white spaces in beginning and ending of string.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).

Note: in emacs GNU Emacs 24.4+ and later, there's `string-trim' function. You need to (require 'subr-x).
"
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun xah-html--get-thing-at-cursor (unit)
  "Return the string and boundary of UNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

UNIT can be:

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
       ( (eq unit 'word)
         (let ((wordcharset "-A-Za-z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
           (skip-chars-backward wordcharset)
           (setq p1 (point))
           (skip-chars-forward wordcharset)
           (setq p2 (point))))

       ( (eq unit 'glyphs)
         (progn
           (skip-chars-backward "[:graph:]")
           (setq p1 (point))
           (skip-chars-forward "[:graph:]")
           (setq p2 (point))))

       ((eq unit 'buffer)
        (progn
          (setq p1 (point-min))
          (setq p2 (point-max))))

       ((eq unit 'line)
        (progn
          (setq p1 (line-beginning-position))
          (setq p2 (line-end-position))))
       ((eq unit 'block)
        (progn
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq p1 (point)))
            (setq p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq p2 (point)))
            (setq p2 (point)))))

       ((eq unit 'filepath)
        (let (p0)
          (setq p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
          (setq p2 (point))))

       ((eq unit 'url)
        (let (p0
              ;; (-delimitors "^ \t\n,()[]{}<>〔〕“”\"`'!$^*|\;")
              (-delimitors "!\"#$%&'*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"))
          (setq p0 (point))
          (skip-chars-backward -delimitors) ;"^ \t\n,([{<>〔“\""
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward -delimitors) ;"^ \t\n,)]}<>〕\"”"
          (setq p2 (point))))

       ((vectorp unit)
        (let (p0)
          (setq p0 (point))
          (skip-chars-backward (elt unit 0))
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward (elt unit 1))
          (setq p2 (point))))))

    (vector (buffer-substring-no-properties p1 p2) p1 p2 )))

(defun xah-html--get-thing-or-selection (unit)
  "Return the string and boundary of text selection or UNIT under cursor.

If `use-region-p' is true, then the region is the unit.  Else,
it depends on the UNIT. See `xah-html--get-thing-at-cursor' for detail about
UNIT.

Returns a vector [text a b], where text is the string and a and b
are its boundary.

Example usage:
 (setq bds (xah-html--get-thing-or-selection 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )"
  (interactive)
  (if (use-region-p)
      (let ((p1 (region-beginning)) (p2 (region-end)))
        (vector (buffer-substring-no-properties p1 p2) p1 p2 ))
    (xah-html--get-thing-at-cursor unit)))

(defun xah-html--get-image-dimensions (*file-path)
  "Returns a vector [width height] of a image's dimension.
The elements are integer datatype.
Support png jpg svg gif and any image type emacs supports.
If it's svg, and dimension cannot be determined, it returns [0 0]
URL `http://ergoemacs.org/emacs/elisp_image_tag.html'
Version 2015-06-13"
  (let ((-x nil)
        (-y nil))
    (cond
     ((string-match "\.svg$" *file-path)
      (progn
        (with-temp-buffer
          ;; hackish. grab the first occurence of width height in file
          (insert-file-contents *file-path)
          (goto-char (point-min))
          (when (search-forward-regexp "width=\"\\([0-9]+\\).*\"" nil 'NOERROR)
            (setq -x (match-string 1 )))
          (goto-char (point-min))
          (if (search-forward-regexp "height=\"\\([0-9]+\\).*\"" nil 'NOERROR)
              (setq -y (match-string 1 ))))
        (if (and (not (null -x)) (not (null -y)))
            (progn (vector (string-to-number -x) (string-to-number -y)))
          (progn [0 0]))))
     (t
      (let (-xy )
        (progn
          (clear-image-cache t)
          (setq -xy (image-size
                     (create-image
                      (if (file-name-absolute-p *file-path)
                          *file-path
                        (concat default-directory *file-path)))
                     t)))
        (vector (car -xy) (cdr -xy)))))))


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
  "HTML attribute names."
:group 'xah-html-mode)
(setq xah-html-attribute-names '( "id" "class" "style" "title" "href" "type" "rel" "http-equiv" "content" "charset" "alt" "src" "width" "height"
  "preload" "name" "value" "size"
"maxlength"
"rows"
"cols"

"target"
"enctype"
"method"
"action"
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



(defun xah-html-get-tag-type (tag-name)
  "Return the wrap-type info of tag-name in `xah-html-html5-tag-names'"
  (elt
   (cdr
    (assoc tag-name xah-html-html5-tag-names)
    ) 0))

(defvar xah-html-lang-name-map nil "a alist that maps lang name. Each element has this form
 (‹lang code› . [‹emacs major mode name› ‹file extension›])
For example:
 (\"emacs-lisp\" . [\"xah-elisp-mode\" \"el\"])")

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
        ("css" . ["xah-css-mode" "css"])
        ("emacs-lisp" . ["xah-elisp-mode" "el"])
        ("dart" . ["dart-mode" "dart"])
        ("haskell" . ["haskell-mode" "hs"])
        ("golang" . ["go-mode" "go"])
        ("html" . ["xah-html-mode" "html"])
        ("mysql" . ["sql-mode" "sql"])
        ("xml" . ["sgml-mode" "xml"])
        ("html6" . ["xah-html6-mode" "html6"])
        ("java" . ["java-mode" "java"])
        ("js" . ["xah-js-mode" "js"])
        ("nodejs" . ["xah-js-mode" "js"])
        ("lsl" . ["xlsl-mode" "lsl"])
        ("latex" . ["latex-mode" "txt"])
        ("ocaml" . ["tuareg-mode" "ml"])
        ("perl" . ["cperl-mode" "pl"])
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

        ("vimrc" . ["vimrc-mode" "vim"])))

(defvar xah-html-lang-name-list nil "List of langcode.")
(setq xah-html-lang-name-list (mapcar 'car xah-html-lang-name-map))

(defvar xah-html-lang-mode-list nil "List of supported language mode names.")
(setq xah-html-lang-mode-list (mapcar (lambda (x) (aref (cdr x) 0)) xah-html-lang-name-map))

(defun xah-html-precode-htmlized-p (*begin *end )
  "Return true if region p1 p2 is htmlized code.
WARNING: it just losely check if it contains span tag."
  (progn
    (goto-char *begin)
    (re-search-forward "<span class=" *end "NOERROR")))

(defun xah-html-get-precode-langCode ()
  "Get the langCode and position boundary of current HTML pre block.
A pre block is text of this form
 <pre class=\"‹langCode›\">…▮…</pre>.
Your cursor must be between the tags.

Returns a vector [langCode pos1 pos2], where pos1 pos2 are the boundary of the text content."
  (interactive)
  (let (-langCode -p1 -p2)
    (save-excursion
      (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
      (setq -langCode (match-string 1))
      (setq -p1 (search-forward ">")) ; text content begin
      (backward-char 1)
      (xah-html-skip-tag-forward)
      (setq -p2 (search-backward "</pre>")) ; text content end
      (vector -langCode -p1 -p2))))

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
Version 2015-08-23"
  (interactive (list xah-html-lang-name-map))
  (let* (
         (-langcodeinfo (xah-html-get-precode-langCode))
         (-langCode (elt -langcodeinfo 0))
         (-p1 (elt -langcodeinfo 1))
         (-p2 (elt -langcodeinfo 2))
         (-textContent (buffer-substring-no-properties -p1 -p2))
         (-fileSuffix (elt (cdr (assoc -langCode lang-name-map)) 1))
         (-majorMode (elt (cdr (assoc -langCode lang-name-map)) 0))
         -fname
         (-buf (generate-new-buffer "untitled")))

    (progn
      (split-window-below)
      (delete-region -p1 -p2 )
      (switch-to-buffer -buf)
      (funcall (intern -majorMode))
      (setq buffer-offer-save t)
      (insert -textContent)
      (when (xah-html-precode-htmlized-p (point-min) (point-max))
        (xah-html-remove-span-tag-region (point-min) (point-max))))

    (if (equal -fileSuffix "java")
        (progn
          (goto-char (point-min))
          (if (search-forward-regexp "public class \\([A-Za-z0-9]+\\) [\n ]*{" nil 'NOERROR)
              (progn
                (setq -fname
                      (format "%s.java" (match-string 1))))
            (progn (setq -fname
                         (format "%s.%s.%d.%s"
                                 "xxtemp"
                                 (format-time-string "%Y%m%d%M%S")
                                 (random 99999)
                                 -fileSuffix)))))
      (progn
        (setq -fname
              (format "%s.%s.%d.%s"
                      "xxtemp"
                      (format-time-string "%Y%m%d%M%S")
                      (random 99999)
                      -fileSuffix))))
    (write-file -fname "CONFIRM")
    (goto-char (point-min))))



(defun xah-html-htmlize-string (source-code-str major-mode-name)
  "Take source-code-str and return a htmlized version using major mode major-mode-name.
The purpose is to syntax color source code in HTML.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive)
  (let (htmlizeOutputBuffer -resultStr)
    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert source-code-str)
      (funcall (intern major-mode-name))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuffer (htmlize-buffer)))
    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuffer
      (let (-p1 -p2 )
        (setq -p1 (search-forward "<pre>"))
        (setq -p2 (search-forward "</pre>"))
        (setq -resultStr (buffer-substring-no-properties (+ -p1 1) (- -p2 6)))))
    (kill-buffer htmlizeOutputBuffer)
    -resultStr ))

(defun xah-html-langcode-to-major-mode-name (lang-code lang-code-map)
  "get the `major-mode' name associated with lang-code."
  (interactive)
  (elt (cdr (assoc lang-code lang-code-map)) 0))

(defun xah-html-htmlize-precode (*lang-code-map)
  "Replace text enclosed by “pre” tag to htmlized code.

For example, if the cursor is inside the pre tags <pre class=\"‹langCode›\">…▮…</pre>, then after calling, the text inside the pre tag will be htmlized. That is, wrapped with many span tags for syntax coloring.

The opening tag must be of the form <pre class=\"‹langCode›\">.  The ‹langCode› determines what emacs mode is used to colorize the text. See `xah-html-lang-name-map' for possible ‹langCode›.

Cursor will end up right before </pre>.

See also: `xah-html-dehtmlize-precode', `xah-html-toggle-syntax-coloring-markup'.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive (list xah-html-lang-name-map))
  (let (-langCode -p1 -p2 -modeName )
    (let* (
           (t78730 (xah-html-get-precode-langCode))
           (-langCode (elt t78730 0))
           (-p1 (elt t78730 1))
           (-p2 (elt t78730 2))
           ;; (-modeName (elt (cdr (assoc -langCode *lang-code-map)) 0))
           (-modeName (xah-html-langcode-to-major-mode-name -langCode *lang-code-map)))
      (xah-html-htmlize-region -p1 -p2 -modeName t))))

(defun xah-html-htmlize-region (p1 p2 mode-name &optional *trim-whitespace-boundary-p)
  "Htmlized region p1 p2 using `major-mode' mode-name.

This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive
   (list (region-beginning)
         (region-end)
         (ido-completing-read "Chose mode for coloring:" xah-html-lang-mode-list)))
  (let* (
         (-input-str (buffer-substring-no-properties p1 p2))
         (-out-str
          (xah-html-htmlize-string (if *trim-whitespace-boundary-p
                                       (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" -input-str))
                                     -input-str
                                     ) mode-name)))

    (if (string= -input-str -out-str)
        nil
      (progn
        (delete-region p1 p2)
        (insert -out-str)))))

(defun xah-html-dehtmlize-precode (*begin *end)
  "Delete span tags between pre tags.
Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.
This command does the inverse of `xah-html-htmlize-precode'."
  (interactive
   (let* ( (-xx (xah-html-get-precode-langCode)))
     (list (elt -xx 1) (elt -xx 2))))
  (save-restriction
    (narrow-to-region *begin *end)
    (xah-html-remove-span-tag-region (point-min) (point-max))
    (xah-html-code-tag-to-brackets (point-min) (point-max))))

(defun xah-html-toggle-syntax-coloring-markup (lang-name-map)
  "Call `xah-html-htmlize-precode' or `xah-html-dehtmlize-precode'."
  (interactive (list xah-html-lang-name-map))
  (let* (
         (-t34342 (xah-html-get-precode-langCode))
         (-p1 (elt -t34342 1))
         (-p2 (elt -t34342 2)))
    (if (xah-html-precode-htmlized-p -p1 -p2)
        (xah-html-dehtmlize-precode -p1 -p2)
      (xah-html-htmlize-precode lang-name-map))))

(defun xah-html-redo-syntax-coloring-buffer (&optional lang-code)
  "redo all pre lang code syntax coloring in current html page."
  (interactive)
  (let (-langCode -p1 -p2 (-i 0))

    ;; (-search-string
    ;;  (if lang-code
    ;;      (progn (format "<pre class=\"\\([-A-Za-z0-9]+\\)\">" lang-code))
    ;;    (progn "<pre class=\"\\([-A-Za-z0-9]+\\)\">")))

    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward "<pre class=\"\\([-A-Za-z0-9]+\\)\">" nil "NOERROR")
        (setq -langCode (match-string 1))
        (setq -p1 (point))
        (backward-char 1)
        (xah-html-skip-tag-forward)
        (search-backward "</pre>")
        (setq -p2 (point))
        (save-restriction
          (narrow-to-region -p1 -p2)
          (xah-html-dehtmlize-precode (point-min) (point-max))
          (xah-html-htmlize-region (point-min) (point-max) (xah-html-langcode-to-major-mode-name -langCode xah-html-lang-name-map) t)
          (setq -i (1+ -i)))))
    (message "xah-html-redo-syntax-coloring-buffer %s redone" -i)))

;; (defun xah-html-open-local-link ()
;;   "Open the link under cursor or insert newline.
;; If cursor is on a src=… or href=…, then if it a file path, open file, if http, open in browser.
;; Else call `newline'.
;; Version 2016-03-22"
;;   (interactive)
;;   (if (xah-html-point-in-src-or-href-q)
;;       (let ((-srcValue (aref (xah-html--get-thing-at-cursor 'filepath) 0)))
;;         (if (string-match "^http" -srcValue)
;;             (browse-url -srcValue)
;;           (find-file -srcValue)))
;;     (newline)))

(defun xah-html-open-local-link ()
  "Open the link under cursor or insert newline.
If cursor is on a src=… or href=…, then if it a file path, open file, if http, open in browser.
Else call `newline'.
Version 2016-07-28"
  (interactive)
  (if (xah-html-point-in-src-or-href-q)
      (let ((-srcValue (aref (xah-html--get-thing-at-cursor 'filepath) 0)))
        (if (string-match "^http:\\|^https:" -srcValue)
            (browse-url -srcValue)
          (if (file-exists-p -srcValue)
            (find-file -srcValue)
            (when (y-or-n-p (format "file no exist at 「%s」. Create new?" -srcValue))
              (find-file -srcValue)))))
    (newline)))

(defun xah-html-point-in-src-or-href-q ()
  "Return true if curser is inside a string of src or href.
Version 2016-03-07"
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
                     (string-match "src" (current-word)))
                    (progn t)
                  (progn nil)))
            nil))
      nil
      )))


;; syntax table
(defvar xah-html-mode-syntax-table nil "Syntax table for `xah-html-mode'.")

(setq xah-html-mode-syntax-table
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



(defface xah-html-curly-quote-f“”
  '((((class color) (min-colors 88) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)

(defface xah-html-curly-quote-f‘’
  '((((class color) (min-colors 88) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)

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



(defun xah-html-tag-self-closing-p (tag-name)
  "Return true if the tag is a self-closing tag, ⁖ br."
  (interactive)
  (member tag-name  xah-html-html5-self-close-tags))

(defun xah-html-cursor-in-tag-markup? (&optional bracketPositions)
  "Return true if cursor is inside a tag markup.
For example,
 <p class=\"…\">…</p>
 If cursor is between the beginning p or ending p markup.
 bracketPositions is optional. If nil, then
 `xah-html-get-bracket-positions' is called to get it."
  (interactive)
  (let ( pl< pl> pr> pr< )
    (when (not bracketPositions)
      (progn
        (setq bracketPositions (xah-html-get-bracket-positions))
        (setq pl< (elt bracketPositions 0))
        (setq pl> (elt bracketPositions 1))
        (setq pr< (elt bracketPositions 2))
        (setq pr> (elt bracketPositions 3))))
    (if (and (< pl> pl<) (< pr> pr<))
        (progn (message "%s" "yes") t)
      (progn (message "%s" "no") nil))))

(defun xah-html-end-tag? (&optional bracketPositions)
  "Return t if cursor is inside a begin tag, else nil.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
 It simply check if the left brack is followed by a slash or not.

bracketPositions is optional. If nil, then
 `xah-html-get-bracket-positions' is called to get it.
"
  (let ( pl< pl> pr> pr< )
    (when (not bracketPositions)
      (progn
        (setq bracketPositions (xah-html-get-bracket-positions))
        (setq pl< (elt bracketPositions 0))
        (setq pl> (elt bracketPositions 1))
        (setq pr< (elt bracketPositions 2))
        (setq pr> (elt bracketPositions 3))))
    (goto-char pl<)
    (forward-char 1)
    (looking-at "/" )))

(defun xah-html-get-tag-name (&optional left<)
  "Return the tag name.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
"
  (let ( -p1 -p2 )
    (when (not left<)
      (setq left< (search-backward "<")))
    (goto-char left<)
    (forward-char 1)
    (when (looking-at "/" )
      (forward-char 1))
    (setq -p1 (point))
    (search-forward-regexp " \\|>")
    (backward-char 1)
    (setq -p2 (point))
    (buffer-substring-no-properties -p1 -p2)))

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
    (if (xah-html-cursor-in-tag-markup?)
        (if (xah-html-end-tag?)
            (progn (message "end %s" (xah-html-get-tag-name)))
          (progn (message "begin %s" (xah-html-get-tag-name))))
      (message "%s" "cursor needs to be inside a tag."))))

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
  (let (-p1 -p2 oldTagName newTagName oldClassName newClassName)
    (search-backward "<" )
    (forward-char 1)
    (setq -p1 (point))
    (setq oldTagName (xah-html-get-tag-name))
    (setq newTagName (ido-completing-read "HTML tag:" xah-html-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "span"))
    (goto-char -p1)
    (delete-char (length oldTagName))
    (insert newTagName)
    (search-forward (concat "</" oldTagName))
    (delete-char (- (length oldTagName)))
    (insert newTagName)

    (progn
      (goto-char -p1)
      (search-forward ">")
      (setq -p2  (point))
      (goto-char -p1)
      (when
          (search-forward-regexp "class[ \n]*=[ \n]*\"" -p2 "NOERROR")
  ;(string-match "class[ \n]*=[ \n]*\"" (buffer-substring-no-properties -p1 -p2))
        (progn
          (setq -p1 (point))
          (search-forward "\"")
          (setq -p2 (- (point) 1))
          (setq oldClassName (buffer-substring-no-properties -p1 -p2))
          (setq newClassName (read-string "new class name:"))
          (if (string-equal newClassName "")
              (progn ; todo need to clean this up. don't use bunch of user functions
                (delete-region -p1 -p2 )
                (backward-kill-word 1)
                (delete-char -1))
            (progn (delete-region -p1 -p2 )
                   (goto-char -p1)
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

;; (defun xah-html-replace-html-chars-to-entities (p1 p2 &optional entity-to-char-p)
;;   "Replace HTML chars & < > to HTML entities on current line or selection.
;; The string replaced are:
;;  & ⇒ &amp;
;;  < ⇒ &lt;
;;  > ⇒ &gt;

;; If `universal-argument' is called, the replacement direction is reversed.

;; When called in lisp code, p1 p2 are region begin/end positions.
;; If entity-to-char-p is true, change entities to chars instead.

;; See also: `xah-html-replace-html-named-entities', `xah-html-replace-html-chars-to-unicode'

;; URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
;; Version 2015-12-05"
;;   (interactive
;;    (if (use-region-p)
;;        (list (region-beginning) (region-end) (if current-prefix-arg t nil))
;;      (list (line-beginning-position) (line-end-position) (if current-prefix-arg t nil))))
;;   (if entity-to-char-p
;;       (xah-replace-pairs-region p1 p2 '( ["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">"] ))
;;     (xah-replace-pairs-region p1 p2 '( ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ))))

(defun xah-html-replace-html-chars-to-entities (*begin *end &optional *entity-to-char-p)
  "Replace HTML chars & < > to HTML entities on current line or selection.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

Print to message buffer occurrences of replacement (if any), with position.

If `universal-argument' is called, the replacement direction is reversed.

When called in lisp code, *begin *end are region begin/end positions. If entity-to-char-p is true, change entities to chars instead.

See also: `xah-html-replace-html-named-entities', `xah-html-replace-html-chars-to-unicode'

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2016-09-02"
  (interactive
   (list
    ;; These are done separately here
    ;; so that command-history will record these expressions
    ;; rather than the values they had this time.
    ;; 2016-07-06 note, if you add a else, it won't work
    (if (use-region-p) (region-beginning))
    (if (use-region-p) (region-end))
    (if current-prefix-arg t nil)))

  (if (null *begin) (setq *begin (line-beginning-position)))
  (if (null *end) (setq *end (line-end-position)))

  (let ((-changedItems '())
        (-findReplaceMap
         (if *entity-to-char-p
             ;; this to prevent creating a replacement sequence out of blue
             [
              ["&amp;" "螽⛫1"] ["&lt;" "螽⛫2"] ["&gt;" "螽⛫3"]
              ["螽⛫1" "&"] ["螽⛫2" "<"] ["螽⛫3" ">"]
              ]
           [ ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ]
           )))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (let ( (case-fold-search nil))
          (mapc
           (lambda (-x)
             (goto-char (point-min))
             (while (search-forward (elt -x 0) nil t)
               (push (format "%s %s" (point) -x) -changedItems)
               (replace-match (elt -x 1) 'FIXEDCASE 'LITERAL)))
           -findReplaceMap))))
    (mapcar
     (lambda (-x) (princ -x) (terpri))
     (reverse -changedItems))))

(defun xah-html-replace-html-chars-to-unicode (*begin *end &optional *fullwidth-to-ascii-p)
  "Replace chars <>& to fullwidth version ＜＞＆ in current line or text selection.

If `universal-argument' is called, the replacement direction is reversed.

When called in lisp code, *begin *end are region begin/end positions.
If *fullwidth-to-ascii-p is true, change entities to chars instead.

See also: `xah-html-replace-html-named-entities', `xah-html-replace-html-chars-to-unicode'

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2015-04-23"
(interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))

  (save-restriction
    (narrow-to-region *begin *end)
    (if *fullwidth-to-ascii-p
        (progn
          (goto-char (point-min))
          (while (search-forward "＆" nil t) (replace-match "&" nil t))
          (goto-char (point-min))
          (while (search-forward "＜" nil t) (replace-match "<" nil t))
          (goto-char (point-min))
          (while (search-forward "＞" nil t) (replace-match ">" nil t)))
      (progn
        (goto-char (point-min))
        (while (search-forward "&" nil t) (replace-match "＆" nil t))
        (goto-char (point-min))
        (while (search-forward "<" nil t) (replace-match "＜" nil t))
        (goto-char (point-min))
        (while (search-forward ">" nil t) (replace-match "＞" nil ))))))

(defun xah-html-replace-html-named-entities (*begin *end)
  "Replace HTML entities to Unicode character in current line or selection.
For example, “&copy;” becomes “©”.

The following HTML Entities are not replaced:
 &amp; &
 &lt; <
 &gt; >

When called in lisp code, *begin *end are region begin/end positions.

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
        (-replaceMap
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
      (narrow-to-region *begin *end)
      (let ( (case-fold-search nil))
        (mapc
         (lambda (-x)
           (goto-char (point-min))
           (while (search-forward (elt -x 0) nil t)
             (replace-match (elt -x 1) 'FIXEDCASE 'LITERAL)))
         -replaceMap)))))

(defun xah-html-get-html-file-title (fname &optional no-error-p)
  "Return fname <title> tag's text.
Assumes that the file contains the string “<title>…</title>”. If not, and if no-error-p is true, then return nil.

Version 2016-07-12"
  (with-temp-buffer
    (insert-file-contents fname nil nil nil t)
    (goto-char 1)
    (if (search-forward "<title>" nil no-error-p)
        (buffer-substring-no-properties
         (point)
         (- (search-forward "</title>") 8))
      nil
      )))

(defun xah-html-lines-to-html-list ()
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
Version 2016-03-19"
  (interactive)
  (let (-bds -p1 -p2 -input-str -resultStr)
    (setq -bds (xah-html--get-thing-or-selection 'block))
    (setq -input-str (elt -bds 0) -p1 (elt -bds 1) -p2 (elt -bds 2))
    (save-excursion
      (setq -resultStr
            (with-temp-buffer
              (insert -input-str)
              (delete-trailing-whitespace)
              (goto-char 1)
              (while
                  (search-forward-regexp  "\.html$" nil t)
                (backward-char 1))
              (goto-char 1)
              (while
                  (not (equal (line-end-position) (point-max)))
                (beginning-of-line) (insert "<li>")
                (end-of-line) (insert "</li>")
                (forward-line 1 ))

              (beginning-of-line) (insert "<li>")
              (end-of-line) (insert "</li>")

              (if current-prefix-arg
                  (progn
                    (goto-char 1)
                    (insert "<ol>\n")
                    (goto-char (point-max))
                    (insert "\n</ol>"))
                (progn
                  (goto-char 1)
                  (insert "<ul>\n")
                  (goto-char (point-max))
                  (insert "\n</ul>")))

              (buffer-string))))
    (delete-region -p1 -p2)
    (insert -resultStr)))

(defun xah-html-make-html-table-string (textBlock delimiter)
  "Transform the string TEXTBLOCK into a HTML marked up table.

 “\\n” is used as delimiter of rows. Extra newlines at the end is discarded.
The argument delimiter is a char used as the delimiter for columns.

 See the parent function `xah-html-make-html-table'."
(let ((txtbk textBlock))
    (setq txtbk (replace-regexp-in-string "\n+$" "\n" (concat txtbk "\n"))) ; make sure ending is just one newline char
    (setq txtbk (replace-regexp-in-string delimiter "</td><td>" txtbk))
    (setq txtbk (replace-regexp-in-string "\n" "</td></tr>\n<tr><td>" txtbk))
    (setq txtbk (substring txtbk 0 -8)) ; delete the beginning “<tr><td>” in last line
    (concat "<table class=\"nrm\">\n<tr><td>" txtbk "</table>")
))

(defun xah-html-make-html-table (sep)
  "Transform the current text block or selection into a HTML table.

If there's a text selection, use the selection as input.
Otherwise, used current text block delimited by empty lines.

SEP is a string used as a delimitor for columns.

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
  (let (-bds -p1 -p2 -str)

    (setq -bds (xah-html--get-thing-or-selection 'block))
    (setq -str (elt -bds 0))
    (setq -p1 (elt -bds 1))
    (setq -p2 (elt -bds 2))
    (delete-region -p1 -p2)
    (insert (xah-html-make-html-table-string -str sep) "\n")))

(defun xah-html-make-html-table-undo ()
  "inverse of `xah-html-make-html-table'."
  (interactive)
  (let ( -p1 -p2 -str)
    (search-backward "<table")
    (setq -p1 (point))
    (search-forward "</table>")
    (setq -p2 (point))

    (xah-replace-regexp-pairs-region -p1 -p2 [
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
  (let (-p0 -p1 -p2 -linkText)
    (if (region-active-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -p0 (point))
        (skip-chars-backward "^ \t\n")
        (setq -p1 (point))
        (goto-char -p0)
        (skip-chars-forward "^ \t\n")
        (setq -p2 (point))))
    (setq -linkText
          (replace-regexp-in-string "_" " " (buffer-substring-no-properties -p1 -p2)))
    (delete-region -p1 -p2)
    (insert (concat "<a href=\"http://en.wikipedia.org/wiki/"
                    (replace-regexp-in-string " " "_" -linkText)
                    "\">" -linkText "</a>"))))

(defun xah-html-remove-span-tag-region (*begin *end)
  "Delete HTML “span” tags in region.
And the following HTML entities are changed:
 &amp; ⇒ &
 &lt; ⇒ <
 &gt; ⇒ >

Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.

When done, the cursor is placed at *end."
  (interactive "r")
  (save-restriction
    (narrow-to-region *begin *end)
    (xah-replace-regexp-pairs-region (point-min) (point-max) '(["<span class=\"[^\"]+\">" ""]))
    (xah-replace-pairs-region (point-min) (point-max) '( ["</span>" ""] ))
    (xah-html-replace-html-chars-to-entities (point-min) (point-max) "ENTITY-TO-CHAR-P")
    (goto-char (point-max))))

(defun xah-html-code-tag-to-brackets (*begin *end &optional *change-entity-p)
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

When done, the cursor is placed at *end.

when called in lisp program,
*begin *end are region begin/end.
If *change-entity-p is true, convert html entities to char.
"
  (interactive
   (let ((-bds (xah-html--get-thing-or-selection 'block)))
     (list (elt -bds 1) (elt -bds 2) (if current-prefix-arg nil t))))

  (save-restriction
    (narrow-to-region *begin *end)
    (xah-replace-regexp-pairs-region (point-min) (point-max) '(["<code class=\"[^\"]+\">" "「"] ["<var class=\"[^\"]+\">" "‹"]))
    (xah-replace-pairs-region
     (point-min) (point-max)
     '(
       ["<code>" "「"]
       ["</code>" "」"]
       ["<var>" "‹"]
       ["</var>" "›"] ))
    (when *change-entity-p (xah-html-replace-html-chars-to-entities (point-min) (point-max) "ENTITY-TO-CHAR-P"))
    (goto-char (point-max))))

(defun xah-html-remove-html-tags (string &optional from to)
  "Delete HTML tags in string or region.
Work on current text block or text selection. (a “text block” is text between blank lines)

When called in lisp code, if string is non-nil, returns a changed string.  If string nil, change the text in the region between positions from to.

WARNING: this command does not cover all HTML tags or convert all HTML entities. For robust solution you might use: 「lynx -dump -display_charset=utf-8 URL」."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((-bds (xah-html--get-thing-or-selection 'block)))
       (list nil (elt -bds 1) (elt -bds 2)))))

  (let (-work-on-string-p -input-str -output-str)
    (setq -work-on-string-p (if string t nil))
    (setq -input-str (if -work-on-string-p string (buffer-substring-no-properties from to)))
    (setq -output-str
          (let ((case-fold-search t) (tempStr -input-str))

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

    (if -work-on-string-p
        -output-str
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert -output-str)))))

;; (defun xah-html-html-to-text (string &optional from to)
;; "Convert html to plain text on text selection or current text block."
;;   (interactive
;;    (if (region-active-p)
;;        (list nil (region-beginning) (region-end))
;;      (let ((-bds (xah-html--get-thing-or-selection 'block)) )
;;        (list nil (elt -bds 1) (elt -bds 2))) ) )

;;   (let (-work-on-string-p -input-str -output-str)
;;     (setq -work-on-string-p (if string t nil))
;;     (setq -input-str (if -work-on-string-p string (buffer-substring-no-properties from to)))
;;     (setq -output-str
;;           (let ((case-fold-search t) (tempStr -input-str))
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

;; (setq -output-str (xah-html-remove-html-tags -output-str) )

;;     (if -work-on-string-p
;;         -output-str
;;       (save-excursion
;;         (delete-region from to)
;;         (goto-char from)
;;         (insert -output-str) )) ) )

(defun xah-html-html-to-text (string &optional from to)
  "Convert html to plain text on text selection or current text block.
Version 2016-07-08."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((-bds (xah-html--get-thing-or-selection 'block)))
       (list nil (elt -bds 1) (elt -bds 2)))))

  (let (-work-on-string-p -input-str -output-str)
    (setq -work-on-string-p (if string t nil))
    (setq -input-str (if -work-on-string-p string (buffer-substring-no-properties from to)))

    (setq
     -output-str
     (with-temp-buffer
       (insert -input-str)
       (goto-char 1)
       (let ((case-fold-search nil))

         (xah-replace-regexp-pairs-region
          (point-min)
          (point-max)
          [
           ["<var class=\"d\">\\([^<]+?\\)</var>" "‹\\1›"]
            ["<script>\\([^\\<]+?\\)</script>" ""]
            ["<a +href=\"\\([^\"]+?\\)\" *>\\([^<]+?\\)</a>" "\\2 〔 \\1 〕"]
            ["<img +src=\"\\([^\"]+?\\)\" +alt=\"\\([^\"]+?\\)\" +width=\"[0-9]+\" +height=\"[0-9]+\" */?>" "〔IMAGE “\\2” \\1 〕"]
           ]
          'FIXEDCASE-P )

         (xah-replace-pairs-region
          1
          (point-max)
          '(
            ;; todo. something wrong here. not supposed to be regex
            ["<li>" "<li>• " ]
            ["</li>" "" ]
            ["<code>" "「" ]
            ["<code class=\"elisp-ƒ\">" "「" ]
            ["</code>" "」" ]
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
            )))
       (buffer-substring 1 (point-max))))

    (setq -output-str (xah-html-remove-html-tags -output-str))

    (if -work-on-string-p
        -output-str
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert -output-str)))))

(defun xah-html-rename-html-inline-image ()
  "Replace current HTML inline image's file name.
This command is for interactive use only.
When cursor is in HTML link file path, e.g.  <img src=\"img/cats.jpg\" > and this command is called, it'll prompt user for a new name. The link path will be changed to the new name, the corresponding file will also be renamed. The operation is aborted if a name exists.
Version 2016-10-17"
  (interactive)
  (let* (
         (-bounds (bounds-of-thing-at-point 'filename))
         (-inputPath (buffer-substring-no-properties (car -bounds) (cdr -bounds)))
         (-expandedPath (expand-file-name -inputPath (file-name-directory (or (buffer-file-name) default-directory ))))
         (-newPath (read-string "New name: " -expandedPath nil -expandedPath )))
    (if (file-exists-p -newPath)
        (progn (user-error "file 「%s」 exist." -newPath ))
      (progn
        (rename-file -expandedPath -newPath)
        (message "rename to %s" -newPath)
        (delete-region (car -bounds) (cdr -bounds))
        (insert (xahsite-filepath-to-href-value -newPath (or (buffer-file-name) default-directory)))))))

(defun xah-html-extract-url (*begin *end &optional *full-path-p)
  "Extract URLs in current block or region to `kill-ring'.

If `universal-argument' is called first, convert relative URL to full path.

This command extracts all text of the forms
 <‹letter› … href=\"…\" …>
 <‹letter› … src=\"…\" …>
that is on a a single line, by regex. The quote may be single quote.

When called in lisp code, *begin *end are region begin/end positions.
Returns a list.

URL `http://ergoemacs.org/emacs/elisp_extract_url_command.html'
Version 2016-07-28"
  (interactive
   (let (-p1 -p2)
     ;; set region boundary -p1 -p2
     (if (use-region-p)
         (progn (setq -p1 (region-beginning))
                (setq -p2 (region-end)))
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq -p1 (point)))
           (setq -p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq -p2 (point)))
           (setq -p2 (point)))))
     (list -p1 -p2 current-prefix-arg)))

  (let ((-regionText (buffer-substring-no-properties *begin *end))
        (-urlList (list)))
    (with-temp-buffer
      (insert -regionText)

      (goto-char 1)
      (while (re-search-forward "<" nil t)
        (replace-match "\n<" "FIXEDCASE" "LITERAL"))

      (goto-char 1)
      (while (re-search-forward
              "<[A-Za-z]+.+?\\(href\\|src\\)[[:blank:]]*?=[[:blank:]]*?\\([\"']\\)\\([^\"']+?\\)\\2" nil t)
        (push (match-string 3) -urlList)))
    (setq -urlList (reverse -urlList))

    (when *full-path-p
      (setq -urlList
            (mapcar
             (lambda (-x)
               (if (string-match "^http:\\|^https:" -x )
                   (progn -x)
                 (progn
                   (expand-file-name -x (file-name-directory (buffer-file-name))))))
             -urlList)))

    (when (called-interactively-p 'any)
      (let ((-printedResult (mapconcat 'identity -urlList "\n")))
        (kill-new -printedResult)
        (message "%s" -printedResult)))
    -urlList ))

(defun xah-html-update-title ( newTitle)
  "Update a HTML article's title and h1 tags.
Update the <title>…</title> and <h1>…</h1> of current buffer."
  (interactive
   (let (oldTitle)
     (save-excursion
       (goto-char 1)
       (search-forward-regexp "<title>\\([^<]+?\\)</title>")
       (setq oldTitle (match-string 1 )))
     (list (read-string "New title:" oldTitle nil oldTitle "INHERIT-INPUT-METHOD"))))
  (let (-p1 -p2)
    (save-excursion
      (goto-char 1)

      (progn (search-forward "<title>")
             (setq -p1 (point))
             (search-forward "</title>")
             (search-backward "<")
             (setq -p2 (point))
             (delete-region -p1 -p2 )
             (goto-char -p1)
             (insert newTitle ))

      (if (search-forward "<h1>")
          (progn
            (setq -p1 (point))
            (search-forward "</h1>")
            (search-backward "<")
            (setq -p2 (point))
            (delete-region -p1 -p2 )
            (goto-char -p1)
            (insert newTitle ))
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

〔<cite>Circus Maximalist</cite> <time>1994-09-12</time> By Paul Gray. @ <a href=\"http://www.time.com/time/magazine/article/0,9171,981408,00.html\">Source www.time.com</a>〕

If there's a text selection, use it for input, otherwise the input is a text block between blank lines.

The order of lines for {title, author, date/time, url} needs not be in that order. Author should start with “by”."
  (interactive)
  (let* (
         (-bds (xah-html--get-thing-or-selection 'block))
         ;; (-inputText (replace-regexp-in-string "^[[:space:]]*" "" (elt -bds 0))) ; remove white space in front
         (-inputText (elt -bds 0))
         (-p1 (elt -bds 1))
         (-p2 (elt -bds 2))
         (-myList (split-string -inputText "[[:space:]]*\n[[:space:]]*" t "[[:space:]]*"))
         -title -author -date -url )

       ;; set title, date, url, author,
    (let (-x (case-fold-search nil))
;; the whole thing here is not optimal implementation. data structure should be hash or so. easier... basically, we have n items, and we need to identify them into n things. that is, pairing them up. Now, some items are easily recognized with 100% certainty. We pair those first. Then, in the end, we'll have 2 or so items that we need to identify, but by then, the items are few, and we can easily distinguish them. So, for this, we need a data structure such that we can easily remove item for those we already identified.
      (while (> (length -myList) 0)
        (setq -x (pop -myList))
(message "「%s」" -x)
        (cond
         ((string-match "^https?://" -x) (setq -url -x))
         ((string-match "^ *[bB]y " -x) (setq -author -x))
         ((string-match "^ *author[: ]" -x ) (setq -author -x))
         ((xah-html--is-datetimestamp-p -x) (setq -date -x))
         (t (setq -title -x)))))

    (when (null -title) (error "I can't find “title” %s" -title))
    (when (null -author) (error "I can't find “author” %s" -author))
    (when (null -date) (error "I can't find “date” %s" -date))
    (when (null -url) (error "I can't find “url” %s" -url))

    (setq -title (xah-html--trim-string -title))
    (setq -title (replace-regexp-in-string "^\"\\(.+\\)\"$" "\\1" -title))
    (setq -title (xah-replace-pairs-in-string -title '(["’" "'"] ["&" "＆"] )))

    (setq -author (xah-html--trim-string -author))
    (setq -author (replace-regexp-in-string "\\. " " " -author)) ; remove period in Initals
    (setq -author (replace-regexp-in-string "^ *[Bb]y +" "" -author))
    (setq -author (upcase-initials (downcase -author)))

    (setq -date (xah-html--trim-string -date))
    (setq -date (xah-fix-datetime-stamp -date))

    (setq -url (xah-html--trim-string -url))
    (setq -url (with-temp-buffer (insert -url) (xah-html-source-url-linkify 1) (buffer-string)))

    (delete-region -p1 -p2 )
    (insert (concat "〔<cite>" -title "</cite> ")
            "<time>" -date "</time>"
            " By " -author
            ". @ " -url
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
  (let (-p1 -p2 -wholeLinkStr -newLinkStr -url -accessedDate)
    (save-excursion
      ;; get the boundary of opening tag
      (forward-char 3)
      (search-backward "<a " ) (setq -p1 (point))
      (search-forward "</a>") (setq -p2 (point))

      ;; get -wholeLinkStr
      (setq -wholeLinkStr (buffer-substring-no-properties -p1 -p2))

      ;; generate replacement text
      (with-temp-buffer
        (insert -wholeLinkStr)

        (goto-char 1)
        (search-forward-regexp  "href=\"\\([^\"]+?\\)\"")
        (setq -url (match-string 1))

        (search-forward-regexp  "data-accessed=\"\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)\"")
        (setq -accessedDate (match-string 1))

        (setq -newLinkStr (format "<s data-accessed=\"%s\" data-defunct-date=\"%s\">%s</s>" -accessedDate (format-time-string "%Y-%m-%d") -url ))))

    (delete-region -p1 -p2)
    (insert -newLinkStr)))

(defun xah-html-source-url-linkify (*prefixArg)
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
         -boundaries
         -p1-input
         -p2-input
         -input-str
         -p1-url -p2-url -p1-tag -p2-tag
         -url -domainName -linkText )

    (if (use-region-p)
        (progn
          (setq -p1-input (region-beginning))
          (setq -p2-input (region-end))
          (setq -input-str (buffer-substring-no-properties -p1-input -p2-input)))
      (progn
        (setq -boundaries (bounds-of-thing-at-point 'url))
        (setq -p1-input (car -boundaries))
        (setq -p2-input (cdr -boundaries))
        (setq -input-str (buffer-substring-no-properties -p1-input -p2-input))))

    ;; check if it's just plain URL or already in linked form 「<a href=…>…</a>」
    ;; If latter, you need to get the boundaries for the entire link too.
    (if (string-match "href=\"" -input-str)
        (save-excursion
          (search-backward "href=" (- (point) 104)) ; search boundary as extra guard for error
          (forward-char 6)
          (setq -p1-url (point))
          (search-forward "\"" (+ -p1-url 104))
          (setq -p2-url (- (point) 1))

          (goto-char -p1-url)
          (search-backward "<a" (- -p1-url 30))
          (setq -p1-tag (point))
          (goto-char -p2-url)
          (search-forward "</a>" (+ -p2-url 140))
          (setq -p2-tag (point)))
      (progn
        (setq -p1-url -p1-input)
        (setq -p2-url -p2-input)
        (setq -p1-tag -p1-input)
        (setq -p2-tag -p2-input)))

    (setq -url (replace-regexp-in-string "&amp;" "&" (buffer-substring-no-properties -p1-url -p2-url) nil "LITERAL")) ; in case it's already encoded. TODO this is only 99% correct.

    ;; get the domain name
    (setq -domainName
          (progn
            (string-match "://\\([^\/]+?\\)/" -url)
            (match-string 1 -url)))

    (setq -linkText
          (cond
           ((equal *prefixArg 1) -url) ; full url
           ((or (equal *prefixArg 2) (equal *prefixArg 4) (equal *prefixArg '(4))) (concat -domainName "…")) ; ‹domain›…
           ((equal *prefixArg 3) "img src") ; img src
           (t (if
                  (or
                   (string-match "wikipedia\\.org.+jpg$" -url)
                   (string-match "wikipedia\\.org.+JPG$" -url)
                   (string-match "wikipedia\\.org.+png$" -url)
                   (string-match "wikipedia\\.org.+PNG$" -url)
                   (string-match "wikipedia\\.org.+svg$" -url)
                   (string-match "wikipedia\\.org.+SVG$" -url))
                  "image source"
                -url
                )) ; smart
           ))

    (setq -url (replace-regexp-in-string "&" "&amp;" -url))

    ;; delete URL and insert the link
    (delete-region -p1-tag -p2-tag)
    (insert (format
             "<a class=\"sorc\" href=\"%s\" data-accessed=\"%s\">%s</a>"
             -url (format-time-string "%Y-%m-%d") -linkText
             ))))

(defun xah-html-image-linkify ()
  "Replace image file path under cursor to HTML img inline link.
Example:
 img/my_cats.jpg
become
 <img src=\"img/my_cats.jpg\" alt=\"emacs logo\" width=\"470\" height=\"456\" />

URL `http://ergoemacs.org/emacs/elisp_image_tag.html'
Version 2015-12-23"
  (interactive)
  (let ( -p1 -p2 -imgPath
             -hrefValue -altText -imgWH -width -height)
    (save-excursion
      ;; get image file path begin end pos
      (let (-p0)
        (setq -p0 (point))
        ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
        (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
        (setq -p1 (point))
        (goto-char -p0)
        (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
        (setq -p2 (point))
        (goto-char -p0))

      (setq -imgPath
            (if (and (fboundp 'xahsite-web-path-to-filepath)
                     (fboundp 'xah-local-url-to-file-path))
                (xahsite-web-path-to-filepath
                 (xah-local-url-to-file-path
                  (buffer-substring-no-properties -p1 -p2 )))
              (buffer-substring-no-properties -p1 -p2 )))
      (when (not (file-exists-p -imgPath))
        (user-error "file not exist at %s"  -imgPath))

      (setq -hrefValue
            (file-relative-name
             -imgPath
             (file-name-directory (or (buffer-file-name) default-directory))))
      (setq -altText
            (replace-regexp-in-string
             "_" " "
             (replace-regexp-in-string
              "\\.[A-Za-z]\\{3,4\\}$" "" (file-name-nondirectory -imgPath) t t) t t))
      (setq -imgWH (xah-html--get-image-dimensions -imgPath))
      (setq -width (number-to-string (elt -imgWH 0)))
      (setq -height (number-to-string (elt -imgWH 1))))

    (delete-region -p1 -p2)
    (insert
     (if (or (equal -width "0") (equal -height "0"))
         (concat
          "<img src=\""
          -hrefValue
          "\"" " " "alt=\"" -altText "\"" " />")
       (concat
        "<img src=\""
        -hrefValue
        "\"" " " "alt=\"" -altText "\""
        " width=\"" -width "\""
        " height=\"" -height "\" />")))))

(defun xah-html-wikipedia-url-linkify ()
  "Change Wikipedia URL under cursor into a HTML link.
If there is a text selection, use that as input.

Example:
http://en.wikipedia.org/wiki/Emacs
⇒
<a class=\"wikipedia-69128\" href=\"http://en.wikipedia.org/wiki/Emacs\" data-accessed=\"2015-09-14\">Emacs</a>.

Works the same way for links to wiktionary, e.g. https://en.wiktionary.org/wiki/%E4%BA%86

URL `http://ergoemacs.org/emacs/elisp_html_wikipedia_url_linkify.html'
Version 2016-06-29."
  (interactive)
  (let (-boundaries
        -p1 -p2
        -input-str
        -link-text
        -output-str
        )
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (let (-p0)
          (progn
            (setq -p0 (point))
            (skip-chars-backward "^ \t\n<>[]")
            (setq -p1 (point))
            (goto-char -p0)
            (skip-chars-forward "^ \t\n<>[]")
            (setq -p2 (point))))

        ;; (setq -boundaries (bounds-of-thing-at-point 'url))
        ;; (setq -p1 (car -boundaries))
        ;; (setq -p2 (cdr -boundaries))
        ))
    (setq -input-str (buffer-substring-no-properties -p1 -p2))
    (require 'url-util)
    (setq -link-text
          (replace-regexp-in-string
           "_" " "
           (decode-coding-string (url-unhex-string (file-name-nondirectory -input-str)) 'utf-8)))
    (setq -output-str
          (format
           "<a class=\"wikipedia-69128\" href=\"%s\" data-accessed=\"%s\">%s</a>"
           (url-encode-url -input-str)
           (format-time-string "%Y-%m-%d")
           -link-text
           ))
    (progn
      (delete-region -p1 -p2)
      (insert -output-str))))

;; (defun xah-html-wikipedia-url-linkify-old (string &optional from-to-pair)
;;   "Make the URL at cursor point into a html link.

;; If there is a text selection, use that as input.

;; Example:
;; http://en.wikipedia.org/wiki/Emacs
;; ⇒
;; <a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>.

;; When called interactively, work on current URL or text selection (of a URL).

;; When called in lisp code, if string is non-nil, returns a changed string.  If string nil, change the text in the region between positions in sequence from-to-pair."

;;   (interactive
;;    (if (region-active-p)
;;        (list nil (vector (region-beginning) (region-end)))
;;      (let (-p0 -p1 -p2)
;;        (progn
;;          (setq -p0 (point))
;;          (skip-chars-backward "^ \t\n<>[]")
;;          (setq -p1 (point))
;;          (goto-char -p0)
;;          (skip-chars-forward "^ \t\n<>[]")
;;          (setq -p2 (point))
;;          (list nil (vector -p1 -p2))))))
;;   (let (-work-on-string-p -input-str -output-str
;;                           (-from (elt from-to-pair 0))
;;                           (-to (elt from-to-pair 1)))
;;     (setq -work-on-string-p (if () t nil))
;;     (setq -input-str (if -work-on-string-p string (buffer-substring-no-properties -from -to)))

;;     (setq -output-str
;;           (format "<a href=\"%s\">%s</a>" (url-encode-url -input-str)
;;                   (replace-regexp-in-string "_" " "
;;                                             (xah-html-url-percent-decode-string (file-name-nondirectory -input-str)))))

;;     (if -work-on-string-p
;;         -output-str
;;       (progn
;;         (delete-region -from -to)
;;         (goto-char -from)
;;         (insert -output-str)))))

(defun xah-html-wrap-url (string &optional from to)
  "Make the URL at cursor point into a html link.

When called interactively, work on current glyph sequence or text selection.

When called in lisp code, if string is non-nil, returns a changed string.  If string nil, change the text in the region between positions from to."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((-bds (xah-html--get-thing-at-cursor 'glyphs)))
       (list nil (elt -bds 1) (elt -bds 2)))))

  (let (-work-on-string-p -input-str -output-str)
    (setq -work-on-string-p (if string t nil))
    (setq -input-str (if -work-on-string-p string (buffer-substring-no-properties from to)))
    (setq -output-str (concat "<a href=\"" (url-encode-url -input-str) "\">" -input-str "</a>" ))

    (if -work-on-string-p
        -output-str
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert -output-str)))))

(defun xah-html-wrap-p-tag ()
  "Add <p>…</p> tag to current text block or text selection.
If there's a text selection, wrap p around each text block (separated by 2 newline chars.)"
  (interactive)
  (let (-bds -p1 -p2 -inputText)

    (setq -bds (xah-html--get-thing-or-selection 'block))
    (setq -inputText (elt -bds 0))
    (setq -p1 (elt -bds 1))
    (setq -p2 (elt -bds 2))

    (delete-region -p1 -p2 )
    (insert "<p>" (replace-regexp-in-string "\n\n+" "</p>\n\n<p>" (xah-html--trim-string -inputText)) "</p>")))

(defun xah-html-emacs-to-windows-kbd-notation (*begin *end)
  "Change emacs key notation to Windows's notation on text selection or current line.

For example:
 「C-h f」⇒ 「Ctrl+h f」
 「M-a」⇒ 「Alt+a」
 「<f9> <f8>」 ⇒ 「F9 F8」

This command will do most emacs syntax correctly, but not 100% correct, especially on notations like <C-M-down>. But works if it's written as C-M-<down>

When called in lisp code, *begin *end are region begin/end positions.
Version 2015-04-08."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-restriction
    (narrow-to-region *begin *end)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (mapc
       (lambda (-x)
         (goto-char (point-min))
         (while
             (search-forward (aref -x 0) nil t)
           (replace-match (aref -x 1) 'FIXEDCASE)))
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
       (lambda (-x)
         (goto-char (point-min))
         (while
             (search-forward-regexp (aref -x 0) nil t)
           (replace-match (aref -x 1) 'FIXEDCASE)))
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

(defun xah-html-htmlize-elisp-keywords (*begin *end)
  "Replace 「square-bracketed」 elisp names to HTML markup, in current line or text selection.

Example:
 「sort-lines」
    becomes
  <code class=\"elisp-ƒ\">sort-lines</code>

When called in lisp code, *begin *end are region begin/end positions.

Note: a word is changed only if all of the following are true:

• The symbol string is tightly enclosed in double curly quotes, e.g. 「sort-lines」 but not 「use sort-lines」.
• `fboundp' or `boundp' returns true.
• symbol string's char contains only alphanumeric or hyphen, even though elisp identifier allows many other chars. e.g. `yas/reload-all', `color-cie-ε'.

This command also makes a report of changed items.

Some issues:

• Some words are common in other lang, e.g. “while”, “print”, “string”, unix “find”, “grep”, HTML's “kbd” tag, etc. But they are also built-in elisp symbols. This command will tag them, but you may not want that.

• Some function/variable are from 3rd party libs, and some are not bundled with GNU emacs , e.g. 「'htmlize」. They may or may not be tagged depending whether they've been loaded.
Version 2016-10-05"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((-changedItems '()) -mStr)
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp
                  "「\\([:-A-Za-z0-9]+\\)」"
                  (point-max) t)
            (setq -mStr (match-string 1))
            (cond
             ((fboundp (intern -mStr))
              (progn
                (push (format "ƒ %s" -mStr) -changedItems)
                (replace-match (concat "<code class=\"elisp-ƒ\">" -mStr "</code>") t t)
                (let (-p1 -p2)
                  (search-backward "</code>" )
                  (setq -p2 (point))
                  (search-backward "<code class=\"elisp-ƒ\">" )
                  (search-forward "<code class=\"elisp-ƒ\">")
                  (setq -p1 (point))
                  (overlay-put (make-overlay -p1 -p2) 'face (list :background "yellow"))
                  (search-forward "</code>"))))
             ((boundp (intern -mStr))
              (progn
                (push (format "υ %s" -mStr) -changedItems)
                (replace-match (concat "<var class=\"elisp\">" -mStr "</var>") t t)
                (let (-p1 -p2)
                  (search-backward "</var>" )
                  (setq -p2 (point))
                  (search-backward "<var class=\"elisp\">" )
                  (search-forward "<var class=\"elisp\">")
                  (setq -p1 (point))
                  (overlay-put (make-overlay -p1 -p2) 'face (list :background "green"))
                  (search-forward "</var>"))))
             (t "do nothing"))))))
    (mapcar
     (lambda (-x)
       (princ -x)
       (terpri))
     (reverse -changedItems))))

(defun xah-html-brackets-to-html (*begin *end)
  "Replace bracketed text to HTML markup in current line or text selection.

• 「emacs-lisp-function-name」 → <code class=\"elisp-ƒ\">emacs-lisp-function-name</code>
• 「…」 → <code>…</code>
• 〈…〉 → <cite>…</cite>
• 《…》 → <cite class=\"book\">…</cite>
• 〔…〕 → <code class=\"path-xl\">\\1</code>
•  ‹…› → <var class=\"d\">…</var>
• 〔<…〕 → 〔➤ <…〕

Changes are reported to message buffer with char position.

When called in lisp code, *begin *end are region begin/end positions.
Version 2016-10-05"
  (interactive
   (let ((-boundaries (xah-html--get-thing-or-selection 'block)))
     (list (elt -boundaries 1) (elt -boundaries 2))))
  (let ((-changedItems '()))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)

        (xah-html-htmlize-elisp-keywords *begin *end)

        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
            (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) -changedItems)
            (save-restriction
              (narrow-to-region (match-beginning 0) (match-end 0))
              (goto-char (point-min))
              (delete-char 1)
              (goto-char (point-max))
              (delete-char -1)
              (xah-html-replace-html-chars-to-entities (point-min) (point-max))
              (goto-char (point-min))
              (insert "<code>")
              (overlay-put (make-overlay (point) (point-max)) 'face 'highlight)
              (goto-char (point-max))
              (insert "</code>"))))

        (goto-char (point-min))
        (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) -changedItems)
          (replace-match "<cite>\\1</cite>" t)
          (let (-p1 -p2)
            (search-backward "</cite>" )
            (setq -p2 (point))
            (search-backward "<cite>" )
            (search-forward "<cite>" )
            (setq -p1 (point))
            (overlay-put (make-overlay -p1 -p2) 'face 'highlight)
            (search-forward "</cite>" )))

        (goto-char (point-min))
        (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) -changedItems)
          (replace-match "<cite class=\"book\">\\1</cite>" t)
          (let (-p1 -p2)
            (search-backward "</cite>" )
            (setq -p2 (point))
            (search-backward "<cite class=\"book\">" )
            (search-forward "<cite class=\"book\">" )
            (setq -p1 (point))
            (overlay-put (make-overlay -p1 -p2) 'face 'highlight)
            (search-forward "</cite>" )))

        (goto-char (point-min))
        (while (search-forward-regexp "‹\\([^›]+?\\)›" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) -changedItems)
          (replace-match "<var class=\"d\">\\1</var>" t)
          (let (-p1 -p2)
            (search-backward "</var>" )
            (setq -p2 (point))
            (search-backward "<var class=\"d\">" )
            (search-forward "<var class=\"d\">" )
            (setq -p1 (point))
            (overlay-put (make-overlay -p1 -p2) 'face 'highlight)
            (search-forward "</var>" )))

        (goto-char (point-min))
        (while (search-forward-regexp "〔<a href=" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) -changedItems)
          (replace-match "〔➤see <a href=" t)
          (let (-p1 -p2)
            (search-backward "〔➤see <a href=" )
            (setq -p1 (point))
            (search-forward "〔➤see <a href=" )
            (setq -p2 (point))
            (overlay-put (make-overlay -p1 -p2) 'face 'highlight)))

        (goto-char (point-min))
        (while (search-forward-regexp "{\\([ -_/\\:~.A-Za-z0-9%]+?\\)}" nil t)
          (push (concat (number-to-string (point)) " " (match-string-no-properties 1)) -changedItems)
          (replace-match "<code class=\"path-xl\">\\1</code>" t)
          (let (-p1 -p2)
            (search-backward "</code>" )
            (setq -p2 (point))
            (search-backward "<code class=\"path-xl\">" )
            (search-forward "<code class=\"path-xl\">")
            (setq -p1 (point))
            (overlay-put (make-overlay -p1 -p2) 'face 'highlight)
            (search-forward "</code>")))))

    (mapcar
     (lambda (-x)
       (princ -x)
       (terpri))
     (reverse -changedItems))))

(defun xah-html-htmlize-keyboard-shortcut-notation (*begin *end)
  "Markup keyboard shortcut notation in HTML tag, on text selection or current line.
Example:
 C-w ⇒ <kbd>Ctrl</kbd>+<kbd>w</kbd>
 ctrl+w ⇒ <kbd>Ctrl</kbd>+<kbd>w</kbd>

When called in lisp code, *begin *end are region begin/end positions.

Version 2016-10-13"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let* (
         (-replaceList [
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

                        ["Help" "<kbd>Help</kbd>"]
                        ["Power" "<kbd>Power</kbd>"]
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
                        ["sysrq" "<kbd>SysRq</kbd>"]
                        ["scrlk" "<kbd>Scroll Lock</kbd>"]
                        ["ScrLk" "<kbd>Scroll Lock</kbd>"]
                        ["scrolllock" "<kbd>Scroll Lock</kbd>"]
                        ["Fn" "<kbd>Fn</kbd>"]

                        ["Copy" "<kbd>Copy</kbd>"]
                        ["Cut" "<kbd>✂ Cut</kbd>"]
                        ["Paste" "<kbd>Paste</kbd>"]
                        ["Undo" "<kbd>⎌ Undo</kbd>"]
                        ["Redo" "<kbd>↷</kbd>"]

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

                        ]))

    (save-restriction
      (narrow-to-region *begin *end)
      (xah-html-emacs-to-windows-kbd-notation (point-min) (point-max))

    (xah-replace-pairs-region (point-min) (point-max) -replaceList 'REPORT 'HILIGHT)

      (let ((case-fold-search nil))
        (mapc
         (lambda (-x)
           (goto-char (point-min))
           (while
               (search-forward-regexp (aref -x 0) nil t)
             (replace-match (aref -x 1) 'FIXEDCASE)))
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

(defun xah-html-add-open-close-tags (tag-name class-name p1 p2)
  "Add HTML open/close tags around region boundary p1 p2.
This function does not `save-excursion'."
  (let* (
         (-class-str (if (null class-name)
                         ""
                       (format " class=\"%s\"" class-name)))
         (-str-left (format "<%s%s>" tag-name -class-str))
         (-str-right (format "</%s>" tag-name )))
    (goto-char p1)
    (if (xah-html-tag-self-closing-p tag-name)
        (insert (format "<%s%s />" tag-name -class-str))
      (progn
        (insert -str-left )
        (goto-char (+ p2 (length -str-left)))
        (insert -str-right)))))

(defun xah-html-wrap-html-tag (*tag-name &optional *class-name)
  "Insert HTML open/close tags to current text unit or text selection.
When there's no text selection, the tag will be wrapped around current {word, line, text-block}, depending on the tag used.

If current line or word is empty, then insert open/end tags and place cursor between them.
If `universal-argument' is called first, then also prompt for a “class” attribute. Empty value means don't add the attribute.
Version 2016-04-24
"
  (interactive
   (list
    (ido-completing-read "HTML tag:" xah-html-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "div")
    (if current-prefix-arg
        (read-string "class:" nil xah-html-class-input-history "")
      nil )))
  (let (-bds -p1 -p2 -wrap-type )
    (setq -wrap-type (xah-html-get-tag-type *tag-name))
    (setq -bds
          (cond
           ((equal -wrap-type "w") (xah-html--get-thing-or-selection 'word))
           ((equal -wrap-type "l") (xah-html--get-thing-or-selection 'line))
           ((equal -wrap-type "b") (xah-html--get-thing-or-selection 'block))
           (t (xah-html--get-thing-or-selection 'word))))
    (setq -p1 (elt -bds 1)
          -p2 (elt -bds 2))

    (save-restriction
      (narrow-to-region -p1 -p2)
      (when ; trim whitespace
          (and
           (not (use-region-p))
           (or
            (equal -wrap-type "l")
            (equal -wrap-type "b"))
           (not (or
                 (string-equal *tag-name "pre")
                 (string-equal *tag-name "code"))))
        (progn
          (goto-char (point-min))
          (delete-horizontal-space)
          (goto-char (point-max))
          (delete-horizontal-space)))

      ;; add blank at start/end
      (when (equal -wrap-type "b")
        (progn
          (goto-char (point-min))
          (insert "\n")
          (goto-char (point-max))
          (insert "\n")))
      (xah-html-add-open-close-tags *tag-name *class-name (point-min) (point-max)))

    (when ; put cursor between when input text is empty
        (not (xah-html-tag-self-closing-p *tag-name))
      (when (and
             (= -p1 -p2))
        (search-backward "</" )))))

(defun xah-html-insert-wrap-source-code (&optional lang-code)
  "Insert/wrap a <pre class=\"‹lang-code›\"> tags to text selection or current text block."
  (interactive
   (list
    (ido-completing-read "lang code:" (mapcar (lambda (x) (car x)) xah-html-lang-name-map) "PREDICATE" "REQUIRE-MATCH" nil xah-html-html-tag-input-history "code")))
  (let (-bds -p1 -p2)
    (setq -bds (xah-html--get-thing-or-selection 'block))
    (setq -p1 (elt -bds 1))
    (setq -p2 (elt -bds 2))
    (xah-html-add-open-close-tags "pre" lang-code -p1 -p2)))

(defun xah-html-mark-unicode (*pos)
  "Wrap a special <mark> tag around the character before cursor.
like this:
 <mark class=\"unicode\" title=\"U+3B1: GREEK SMALL LETTER ALPHA\">α</mark>

If the char is any of 「&」 「<」 「>」, then replace them with 「&amp;」「&lt;」「&gt;」.

When called in elisp program, wrap the tag around char before position *pos.
Version 2016-09-28"
  (interactive (list (point)))
  (let* (
         (-codepoint (string-to-char (buffer-substring-no-properties (- *pos 1) *pos )))
         (-name (get-char-code-property -codepoint 'name))
         (-char (buffer-substring-no-properties (- *pos 1) *pos)))
    (goto-char (- *pos 1))
    (insert (format "<mark class=\"unicode\" title=\"U+%X: %s\">" -codepoint -name))
    (right-char 1)
    (insert (format "</mark>"))

    (cond
     ((string-equal -char "&") (search-backward "<" ) (insert "amp;"))
     ((string-equal -char "<") (search-backward "<" ) (delete-char -1) (insert "&lt;"))
     ((string-equal -char ">") (search-backward "<" ) (delete-char -1) (insert "&gt;")))))

(defun xah-html-markup-ruby (&optional begin end)
  "Wrap HTML ruby annotation tag on current line or selection.
Chars inside paren are wrapped with “rt” tag.
For example
 abc (xyz)
becomes
 <ruby class=\"ruby88\">abc <rt>xyz</rt></ruby>

When called in lisp code, begin end are region begin/end positions.

URL `http://ergoemacs.org/emacs/elisp_html-ruby-annotation-markup.html'
Version 2015-07-27"
  (interactive)
  (progn
    (if (null begin)
        (progn
          (if (use-region-p)
              (progn
                (setq begin (region-beginning))
                (setq end (region-end)))
            (progn
              (setq begin (line-beginning-position))
              (setq end (line-end-position)))))
      (progn
        (setq begin (line-beginning-position))
        (setq end (line-end-position))))

    (save-restriction
      (narrow-to-region begin end)
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
         (-bds (xah-html--get-thing-or-selection 'buffer))
         (-p1 (elt -bds 1))
         (-p2 (elt -bds 2)))
    (save-excursion
      (save-restriction
        (narrow-to-region -p1 -p2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "[ \t]+\n" nil "noerror")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp " *<p>\n+" nil "noerror")
            (replace-match "<p>")))))))

(defun xah-html-url-percent-decode-string (string)
  "Returns string URL percent-encoded

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
  (decode-coding-string (url-unhex-string string) 'utf-8))

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
  (let (-boundaries -p1 -p2 -input-str)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -boundaries (bounds-of-thing-at-point 'url))
        (setq -p1 (car -boundaries))
        (setq -p2 (cdr -boundaries))))
    (setq -input-str (buffer-substring-no-properties -p1 -p2))
    (require 'url-util)
    (delete-region -p1 -p2)
    (insert (decode-coding-string (url-unhex-string -input-str) 'utf-8))))

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
  (let (-boundaries -p1 -p2 -input-str)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -boundaries (bounds-of-thing-at-point 'url))
        (setq -p1 (car -boundaries))
        (setq -p2 (cdr -boundaries))))
    (setq -input-str (buffer-substring-no-properties -p1 -p2))
    (require 'url-util)
    (delete-region -p1 -p2)
    (insert (url-encode-url -input-str))))



(defun xah-html-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
;; (let ((-syntax-state (syntax-ppss)))
;;     (if (or (nth 3 -syntax-state) (nth 4 -syntax-state))
;;         (progn nil)
;;       t))
t
)

(setq xah-html-mode-abbrev-table nil)

(define-abbrev-table 'xah-html-mode-abbrev-table
  '(

    ("cdata" "<![CDATA[▮]]>" nil :system t)
    ("hr" "<hr />")
    ("br" "<br />")
    ("cl" "class=\"\"" nil :system t)
    ("id" "id=\"\"" nil :system t)

    ("w" "width" nil :system t)
    ("h" "height" nil :system t)
    ("bgc" "background-color" nil :system t)

    ("zcss" "<link rel=\"stylesheet\" href=\"lbasic.css\" />")
    ("zstyle" "<style type=\"text/css\">\np {line-height:130%}\n</style>")
    ("zrefresh" "<meta http-equiv=\"refresh\" content=\"0; url=http://example.com/\">")

    ("iframe" "<iframe src=\"some.html\" width=\"200\" height=\"300\"></iframe>")

    ;; todo
;; http://xahlee.info/js/css_colors.html
;; http://xahlee.info/js/css_color_names.html
    ("zwhite" "#ffffff" nil :system t)
    ("zsilver" "#c0c0c0" nil :system t)
    ("zgray" "#808080" nil :system t)
    ("zblack" "#000000" nil :system t)
    ("zred" "#ff0000" nil :system t)
    ("zmaroon" "#800000" nil :system t)
    ("zyellow" "#ffff00" nil :system t)
    ("zolive" "#808000" nil :system t)
    ("zlime" "#00ff00" nil :system t)
    ("zgreen" "#008000" nil :system t)
    ("zaqua" "#00ffff" nil :system t)
    ("zteal" "#008080" nil :system t)
    ("zblue" "#0000ff" nil :system t)
    ("znavy" "#000080" nil :system t)
    ("zfuchsia" "#ff00ff" nil :system t)
    ("zpurple" "#800080" nil :system t)
    ("zorange" "#ffa500" nil :system t)
    ("hsl" "hsl(0,100%,50%)" nil :system t)

    ("og" "<meta property=\"og:image\" content=\"http://ergoemacs.org/emacs/i/geek_vs_non_geek_repetitive_tasks.png\" />" nil :system t)

    ("zhtml5" "<!DOCTYPE html>" nil :system t)
    ("html4s" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" nil :system t)
    ("html4t" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" nil :system t)
    ("xhtml" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" nil :system t)
    ("zhtml" "<!doctype html><html><head><meta charset=\"utf-8\" />
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

;; (symbol-plist 'xah-html-mode-abbrev-table)
;; (get 'xah-html-mode-abbrev-table 'case-fixed)
;; (abbrev-table-get xah-html-mode-abbrev-table :case-fixed)
;; (abbrev-table-get xah-html-mode-abbrev-table :enable-function)
;; (abbrev-table-get xah-html-mode-abbrev-table :parents)

  ;; :enable-function 'xah-html-abbrev-enable-function


;; keybinding

(defvar xah-html-mode-map nil "Keybinding for `xah-html-mode'")
(progn
  (setq xah-html-mode-map (make-sparse-keymap))
  (define-key xah-html-mode-map (kbd "RET") 'xah-html-open-local-link)

  (define-key xah-html-mode-map (kbd "TAB") 'xah-html-wrap-html-tag)

  (define-prefix-command 'xah-html-mode-no-chord-map)

  (define-key xah-html-mode-no-chord-map (kbd "<right>") 'xah-html-skip-tag-forward)
  (define-key xah-html-mode-no-chord-map (kbd "<left>") 'xah-html-skip-tag-backward)

  (define-key xah-html-mode-no-chord-map (kbd ",") 'xah-html-replace-html-chars-to-entities)
  (define-key xah-html-mode-no-chord-map (kbd ";") 'xah-html-emacs-to-windows-kbd-notation)
  (define-key xah-html-mode-no-chord-map (kbd ".") 'xah-html-decode-percent-encoded-url)
  (define-key xah-html-mode-no-chord-map (kbd "1") 'xah-html-get-precode-make-new-file)
  (define-key xah-html-mode-no-chord-map (kbd "2") 'xah-html-toggle-syntax-coloring-markup)

  (define-key xah-html-mode-no-chord-map (kbd "3") 'xah-html-update-title)
  (define-key xah-html-mode-no-chord-map (kbd "4") 'xah-html-markup-ruby)
  (define-key xah-html-mode-no-chord-map (kbd "5") 'xah-html-mark-unicode)
  (define-key xah-html-mode-no-chord-map (kbd "6") 'xah-html-html-to-text)

  (define-key xah-html-mode-no-chord-map (kbd "7") nil)
  (define-key xah-html-mode-no-chord-map (kbd "8") nil)
  (define-key xah-html-mode-no-chord-map (kbd "9") 'xah-html-redo-syntax-coloring-buffer)
  (define-key xah-html-mode-no-chord-map (kbd "DEL") 'xah-html-remove-html-tags)

  (define-key xah-html-mode-no-chord-map (kbd "a") 'xah-html-rename-html-inline-image)
  (define-key xah-html-mode-no-chord-map (kbd "b") 'xah-html-wikipedia-url-linkify)
  (define-key xah-html-mode-no-chord-map (kbd "b") nil)
  (define-key xah-html-mode-no-chord-map (kbd "c") 'xah-html-lines-to-html-list)
  (define-key xah-html-mode-no-chord-map (kbd "d") 'xah-html-extract-url)
  (define-key xah-html-mode-no-chord-map (kbd "e") 'xah-html-source-url-linkify)
  (define-key xah-html-mode-no-chord-map (kbd "f") 'xah-html-image-linkify)
  (define-key xah-html-mode-no-chord-map (kbd "g") 'xah-html-brackets-to-html)
  (define-key xah-html-mode-no-chord-map (kbd "h") 'xah-html-wrap-url)
  (define-key xah-html-mode-no-chord-map (kbd "i") nil)
  (define-key xah-html-mode-no-chord-map (kbd "j") nil)
  (define-key xah-html-mode-no-chord-map (kbd "k") 'xah-html-htmlize-keyboard-shortcut-notation)
  (define-key xah-html-mode-no-chord-map (kbd "l") 'xah-html-htmlize-elisp-keywords)
  (define-key xah-html-mode-no-chord-map (kbd "m") 'xah-html-insert-wrap-source-code)
  (define-key xah-html-mode-no-chord-map (kbd "n") nil)
  (define-key xah-html-mode-no-chord-map (kbd "o") nil)
  (define-key xah-html-mode-no-chord-map (kbd "p") 'browse-url-of-buffer)
  (define-key xah-html-mode-no-chord-map (kbd "q") 'xah-html-make-link-defunct)
  (define-key xah-html-mode-no-chord-map (kbd "q") nil)
  (define-key xah-html-mode-no-chord-map (kbd "r") 'xah-html-word-to-wikipedia-linkify)
  (define-key xah-html-mode-no-chord-map (kbd "s") nil)
  (define-key xah-html-mode-no-chord-map (kbd "t") 'xah-html-wrap-p-tag)
  (define-key xah-html-mode-no-chord-map (kbd "u") nil)
  (define-key xah-html-mode-no-chord-map (kbd "v") 'xah-html-make-html-table)
  (define-key xah-html-mode-no-chord-map (kbd "w") 'xah-html-replace-html-named-entities)
  (define-key xah-html-mode-no-chord-map (kbd "x") 'xah-html-replace-html-chars-to-unicode)
  (define-key xah-html-mode-no-chord-map (kbd "y") 'xah-html-make-citation)
  (define-key xah-html-mode-no-chord-map (kbd "z") 'xah-html-make-html-table-undo)

  ;; define separate, so that user can override the lead key
  (define-key xah-html-mode-map (kbd "C-c C-c") xah-html-mode-no-chord-map))



(setq xah-html-font-lock-keywords
      (let (
            (htmlElementNamesRegex (regexp-opt xah-html-html5-tag-list))
            (htmlAttributeNamesRegexp (regexp-opt xah-html-attribute-names))
            (htmlBooleanAttributeNamesRegexp (regexp-opt xah-html-boolean-attribute-names))
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
          (,(format "“%s”" textNodeRegex) . (1 'xah-html-curly-quote-f“”))
          (,(format "‘%s’" textNodeRegex) . (1 'xah-html-curly-quote-f‘’))
          (,(format "<title>%s</title>" textNodeRegex) . (1 "bold"))
          (,(format "<span%s>%s</span>" attriRegex textNodeRegex) . (1 'xah-html-span-f))
          (,(format "<mark>%s</mark>" textNodeRegex) . (1 'xah-html-mark-f))
          (,(format "<mark%s>%s</mark>" attriRegex textNodeRegex) . (1 'xah-html-mark-f))
          (,(format "<b%s>%s</b>" attriRegex textNodeRegex) . (1 "bold"))

          (,(concat "</\\(" htmlElementNamesRegex "\\) *>") . (1 font-lock-function-name-face))
          (,(concat "<\\(" htmlElementNamesRegex "\\).*?>") . (1 font-lock-function-name-face))

          (,(concat " +\\(" htmlAttributeNamesRegexp "\\) *= *['\"]") . (1 font-lock-variable-name-face))

          (,htmlBooleanAttributeNamesRegexp . font-lock-constant-face)

          (,cssPropertieNames . font-lock-type-face)
          ;; (,(concat ":\\(" cssValueNames " *\\)+") . (1 font-lock-keyword-face))
          ;; (,(concat ": *\\(" cssValueNames "\\)") . (1 font-lock-keyword-face))
          ;; (,(concat "\\(" cssValueNames "\\).*?;") . (1 font-lock-keyword-face))
          ;; (,(concat ":.*?\\(" cssValueNames "\\).*?;") . (1 font-lock-keyword-face))
          (,cssValueNames . font-lock-keyword-face)
          (,cssColorNames . font-lock-preprocessor-face)
          (,cssUnitNames . font-lock-reference-face))))



;;;###autoload
(define-derived-mode
    xah-html-mode
    prog-mode
    "ξhtml"
  "A simple major mode for HTML5.
HTML5 keywords are colored.

URL `http://ergoemacs.org/emacs/xah-html-mode.html'

\\{xah-html-mode-map}"

  (setq font-lock-defaults '((xah-html-font-lock-keywords)))

  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  :group 'xah-html-mode
  )

(add-to-list 'auto-mode-alist '("\\.html\\'" . xah-html-mode))

(provide 'xah-html-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-html-mode.el ends here
