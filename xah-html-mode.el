;;; xah-html-mode.el --- Major mode for editing pure html5. -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2012-05-12
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; Commentary:
;; Major mode for editing pure HTML5 files.
;; home page: http://ergoemacs.org/emacs/xah-html-mode.html
;; beta stage. Mostly just used by me. There are about 20 functions that act on HTML. They have inline doc.

;;; HISTORY

;; 2014-06-16 renamed xhm-htmlize-or-de-precode to xhm-toggle-syntax-coloring-markup. lots of other changes.
;; 0.8, 2014-06-14 added keybindings, and some style cleanup.
;; 0.7.1, 2014-03-18 added a “main” tag to “xhm-wrap-html-tag”.
;; 0.7.0, 2014-01-26 xhm-wrap-html-tag now will always do a class. (no need for prefix arg anymore). Also, now the default tag is “div” instead of “span”
;; 0.6.9, 2014-01-11 xhm-make-link-defunct behavier changed slightly. See inline doc.
;; 0.6.8, 2014-01-11 added xhm-html-to-text. modified xhm-remove-html-tags.
;; 0.6.7, 2014-01-10 bug-fix on xhm-extract-url. Now, if the url start with 「http」, don't result in 「http://http://」
;; 0.6.6, 2013-06-20 critical bug-fix on xhm-htmlize-or-de-precode. Before, it'll just remove html entities for & < >.
;; 0.6.5, 2013-05-10 improved on “xhm-make-citation”
;; 0.6.4, 2013-04-29 added xhm-change-current-tag
;; 0.6.3, 2013-04-23 now xhm-wrap-html-tag will smartly decide to wrap tag around word or line or text block, depending on the tag given, when there's no text selection.
;; 0.6.2, 2013-04-22 now, ‘single curly quoted text’ also colored.
;; 0.6.1, 2013-04-21 added xhm-insert-wrap-source-code.
;; 0.6.0, 2013-04-17 added feature to htmlize <pre> code block. ⁖ xhm-htmlize-or-de-precode and xhm-get-precode-make-new-file. The function names may change in the future.
;; 0.5.9, 2013-04-10 added “xhm-emacs-to-windows-kbd-notation” and improved “xhm-htmlize-keyboard-shortcut-notation” to take emacs notation.
;; 0.5.8, 2013-03-19 now xhm-extract-url will also put result to kill-ring
;; 0.5.7, 2013-03-03 removed the id option in xhm-wrap-html-tag
;; 0.5.6, 2013-02-16 added xhm-replace-html-named-entities
;; 0.5.5, 2013-02-03 added xhm-replace-html-&<>-to-entities, xhm-replace-html-chars-to-unicode
;; 0.5.4, 2013-01-26 lots additions and changes. added xhm-wrap-html-tag xhm-wrap-p-tag xhm-lines-to-html-list xhm-make-html-table xhm-word-to-wikipedia-linkify xhm-wrap-url xhm-wikipedia-url-linkify xhm-source-url-linkify xhm-make-link-defunct xhm-make-citation xhm-update-title xhm-extract-url xhm-remove-html-tags xhm-remove-span-tag-region xhm-htmlize-keyboard-shortcut-notation
;; 0.5.3, 2012-12-07 removed loading sgml-mode and all call to its functions. The sgml-mode seems to have bugs about keys. That is, global numberpad keys won't work.
;; 0.5.2, 2012-09-25 added a color for curly quoted text.
;; 0.5, 2012-05-13 fixed sgml-skip-tag-forward sgml-skip-tag-backward. But sgml-delete-tag still doesn't work.
;; 0.4, 2012-05-13 added sgml-delete-tag sgml-skip-tag-forward sgml-skip-tag-backward.
;; 0.3, 2012-05-13 added comment handling. improved syntax coloring. Added keymap and syntax table."
;; 0.2, 2012-05-12 first version

(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)
(require 'htmlize)
(require 'url-util)
(require 'browse-url)

(progn
  ;; part of emacs
  (require 'ido)
  (require 'sgml-mode)
  (require 'hi-lock) ; uses its face definitions
  )

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")



(defcustom xhm-html5-tag-names nil
  "A alist of HTML5 tag names. For each element, the keys is tag names, value is a vector of one element, w means word, l means line, b means block, others are placeholder for unknown. The purpose of the value is to indicate the default way to wrap the tag around cursor. "
; todo: need to go the the list and look at the type carefully. Right now it's just quickly done. lots are “z”, for unkown. Also, some are self closing tags, current has mark of “n”.
:group 'xah-html-mode
)
(setq xhm-html5-tag-names
'(
  ;; most frequently used should be on top. todo: reorder based on user. do a cache or such, look at ido-switch-buffer

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
("small" . ["z"])
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
("style" . ["z"])
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
("u" . ["z"])
("ul" . ["b"])
("video" . ["z"])
("wbr" . ["z"])

)
 )

(defvar xhm-html5-tag-list nil "list version of `xhm-html5-tag-names'")
(setq xhm-html5-tag-list (mapcar (lambda (x) (car x)) xhm-html5-tag-names))

(defcustom xhm-attribute-names nil
  "HTML5 attribute names."
:group 'xah-html-mode)
(setq xhm-attribute-names '( "id" "class" "style" "title" "href" "type" "rel" "http-equiv" "content" "charset" "alt" "src" "width" "height" "controls" "autoplay" "preload" "name" "value" "size" "async" "defer" ))

(defcustom xhm-html5-self-close-tags nil
  "a list of HTML5 self-closing tag name. "
  :group 'xah-html-mode )
(setq xhm-html5-self-close-tags '( "area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "track" "wbr"))

(defvar xhm-css-color-names nil "a list of CSS color names.")
(setq xhm-css-color-names
'("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet" "brown" "burlywood" "cadetblue" "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk" "crimson" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray" "darkgreen" "darkgrey" "darkkhaki" "darkmagenta" "darkolivegreen" "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen" "darkslateblue" "darkslategray" "darkslategrey" "darkturquoise" "darkviolet" "deeppink" "deepskyblue" "dimgray" "dimgrey" "dodgerblue" "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro" "ghostwhite" "gold" "goldenrod" "gray" "green" "greenyellow" "grey" "honeydew" "hotpink" "indianred" "indigo" "ivory" "khaki" "lavender" "lavenderblush" "lawngreen" "lemonchiffon" "lightblue" "lightcoral" "lightcyan" "lightgoldenrodyellow" "lightgray" "lightgreen" "lightgrey" "lightpink" "lightsalmon" "lightseagreen" "lightskyblue" "lightslategray" "lightslategrey" "lightsteelblue" "lightyellow" "lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise" "mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin" "navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange" "orangered" "orchid" "palegoldenrod" "palegreen" "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue" "purple" "red" "rosybrown" "royalblue" "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell" "sienna" "silver" "skyblue" "slateblue" "slategray" "slategrey" "snow" "springgreen" "steelblue" "tan" "teal" "thistle" "tomato" "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow" "yellowgreen")
 )

(defvar xhm-css-property-names nil "a list of CSS property names.")
(setq xhm-css-property-names
      '(
        "background" "background-color" "background-image" "background-position" "background-repeat" "border" "border-bottom" "border-collapse" "border-color" "border-left" "border-radius" "border-top" "box-shadow" "clear" "color" "content" "cursor" "direction" "display" "filter" "float" "font-family" "font-size" "font-style" "font-weight" "height" "line-height" "list-style" "list-style-image" "list-style-type" "margin" "margin-bottom" "margin-left" "margin-right" "margin-top" "max-width" "min-width" "opacity" "orphans" "overflow" "padding" "padding-left" "padding-right" "padding-top" "page-break-after" "page-break-inside" "position" "pre-wrap" "table" "table-cell" "text-align" "text-decoration" "unicode-bidi" "vertical-align" "white-space" "widows" "width" "word-wrap" "z-index"
        ))

(defvar xhm-css-unit-names nil "a list of CSS unite names.")
(setq xhm-css-unit-names '("px" "pt" "pc" "cm" "mm" "in" "em" "ex" "%"))

(defvar xhm-css-value-kwds nil "a list of CSS value names")
(setq xhm-css-value-kwds
      '(
        "!important" "absolute" "alpha" "auto" "avoid" "block" "bold" "both" "bottom" "break-word" "center" "collapse" "dashed" "dotted" "embed" "fixed" "help" "hidden" "hsl" "hsla" "inherit" "inline" "inline-block" "italic" "large" "left" "ltr" "middle" "monospace" "no-repeat" "none" "normal" "nowrap" "pointer" "relative" "rgb" "rgba" "right" "rtl" "sans-serif" "serif" "small" "smaller" "solid" "square" "static" "thin" "top" "transparent" "underline" "url" "x-large" "xx-large"
        ))



(defun xhm-get-tag-type (φtag-name)
  "Return the wrap-type info of φtag-name in `xhm-html5-tag-names'"
  (elt
   (cdr
    (assoc φtag-name xhm-html5-tag-names)
    ) 0))

(defvar xhm-lang-name-map nil "a alist that maps lang name. Each element has this form 「(‹lang code› . [‹emacs major mode name› ‹file extension›])」")
(setq xhm-lang-name-map
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
        ("scheme" . ["scheme-mode" "scm"])
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

(defvar xhm-lang-name-list nil "a list of langcode.")
(setq xhm-lang-name-list (mapcar 'car xhm-lang-name-map))

(defun xhm-precode-htmlized-p (φp1 φp2)
  "Return true if region φp1 φp2 is htmlized code.
WARNING: it just losely check if it contains span tag."
  (progn
    (goto-char φp1)
    (re-search-forward "<span class=" φp2 "NOERROR")))

(defun xhm-get-precode-langCode ()
  "Get the langCode and position boundary of current HTML pre block.
A pre block is text of this form
 <pre class=\"‹langCode›\">…▮…</pre>.
Your cursor must be between the tags.

Returns a vector [langCode pos1 pos2], where pos1 pos2 are the boundary of the text content."
  (interactive)
  (let (ξlangCode p1 p2)
    (save-excursion
      (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
      (setq ξlangCode (match-string 1))
      (setq p1 (search-forward ">")) ; text content begin
      (backward-char 1)
      (xhm-skip-tag-forward)
      (setq p2 (search-backward "</pre>")) ; text content end
      (vector ξlangCode p1 p2))))

(defun xhm-get-precode-make-new-file (φlang-name-map)
  "Create a new file in current dir with content from text inside pre code block.
For example, if the cursor is somewhere between the tags:
<pre class=\"ruby\">print 7</pre>

after calling, a new file of name 「xxtemp‹n›.rb」 is created in current dir, with content “print 7”. ‹n› is a integer.

If there's a text selection, use that region as content."
  (interactive (list xhm-lang-name-map))
  (let* (
         (ξxx (xhm-get-precode-langCode))
         (ξlangCode (elt ξxx 0))
         (p1 (elt ξxx 1))
         (p2 (elt ξxx 2))
         (ξtextContent (buffer-substring-no-properties p1 p2))
         (ξyy (cdr (assoc ξlangCode φlang-name-map)))
         (ξfileSuffix (elt ξyy 1))
         (ξn 1)
         ;; (format-time-string "%Y%m%d%M%S")
         (ξfname (format "xxtemp%d.%s" ξn ξfileSuffix)))

    (while
        (file-exists-p ξfname)
      (setq ξn (1+ ξn))
      (setq ξfname (format "xxtemp%d.%s" ξn ξfileSuffix)))

    (progn
      (delete-region p1 p2 )
      (split-window-vertically)
      (find-file ξfname)
      (insert ξtextContent)
      (when (xhm-precode-htmlized-p (point-min) (point-max))
        (xhm-remove-span-tag-region (point-min) (point-max))))))



(defun xhm-htmlize-string (φsource-code-str φmajor-mode-name)
  "Take φsource-code-str and return a htmlized version using major mode φmajor-mode-name.
The purpose is to syntax color source code in HTML.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive)
  (let (htmlizeOutputBuffer resultStr)
    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert φsource-code-str)
      (funcall (intern φmajor-mode-name))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuffer (htmlize-buffer)))
    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuffer
      (let (p1 p2 )
        (setq p1 (search-forward "<pre>"))
        (setq p2 (search-forward "</pre>"))
        (setq resultStr (buffer-substring-no-properties (+ p1 1) (- p2 6)))))
    (kill-buffer htmlizeOutputBuffer)
    resultStr ))

(defun xhm-langcode-to-major-mode-name (φlang-code φlang-code-map)
  "get the `major-mode' name associated with φlang-code."
  (interactive)
  (elt (cdr (assoc φlang-code φlang-code-map)) 0))

(defun xhm-htmlize-precode (φlang-code-map)
  "Replace text enclosed by “pre” tag to htmlized code.

For example, if the cursor is inside the pre tags <pre class=\"‹langCode›\">…▮…</pre>, then after calling, the text inside the pre tag will be htmlized. That is, wrapped with many span tags for syntax coloring.

The opening tag must be of the form <pre class=\"‹langCode›\">.  The ‹langCode› determines what emacs mode is used to colorize the text. See `xhm-lang-name-map' for possible ‹langCode›.

Cursor will end up right before </pre>.

See also: `xhm-dehtmlize-precode', `xhm-toggle-syntax-coloring-markup'.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive (list xhm-lang-name-map))
  (let (ξlangCode p1 p2 ξmodeName )
    (let* (
           (t78730 (xhm-get-precode-langCode))
           (ξlangCode (elt t78730 0))
           (p1 (elt t78730 1))
           (p2 (elt t78730 2))
           ;; (ξmodeName (elt (cdr (assoc ξlangCode φlang-code-map)) 0))
           (ξmodeName (xhm-langcode-to-major-mode-name ξlangCode φlang-code-map)))
      (xhm-htmlize-region p1 p2 ξmodeName t))))

(defun xhm-htmlize-region (φp1 φp2 φmode-name &optional φtrim-whitespace-boundary?)
  "Htmlized region φp1 φp2 using `major-mode' φmode-name.

This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive
   (region-beginning)
   (region-end)
   (list (ido-completing-read "Chose mode for coloring:" (mapcar 'cdr auto-mode-alist))))
  (let* (
         (ξinput-str (buffer-substring-no-properties φp1 φp2))
         (ξout-str
          (xhm-htmlize-string (if φtrim-whitespace-boundary?
                                  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" ξinput-str))
                                ξinput-str
                                ) φmode-name)))

    (if (string= ξinput-str ξout-str)
        nil
      (progn
        (delete-region φp1 φp2)
        (insert ξout-str)))))

(defun xhm-dehtmlize-precode (φp1 φp2)
  "Delete span tags between pre tags.
Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.
This command does the inverse of `xhm-htmlize-precode'."
  (interactive
   (let* (
          (t55238 (xhm-get-precode-langCode))
          (list (elt t55238 1) (elt t55238 2)))))
  (save-restriction
    (narrow-to-region φp1 φp2)
    (xhm-remove-span-tag-region (point-min) (point-max))
    (xhm-code-tag-to-brackets (point-min) (point-max))))

(defun xhm-toggle-syntax-coloring-markup (φlang-name-map)
  "Call `xhm-htmlize-precode' or `xhm-dehtmlize-precode'."
  (interactive (list xhm-lang-name-map))
  (let* (
         (ξt34342 (xhm-get-precode-langCode))
         (p1 (elt ξt34342 1))
         (p2 (elt ξt34342 2)))
    (if (xhm-precode-htmlized-p p1 p2)
        (progn
          (xhm-dehtmlize-precode p1 p2))
      (progn
        (xhm-htmlize-precode φlang-name-map)))))

(defun xhm-redo-syntax-coloring-buffer (&optional φlang-code)
  "redo all pre lang code syntax coloring in current html page."
  (interactive)
  (let (ξlangCode p1 p2 (ξi 0))

    ;; (ξsearch-string
    ;;  (if φlang-code
    ;;      (progn (format "<pre class=\"\\([-A-Za-z0-9]+\\)\">" φlang-code))
    ;;    (progn "<pre class=\"\\([-A-Za-z0-9]+\\)\">")))

    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward "<pre class=\"\\([-A-Za-z0-9]+\\)\">" nil "NOERROR")
        (setq ξlangCode (match-string 1))
        (setq p1 (point))
        (backward-char 1)
        (xhm-skip-tag-forward)
        (search-backward "</pre>")
        (setq p2 (point))
        (save-restriction
          (narrow-to-region p1 p2)
          (xhm-dehtmlize-precode (point-min) (point-max))
          (xhm-htmlize-region (point-min) (point-max) (xhm-langcode-to-major-mode-name ξlangCode xhm-lang-name-map) t)
          (setq ξi (1+ ξi)))))
    (message "xhm-redo-syntax-coloring-buffer %s redone" ξi)))


;; syntax table
(defvar xhm-syntax-table nil "Syntax table for `xah-html-mode'.")

(setq xhm-syntax-table
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



(defface xhm-curly“”-quoted-text-face
  '((((class color) (min-colors 88) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)

(defface xhm-curly‘’-quoted-text-face
  '((((class color) (min-colors 88) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)



(defun xhm-tag-self-closing? (φtag-name)
  "Return true if the tag is a self-closing tag, ⁖ br."
  (interactive)
  (member φtag-name  xhm-html5-self-close-tags))

(defun xhm-cursor-in-tag-markup? (&optional φbracketPositions)
  "Return true if cursor is inside a tag markup.
For example,
 <p class=\"…\">…</p>
 If cursor is between the beginning p or ending p markup.
 φbracketPositions is optional. If nil, then
 `xhm-get-bracket-positions' is called to get it."
  (interactive)
  (let ( pl< pl> pr> pr< )
    (when (not φbracketPositions)
      (progn
        (setq φbracketPositions (xhm-get-bracket-positions))
        (setq pl< (elt φbracketPositions 0))
        (setq pl> (elt φbracketPositions 1))
        (setq pr< (elt φbracketPositions 2))
        (setq pr> (elt φbracketPositions 3))))
    (if (and (< pl> pl<) (< pr> pr<))
        (progn (message "%s" "yes") t)
      (progn (message "%s" "no") nil))))

(defun xhm-end-tag? (&optional φbracketPositions)
  "Return t if cursor is inside a begin tag, else nil.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
 It simply check if the left brack is followed by a slash or not.

φbracketPositions is optional. If nil, then
 `xhm-get-bracket-positions' is called to get it.
"
  (let ( pl< pl> pr> pr< )
    (when (not φbracketPositions)
      (progn
        (setq φbracketPositions (xhm-get-bracket-positions))
        (setq pl< (elt φbracketPositions 0))
        (setq pl> (elt φbracketPositions 1))
        (setq pr< (elt φbracketPositions 2))
        (setq pr> (elt φbracketPositions 3))))
    (goto-char pl<)
    (forward-char 1)
    (looking-at "/" )))

(defun xhm-get-tag-name (&optional φleft<)
  "Return the tag name.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
"
  (let ( p1 p2 )
    (when (not φleft<)
      (setq φleft< (search-backward "<")))
    (goto-char φleft<)
    (forward-char 1)
    (when (looking-at "/" )
      (forward-char 1))
    (setq p1 (point))
    (search-forward-regexp " \\|>")
    (backward-char 1)
    (setq p2 (point))
    (buffer-substring-no-properties p1 p2)))

(defun xhm-get-bracket-positions ()
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

(defun xhm-delete-tag ()
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
      (if (xhm-cursor-in-tag-markup?)
          (progn
            (if (xhm-end-tag?)
                (progn (message "end %s" (xhm-get-tag-name)))
              (progn (message "begin %s" (xhm-get-tag-name)))))
        (progn (message "%s" "cursor needs to be inside a tag."))))))

(defun xhm-skip-tag-forward ()
  "Move cursor to the closing tag."
  (interactive)
  (sgml-skip-tag-forward 1)
  )

(defun xhm-skip-tag-backward ()
  "Move cursor to the beginning tag."
  (interactive)
  (sgml-skip-tag-backward 1)
  )

(defun xhm-change-current-tag ()
  "change the tag name of current tag, and class name if there's one. WARNING:
this is a quick 1 min hackjob, works only when there's no nesting."
  (interactive)
  (let (p1 p2 oldTagName newTagName oldClassName newClassName)
    (search-backward "<" )
    (forward-char 1)
    (setq p1 (point))
    (setq oldTagName (xhm-get-tag-name))
    (setq newTagName (ido-completing-read "HTML tag:" xhm-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xhm-html-tag-input-history "span"))
    (goto-char p1)
    (delete-char (length oldTagName))
    (insert newTagName)
    (search-forward (concat "</" oldTagName))
    (delete-char (- (length oldTagName)))
    (insert newTagName)

    (progn
      (goto-char p1)
      (search-forward ">")
      (setq p2  (point))
      (goto-char p1)
      (when
          (search-forward-regexp "class[ \n]*=[ \n]*\"" p2 "NOERROR")
  ;(string-match "class[ \n]*=[ \n]*\"" (buffer-substring-no-properties p1 p2))
        (progn
          (setq p1 (point))
          (search-forward "\"")
          (setq p2 (- (point) 1))
          (setq oldClassName (buffer-substring-no-properties p1 p2))
          (setq newClassName (read-string "new class name:"))
          (if (string-equal newClassName "")
              (progn ; todo need to clean this up. don't use bunch of user functions
                (delete-region p1 p2 )
                (backward-kill-word 1)
                (delete-char -1))
            (progn (delete-region p1 p2 )
                   (goto-char p1)
                   (insert newClassName))))))))

;; (defun xhm-split-tag ()
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

(defun xhm-replace-html-&<>-to-entities (φp1 φp2 &optional φentity-to-char-p)
  "Replace HTML chars & < > to HTML entities.
This works on the current text selection or text block.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

If `universal-argument' is called, the replacement direction is reversed. That is &amp; ⇒ & etc.

When called in lisp program, φp1 φp2 are region begin/end. if φentity-to-char-p is true, change entities to chars instead.

See also: `xhm-replace-html-named-entities', `xhm-replace-html-chars-to-unicode'"
  (interactive
   (let ((bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2) (if current-prefix-arg t nil))))
  (save-excursion
    (if φentity-to-char-p
        (replace-pairs-region φp1 φp2 '( ["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">"] ))
      (replace-pairs-region φp1 φp2 '( ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] )))))

(defun xhm-replace-html-chars-to-unicode ()
  "Replace HTML < > & to Unicode chars 〈 〉 ＆.
This works on the current text selection or block of text.

See also:
`xhm-replace-html-named-entities'
`xhm-replace-html-&<>-to-entities'"
  (interactive)
  (let (bds p1 p2 myText)
    (setq bds (get-selection-or-unit 'block))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2))
    (replace-pairs-region p1 p2 '( ["&" "＆"] ["<" "〈"] [">" "〉"] ))))

(defun xhm-replace-html-named-entities (φstring &optional φfrom φto)
  "Replace HTML entities to Unicode character.
For example, “&copy;” becomes “©”.

When called interactively, work on current text block or text selection. (a “text block” is text between blank lines)

When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions φfrom φto.

The following HTML Entities are not replaced:
 &amp; &
 &lt; <
 &gt; >

See also:
`xhm-replace-html-&<>-to-entities'
`xhm-replace-html-chars-to-unicode'
"
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)))
       (list nil (elt bds 1) (elt bds 2)))))

  (let (ξwork-on-string-p ξinput-str ξoutput-str)
    (setq ξwork-on-string-p (if φstring t nil))
    (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))

    (setq ξoutput-str
          (let ((case-fold-search nil))
            (replace-pairs-in-string ξinput-str
                                     [
                                      ["&nbsp;" " "] ["&ensp;" " "] ["&emsp;" " "] ["&thinsp;" " "]

                                      ["&rlm;" "‏"] ["&lrm;" "‎"] ["&zwj;" "‍"] ["&zwnj;" "‌"]

                                      ["&iexcl;" "¡"] ["&cent;" "¢"] ["&pound;" "£"] ["&curren;" "¤"] ["&yen;" "¥"] ["&brvbar;" "¦"] ["&sect;" "§"] ["&uml;" "¨"] ["&copy;" "©"] ["&ordf;" "ª"] ["&laquo;" "«"] ["&not;" "¬"] ["&shy;" "­"] ["&reg;" "®"] ["&macr;" "¯"] ["&deg;" "°"] ["&plusmn;" "±"] ["&sup2;" "²"] ["&sup3;" "³"] ["&acute;" "´"] ["&micro;" "µ"] ["&para;" "¶"] ["&middot;" "·"] ["&cedil;" "¸"] ["&sup1;" "¹"] ["&ordm;" "º"] ["&raquo;" "»"] ["&frac14;" "¼"] ["&frac12;" "½"] ["&frac34;" "¾"] ["&iquest;" "¿"] ["&Agrave;" "À"] ["&Aacute;" "Á"] ["&Acirc;" "Â"] ["&Atilde;" "Ã"] ["&Auml;" "Ä"] ["&Aring;" "Å"] ["&AElig;" "Æ"] ["&Ccedil;" "Ç"] ["&Egrave;" "È"] ["&Eacute;" "É"] ["&Ecirc;" "Ê"] ["&Euml;" "Ë"] ["&Igrave;" "Ì"] ["&Iacute;" "Í"] ["&Icirc;" "Î"] ["&Iuml;" "Ï"] ["&ETH;" "Ð"] ["&Ntilde;" "Ñ"] ["&Ograve;" "Ò"] ["&Oacute;" "Ó"] ["&Ocirc;" "Ô"] ["&Otilde;" "Õ"] ["&Ouml;" "Ö"] ["&times;" "×"] ["&Oslash;" "Ø"] ["&Ugrave;" "Ù"] ["&Uacute;" "Ú"] ["&Ucirc;" "Û"] ["&Uuml;" "Ü"] ["&Yacute;" "Ý"] ["&THORN;" "Þ"] ["&szlig;" "ß"] ["&agrave;" "à"] ["&aacute;" "á"] ["&acirc;" "â"] ["&atilde;" "ã"] ["&auml;" "ä"] ["&aring;" "å"] ["&aelig;" "æ"] ["&ccedil;" "ç"] ["&egrave;" "è"] ["&eacute;" "é"] ["&ecirc;" "ê"] ["&euml;" "ë"] ["&igrave;" "ì"] ["&iacute;" "í"] ["&icirc;" "î"] ["&iuml;" "ï"] ["&eth;" "ð"] ["&ntilde;" "ñ"] ["&ograve;" "ò"] ["&oacute;" "ó"] ["&ocirc;" "ô"] ["&otilde;" "õ"] ["&ouml;" "ö"] ["&divide;" "÷"] ["&oslash;" "ø"] ["&ugrave;" "ù"] ["&uacute;" "ú"] ["&ucirc;" "û"] ["&uuml;" "ü"] ["&yacute;" "ý"] ["&thorn;" "þ"] ["&yuml;" "ÿ"] ["&fnof;" "ƒ"] ["&Alpha;" "Α"] ["&Beta;" "Β"] ["&Gamma;" "Γ"] ["&Delta;" "Δ"] ["&Epsilon;" "Ε"] ["&Zeta;" "Ζ"] ["&Eta;" "Η"] ["&Theta;" "Θ"] ["&Iota;" "Ι"] ["&Kappa;" "Κ"] ["&Lambda;" "Λ"] ["&Mu;" "Μ"] ["&Nu;" "Ν"] ["&Xi;" "Ξ"] ["&Omicron;" "Ο"] ["&Pi;" "Π"] ["&Rho;" "Ρ"] ["&Sigma;" "Σ"] ["&Tau;" "Τ"] ["&Upsilon;" "Υ"] ["&Phi;" "Φ"] ["&Chi;" "Χ"] ["&Psi;" "Ψ"] ["&Omega;" "Ω"] ["&alpha;" "α"] ["&beta;" "β"] ["&gamma;" "γ"] ["&delta;" "δ"] ["&epsilon;" "ε"] ["&zeta;" "ζ"] ["&eta;" "η"] ["&theta;" "θ"] ["&iota;" "ι"] ["&kappa;" "κ"] ["&lambda;" "λ"] ["&mu;" "μ"] ["&nu;" "ν"] ["&xi;" "ξ"] ["&omicron;" "ο"] ["&pi;" "π"] ["&rho;" "ρ"] ["&sigmaf;" "ς"] ["&sigma;" "σ"] ["&tau;" "τ"] ["&upsilon;" "υ"] ["&phi;" "φ"] ["&chi;" "χ"] ["&psi;" "ψ"] ["&omega;" "ω"] ["&thetasym;" "ϑ"] ["&upsih;" "ϒ"] ["&piv;" "ϖ"] ["&bull;" "•"] ["&hellip;" "…"] ["&prime;" "′"] ["&Prime;" "″"] ["&oline;" "‾"] ["&frasl;" "⁄"] ["&weierp;" "℘"] ["&image;" "ℑ"] ["&real;" "ℜ"] ["&trade;" "™"] ["&alefsym;" "ℵ"] ["&larr;" "←"] ["&uarr;" "↑"] ["&rarr;" "→"] ["&darr;" "↓"] ["&harr;" "↔"] ["&crarr;" "↵"] ["&lArr;" "⇐"] ["&uArr;" "⇑"] ["&rArr;" "⇒"] ["&dArr;" "⇓"] ["&hArr;" "⇔"] ["&forall;" "∀"] ["&part;" "∂"] ["&exist;" "∃"] ["&empty;" "∅"] ["&nabla;" "∇"] ["&isin;" "∈"] ["&notin;" "∉"] ["&ni;" "∋"] ["&prod;" "∏"] ["&sum;" "∑"] ["&minus;" "−"] ["&lowast;" "∗"] ["&radic;" "√"] ["&prop;" "∝"] ["&infin;" "∞"] ["&ang;" "∠"] ["&and;" "∧"] ["&or;" "∨"] ["&cap;" "∩"] ["&cup;" "∪"] ["&int;" "∫"] ["&there4;" "∴"] ["&sim;" "∼"] ["&cong;" "≅"] ["&asymp;" "≈"] ["&ne;" "≠"] ["&equiv;" "≡"] ["&le;" "≤"] ["&ge;" "≥"] ["&sub;" "⊂"] ["&sup;" "⊃"] ["&nsub;" "⊄"] ["&sube;" "⊆"] ["&supe;" "⊇"] ["&oplus;" "⊕"] ["&otimes;" "⊗"] ["&perp;" "⊥"] ["&sdot;" "⋅"] ["&lceil;" "⌈"] ["&rceil;" "⌉"] ["&lfloor;" "⌊"] ["&rfloor;" "⌋"] ["&lang;" "〈"] ["&rang;" "〉"] ["&loz;" "◊"] ["&spades;" "♠"] ["&clubs;" "♣"] ["&hearts;" "♥"] ["&diams;" "♦"] ["&quot;" "\""] ["&OElig;" "Œ"] ["&oelig;" "œ"] ["&Scaron;" "Š"] ["&scaron;" "š"] ["&Yuml;" "Ÿ"] ["&circ;" "ˆ"] ["&tilde;" "˜"] ["&ndash;" "–"] ["&mdash;" "—"] ["&lsquo;" "‘"] ["&rsquo;" "’"] ["&sbquo;" "‚"] ["&ldquo;" "“"] ["&rdquo;" "”"] ["&bdquo;" "„"] ["&dagger;" "†"] ["&Dagger;" "‡"] ["&permil;" "‰"] ["&lsaquo;" "‹"] ["&rsaquo;" "›"] ["&euro;" "€"]
                                      ]
                                     )))

    (if ξwork-on-string-p
        ξoutput-str
      (save-excursion
        (delete-region φfrom φto)
        (goto-char φfrom)
        (insert ξoutput-str)))))

(defun xhm-get-html-file-title (φfname)
  "Return φfname <title> tag's text.
Assumes that the file contains the string
“<title>…</title>”."
  (with-temp-buffer
    (insert-file-contents φfname nil nil nil t)
    (goto-char 1)
    (buffer-substring-no-properties
     (search-forward "<title>") (- (search-forward "</title>") 8))))

(defun xhm-lines-to-html-list ()
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
  (let (bds p1 p2 ξinput-str resultStr)
    (setq bds (get-selection-or-unit 'block))
    (setq ξinput-str (elt bds 0) p1 (elt bds 1) p2 (elt bds 2))
    (save-excursion
      (setq resultStr
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
    (delete-region p1 p2)
    (insert resultStr)))

(defun xhm-make-html-table-string (φtextBlock φdelimiter)
  "Transform the string TEXTBLOCK into a HTML marked up table.

 “\\n” is used as delimiter of rows. Extra newlines at the end is discarded.
The argument φdelimiter is a char used as the delimiter for columns.

 See the parent function `xhm-make-html-table'."
(let ((txtbk φtextBlock))
    (setq txtbk (replace-regexp-in-string "\n+$" "\n" (concat txtbk "\n"))) ; make sure ending is just one newline char
    (setq txtbk (replace-regexp-in-string φdelimiter "</td><td>" txtbk))
    (setq txtbk (replace-regexp-in-string "\n" "</td></tr>\n<tr><td>" txtbk))
    (setq txtbk (substring txtbk 0 -8)) ; delete the beginning “<tr><td>” in last line
    (concat "<table class=\"nrm\">\n<tr><td>" txtbk "</table>")
))

(defun xhm-make-html-table (φsep)
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
  (let (bds p1 p2 myStr)

    (setq bds (get-selection-or-unit 'block))
    (setq myStr (elt bds 0))
    (setq p1 (elt bds 1))
    (setq p2 (elt bds 2))
    (delete-region p1 p2)
    (insert (xhm-make-html-table-string myStr φsep) "\n")))

(defun xhm-make-html-table-undo ()
  "inverse of `xhm-make-html-table'."
  (interactive)
  (let ( p1 p2 myStr)
    (search-backward "<table")
    (setq p1 (point))
    (search-forward "</table>")
    (setq p2 (point))
  ;(replace-pairs-region p1 p2 [
  ;])

    (replace-regexp-pairs-region p1 p2 [
                                        ["<table \\([^>]+?\\)>" ""]
                                        ["</th><th>" "•"]
                                        ["</td><td>" "•"]
                                        ["<tr>" ""]
                                        ["</tr>" ""]
                                        ["</table>" ""]
                                        ]
                                 "FIXEDCASE" "LITERAL"
                                 )))

(defun xhm-word-to-wikipedia-linkify ()
  "Make the current word or text selection into a Wikipedia link.

For Example: 「Emacs」 ⇒ 「<a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>」"
  (interactive)
  (let (linkText bds p0 p1 p2 wikiTerm resultStr)

    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end)))
      (progn
        (setq p0 (point))
        (skip-chars-backward "^ \t\n")
        (setq p1 (point))
        (goto-char p0)
        (skip-chars-forward "^ \t\n")
        (setq p2 (point))))

    (setq linkText (buffer-substring-no-properties p1 p2))
    (setq wikiTerm (replace-regexp-in-string " " "_" linkText))
    (setq resultStr (concat "<a href=\"http://en.wikipedia.org/wiki/" wikiTerm "\">" linkText "</a>"))
    (delete-region p1 p2)
    (insert resultStr)))

(defun xhm-remove-span-tag-region (φp1 φp2)
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
    (replace-regexp-pairs-region (point-min) (point-max) '(["<span class=\"[^\"]+\">" ""]))
    (replace-pairs-region (point-min) (point-max) '( ["</span>" ""] ))
    (xhm-replace-html-&<>-to-entities (point-min) (point-max) "ΦENTITY-TO-CHAR-P")
    (goto-char (point-max))))

(defun xhm-code-tag-to-brackets (φp1 φp2 &optional φchange-entity-p)
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
   (let ((bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2) (if current-prefix-arg nil t))))

  (save-restriction
    (narrow-to-region φp1 φp2)
    (replace-regexp-pairs-region (point-min) (point-max) '(["<code class=\"[^\"]+\">" "「"] ["<var class=\"[^\"]+\">" "‹"]))
    (replace-pairs-region
     (point-min) (point-max)
     '(
       ["<code>" "「"]
       ["</code>" "」"]
       ["<var>" "‹"]
       ["</var>" "›"] ))
    (when φchange-entity-p (xhm-replace-html-&<>-to-entities (point-min) (point-max) "ΦENTITY-TO-CHAR-P"))
    (goto-char (point-max))))

(defun xhm-remove-html-tags (φstring &optional φfrom φto)
  "Delete HTML tags in string or region.
Work on current text block or text selection. (a “text block” is text between blank lines)

When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions φfrom φto.

WARNING: this command does not cover all HTML tags or convert all HTML entities. For robust solution you might use: 「lynx -dump -display_charset=utf-8 URL」."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)))
       (list nil (elt bds 1) (elt bds 2)))))

  (let (ξwork-on-string-p ξinput-str ξoutput-str)
    (setq ξwork-on-string-p (if φstring t nil))
    (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))
    (setq ξoutput-str
          (let ((case-fold-search t) (tempStr ξinput-str))

            (setq tempStr
                  (replace-regexp-pairs-in-string
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

;; (defun xhm-html-to-text (φstring &optional φfrom φto)
;; "Convert html to plain text on text selection or current text block."
;;   (interactive
;;    (if (region-active-p)
;;        (list nil (region-beginning) (region-end))
;;      (let ((bds (get-selection-or-unit 'block)) )
;;        (list nil (elt bds 1) (elt bds 2))) ) )

;;   (let (ξwork-on-string-p ξinput-str ξoutput-str)
;;     (setq ξwork-on-string-p (if φstring t nil))
;;     (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))
;;     (setq ξoutput-str
;;           (let ((case-fold-search t) (tempStr ξinput-str))
;; (setq tempStr (replace-regexp-pairs-in-string tempStr '(
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

;; (setq ξoutput-str (xhm-remove-html-tags ξoutput-str) )

;;     (if ξwork-on-string-p
;;         ξoutput-str
;;       (save-excursion
;;         (delete-region φfrom φto)
;;         (goto-char φfrom)
;;         (insert ξoutput-str) )) ) )

(defun xhm-html-to-text (φstring &optional φfrom φto)
  "Convert html to plain text on text selection or current text block."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)))
       (list nil (elt bds 1) (elt bds 2)))))

  (let (ξwork-on-string-p ξinput-str ξoutput-str)
    (setq ξwork-on-string-p (if φstring t nil))
    (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties φfrom φto)))

    (setq ξoutput-str
          (with-temp-buffer
            (insert ξinput-str)
            (goto-char 1)
            (let ((case-fold-search nil))
              (replace-pairs-region 1 (point-max)
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
            ;; (xahsite-filepath-to-url (xahsite-href-value-to-filepath ξx (buffer-file-name) ))
            (buffer-substring 1 (point-max))))

    (setq ξoutput-str (xhm-remove-html-tags ξoutput-str))

    (if ξwork-on-string-p
        ξoutput-str
      (save-excursion
        (delete-region φfrom φto)
        (goto-char φfrom)
        (insert ξoutput-str)))))

(defun xhm-extract-url (φhtml-text &optional φconvert-relative-URL-p)
  "Returns a list of URLs in the HTML text string φhtml-text.

When called interactively, use text selection as input, or current paragraph. output is copied to `kill-ring'.

If `universal-argument' is called first, convert relative URL to full path.

When called in lisp program, φhtml-text is the input string.

This command extracts all text of the form
 <‹letter› … href/src=\"…\" …>
on a single line, by regex. The quote may be single quote."
  (interactive (list
                (if (use-region-p)
                    (progn (buffer-substring-no-properties (region-beginning) (region-end)))
                  (progn (thing-at-point 'paragraph)))
                current-prefix-arg ))
  (let ((urlList (list)))
    (with-temp-buffer
      (insert φhtml-text)
      (goto-char 1)
      (while (re-search-forward
              "<[[:alpha:]]+.+?\\(href\\|src\\)[[:blank:]]*=[[:blank:]]*\\([\"']\\)\\([^\2]+?\\)\\2.+?>" nil t)
        (push (match-string 3) urlList)))
    (setq urlList (reverse urlList))

    (when φconvert-relative-URL-p
      (setq urlList
            (mapcar
             (lambda (ξx)
               (if (string-match "^http" ξx )
                   (progn ξx)
                 (progn
                   ;; (xahsite-filepath-to-url (xahsite-href-value-to-filepath ξx (buffer-file-name)))
                   (expand-file-name ξx (file-name-directory (buffer-file-name))))))
             urlList)))

    (when (called-interactively-p 'any)
      (let ((printedResult (mapconcat 'identity urlList "\n")))
        (kill-new printedResult)
        (message "%s" printedResult)))
    urlList ))

(defun xhm-update-title ( φnewTitle)
  "Update a HTML article's title and h1 tags.
Update the <title>…</title> and <h1>…</h1> of current buffer."
  (interactive
   (let (oldTitle)
     (save-excursion
       (goto-char 1)
       (search-forward-regexp "<title>\\([^<]+?\\)</title>")
       (setq oldTitle (match-string 1 )))
     (list (read-string "New title:" oldTitle nil oldTitle "INHERIT-INPUT-METHOD"))))
  (let (p1 p2)
    (save-excursion
      (goto-char 1)

      (progn (search-forward "<title>")
             (setq p1 (point))
             (search-forward "</title>")
             (search-backward "<")
             (setq p2 (point))
             (delete-region p1 p2 )
             (goto-char p1)
             (insert φnewTitle ))

      (if (search-forward "<h1>")
          (progn
            (setq p1 (point))
            (search-forward "</h1>")
            (search-backward "<")
            (setq p2 (point))
            (delete-region p1 p2 )
            (goto-char p1)
            (insert φnewTitle ))
        (progn
          (message "<h1> tag not found. adding"))))))

(defun xhm-make-citation ()
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
         (bds (get-selection-or-unit 'block))
         (inputText (elt bds 0))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
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
         ((is-datetimestamp-p ξx) (setq ξdate ξx))
         (t (setq ξtitle ξx)))))

    (message "title:「%s」\n author:「%s」\n date:「%s」\n url:「%s」" ξtitle ξauthor ξdate ξurl)

    (when (null ξtitle) (error "I can't find “title”"))
    (when (null ξauthor) (error "I can't find “author”"))
    (when (null ξdate) (error "I can't find “date”"))
    (when (null ξurl) (error "I can't find “url”"))

    (setq ξtitle (trim-string ξtitle))
    (setq ξtitle (replace-regexp-in-string "^\"\\(.+\\)\"$" "\\1" ξtitle))
    (setq ξtitle (replace-pairs-in-string ξtitle '(["’" "'"] ["&" "＆"] )))

    (setq ξauthor (trim-string ξauthor))
    (setq ξauthor (replace-regexp-in-string "\\. " " " ξauthor)) ; remove period in Initals
    (setq ξauthor (replace-regexp-in-string "[Bb]y +" "" ξauthor))
    (setq ξauthor (upcase-initials (downcase ξauthor)))

    (setq ξdate (trim-string ξdate))
    (setq ξdate (fix-datetimestamp ξdate))

    (setq ξurl (trim-string ξurl))
    (setq ξurl (with-temp-buffer (insert ξurl) (xhm-source-url-linkify 1) (buffer-string)))

    (delete-region p1 p2 )
    (insert (concat "〔<cite>" ξtitle "</cite> ")
            "<time>" ξdate "</time>"
            " By " ξauthor
            ". @ " ξurl
            "〕")))

(defun xhm-make-link-defunct ()
  "Make the html link under cursor to a defunct form.
Example:
If cursor is inside this tag
 <a class=\"sorc\" href=\"http://example.com/\" data-accessed=\"2008-12-26\">…</a>
 (and inside the opening tag.)

It becomes:

 <s data-accessed=\"2006-03-11\" data-defunct-date=\"2014-01-11\">http://www.math.ca/cgi/kabol/search.pl</s>

old version output:
<s class=\"deadurl\" title=\"accessed:2008-12-26; defunct:2008-12-26; http://example.com\">…</s>"
  (interactive)
  (let (p1 p2 wholeLinkStr newLinkStr ξurl accessedDate)
    (save-excursion
      ;; get the boundary of opening tag
      (forward-char 3)
      (search-backward "<a " ) (setq p1 (point))
      (search-forward "</a>") (setq p2 (point))

      ;; get wholeLinkStr
      (setq wholeLinkStr (buffer-substring-no-properties p1 p2))

      ;; generate replacement text
      (with-temp-buffer
        (insert wholeLinkStr)

        (goto-char 1)
        (search-forward-regexp  "href=\"\\([^\"]+?\\)\"")
        (setq ξurl (match-string 1))

        (search-forward-regexp  "data-accessed=\"\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)\"")
        (setq accessedDate (match-string 1))

        (setq newLinkStr (format "<s data-accessed=\"%s\" data-defunct-date=\"%s\">%s</s>" accessedDate (format-time-string "%Y-%m-%d") ξurl ))))

    (delete-region p1 p2)
    (insert newLinkStr)))

(defun xhm-source-url-linkify (prefixArgCode)
  "Make URL at cursor point into a html link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
<a class=\"sorc\" href=\"http://example.com/xyz.htm\" data-accessed=\"2008-12-25\">example.com…</a>

The anchor text may be of 4 possibilities, depending on value of `universal-argument'.

1 → 「‹full url›」
2 or 4 → 「‹domain›…」
3 → 「img src」
0 or any → smartly decide."

  (interactive "P")
  (let (ξinput-str
        bds p1-input p2-input
        p1-url p2-url p1-tag p2-tag
        ξurl domainName linkText resultLinkStr)

    (setq bds (get-selection-or-unit 'url))
    (setq ξinput-str (elt bds 0))
    (setq p1-input (elt bds 1))
    (setq p2-input (elt bds 2))

    ;; check if it's just plain URL or already in linked form 「<a href=…>…</a>」
    ;; If latter, you need to get the boundaries for the entire link too.
    (if (string-match "href=\"" ξinput-str)
        (save-excursion
          (search-backward "href=" (- (point) 104)) ; search boundary as extra guard for error
          (forward-char 6)
          (setq p1-url (point))
          (search-forward "\"" (+ p1-url 104))
          (setq p2-url (- (point) 1))

          (goto-char p1-url)
          (search-backward "<a" (- p1-url 30))
          (setq p1-tag (point))
          (goto-char p2-url)
          (search-forward "</a>" (+ p2-url 140))
          (setq p2-tag (point)))
      (progn
        (setq p1-url p1-input)
        (setq p2-url p2-input)
        (setq p1-tag p1-input)
        (setq p2-tag p2-input)))

    (setq ξurl (replace-regexp-in-string "&amp;" "&" (buffer-substring-no-properties p1-url p2-url) nil "LITERAL")) ; in case it's already encoded. TODO this is only 99% correct.

    ;; get the domainName
    (setq domainName
          (progn
            (string-match "://\\([^\/]+?\\)/" ξurl)
            (match-string 1 ξurl)))

    (setq linkText
          (cond
           ((equal prefixArgCode 1) ξurl) ; full url
           ((or (equal prefixArgCode 2) (equal prefixArgCode 4) (equal prefixArgCode '(4))) (concat domainName "…")) ; ‹domain›…
           ((equal prefixArgCode 3) "img src") ; img src
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
    (setq resultLinkStr
          (format "<a class=\"sorc\" href=\"%s\" data-accessed=\"%s\">%s</a>"
                  ξurl (format-time-string "%Y-%m-%d") linkText
                  ))

    ;; delete URL and insert the link
    (delete-region p1-tag p2-tag)
    (insert resultLinkStr)))

(defun xhm-wikipedia-url-linkify (φstring &optional φfrom-to-pair)
  "Make the URL at cursor point into a html link.

If there is a text selection, use that as input.

Example:
http://en.wikipedia.org/wiki/Emacs
⇒
<a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>.

When called interactively, work on current URL or text selection (of a URL).

When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions in sequence φfrom-to-pair."

  (interactive
   (if (region-active-p)
       (list nil (vector (region-beginning) (region-end)))
     (let (p0 p1 p2)
       (progn
         (setq p0 (point))
         (skip-chars-backward "^ \t\n<>[]")
         (setq p1 (point))
         (goto-char p0)
         (skip-chars-forward "^ \t\n<>[]")
         (setq p2 (point))
         (list nil (vector p1 p2))))))

  (let (ξwork-on-string-p ξinput-str ξoutput-str
                          (ξfrom (elt φfrom-to-pair 0))
                          (ξto (elt φfrom-to-pair 1)))
    (setq ξwork-on-string-p (if () t nil))
    (setq ξinput-str (if ξwork-on-string-p φstring (buffer-substring-no-properties ξfrom ξto)))

    (setq ξoutput-str
          (format "<a href=\"%s\">%s</a>" (url-encode-url ξinput-str)
                  (replace-regexp-in-string "_" " "
                                            (xhm-url-percent-decode-string (file-name-nondirectory ξinput-str)))))

    (if ξwork-on-string-p
        ξoutput-str
      (progn
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert ξoutput-str)))))

(defun xhm-wrap-url (φstring &optional φfrom φto)
  "Make the URL at cursor point into a html link.

When called interactively, work on current glyph sequence or text selection.

When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions φfrom φto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (unit-at-cursor 'glyphs)))
       (list nil (elt bds 1) (elt bds 2)))))

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

(defun xhm-wrap-p-tag ()
  "Add <p>…</p> tag to current text block or text selection.
If there's a text selection, wrap p around each text block (separated by 2 newline chars.)"
  (interactive)
  (let (bds p1 p2 inputText)

    (setq bds (get-selection-or-unit 'block))
    (setq inputText (elt bds 0))
    (setq p1 (elt bds 1))
    (setq p2 (elt bds 2))

    (delete-region p1 p2 )
    (insert "<p>" (replace-regexp-in-string "\n\n+" "</p>\n\n<p>" (trim-string inputText)) "</p>")))

(defun xhm-emacs-to-windows-kbd-notation-string (φinput-string)
  "Change emacs keyboard-shortcut notation to Windows's notation.
For example:
 「C-h f」⇒ 「Ctrl+h f」
 「M-a」⇒ 「Meta+a」
 「<f9> <f8>」 ⇒ 「F9 F8」

This command will do most emacs syntax correctly, but not 100% correct.
"
  (let ((case-fold-search nil))
    (replace-regexp-pairs-in-string φinput-string
                                    [
                                     ["C-\\(.\\)" "Ctrl+\\1"]
                                     ["M-\\(.\\)" "Meta+\\1"]
                                     ["S-\\(.\\)" "Shift+\\1"]
                                     ["s-\\(.\\)" "Super+\\1"]
                                     ["H-\\(.\\)" "Hyper+\\1"]

                                     ["<prior>" "PageUp"]
                                     ["<next>" "PageDown"]
                                     ["<home>" "Home"]
                                     ["<end>" "End"]

                                     ["<f1>" "F1"] ["<f2>" "F2"] ["<f3>" "F3"] ["<f4>" "F4"] ["<f5>" "F5"] ["<f6>" "F6"] ["<f7>" "F7"] ["<f8>" "F8"] ["<f9>" "F9"] ["<f10>" "F10"] ["<f11>" "F11"] ["<f12>" "F12"]

                                     ["RET" "Enter"]
                                     ["<return>" "Return"]
                                     ["TAB" "Tab"]
                                     ["<tab>" "Tab"]

                                     ["<right>" "→"]
                                     ["<left>" "←"]
                                     ["<up>" "↑"]
                                     ["<down>" "↓"]

                                     ["<insert>" "Insert"]
                            ["<delete>" "Delete"]

                            ["<backspace>" "Backspace"]
                            ["DEL" "Delete"]
                            ]
 "FIXEDCASE"))
 )

(defun xhm-emacs-to-windows-kbd-notation (φp1 φp2)
  "Change emacs key notation to Windows's notation.

For example:
 【C-h f】⇒ 【Ctrl+h f】
 【M-a】⇒ 【Meta+a】

When called interactively, work on text selection or text enclosed in 【…】.

For detail on exactly which string are changed, see `xhm-emacs-to-windows-kbd-notation-string'.
"
  (interactive
   (let ((bds (get-selection-or-unit ["^【" "^】"])))
     (list (elt bds 1) (elt bds 2))))

  (let (  (case-fold-search nil)
          (ξinput-str (buffer-substring-no-properties φp1 φp2)))
    (delete-region φp1 φp2)
    (insert
     (xhm-emacs-to-windows-kbd-notation-string ξinput-str))))

(defun xhm-htmlize-elisp-keywords (φp1 φp2)
  "Replace curly quoted elisp function/variable names to HTML markup.

Example:
 Call “sort-lines” to sort.
    ⇓
 Call <code class=\"elisp-ƒ\">sort-lines</code> to sort.

 Set “fill-column”
    ⇓
 Set <var class=\"elisp\">fill-column</var>

Works on text selection or current text block.

When called in lisp program, the arguments φp1 φp2 are region positions.

Note: a word is changed only if all of the following are true:

• The symbol string is tightly enclosed in double curly quotes, e.g. “sort-lines” but not “use sort-lines”.
• `fboundp' or `boundp' returns true (for function and variable.).
• symbol string's char contains only alphanumeric or hyphen, even though elisp identifier allows many other chars. e.g. `yas/reload-all', `color-cie-ε'.

This command also makes a report of changed items.

Some issues:

• Some words are common in other lang, e.g. “while”, “print”, “string”, unix “find”, “grep”, HTML's “kbd” tag, etc. But they are also built-in elisp symbols. This command will tag them, but you may not want that.

• Some function/variable are from 3rd party libs, and some are not bundled with GNU emacs , e.g. 「'cl」, 「'htmlize」. They may or may not be tagged depending whether they've been loaded."
  ;; (interactive (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ) )
  (interactive
   (cond
    ((equal current-prefix-arg nil) ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2))))
    (t ; all other cases
     (list (point-min) (point-max)))))
  (let*
      (ξinput-str
       resultStr
       (changedItems nil)
       (elispIdentifierRegex "\\([:-A-Za-z0-9]+\\)")
       (wantedRegex (concat "“" elispIdentifierRegex "”")))
    (setq ξinput-str (buffer-substring-no-properties φp1 φp2))
    (setq resultStr
          (let ( mStr (case-fold-search nil) (ξsomeStr ξinput-str))
            (with-temp-buffer
              (insert ξsomeStr)
              (goto-char 1)
              (while (search-forward-regexp wantedRegex (point-max) t)
                (setq mStr (match-string 1))
                (cond
                 ((fboundp (intern mStr))
                  (progn
                    (setq changedItems (cons (format "ƒ %s" mStr) changedItems ))
                    (replace-match (concat "<code class=\"elisp-ƒ\">" mStr "</code>") t t)))
                 ((boundp (intern mStr))
                  (progn
                    (setq changedItems (cons (format "υ %s" mStr) changedItems ))
                    (replace-match (concat "<var class=\"elisp\">" mStr "</var>") t t)))
                 (t "do nothing")))
              (buffer-string))))
    (if (equal (length changedItems) 0)
        (progn (message "%s" "No change needed."))
      (progn
        (delete-region φp1 φp2)
        (insert resultStr)
        (with-output-to-temp-buffer "*changed items*"
          (mapcar (lambda (x) (princ x) (princ "\n")) (reverse changedItems)))))))

(defun xhm-htmlize-keyboard-shortcut-notation ()
  "Wrap a “kbd” tag around keyboard keys on text selection or current line.
Example: 【ctrl+w】 ⇒ 【<kbd>Ctrl</kbd>+<kbd>w</kbd>】
Emacs's key notation also supported. Example: 【C-x t】 ⇒ 【<kbd>Ctrl</kbd>+<kbd>x</kbd> <kbd>t</kbd>】
Same for Alt, Shift, Cmd, Win, Enter, Return, Home… and other strings.
Case shouldn't matter, except when it's emacs's key notation.
"
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'line))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         (replaceList [
                       ;; case in find string shouldn't matter.
                       ["ctrl" "<kbd>Ctrl</kbd>"]
                       ["control" "<kbd>Ctrl</kbd>"]
                       ["altgr" "<kbd>AltGr</kbd>"]
                       ["compose" "<kbd>⎄ Compose</kbd>"]
                       ["alt" "<kbd>Alt</kbd>"]
                       ["shift" "<kbd>⇧ Shift</kbd>"]
                       ["cmd" "<kbd>⌘ Cmd</kbd>"]
                       ["option" "<kbd>⌥ Opt</kbd>"]
                       ["opt" "<kbd>⌥ Opt</kbd>"]
                       ["win" "<kbd>❖ Win</kbd>"]
                       ["menu" "<kbd>▤ Menu</kbd>"]
                       ["meta" "<kbd>◆ Meta</kbd>"]
                       ["super" "<kbd>❖ Super</kbd>"]
                       ["hyper" "<kbd>Hyper</kbd>"]

                       ["return" "<kbd>Return ↩</kbd>"]
                       ["enter" "<kbd>Enter ↵</kbd>"]
                       ["backspace" "<kbd>⌫ Backspace</kbd>"]
                       ["delete" "<kbd>⌦ Delete</kbd>"]
                       ["del" "<kbd>⌦ Delete</kbd>"]
                       ["space" "<kbd>Space</kbd>"]
                       ["SPC" "<kbd>Space</kbd>"]
                       ["capslock" "<kbd>Caps Lock</kbd>"]
                       ["f-lock" "<kbd>F Lock</kbd>"]
                       ["f lock" "<kbd>F Lock</kbd>"]
                       ["numlock" "<kbd>Num Lock</kbd>"]
                       ["scrolllock" "<kbd>Scroll Lock</kbd>"]
                       ["tab" "<kbd>Tab ↹</kbd>"]
                       ["esc" "<kbd>Esc</kbd>"]

                       ["Home" "<kbd>↖ Home</kbd>"]
                       ["End" "<kbd>↘ End</kbd>"]
                       ["PageUp" "<kbd>⇞ Page △</kbd>"]
                       ["Page Up" "<kbd>⇞ Page △</kbd>"]
                       ["PgUp" "<kbd>⇞ Page △</kbd>"]
                       ["PageDown" "<kbd>⇟ Page ▽</kbd>"]
                       ["Page Down" "<kbd>⇟ Page ▽</kbd>"]
                       ["PgDn" "<kbd>⇟ Page ▽</kbd>"]
                       ["insert" "<kbd>Insert</kbd>"]
                       ["ins" "<kbd>Insert</kbd>"]
                       ["pause" "<kbd>Pause</kbd>"]
                       ["PrtScn" "<kbd>PrtScn</kbd>"]
                       ["prtsc" "<kbd>PrtScn</kbd>"]
                       ["prntscr" "<kbd>PrtScn</kbd>"]
                       ["scrlk" "<kbd>Scroll Lock</kbd>"]
                       ["Fn" "<kbd>Fn</kbd>"]

                       ["copy" "<kbd>Copy</kbd>"]
                       ["cut" "<kbd>✂ Cut</kbd>"]
                       ["paste" "<kbd>Paste</kbd>"]
                       ["undo" "<kbd>⎌ Undo</kbd>"]
                       ["redo" "<kbd>↷</kbd>"]

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

                       ["←" "<kbd>←</kbd>"]
                       ["→" "<kbd>→</kbd>"]
                       ["↑" "<kbd>↑</kbd>"]
                       ["↓" "<kbd>↓</kbd>"]

                       ["‹key›" "<kbd>‹key›</kbd>"]
                       ]))

    (let ((case-fold-search t) (case-replace nil))
      (save-restriction
        (narrow-to-region p1 p2)
        (xhm-emacs-to-windows-kbd-notation (point-min) (point-max))

        (goto-char (point-min))

        (replace-pairs-region (point-min) (point-max) replaceList)
        (replace-regexp-pairs-region
         (point-min) (point-max)
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

(defvar xhm-html-tag-input-history nil "for input history of `xhm-wrap-html-tag'")
(setq xhm-tag-input-history (list))

(defvar xhm-class-input-history nil "for input history of `xhm-wrap-html-tag'")
(setq xhm-class-input-history (list))

(defvar xhm-class-input-history nil "for input history of `xhm-wrap-html-tag'")
(setq xhm-class-input-history (list))

(defun xhm-add-open/close-tag (φtag-name φclass-name φp1 φp2)
  "Add HTML open/close tags around region boundary φp1 φp2.
This function does not `save-excursion'."
  (let* (
         (classStr (if (or (equal φclass-name nil) (string= φclass-name "")) "" (format " class=\"%s\"" φclass-name)))
         (insStrLeft (format "<%s%s>" φtag-name classStr))
         (insStrRight (format "</%s>" φtag-name )))

    (goto-char φp1)

    (if (xhm-tag-self-closing? φtag-name)
        (progn (insert (format "<%s%s />" φtag-name classStr)))
      (progn
        (insert insStrLeft )
        (goto-char (+ φp2 (length insStrLeft)))
        (insert insStrRight )))))

(defun xhm-wrap-html-tag (φtag-name &optional φclass-name)
  "Insert/wrap HTML tag to current text unit or text selection.
When there's no text selection, the tag will be wrapped around current {word, line, text-block}, depending on the tag used.

If current line or word is empty, then insert open/end tags and place cursor between them.
If `universal-argument' is called first, then also prompt for a “class” attribute. Empty value means don't add the attribute."
  (interactive
   (list
    (ido-completing-read "HTML tag:" xhm-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xhm-html-tag-input-history "div")
    (if current-prefix-arg
        (read-string "class:" nil xhm-class-input-history "")
      nil )))
  (let (bds p1 p2
            lineWordBlock
            )
    (progn
      (setq lineWordBlock (xhm-get-tag-type φtag-name))
      (setq bds
            (cond
             ((equal lineWordBlock "w") (get-selection-or-unit 'word))
             ((equal lineWordBlock "l") (get-selection-or-unit 'line))
             ((equal lineWordBlock "b") (get-selection-or-unit 'block))
             (t (get-selection-or-unit 'word))))
      (setq p1 (elt bds 1))
      (setq p2 (elt bds 2))
      (xhm-add-open/close-tag φtag-name φclass-name p1 p2)

      (when ; put cursor between when input text is empty
          (and (equal p1 p2) (not (xhm-tag-self-closing? φtag-name)))
        (progn (search-backward "</" ))))))

(defun xhm-insert-wrap-source-code (&optional φlang-code)
  "Insert/wrap a <pre class=\"‹φlang-code›\"> tags to text selection or current text block."
  (interactive
   (list
    (ido-completing-read "lang code:" (mapcar (lambda (x) (car x)) xhm-lang-name-map) "PREDICATE" "REQUIRE-MATCH" nil xhm-html-tag-input-history "code")))
  (let (bds p1 p2)
    (setq bds (get-selection-or-unit 'block))
    (setq p1 (elt bds 1))
    (setq p2 (elt bds 2))
    (xhm-add-open/close-tag "pre" φlang-code p1 p2)))

(defun xhm-rename-html-inline-image (φnew-file-path)
  "Replace current HTML inline image's file name.

When cursor is in HTML link file path, e.g.  <img src=\"gki/macosxlogo.png\" > and this command is called, it'll prompt user for a new name. The link path will be changed to the new name, the corresponding file will also be renamed. The operation is aborted if a name exists."
  (interactive
   (let (
         (defaultInput (expand-file-name
                        (elt (get-selection-or-unit 'filepath) 0)
                        (file-name-directory (or (buffer-file-name) default-directory )))))
     (list (read-string "New name: " defaultInput nil defaultInput ))))
  (let* (
         (bds (get-selection-or-unit 'filepath))
         (ξinputPath (elt bds 0))
         (p1 (aref bds 1))
         (p2 (aref bds 2))
         (ξffp (local-url-to-file-path (expand-file-name ξinputPath (file-name-directory (or (buffer-file-name) default-directory ))))) ;full path
         ;; (setq ξffp (windows-style-path-to-unix (local-url-to-file-path ξffp)))
         )

    (if (file-exists-p φnew-file-path)
        (progn (error "file 「%s」 exist." φnew-file-path ))
      (progn
        (rename-file ξffp φnew-file-path )
        (message "rename to %s" φnew-file-path)
        (delete-region p1 p2)
        (insert (xahsite-filepath-to-href-value φnew-file-path (or (buffer-file-name) default-directory)))))))

(defun xhm-mark-unicode (φp1)
  "Wrap a special <mark> tag  around the character before cursor.
like this:
 <mark class=\"unicode\" title=\"U+3B1: GREEK SMALL LETTER ALPHA\">α</mark>

When called in elisp program, wrap the tag around charbefore position φp1."
  (interactive (list (point)))
  (let* (
         (ξcodepoint (string-to-char (buffer-substring-no-properties (- φp1 1) φp1 )))
         (ξname (get-char-code-property ξcodepoint 'name)))
    (goto-char (- φp1 1))
    (insert (format "<mark class=\"unicode\" title=\"U+%X: %s\">" ξcodepoint ξname))
    (right-char 1)
    (insert (format "</mark>"))))

(defun xhm-clean-whitespace ()
  "Delete redundant whitespace in HTML file.
Work on text selection or whole buffer.
This is heuristic based, does not remove ALL possible redundant whitespace."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'buffer))
         (p1 (elt bds 1))
         (p2 (elt bds 2)))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "[ \t]+\n" nil "noerror")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp " *<p>\n+" nil "noerror")
            (replace-match "<p>")))))))

(defun xhm-url-percent-decode-string (φstring)
  "Returns URL percent-encoded

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_%28D%C3%BCrer%29
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)

    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
    http://zh.wikipedia.org/wiki/文本编辑器"
  (decode-coding-string (url-unhex-string φstring) 'utf-8))

(defun xhm-decode-percent-encoded-uri (&optional φp1 φp2)
  "decode URI percent encoding of current line or selection."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((myStr (buffer-substring-no-properties φp1 φp2)))
    (save-excursion
      (save-restriction
        (delete-region φp1 φp2 )
        (insert (decode-coding-string (url-unhex-string myStr) 'utf-8))))))

(defun xhm-decode-percent-encoded-uri-js (φp1 φp2)
  "Percent decode URI for text selection.
Requires a node.js script. See source code.
See also `xhm-decode-percent-encoded-uri'."
  (interactive "r")
  (let (scriptName)
    (save-excursion
      (setq scriptName (concat "/usr/bin/node ~/git/xahscripts/emacs_uri_decode.js"))
      (shell-command-on-region φp1 φp2 scriptName nil "REPLACE" nil t))))



(defun xhm-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
;; (let ((ξsyntax-state (syntax-ppss)))
;;     (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
;;         (progn nil)
;;       t))
t
)

(setq xhm-abbrev-table nil)

(define-abbrev-table 'xhm-abbrev-table
  '(

    ("tla" "<div class=\"¤tla\"><a href=\"url\">text</a></div>")
    ("8menu" "〖a ▸ b ▸ c〗")
    ("8key" "【Alt+f】")
    ("8song" "singer ❀ 〈title〉")
    ("8faq" "<div class=\"question-box32371\">
<p class=\"q\">How to do this?</p>
<p>this way</p>
</div>

")

    ("cdata" "<![CDATA[▮]]>" nil :system t)
    ("--" "<hr />")
    ("cl" "class=\"\"" nil :system t)

    ("8w" "width" nil :system t)
    ("8h" "height" nil :system t)
    ("bgc" "background-color" nil :system t)

    ("3css" "<link rel=\"stylesheet\" href=\"lbasic.css\" />")
    ("3style" "<style type=\"text/css\">\np {line-height:130%}\n</style>")
    ("refresh" "<meta http-equiv=\"refresh\" content=\"0; url=http://example.com/\">")

    ("8iframe" "<iframe src=\"some.html\" width=\"200\" height=\"300\"></iframe>")

    ;; todo
;; http://xahlee.info/js/css_colors.html
;; http://xahlee.info/js/css_color_names.html
    ("8white" "#ffffff" nil :system t)
    ("8silver" "#c0c0c0" nil :system t)
    ("8gray" "#808080" nil :system t)
    ("8black" "#000000" nil :system t)
    ("8red" "#ff0000" nil :system t)
    ("8maroon" "#800000" nil :system t)
    ("8yellow" "#ffff00" nil :system t)
    ("8olive" "#808000" nil :system t)
    ("8lime" "#00ff00" nil :system t)
    ("8green" "#008000" nil :system t)
    ("8aqua" "#00ffff" nil :system t)
    ("8teal" "#008080" nil :system t)
    ("8blue" "#0000ff" nil :system t)
    ("8navy" "#000080" nil :system t)
    ("8fuchsia" "#ff00ff" nil :system t)
    ("8purple" "#800080" nil :system t)
    ("8orange" "#ffa500" nil :system t)
    ("8hsl" "hsl(0,100%,50%)" nil :system t)

    ("8html5" "<!DOCTYPE html>" nil :system t)
    ("8html4s" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" nil :system t)
    ("8html4t" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" nil :system t)
    ("8xhtml" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" nil :system t)
    ("8html" "<!doctype html><html><head><meta charset=\"utf-8\" />
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

;; (symbol-plist 'xhm-abbrev-table)
;; (get 'xhm-abbrev-table 'case-fixed)
;; (abbrev-table-get xhm-abbrev-table :case-fixed)
;; (abbrev-table-get xhm-abbrev-table :enable-function)
;; (abbrev-table-get xhm-abbrev-table :parents)

  ;; :enable-function 'xhm-abbrev-enable-function


;; keybinding

(defvar xhm-keymap nil "Keybinding for `xah-html-mode'")
(progn
  (setq xhm-keymap (make-sparse-keymap))
  (define-key xhm-keymap (kbd "<C-right>") 'xhm-skip-tag-forward)
  (define-key xhm-keymap (kbd "<C-left>") 'xhm-skip-tag-backward)

  (define-key xhm-keymap (kbd "<tab>") 'xhm-wrap-html-tag)

  (define-prefix-command 'xhm-single-keys-keymap)
  (define-key xhm-keymap (kbd "<menu> e") xhm-single-keys-keymap)

  (define-key xhm-single-keys-keymap (kbd "c") 'xhm-lines-to-html-list)
  (define-key xhm-single-keys-keymap (kbd "g") 'browse-url-of-buffer)
  (define-key xhm-single-keys-keymap (kbd "k") 'xhm-htmlize-keyboard-shortcut-notation)
  (define-key xhm-single-keys-keymap (kbd "m") 'xhm-insert-wrap-source-code)

  (define-key xhm-single-keys-keymap (kbd "<backspace>") 'xhm-remove-html-tags)
  (define-key xhm-single-keys-keymap (kbd ".") 'xhm-decode-percent-encoded-uri)
  (define-key xhm-single-keys-keymap (kbd ",") 'xhm-update-title)
  (define-key xhm-single-keys-keymap (kbd "5") 'xhm-mark-unicode)
  (define-key xhm-single-keys-keymap (kbd "6") 'xhm-html-to-text)
  (define-key xhm-single-keys-keymap (kbd "7") 'xhm-toggle-syntax-coloring-markup)
  (define-key xhm-single-keys-keymap (kbd "8") 'xhm-get-precode-make-new-file)
  (define-key xhm-single-keys-keymap (kbd "9") 'xhm-redo-syntax-coloring-buffer)
  (define-key xhm-single-keys-keymap (kbd "l 3") 'xhm-source-url-linkify)
  (define-key xhm-single-keys-keymap (kbd "l s") 'xhm-make-link-defunct)
  (define-key xhm-single-keys-keymap (kbd "l w") 'xhm-word-to-wikipedia-linkify)
  (define-key xhm-single-keys-keymap (kbd "l g") 'xhm-wikipedia-url-linkify)
  (define-key xhm-single-keys-keymap (kbd "h") 'xhm-wrap-url)
  (define-key xhm-single-keys-keymap (kbd "r ,") 'xhm-replace-html-chars-to-unicode)
  (define-key xhm-single-keys-keymap (kbd "r .") 'xhm-replace-html-&<>-to-entities)
  (define-key xhm-single-keys-keymap (kbd "r e") 'xhm-htmlize-elisp-keywords)
  (define-key xhm-single-keys-keymap (kbd "r j") 'xhm-emacs-to-windows-kbd-notation)
  (define-key xhm-single-keys-keymap (kbd "r m") 'xhm-make-html-table)
  (define-key xhm-single-keys-keymap (kbd "r v") 'xhm-make-html-table-undo)
  (define-key xhm-single-keys-keymap (kbd "t") 'xhm-wrap-p-tag)
  (define-key xhm-single-keys-keymap (kbd "n") nil)
  (define-key xhm-single-keys-keymap (kbd "n u") 'xhm-extract-url)
  (define-key xhm-single-keys-keymap (kbd "x") 'xhm-rename-html-inline-image)
  (define-key xhm-single-keys-keymap (kbd "y") 'xhm-make-citation)

)



;; define the mode
(define-derived-mode xah-html-mode prog-mode
  "∑html"
  "A simple major mode for HTML5.
HTML5 keywords are colored.

\\{xhm-keymap}"

  (setq xhm-font-lock-keywords
        (let (
              (htmlElementNamesRegex (regexp-opt xhm-html5-tag-list 'words))
              (htmlAttributeNamesRegexp (regexp-opt xhm-attribute-names 'words))
              (cssPropertieNames (regexp-opt xhm-css-property-names 'words))
              (cssValueNames (regexp-opt xhm-css-value-kwds 'words))
              (cssColorNames (regexp-opt xhm-css-color-names 'words))
              (cssUnitNames (regexp-opt xhm-css-unit-names 'words))

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
            (,(format "“%s”" textNodeRegex) . (1 'xhm-curly“”-quoted-text-face))
            (,(format "‘%s’" textNodeRegex) . (1 'xhm-curly‘’-quoted-text-face))
            (,(format "<title>%s</title>" textNodeRegex) . (1 "bold"))
            (,(format "<span%s>%s</span>" attriRegex textNodeRegex) . (1 "hi-pink"))
            (,(format "<mark>%s</mark>" textNodeRegex) . (1 "hi-yellow"))
            (,(format "<mark%s>%s</mark>" attriRegex textNodeRegex) . (1 "hi-yellow"))
            (,(format "<b%s>%s</b>" attriRegex textNodeRegex) . (1 "bold"))

            (,htmlElementNamesRegex . font-lock-function-name-face)
            (,htmlAttributeNamesRegexp . font-lock-variable-name-face)
            (,cssPropertieNames . font-lock-type-face)
            (,cssValueNames . font-lock-keyword-face)
            (,cssColorNames . font-lock-preprocessor-face)
            (,cssUnitNames . font-lock-reference-face))))

  (setq font-lock-defaults '((xhm-font-lock-keywords)))
  (setq local-abbrev-table xhm-abbrev-table)

  (set-syntax-table xhm-syntax-table)
  (use-local-map xhm-keymap)

  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")

  ;;  (setq mode-name "xah-html")
  (run-mode-hooks 'xah-html-mode-hook)
  )

(provide 'xah-html-mode)
