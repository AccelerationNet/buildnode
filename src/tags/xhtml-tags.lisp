(in-package :net.acceleration.buildnode)

#.(unless (find-package :net.acceleration.xhtml)
    (defpackage :net.acceleration.xhtml
	(:nicknames :xhtml)
      (:use )
      (:export
       #:unordered-list
       #:tab-container
       #:tab-title
       #:tab
       #:iframe-tab
       #:a
       #:abbr
       #:acronym
       #:address
       #:applet
       #:area
       #:b
       #:base
       #:basefont
       #:bdo
       #:big
       #:blockquote
       #:body
       #:br
       #:button
       #:caption
       #:center
       #:cite
       #:code
       #:col
       #:colgroup
       #:dd
       #:del
       #:dir
       #:div
       #:dfn
       #:dl
       #:dt
       #:em
       #:fieldset
       #:font
       #:form
       #:frame
       #:frameset
       #:h1
       #:h2
       #:h3
       #:h4
       #:h5
       #:h6
       #:head
       #:hr
       #:html
       #:i
       #:iframe
       #:img
       #:input
       #:ins
       #:isindex
       #:kbd
       #:label
       #:legend
       #:li
       #:link
       #:map
       #:menu
       #:meta
       #:noframes
       #:noscript
       #:object
       #:ol
       #:optgroup
       #:option
       #:p
       #:param
       #:pre
       #:q
       #:s
       #:samp
       #:script
       #:select
       #:small
       #:span
       #:strike
       #:strong
       #:style
       #:sub
       #:sup
       #:table
       #:tbody
       #:td
       #:textarea
       #:tfoot
       #:th
       #:thead
       #:title
       #:tr
       #:tt
       #:u
       #:ul
       #:var
       #:xmp
       )))

;(def-html-tag "!--" "Defines a comment")
;(def-html-tag "!doctype" "Defines the document type")
(def-html-tag "a" "Defines an anchor")
(def-html-tag "abbr" "Defines an abbreviation")
(def-html-tag "acronym" "Defines an acronym")
(def-html-tag "address" "Defines an address element")
(def-html-tag "applet" "Deprecated. Defines an applet")
(def-html-tag "area" "Defines an area inside an image map")
(def-html-tag "b" "Defines bold text")
(def-html-tag "base" "Defines a base URL for all the links in a page")
(def-html-tag "basefont" "Deprecated. Defines a base font")
(def-html-tag "bdo" "Defines the direction of text display")
(def-html-tag "big" "Defines big text")
(def-html-tag "blockquote" "Defines a long quotation")
(def-html-tag "body" "Defines the body element")
(def-html-tag "br" "Inserts a single line break")
(def-html-tag "button" "Defines a push button")
(def-html-tag "caption" "Defines a table caption")
(def-html-tag "center" "Deprecated. Defines centered text")
(def-html-tag "cite" "Defines a citation")
(def-html-tag "code" "Defines computer code text")
(def-html-tag "col" "Defines attributes for table columns")
(def-html-tag "colgroup" "Defines groups of table columns")
(def-html-tag "dd" "Defines a definition description")
(def-html-tag "del" "Defines deleted text")
(def-html-tag "dir" "Deprecated. Defines a directory list")
(def-html-tag "div" "Defines a section in a document")
(def-html-tag "dfn" "Defines a definition term")
(def-html-tag "dl" "Defines a definition list")
(def-html-tag "dt" "Defines a definition term")
(def-html-tag "em" "Defines emphasized text")
(def-html-tag "fieldset" "Defines a fieldset")
(def-html-tag "font" "Deprecated. Defines text font, size, and color")
(def-html-tag "form" "Defines a form")
(def-html-tag "frame" "Defines a sub window (a frame)")
(def-html-tag "frameset" "Defines a set of frames")
(def-html-tag "h1" "to <h6>	 Defines header 1 to header 6")
(def-html-tag "h2" "to <h6>	 Defines header 1 to header 6")
(def-html-tag "h3" "to <h6>	 Defines header 1 to header 6")
(def-html-tag "h4" "to <h6>	 Defines header 1 to header 6")
(def-html-tag "h5" "to <h6>	 Defines header 1 to header 6")
(def-html-tag "h6" "to <h6>	 Defines header 1 to header 6")
(def-html-tag "head" "Defines information about the document")
(def-html-tag "hr" "Defines a horizontal rule")
(def-html-tag "html" "Defines an html document")
(def-html-tag "i" "Defines italic text")
(def-html-tag "iframe" "Defines an inline sub window (frame)")
(def-html-tag "img" "Defines an image")
(def-html-tag "input" "Defines an input field")
(def-html-tag "ins" "Defines inserted text")
(def-html-tag "isindex" "Deprecated. Defines a single-line input field")
(def-html-tag "kbd" "Defines keyboard text")
(def-html-tag "label" "Defines a label for a form control")
(def-html-tag "legend" "Defines a title in a fieldset")
(def-html-tag "li" "Defines a list item")
(def-html-tag "link" "Defines a resource reference")
(def-html-tag "map" "Defines an image map")
(def-html-tag "menu" "Deprecated. Defines a menu list")
(def-html-tag "meta" "Defines meta information")
(def-html-tag "noframes" "Defines a noframe section")
(def-html-tag "noscript" "Defines a noscript section")
(def-html-tag "object" "Defines an embedded object")
(def-html-tag "ol" "Defines an ordered list")
(def-html-tag "optgroup" "Defines an option group")
(def-html-tag "option" "Defines an option in a drop-down list")
(def-html-tag "p" "Defines a paragraph")
(def-html-tag "param" "Defines a parameter for an object")
(def-html-tag "pre" "Defines preformatted text")
(def-html-tag "q" "Defines a short quotation")
(def-html-tag "s" "Deprecated. Defines strikethrough text")
(def-html-tag "samp" "Defines sample computer code")
(def-html-tag "script" "Defines a script")
(def-html-tag "select" "Defines a selectable list")
(def-html-tag "small" "Defines small text")
(def-html-tag "span" "Defines a section in a document")
(def-html-tag "strike" "Deprecated. Defines strikethrough text")
(def-html-tag "strong" "Defines strong text")
(def-html-tag "style" "Defines a style definition")
(def-html-tag "sub" "Defines subscripted text")
(def-html-tag "sup" "Defines superscripted text")
(def-html-tag "table" "Defines a table")
(def-html-tag "tbody" "Defines a table body")
(def-html-tag "td" "Defines a table cell")
(def-html-tag "textarea" "Defines a text area")
(def-html-tag "tfoot" "Defines a table footer")
(def-html-tag "th" "Defines a table header")
(def-html-tag "thead" "Defines a table header")
(def-html-tag "title" "Defines the document title")
(def-html-tag "tr" "Defines a table row")
(def-html-tag "tt" "Defines teletype text")
(def-html-tag "u" "Deprecated. Defines underlined text")
(def-html-tag "ul" "Defines an unordered list")
(def-html-tag "var" "Defines a variable")
(def-html-tag "xmp" "Deprecated. Defines preformatted text")
