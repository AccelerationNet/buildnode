(in-package :net.acceleration.buildnode)
(cl-interpol:enable-interpol-syntax)

(eval-always
  (unless (find-package :net.acceleration.html5)
    (defpackage :net.acceleration.html5 (:nicknames :html5) (:use ))))

(pushnew :buildnode-html5 *features* :test #'eql)

(defparameter +html5-namespace+ "http://www.w3.org/1999/xhtml")

(defparameter +html5-tag-spec+
  "a – hyperlink CHANGED
abbr – abbreviation
address – contact information
area – image-map hyperlink
article – article NEW
aside – tangential content NEW
audio – audio stream NEW
b – offset text conventionally styled in bold CHANGED
base – base URL
bdi – BiDi isolate NEW
bdo – BiDi override
blockquote – block quotation
body – document body
br – line break
button – button
button type=submit – submit button
button type=reset – reset button
button type=button – button with no additional semantics
canvas – canvas for dynamic graphics NEW
caption – table title
cite – cited title of a work CHANGED
code – code fragment
col – table column
colgroup – table column group
command – command NEW
datalist – predefined options for other controls NEW
dd – description or value
del – deleted text
details – control for additional on-demand information NEW
dfn – defining instance
div – generic flow container
dl – description list
dt – term or name
em – emphatic stress
embed – integration point for plugins NEW
fieldset – set of related form controls
figcaption – figure caption NEW
figure – figure with optional caption NEW
footer – footer NEW
form – user-submittable form
h1 – heading
h2 – heading
h3 – heading
h4 – heading
h5 – heading
h6 – heading
head – document metadata container
header – header NEW
hgroup – heading group NEW
hr – thematic break CHANGED
html – root element
i – offset text conventionally styled in italic CHANGED
iframe – nested browsing context (inline frame)
img – image
input – input control CHANGED
ins – inserted text
kbd – user input
keygen – key-pair generator/input control NEW
label – caption for a form control
legend – title or explanatory caption
li – list item
link – inter-document relationship metadata
map – image-map definition
mark – marked (highlighted) text NEW
menu – list of commands CHANGED
meta – metadata CHANGED
meter – scalar gauge NEW
nav – group of navigational links NEW
noscript – fallback content for script
object – generic external content
ol – ordered list
optgroup – group of options
option – option
output – result of a calculation in a form NEW
p – paragraph
param – initialization parameters for plugins
pre – preformatted text
progress – progress indicator NEW
q – quoted text
rp – ruby parenthesis NEW
rt – ruby text NEW
ruby – ruby annotation NEW
s – struck text CHANGED
samp – (sample) output
script – embedded script
section – section NEW
select – option-selection form control
small – small print CHANGED
source – media source NEW
span – generic span
strong – strong importance
style – style (presentation) information
sub – subscript
summary – summary, caption, or legend for a details control NEW
sup – superscript
table – table
tbody – table row group
td – table cell
textarea – text input area
tfoot – table footer row group
th – table header cell
thead – table heading group
time – date and/or time NEW
title – document title
tr – table row
track – supplementary media track NEW
u – offset text conventionally styled with an underline CHANGED
ul – unordered list
var – variable or placeholder text
video – video NEW
wbr – line-break opportunity")

(iter (for row in (cl-ppcre:split #?r"\n" +html5-tag-spec+))
  (for (name doc) = (cl-ppcre:split #?r" – " row))
  (for sname = (symbol-munger:english->lisp-symbol name :net.acceleration.html5))
  (def-tag-node :net.acceleration.html5 name +html5-namespace+ doc)
  (export sname :net.acceleration.html5))
