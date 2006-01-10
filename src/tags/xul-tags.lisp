(in-package :net.acceleration.buildnode)

;;;THIS DOCUMENT CONTAINS THE GENERATED DEFINITIONS OF ALL THE XUL elements pulled from XULPlanet

(DEF-XUL-ELEMENT "action"
                 "Should be contained within a rule element. It is used to specify the generated  content for each matched node. Within the action, attributes are parsed for   resource and variable references .

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_action.html")
(DEF-XUL-ELEMENT "arrowscrollbox"
                 
                 "A box which provides scroll arrows along its edges for scolling through  the contents of the box. The user only needs to hover the mouse over the  arrows to scroll the box. This element is typically used for large popup  menus.

link:http://www.xulplanet.com/tutorials/xultu/menuscroll.html
src:http://www.xulplanet.com/reference/elemref/ref_arrowscrollbox.html")
(DEF-XUL-ELEMENT "binding"
                 
                 "Should be contained within a bindings element. A binding is used to  bind a variable to a node. Like the triple element in syntax, it can  be used to bind a particular property of a matched node to a particular  variable name. That name can then be used within the action of a rule.

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_binding.html")
(DEF-XUL-ELEMENT "bindings"
                 
                 "Should be contained within a rule. Used to specify a set of variable  bindings for a rule. This element should contain one or more binding  elements.

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_bindings.html")
(DEF-XUL-ELEMENT "box"
                 
                 "A container element which can contain any number of child elements. If the  box has an orient attribute that is set to horizontal, the child elements  are laid out from left to right in the order that they appear in the box.  If orient is set to vertical, the child elements are laid out from top to  bottom. Child elements do not overlap. The default orientation is horizontal.

link:http://www.xulplanet.com/tutorials/xultu/boxes.html
src:http://www.xulplanet.com/reference/elemref/ref_box.html")
(DEF-XUL-ELEMENT "broadcaster"
                 
                 "A broadcaster is used whn you want multiple elements to share one or more  attribute values, or when you want elements to respond to a state change.  Any elements that are observing the broadcaster will share the attributes  placed on the broadcaster. For instance, if the broadcaster has a label  attribute, the observes will use that label. If the label changes, the  labels of the observers will update automatically. An onbroadcast event  will be sent to the observers when a change is made.        For menuitems or buttons that just want to have their disabled status set  when the feature should be disabled, you should use a command element  instead.

link:http://www.xulplanet.com/tutorials/xultu/broadob.html
src:http://www.xulplanet.com/reference/elemref/ref_broadcaster.html")
(DEF-XUL-ELEMENT "broadcasterset"
                 
                 "A container element for broadcaster elements. The broadcasterset and its  descendants are not displayed.

link:http://www.xulplanet.com/tutorials/xultu/broadob.html
src:http://www.xulplanet.com/reference/elemref/ref_broadcasterset.html")
(DEF-XUL-ELEMENT "button"
                 
                 "A button that can be pressed by the user. Event handlers can be used to trap  mouse, keyboard and other events. It is typically rendered as a grey outset  rectangle. You can specify the label of the button using the label attribute  or by placing content inside the button.

link:http://www.xulplanet.com/tutorials/xultu/buttons.html
src:http://www.xulplanet.com/reference/elemref/ref_button.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the button.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the button will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("autoCheck"
                  "If this attribute is true, or left out, the checked state of the button will  be switched each time the button is pressed. If this attribute is false, the  checked state must be adjusted manually.")
                 ("checkState"
                  "This attribute may be used to create three state buttons, numbered 0, 1 and  2. When in state 0 or 1, pressing the button will switch to the opposite  state. When in state 2, pressing the button will switch to state 0. This  means that the button acts like a checkbox except that there is a third  state which must be set manually by adjusting the check state. If you wish  to have different behavior for how the states are adjusted, set the  autoCheck attribute to false and adjust the state with a script. The type  attribute must be set to 'checkbox' for buttons with a check state.  Constants for the possible values for this attribute are in the   nsIDOMXULButtonElement   interface.")
                 ("checked" "Indicates whether the button is checked or not.")
                 ("command"
                  "Set to the id of a command element that is being observed by the  element.")
                 ("crop"
                  "If the label of the button is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("dir"
                  "Specifies which side of the button's label that its image is placed.")
                 ("disabled"
                  "Indicates whether the button is disabled or not. If this attribute is set to true,  the button is disabled. This is usually drawn with the text in grey. If the button is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the button, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("dlgType"
                  "The dialog type of the button, used only when the button is in a dialog box.  You can use this feature to replace the standard dialog box buttons with custom  buttons, yet the dialog event methods will still function. For example, if the  dlgType is set to 'accept', the button will replace the dialog box's accept  button, which is usually labeled OK. Using this attribute on a button that is  not in a dialog box has no effect. The following values can be used as the dialog type:")
                 ("group"
                  "Buttons with the same value for their group attribute are put into the same  group. Only one button from each group can be checked at a time. If the user  selects one the radio buttons, the others in the group are unchecked.")
                 ("icon"
                  "This attribute should be used to set the usage for common buttons. Some  platforms display these buttons with a small icon indicating their usage.  This should be used in place of the image attribute. Possible values include:     accept ,     cancel ,     help ,     open ,     save ,     find ,     clear ,     yes ,     no ,     apply ,     close ,     print ,     add ,     remove ,     refresh ,     go-forward ,     go-back ,     properties ,     select-font ,     select-color ,     network .  If you are using a button that matches one of these common usages, use the icon  attribute to indicate this.")
                 ("image"
                  "The URL of the image to appear on the button. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes.")
                 ("label"
                  "The label that will appear on the button. If this is left out, no text appears.")
                 ("open"
                  "For the menu type buttons, the open attribute is set to true when the menu is  open. The open attribute is not present if the menu is closed.")
                 ("orient"
                  "Along with the dir attribute, the orient attribute is used to indicate  where the button's image appears in relation to the the label.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("type"
                  "The type of button. If this attribute is not present, a normal button is created.  Leave the attribute out for a normal button."))
(DEF-XUL-ELEMENT "browser"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_browser.html
src:
<p>  A frame which is expected to contain a view of a Web document. It is  similar to an iframe except that it holds a page history and contains  additional methods to manipulate the currently displayed page.  </p>  <p>  Most of the properties and methods of the browser would rarely be used and can only  be called from chrome URLs. Other URLs will need to use the document and history  objects to change the displayed document.</p>
"
                 ("autocompleteenabled"
                  "Set to true to enable autocomplete of fields.")
                 ("autocompletepopup"
                  "The id of a popup element used to hold autocomplete results for the browser.")
                 ("autoscroll"
                  "Set to false to disable autoscroll for this browser. If this attribute is set to  true or omitted, autoscroll will be enabled or depending on the user  preference 'general.autoScroll'.")
                 ("disablehistory"
                  "If false, an arrow button will appear on the end of the textbox which, when  pressed, will open a dropdown menu of all available results. The default  value is true, which will hide the dropdown button.")
                 ("disablesecurity"
                  "Set this attribute to true to disable the security UI for this browser.  Leave the attribute off to enable it.")
                 ("homepage"
                  "This attribute allows you to set a homepage for the browser element. It does  not have any correlation with the user's browser homepage; instead it is a  convenient property to hold a home page. You can switch to this home page  using the goHome method.")
                 ("src" "The URL of the page to appear in the browser.")
                 ("type"
                  "The type of browser, which can be used to set the access of the document  loaded inside the browser. If this is not set, the loaded document has the  same access as the window containing the <browser>."))
(DEF-XUL-ELEMENT "checkbox"
                 
                 "An element that can be turned on and off. This is most commonly rendered as  a box when the element is off and a box with a check when the element is on.  The user can switch the state of the check box by selecting it with the mouse.  A label, specified with the label attribute may be added beside the check box  to indicate to the user as to its function.

link:http://www.xulplanet.com/tutorials/xultu/inputs.html
src:http://www.xulplanet.com/reference/elemref/ref_checkbox.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the checkbox.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the checkbox will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("checked"
                  "Indicates whether the checkbox is checked or not.")
                 ("command"
                  "Set to the id of a command element that is being observed by the  element.")
                 ("crop"
                  "If the label of the checkbox is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disabled"
                  "Indicates whether the checkbox is disabled or not. If this attribute is set to true,  the checkbox is disabled. This is usually drawn with the text in grey. If the checkbox is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the checkbox, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("image"
                  "The URL of the image to appear on the checkbox. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes.")
                 ("label"
                  "The label that will appear beside the checkbox. If this is left out, no text appears.  The labels on checkboxes will wrap if there is not enough space.")
                 ("preference"
                  "Connects the checkbox to a corresponding preference. This attribute only  has any effect when used inside a prefwindow. The preference should be a  boolean type.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence."))
(DEF-XUL-ELEMENT "caption"
                 
                 "A header for a groupbox. It may contain either a text label, using the  label attribute, or child elements for a more complex caption.

link:http://www.xulplanet.com/tutorials/xultu/titledbox.html
src:http://www.xulplanet.com/reference/elemref/ref_caption.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the caption.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the caption will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("crop"
                  "If the label of the caption is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("image"
                  "The URL of the image to appear on the caption. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes.")
                 ("label"
                  "The label that will appear on the caption. If this is left out, no text appears.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence."))
(DEF-XUL-ELEMENT "column"
                 
                 "A single column in a columns element. Each child of the column element is placed  in each successive cell of the grid. The column with the most child elements  determines the number of rows in each column.

link:http://www.xulplanet.com/tutorials/xultu/grids.html
src:http://www.xulplanet.com/reference/elemref/ref_column.html")
(DEF-XUL-ELEMENT "columns"
                 
                 "Defines the columns of a grid. Each child of a columns element should be a  column element.

link:http://www.xulplanet.com/tutorials/xultu/grids.html
src:http://www.xulplanet.com/reference/elemref/ref_columns.html")
(DEF-XUL-ELEMENT "commandset"
                 
                 "This element is not displayed and serves as a container for command elements.        In addition, this element can serve as a command updater, which is used to  update the enabled state of one or more commands when certain events occur.  A common use of the command updater is to update the cut, copy, and paste  menu items as the user selects text.

link:http://www.xulplanet.com/tutorials/xultu/commandupdate.html
src:http://www.xulplanet.com/reference/elemref/ref_commandset.html"
                 ("commandupdater"
                  "If true, the commandset is used for updating commands. Typically, this  is used to update menu commands such as Undo and Cut based on when  an event occurs. For example, since the Cut command is only valid when  something is selected, a command updater might be used when the select  event occurs.")
                 ("events"
                  "A comma separated list of event names that the command updater will update upon.  If this attribute is not specified, or you set it to the value '*', all events  are valid. Valid events are listed below, or you can use your own events. You can  send a custom event by calling the UpdateCommands method of the   command dispatcher .")
                 ("oncommandupdate"
                  "This event occurs when a command update occurs. You would use this to update  the disabled status of items.")
                 ("targets"
                  "A comma separated list of element ids that the command updater will update upon.  If this attribute is not specified, or you set it to the value '*', all elements  are valid. The command update will only occur when the event occurs to one of  the specified elements."))
(DEF-XUL-ELEMENT "command"
                 
                 "A command element can be used to invoke an operation that can come from multiple  sources. For example, a clipboard paste operation can be invoked from the Edit  menu, a context menu and by pressing a keyboard shortcut. You attach the code  to the command using the oncommand attribute. It will be called no matter how it  is invoked by the user. In addition, disabling the command will automatically  disable the menu items and keyboard shortcuts.        Commands are identified by their id. If you include the script  chrome://global/content/globalOverlay.js in your window, you can use the  function goDoCommand function to invoke the command. Using this function has  the advantage that the command will be sent to the part of the UI which will  respond to it. Typically, this will be the currently focused element. For  example, the following code will sent a paste command (cmd_paste) to the  currently focused element:         goDoCommand(\"cmd_paste\");     Like a broadcaster, commands forward attributes to other elements.

link:http://www.xulplanet.com/tutorials/xultu/commands.html
src:http://www.xulplanet.com/reference/elemref/ref_command.html"
                 ("disabled"
                  "If you set the disabled attribute to true, any menu items and keys attached to  the command become disabled. If you remove the disabled attribute, the menu  items and keys become enabled.")
                 ("label"
                  "The label inherited by the menu items and keys that are attached to the command.")
                 ("oncommand"
                  "This event handler is called when the command is activated. This occurs when a  user selects a menu item or presses a keyboard shortcut attached to the command."))
(DEF-XUL-ELEMENT "conditions"
                 
                 "This element should appear directly inside a rule element and is used to  define conditions for the rule. Within the conditions can be placed content,  member and triple elements. These may have attributes whose value is a  variable name beginning with a question mark (?). When evaluating the rule for  a particular RDF resource, the variables are replaced with values from the  resource. If all variables can be replaced, the rule matched.

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_conditions.html")
(DEF-XUL-ELEMENT "content"
                 
                 "This element should appear inside a conditions element. A rule's conditions  should contain one and only one content element. The content element should  also have a uri attribute. It is used to bind a variable to a content node  during matching. When using a tree with the 'flags' attribute set to  'dont-build-content', use a treeitem element instead.

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_content.html")
(DEF-XUL-ELEMENT "deck"
                 
                 "An element that displays only one of its children at a time. The selectedIndex  attribute determines which child is displayed.

link:http://www.xulplanet.com/tutorials/xultu/stacks.html
src:http://www.xulplanet.com/reference/elemref/ref_deck.html"
                 ("selectedIndex"
                  "Gets and sets the index of the currently selected panel in the deck.  The first item is at index 0."))
(DEF-XUL-ELEMENT "description"
                 
                 "This element is used to create a block of text. The text can be set either  with the value attribute or by placing text inside the open and close  description tags. The value attribute is used to set text that appears in  a single line. If text appears as a child of the description, it will wrap  to multiple lines. If may contain arbitrary markup, which can be styled  as needed.

link:http://www.xulplanet.com/tutorials/xultu/textimage.html
src:http://www.xulplanet.com/reference/elemref/ref_description.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the description.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the description will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("control"
                  "This attribute should be set to the id of an element that the label is  associated with. When the user clicks on the label, the associated element  is given the focus.")
                 ("crop"
                  "If the label of the description is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disabled"
                  "Indicates whether the description is disabled or not. If this attribute is set to true,  the description is disabled. This is usually drawn with the text in grey. If the description is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the description, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("value"
                  "The text to be used for the description. The text will appear on a single  line and will not wrap. Normally, you would not use a value attribute on a  description element and instead place the text as the content inside the  description tag."))
(DEF-XUL-ELEMENT "dialogheader"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_dialogheader.html
src:
  If the label of the dialogheader is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.
"
                 ("crop"
                  "If the label of the dialogheader is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("description"
                  "Descriptive text to appear in addition to the dialog title.")
                 ("title" "A title for the dialogheader."))
(DEF-XUL-ELEMENT "editor"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_editor.html
src:
<p>  A frame which is expected to contain an editable document. Set the value of  the editortype attribute to 'html' to create an editor document. Mozilla  provides two types of editors, the HTML editor and the plaintext editor.  The editor does not provide any editing user interface; you would supply that  yourself. However, text editing, image resizing, and table row and cell  editing capabilities are provided. If you do not set the editortype  attribute on an editor, you must enable editing using the makeEditable  method.  </p>  <p>  To specify the document to load in the editor use the src attribute.  However, an issue is that if you specify the src attribute initially on the  editor tag in the XUL file, the document does not become editable by  default. To enable editing on an editor, do one of two things:  </p>  <ol>    <li>Set the src attribute on the editor after the outer window has loaded,        for example, in the onload handler. You might also set the src        attribute based on what the user selects from a file dialog. In this        case, set the editortype attribute on the editor.</li>    <li>Call the makeEditable function to make the document loaded in the        editor editable.</li>  </ol>  <p>  To edit a new document, set the src attribute to 'about:blank'.  </p></p>
"
                 ("editortype"
                  "The type of editor to use. This value will be overriden depending on the  content type of the document in the editor.")
                 ("src" "The URL of the file to load into the editor.")
                 ("type"
                  "If set to the string 'content-primary', this editor becomes the primary  content for the page. The window for the primary content can be retrieved  more conveniently using 'window.content'."))
(DEF-XUL-ELEMENT "grid"
                 
                 "A grid is an element that contains both rows and columns elements. It is used  to create a grid of elements. Both the rows and columns are displayed at once  although only one will typically contain content, while the other may provide  size information. Whichever is last in the grid is displayed on top.

link:http://www.xulplanet.com/tutorials/xultu/grids.html
src:http://www.xulplanet.com/reference/elemref/ref_grid.html")
(DEF-XUL-ELEMENT "grippy"
                 
                 "An element that may be used inside a splitter which can be used to  collapse a sibling element of the splitter.

link:http://www.xulplanet.com/tutorials/xultu/splitter.html
src:http://www.xulplanet.com/reference/elemref/ref_grippy.html")
(DEF-XUL-ELEMENT "groupbox"
                 
                 "A box that draws a frame around it, intended to group a set of elements  together. If a caption element is placed inside the groupbox, it will be  used as a caption which appears along the top of the groupbox.

link:http://www.xulplanet.com/tutorials/xultu/titledbox.html
src:http://www.xulplanet.com/reference/elemref/ref_groupbox.html")
(DEF-XUL-ELEMENT "hbox"
                 
                 "A container element which can contain any number of child elements. This is  equivalent to the box element.

link:http://www.xulplanet.com/tutorials/xultu/boxes.html
src:http://www.xulplanet.com/reference/elemref/ref_hbox.html")
(DEF-XUL-ELEMENT "iframe"
                 
                 "An inner frame that works much the HTML iframe element. The src attribute can  be used to specify the content of the frame. This content is a separate  document. The children of the iframe are ignored.

link:http://www.xulplanet.com/tutorials/xultu/cpanels.html
src:http://www.xulplanet.com/reference/elemref/ref_iframe.html"
                 ("src" "The URL of the page to appear in the iframe."))
(DEF-XUL-ELEMENT "image"
                 
                 "An element that displays an image, much like the HTML img element. The src  attribute can be used to specify the URL of the image.

link:http://www.xulplanet.com/tutorials/xultu/textimage.html
src:http://www.xulplanet.com/reference/elemref/ref_image.html"
                 ("onerror"
                  "This event is sent to an image element when an error occurs loading the image.")
                 ("onload"
                  "This event handler will be called on the image element when the image has  finished loading. This applies whether the image is applied via the src  attribute or the list-style-image style property. If you change the image,  the event will fire again when the new image loads. This event will not bubble  up the element tree.")
                 ("src"
                  "The URL of the image to appear on the image. If this attribute is left  out, no image appears.")
                 ("validate"
                  "This attribute indicates whether to load the image from the cache or not.  This would be useful if the images are stored remotely or you plan on swapping  the image frequently. The following values are accepted, or leave out the  attribute entirely for default handling:"))
(DEF-XUL-ELEMENT "key"
                 
                 "The key element defines a keyboard shortcut. Event handlers can be used  to respond when the appropriate keys are pressed. A key element must be  placed inside a keyset element.

link:http://www.xulplanet.com/tutorials/xultu/keyshort.html
src:http://www.xulplanet.com/reference/elemref/ref_key.html"
                 ("command"
                  "Set to the id of a command element that is being observed by the  element.")
                 ("disabled"
                  "Set to true to disable the key. This is often used with a broadcaster to  disable a menu command and key at the same time.")
                 ("key"
                  "The character that is must be pressed. This should be set to a displayable  character.")
                 ("keycode"
                  "For keys that do not have displayable characters, such as the enter key or  function keys, use this attribute, instead of the key attribute. Valid keys  are listed  here .")
                 ("keytext"
                  "A label for the keyboard shortcut. This text would appear next to a menuitem  label if that menuitem is associated with the key element via its key  attribute.")
                 ("modifiers"
                  "A list of modifier keys that should be pressed to invoke the keyboard  shortcut. Multiple keys may be separated by spaces or commas. Keys will map  to other keys on platforms that do not have them.")
                 ("oncommand"
                  "This event handler is called when an element is activated. How it is activated  varies for each element and in many cases, there are several ways to activate  an element. For example, a button can be activated by clicking on it with the  mouse or by pressing ENTER while it has the focus. Menus can be activated by  selecting them with the mouse or by pressing a shortcut key. You should always  use the oncommand event instead of onclick because it will be called in all of  the needed cases.")
                 ("phase"
                  "The event phase where the handler is invoked. This should be set to the  value 'capturing' to indicate during the event capturing phase or 'target'  to indicate at the target element or left out entirely for the bubbling  phase."))
(DEF-XUL-ELEMENT "keyset"
                 
                 "A container element for key elements. The keyset and its descendants are not  displayed.

link:http://www.xulplanet.com/tutorials/xultu/keyshort.html
src:http://www.xulplanet.com/reference/elemref/ref_keyset.html")
(DEF-XUL-ELEMENT "label"
                 
                 "This element is used to provide a label for an control element, If the user  clicks the label, it will move the focus to the associated control, specified  with the control attribute.

link:http://www.xulplanet.com/tutorials/xultu/textimage.html
src:http://www.xulplanet.com/reference/elemref/ref_label.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the label.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the label will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("control"
                  "This attribute should be set to the id of an element that the label is  associated with. When the user clicks on the label, the associated element  is given the focus.")
                 ("crop"
                  "If the label of the label is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disabled"
                  "If this attribute is set to true, the label is disabled. This is usually  drawn with the text in grey. Leave the attribute out entirely for a regular  enabled label.")
                 ("value" "The text to be used for the label."))
(DEF-XUL-ELEMENT "listbox"
                 
                 "This element is used to create a list of items. This is a simpler version of  a tree. Nested rows are not supported, but a listbox may contain multiple  columns. There are numerous methods which allow the items in the listbox to  be retrieved and modified.        You may specify the number of rows to display in the list using the rows  attribute. Additional rows can be viewed by using a scroll bar. All of the  rows in the listbox are the same height, which is the height of the largest  item in the list.  If you are using Mozilla 1.8 (Firefox 1.5) or later and wish to create a list with  non-text content, you might consider using the richlistbox element instead.

link:http://www.xulplanet.com/tutorials/xultu/lists.html
src:http://www.xulplanet.com/reference/elemref/ref_listbox.html"
                 ("disableKeyNavigation"
                  "If this attribute is not used, the user can navigate to specific items in  the list by pressing the first the first letter of the item's label. This is  done incrementally, so pressing additional keys will select more specific  items. This feature may be disabled for a listbox by setting this attribute  to true.")
                 ("disabled"
                  "Indicates whether the listbox is disabled or not. If this attribute is set to true,  the listbox is disabled. This is usually drawn with the text in grey. If the listbox is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the listbox, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("preference"
                  "Connects the listbox to a corresponding preference. This attribute only has  any effect when used inside a prefwindow. The value of the preference  will be updated to match the value property of the listbox.")
                 ("rows"
                  "The number of rows to display in the list box. If the listbox contains more  than this many rows, a scrollbar will appear which the user can use to  scroll to the other rows. To get the actual number of rows in the listbox,  use the getRowCount method.")
                 ("seltype"
                  "Used to indicate whether multiple selection is allowed.")
                 ("suppressonselect"
                  "If this attribue is not specified, a select event is fired whenever an item  is selected, either by the user or by calling one of the select methods. If  set to true, the select event is never fired.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("value"
                  "You can associate a data value with each listitem. It is not used for any  specific purpose but you can access it with a script for your own use."))
(DEF-XUL-ELEMENT "listcell"
                 
                 "A single cell of a listbox, used for cells which contain text only.

link:http://www.xulplanet.com/tutorials/xultu/morelists.html
src:http://www.xulplanet.com/reference/elemref/ref_listcell.html"
                 ("crop"
                  "If the label of the listcell is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disabled"
                  "Indicates whether the listcell is disabled or not. If this attribute is set to true,  the listcell is disabled. This is usually drawn with the text in grey. If the listcell is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the listcell, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("image"
                  "The URL of the image to appear on the listcell. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes. You must use the class 'listcell-iconic' to have an  image appear.")
                 ("label"
                  "The label that will appear on the listcell. If this is left out, no text appears.")
                 ("type"
                  "You can make a cell in a listbox a checkbox by setting this attribute to  the value 'checkbox'."))
(DEF-XUL-ELEMENT "listcol"
                 
                 "A column in a list box. You can make some columns flexible and other columns  non-flexible.

link:http://www.xulplanet.com/tutorials/xultu/morelists.html
src:http://www.xulplanet.com/reference/elemref/ref_listcol.html")
(DEF-XUL-ELEMENT "listcols"
                 
                 "A container for the columns of a listbox, each of which are created with  the listcol element. There should be only one listcols element in a list  box. If there is no listcols element, the list box has a single column.

link:http://www.xulplanet.com/tutorials/xultu/morelists.html
src:http://www.xulplanet.com/reference/elemref/ref_listcols.html")
(DEF-XUL-ELEMENT "listhead"
                 
                 "A header row of a listbox. It is usual to place listheader elements  inside the listhead, one for each column.

link:http://www.xulplanet.com/tutorials/xultu/morelists.html
src:http://www.xulplanet.com/reference/elemref/ref_listhead.html"
                 ("disabled"
                  "Indicates whether the listhead is disabled or not. If this attribute is set to true,  the listhead is disabled. This is usually drawn with the text in grey. If the listhead is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the listhead, leave the attribute out entirely as opposed to  setting the value to false."))
(DEF-XUL-ELEMENT "listheader"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_listheader.html
src:
  Indicates whether the listheader is disabled or not. If this attribute is set to true,  the listheader is disabled. This is usually drawn with the text in grey. If the listheader is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the listheader, leave the attribute out entirely as opposed to  setting the value to false.
"
                 ("disabled"
                  "Indicates whether the listheader is disabled or not. If this attribute is set to true,  the listheader is disabled. This is usually drawn with the text in grey. If the listheader is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the listheader, leave the attribute out entirely as opposed to  setting the value to false."))
(DEF-XUL-ELEMENT "listitem"
                 
                 "A single row in a list box. The text of the listitem is specified either  using listcell elements, or by placing a label attribute directly on the  listitem element. You may also place other elements inside the listitem for  more complex content.

link:http://www.xulplanet.com/tutorials/xultu/lists.html
src:http://www.xulplanet.com/reference/elemref/ref_listitem.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the listitem.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the listitem will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("checked"
                  "Indicates whether the listitem is checked or not.")
                 ("command"
                  "Set to the id of a command element that is being observed by the  element.")
                 ("crop"
                  "If the label of the listitem is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("current"
                  "This attribute will be set to true if the listitem is the current item. This  is typically used by a theme to customize the focus ring. To change the  currently selected item in a listbox, use the listbox property selectedItem.")
                 ("disabled"
                  "Indicates whether the listitem is disabled or not. If this attribute is set to true,  the listitem is disabled. This is usually drawn with the text in grey. If the listitem is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the listitem, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("image"
                  "The URL of the image to appear on the listitem. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes. You must use the class 'listitem-iconic' to have an  image appear.")
                 ("label"
                  "The label that will appear on the listitem. If this is left out, no text appears.")
                 ("preference"
                  "Connects the listitem to a corresponding preference. This attribute only has  any effect when used inside a prefwindow. The value of the preference  will be updated to match the value property of the listitem.")
                 ("selected"
                  "Indicates whether the listitem is selected or not. This property is read  only. To change the selection, set either the selectedIndex or selectedItem  property of the containing listbox.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("type"
                  "You can make an item in a listbox a checkbox by setting this attribute to  the value 'checkbox'.")
                 ("value"
                  "A value associated with the listitem. You may use it in a script for your own  purposes."))
(DEF-XUL-ELEMENT "member"
                 
                 "Used within a rule's conditions element to match elements that are containers  or are contained within another element. Both the container and child  attributes may use variables.

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_member.html")
(DEF-XUL-ELEMENT "menu"
                 
                 "An element, much like a button, that is placed on a menu bar. When the user  clicks the menu element, the child menupopup of the menu will be displayed.  This element is also used to create submenus.

link:http://www.xulplanet.com/tutorials/xultu/menubar.html
src:http://www.xulplanet.com/reference/elemref/ref_menu.html"
                 ("acceltext"
                  "Text that appears beside beside the menu label to indicate the shortcut key  (accelerator key) to use to invoke the command. If this value is set, it  overrides an assigned key set in the key attribute. This attribute does not  apply to menus directly on the menubar.")
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the menu.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the menu will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("allowevents"
                  "If true, events are passed to children of the menu. Otherwise, events are  passed to the menu only.")
                 ("crop"
                  "If the label of the menu is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disabled"
                  "Indicates whether the menu is disabled or not. If this attribute is set to true,  the menu is disabled. This is usually drawn with the text in grey. If the menu is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the menu, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("key"
                  "The id of a key element that is used as the menu command's shortcut key.  If used, text will be displayed beside the menu label to indicate which  keys can be pressed to invoke the command. Normally, you would not set a  key on a menu element.")
                 ("label"
                  "The label that will appear on the menu. If this is left out, no text appears.")
                 ("menuactive"
                  "This attribute is set on an item in a menu when it is being hovered over.  Typcially, the theme will use this to highlight the item. A  DOMMenuItemActive event will be sent to the item when the item is hovered  over, and a DOMMenuItemInactive event will be sent to the item when the  selection moves away.")
                 ("open"
                  "This attribute is set on the menu when it is open. To open the menu, call  the showPopup method of the menupopup.")
                 ("sizetopopup"
                  "Indicates how the menu width and the popup width are determined. If the  sizetopopup attribute is left out or set to none, the menu will be its preferred  width and the popup may extend outside of this width, unaffected by the maximum  width of the menu itself.")
                 ("value"
                  "A value associated with the menu. You may use it in a script for your own  purposes."))
(DEF-XUL-ELEMENT "menubar"
                 
                 "A container that usually contains menu elements. On the Macintosh, the menubar  is displayed along the top of the screen, and all non-menu related elements  inside the menubar will be ignored.

link:http://www.xulplanet.com/tutorials/xultu/menubar.html
src:http://www.xulplanet.com/reference/elemref/ref_menubar.html"
                 ("accessible"
                  "Returns the accessibility object for the menubar.")
                 ("grippyhidden"
                  "When set to true, the grippy will be hidden. When set to false, the default, the  grippy will be shown.")
                 ("statusbar"
                  "If you set this attribute to the id of a statusbar element, the label on  the statusbar will update to the statustext of the items on the menu as the  user moves the mouse over them."))
(DEF-XUL-ELEMENT "menuitem"
                 
                 "A single choice in a menupopup element. It acts much like a button but it is  rendered on a menu.

link:http://www.xulplanet.com/tutorials/xultu/menubar.html
src:http://www.xulplanet.com/reference/elemref/ref_menuitem.html"
                 ("acceltext"
                  "Text that appears beside beside the menu label to indicate the shortcut key  (accelerator key) to use to invoke the command. If this value is set, it  overrides an assigned key set in the key attribute. This attribute does not  apply to menus directly on the menubar.")
                 ("accessible"
                  "Returns the accessibility object for the menuitem.")
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the menuitem.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the menuitem will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("allowevents"
                  "If true, events are passed to children of the menuitem. Otherwise, events are  passed to the menuitem only.")
                 ("autocheck"
                  "If this attribute is true, or left out, the menuitem check mark will update  each time the menu item is selected. If this attribute is false, the  check mark must be adjusted manually.")
                 ("checked"
                  "Indicates whether the menuitem is checked or not.")
                 ("command"
                  "Set to the id of a command element that is being observed by the menuitem.  If the command element's disabled attribute is set to true, the menuitem will  become disabled. If the command disabled attribue is removed, the menuitem will  be enabled. Similarly, the command's label attribute can be used to set the  menuitem's label.")
                 ("crop"
                  "If the label of the menuitem is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("description"
                  "An optional description to appear alongside the label. This is typically used  for a menuitem in a menulist.")
                 ("disabled"
                  "Indicates whether the menuitem is disabled or not. If this attribute is set to true,  the menuitem is disabled. This is usually drawn with the text in grey. If the menuitem is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the menuitem, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("image"
                  "The URL of the image to appear on the menuitem. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes. You must use the class 'menuitem-iconic' to have an  image appear.")
                 ("key"
                  "The id of a key element that is used as the menu command's shortcut key.  If used, text will be displayed beside the menu label to indicate which  keys can be pressed to invoke the command.")
                 ("label"
                  "The label that will appear on the menuitem. If this is left out, no text appears.")
                 ("name"
                  "Radio menuitems with the same name as put into a group. Only one  menuitem from each radio group can be checked at a time.")
                 ("selected"
                  "Indicates that the menuitem is selected in a menulist. This property is  read only. To change the selection, set either the selectedIndex or  selectedItem property of the containing menulist.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("type"
                  "Can be used to create checkable menuitems or for radio button  menuitems.")
                 ("validate"
                  "This attribute indicates whether to load the image from the cache or not.  This would be useful if the images are stored remotely or you plan on swapping  the image frequently. The following values are accepted, or leave out the  attribute entirely for default handling:")
                 ("value"
                  "You can associate a data value with each menu and menuitem. It is  not used for any specific purpose but you can access it with a script  for your own use."))
(DEF-XUL-ELEMENT "menulist"
                 
                 "An element that can be used for drop-down choice lists. The user may select one  of the elements displayed in the menulist. The currently selected choice is  displayed on the menulist element. To create the drop-down, put a menupopup  inside the menulist containing the choices as menuitem elements.

link:http://www.xulplanet.com/tutorials/xultu/lists.html
src:http://www.xulplanet.com/reference/elemref/ref_menulist.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the menulist.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the menulist will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("crop"
                  "If the label of the menulist is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disableautoselect"
                  "If this attribute is true or omitted, the selected item on the menu will  update to match what the user entered in the textbox. If the text does not  match any of the items in the list, the menu selection is cleared. If this  attribute is false, the selection is never updated to match the text box.  This attribute applies only to editable menulists.")
                 ("disabled"
                  "Indicates whether the menulist is disabled or not. If this attribute is set to true,  the menulist is disabled. This is usually drawn with the text in grey. If the menulist is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the menulist, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("editable"
                  "Indicates that the value of the menulist can be modified by typing directly  into the value field. This is rendered as a textbox with a drop-down arrow  beside it. The user may enter text into the textbox or select one of the choices  by clicking from the drop-down.")
                 ("focused"
                  "This attribute is set to true if the menulist element is focused.")
                 ("image"
                  "The URL of the image to appear on the menulist. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes.")
                 ("label"
                  "The label that will appear on the menulist. If this is left out, no text appears.")
                 ("open"
                  "If true, the menu popup is showing. You can change this value to show or hide the popup.")
                 ("preference"
                  "Connects the menulist to a corresponding preference. This attribute only has  any effect when used inside a prefwindow. The value of the preference  will be updated to match the value property of the menulist.")
                 ("readonly"
                  "If set to true, then the user cannot type into the field of an editable menulist.  However, the value may still be modified by a script.")
                 ("sizetopopup"
                  "Indicates how the menu width and the popup width are determined. If the  sizetopopup attribute is left out or set to none, the menu will be its preferred  width and the popup may extend outside of this width, unaffected by the maximum  width of the menu itself.")
                 ("src"
                  "The URL of the image to appear on the menulist. If this attribute is left  out, no image appears.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("value"
                  "The value of the value property of the currently selected item in the menulist.  You can select an item with a particular value by setting this property."))
(DEF-XUL-ELEMENT "menupopup"
                 
                 "A container used to display menus. It should be placed inside a menu,  menulist or menu-type button element. It can contain any element but  usually will contain menuitem elements. It is a type of box that defaults  to vertical orientation.

link:http://www.xulplanet.com/tutorials/xultu/menubar.html
src:http://www.xulplanet.com/reference/elemref/ref_menupopup.html"
                 ("ignorekeys"
                  "If true, keyboard navigation between menu items in the popup is disabled.")
                 ("left"
                  "Overrides the horizontal position of the popup specified by the showPopup  function.")
                 ("onpopuphidden"
                  "This event is sent to a popup after it has been hidden.")
                 ("onpopuphiding"
                  "This event is sent to a popup when it is about to be hidden.")
                 ("onpopupshowing"
                  "This event is sent to a menupopup just before it is popped open. This handler is  usually used to dynamically set the commands on a menu when the user requests  to display it. Returning false from this event handler prevents the popup from  appearing.")
                 ("onpopupshown"
                  "This is event is sent to a popup after it has been opened, much like the onload  event is sent to a window when it is opened.")
                 ("position"
                  "The position attribute determines where the popup appears relative to the element  the user clicked to invoke the popup. This allows you to place the menu on one side  on a button.")
                 ("top"
                  "Overrides the vertical position of the popup specified by the showPopup  function."))
(DEF-XUL-ELEMENT "menuseparator"
                 
                 "Used to create a separator between menu items. Typically drawn as a thin line.

link:http://www.xulplanet.com/tutorials/xultu/menubar.html
src:http://www.xulplanet.com/reference/elemref/ref_menuseparator.html")
(DEF-XUL-ELEMENT "observes"
                 
                 "The observes element can be used to listen to a broadcaster and receive  events and attributes from it. The observes element should be placed inside  the element that wants to observe the broadcaster. When an observed  attribute is modified on the broadcaster, the attribute's value will be  forwarded and set on the parent element of the observer.

link:http://www.xulplanet.com/tutorials/xultu/broadob.html
src:http://www.xulplanet.com/reference/elemref/ref_observes.html"
                 ("attribute"
                  "The attribute that the observer is observing. When the value of the  attribute changes, the broadcast event is called on the observer. Use the  value '*' to observe all attribute of the broadcasters. The id, ref and  persist attributes are not observed.")
                 ("element"
                  "The id of the broadcaster element that the observer is observing."))
(DEF-XUL-ELEMENT "overlay"
                 
                 "An overlay is used when it is desirable for a block of content to be shared  between several different windows. In addition, it can be used to append or  alter content in an existing window. An overlay is defined in a separate XUL  file. Overlays are applied while the XUL is being loaded.        Each element within the overlay is inserted at a location in the master window,  determined by matching id attributes. For instance, if an element in an overlay  has an id of 'filemenu', the corresponding element with the id 'filemenu' in  the master window that uses the overlay would be altered. Attributes declared  in the overlay are added to that element and child elements are inserted into  the window within that element. Elements directly inside the overlay element  as children that do not have id attributes are appended to the master  window. This allows the addition of scripts to the master window from the  overlay.        Overlays do not have an onload event. To have initialization code in an  overlay, place it directly in a script outside of a function. This is  commonly used to call the addEventListener method to listen to the load  event for the window which does not fire until the master window and all  overlays are applied.     Overlays may be applied to windows in other packages. Files, such as scripts,  that are referenced from the overlay should be specified as absolute URLs. If  you do specify a relative URL, it will be relative to the window that the  overlay applies to, not the overlay's file.

link:http://www.xulplanet.com/tutorials/xultu/overlay.html
src:http://www.xulplanet.com/reference/elemref/ref_overlay.html"
                 ("class"
                  "The style class of the overlay. Multiple classes may be specified by  separating them with spaces.")
                 ("id"
                  "A unique identifier so that you can identify the overlay with. You can use this as  a parameter to getElementById and other DOM functions and to reference the  element in style sheets."))
(DEF-XUL-ELEMENT "popup"
                 
                 "A container that appears in a child window. The popup window does not have any  special frame. Popups can be displayed when an element is clicked by assigning  the id of the popup to either the popup, context or tooltip attribute of the  element. A popup is a type of box that defaults to vertical orientation.

link:http://www.xulplanet.com/tutorials/xultu/popups.html
src:http://www.xulplanet.com/reference/elemref/ref_popup.html"
                 ("ignorekeys"
                  "If true, keyboard navigation between menu items in the popup is disabled.")
                 ("left"
                  "Overrides the horizontal position of the popup specified by the showPopup  function.")
                 ("onpopuphidden"
                  "This event is sent to a popup after it has been hidden.")
                 ("onpopuphiding"
                  "This event is sent to a popup when it is about to be hidden.")
                 ("onpopupshowing"
                  "This event is sent to a popup just before it is popped open. This handler is  usually used to dynamically set the contents when the user requests to display it.  Returning false from this event handler prevents the popup from appearing.")
                 ("onpopupshown"
                  "This is event is sent to a popup after it has been opened, much like the onload  event is sent to a window when it is opened.")
                 ("position"
                  "The position attribute determines where the popup appears relative to the element  the user clicked to invoke the popup.")
                 ("top"
                  "Overrides the vertical position of the popup specified by the showPopup  function."))
(DEF-XUL-ELEMENT "popupset"
                 
                 "A container for popup elements. You should declare all popup elements as  children of a popupset. This element does not directly display on screen.  Child popups will be displayed when asked to by other elements.

link:http://www.xulplanet.com/tutorials/xultu/popups.html
src:http://www.xulplanet.com/reference/elemref/ref_popupset.html")
(DEF-XUL-ELEMENT "progressmeter"
                 
                 "A meter which can be used to display the progress of a lengthy operation.  It is drawn as a bar that is filled as the operation completes. In addition,  an undeterminate progressmeter may be created by setting the mode attribute.  This is used when the length of time to complete an operation is not known  beforehand.

link:http://www.xulplanet.com/tutorials/xultu/progress.html
src:http://www.xulplanet.com/reference/elemref/ref_progressmeter.html"
                 ("mode"
                  "A determined progressmeter is used in cases where you know how long an  operation will take. Undeterminate progressmeters can be used when  you don't and will typically be drawn as a spinning barber pole.")
                 ("value"
                  "A percentage value that specifies the amount of the meter that is filled  in. Because it is a percentage, it ranges from 0 to 100."))
(DEF-XUL-ELEMENT "radio"
                 
                 "An element that can be turned on and off. Radio buttons are almost always  grouped together in groups. Only one radio button within the same  radiogroup may be selected at a time. The user can switch which radio  button is turned on by selecting it with the mouse or keyboard. Other radio  buttons in the same group are turned off. A label, specified with the label  attribute may be added beside the radio button to indicate to the user as to  its function.

link:http://www.xulplanet.com/tutorials/xultu/inputs.html
src:http://www.xulplanet.com/reference/elemref/ref_radio.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the radio.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the radio will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("command"
                  "Set to the id of a command element that is being observed by the  element.")
                 ("crop"
                  "If the label of the radio is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disabled"
                  "Indicates whether the radio is disabled or not. If this attribute is set to true,  the radio is disabled. This is usually drawn with the text in grey. If the radio is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the radio, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("focused"
                  "This attribute is set to true if the radio element is focused.")
                 ("image"
                  "The URL of the image to appear on the radio. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes.")
                 ("label"
                  "The label that will appear beside the radio. If this is left out, no text appears.  The labels on radio buttons will wrap if there is not enough space.")
                 ("selected"
                  "This attribute is set to true if the radio button is selected. To  change the currently selected radio button, modify either the selectedIndex  or selectedItem property of the radiogroup.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("value"
                  "You can associate a data value with each radio button."))
(DEF-XUL-ELEMENT "radiogroup"
                 
                 "A group of radio buttons. Only one radio button inside the group can be  selected at a time. The radio buttons may either direct children of the  radiogroup or descendants. Place the radiogroup inside a groupbox if you  would like a border or caption around the group. The radiogroup defaults  to vertical orientation.        To set the radio button that will be selected by default, set the selected  attribute of a child radio element.

link:http://www.xulplanet.com/tutorials/xultu/inputs.html
src:http://www.xulplanet.com/reference/elemref/ref_radiogroup.html"
                 ("disabled"
                  "Indicates whether the radiogroup is disabled or not. If this attribute is set to true,  the radiogroup is disabled. This is usually drawn with the text in grey. If the radiogroup is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the radiogroup, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("focused"
                  "This attribute is set to true if the radiogroup element is focused.")
                 ("preference"
                  "Connects the radiogroup to a corresponding preference. This attribute only has  any effect when used inside a prefwindow. The value of the preference  will be updated to match the value property of the radiogroup.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("value"
                  "Returns the value property of the currently selected radio button. To  change the selection, use the selectedIndex or selectedItem property."))
(DEF-XUL-ELEMENT "resizer"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_resizer.html
src:
  The direction that the window is resized.
"
                 ("dir" "The direction that the window is resized."))
(DEF-XUL-ELEMENT "row"
                 
                 "A single row in a rows element. Each child of the row element is placed in  each successive cell of the grid. The row with the most child elements  determines the number of columns in each row.

link:http://www.xulplanet.com/tutorials/xultu/grids.html
src:http://www.xulplanet.com/reference/elemref/ref_row.html")
(DEF-XUL-ELEMENT "rows"
                 
                 "Defines the rows of a grid. Each child of a rows element should be a row  element.

link:http://www.xulplanet.com/tutorials/xultu/grids.html
src:http://www.xulplanet.com/reference/elemref/ref_rows.html")
(DEF-XUL-ELEMENT "rule"
                 
                 "A rule is used in a template. The children of the rule are used to declare the  conditions in which the rule matches and the content that is generated. When  the content needs to be displayed, the template builder scans through the  RDF datasource searching for nodes that match the rules. When a rule is  matched, the corresponding content is generated.        A rule contains two or three child elements. The conditions element is used to  specify the conditions on which the resources in a datasource can match.  Matching resources are used to generate content. Non-matching resources have no  content generated for them. The action element specifies the content that is  generated. The bindings element is optional and may specify additional variable  bindings to be used.        All three children may use variables in place of attribute values. Variables are  a question mark followed by a name. For a matched rule, each variable will be a  reference to an RDF resource node. If the same variable appears multiple times,  it must have the same value in each place.     A rule may use a shortcut syntax for matching that involves placing additional  attributes on the rule element. The rule will match if the attribute matches  an attribute on the RDF resource. When comparing attributes, the id, property  and instanceOf attributes are ignored.        Each generated element will be given an id values automatically. The id values  will correspond to an id of the resource in the datasource. You can use this id  in a script to examine the resource.

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_rule.html"
                 ("iscontainer"
                  "Indicates whether rules match based on containment. If not specified, the rule  may match regardless of whether a node is a container or not.")
                 ("isempty"
                  "Indicates whether rules match based on emptyness. If not specified, the rule  may match regardless of whether a node has children or not.")
                 ("parent"
                  "If set, the rule will only match the corresponding tag. This may be used to  have separate rules for leaf and container nodes with different tags.")
                 ("parsetype"
                  "If this attribute is set to 'Integer', the rule will only match RDF nodes  with a parse type of Integer."))
(DEF-XUL-ELEMENT "script"
                 
                 "Much like the HTML script element, this is used to declare a script to be  used by the XUL window. You should usually put scripts in a separate file  pointed to by the src attribute, but you may also place the script inline  inside the opening and closing script tags.

link:http://www.xulplanet.com/tutorials/xultu/events.html
src:http://www.xulplanet.com/reference/elemref/ref_script.html"
                 ("src" "The URL of the script.")
                 ("type"
                  "The language of the script. Usually, you would set this to  'application/x-javascript'."))
(DEF-XUL-ELEMENT "scrollbar"
                 
                 "When a container contains contents which are larger that the size of the  content, scroll bars may be placed at the side of the container to allow the  user to scroll around in the container. The scroll bar may also be used  independently when a numeric value or percentage needs to be selected by the  user. The user can adjust the position of the scroll bar by clicking arrows  on either end of the scroll bar or by dragging the scroll bar thumb around.

link:http://www.xulplanet.com/tutorials/xultu/scroll.html
src:http://www.xulplanet.com/reference/elemref/ref_scrollbar.html"
                 ("curpos"
                  "The current position of the scrollbar, which ranges from 0 to the value of the  maxpos attribute. The default value is 0.")
                 ("increment"
                  "The amount that the value of the curpos attribute changes by when the scroll bar  arrows are clicked. The default value is 1.")
                 ("maxpos"
                  "The maximum position of the scrollbar. The default value is 100.")
                 ("pageincrement"
                  "The amount that the value of the curpos attribute changes by when the tray  of the scroll bar is clicked. The tray is the area in which the scroll bar  thumb moves along. The default value is 10."))
(DEF-XUL-ELEMENT "separator"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_separator.html
src:
  Used to specify whether the separator is a horizontal or vertical separator.  Note that the values are the reverse of what seems more likely.
"
                 ("orient"
                  "Used to specify whether the separator is a horizontal or vertical separator.  Note that the values are the reverse of what seems more likely."))
(DEF-XUL-ELEMENT "spacer"
                 
                 "An element that takes up space but does not display anything. It is usually  used to place spacing within a container. If you don't specify that the  spacer has a size or is flexible, the spacer does not occupy any space. If  you want a small gap, consider using a separator instead.

link:http://www.xulplanet.com/tutorials/xultu/springs.html
src:http://www.xulplanet.com/reference/elemref/ref_spacer.html")
(DEF-XUL-ELEMENT "splitter"
                 
                 "An element which should appear before or after an element inside a container.  When the splitter is dragged, the sibling elements of the splitter are resized.  If a grippy in placed inside the splitter, one sibling element of the  splitter is collapsed when the grippy is clicked.

link:http://www.xulplanet.com/tutorials/xultu/splitter.html
src:http://www.xulplanet.com/reference/elemref/ref_splitter.html"
                 ("collapse"
                  "Determines which side of the splitter is collapsed when its grippy is  clicked. If this attribute is not specified, the splitter will not  cause a collapse. You should put a grippy element inside the splitter  when it is used for collapsing.")
                 ("resizeafter"
                  "This attribute indicates which element to the right or below the splitter should  be resized when the splitter is repositioned.")
                 ("resizebefore"
                  "This attribute indicates which element to the left or above the splitter should  be resized when the splitter is repositioned.")
                 ("state"
                  "Indicates whether the splitter has collapsed content or not. This attribute  will be updated automatically as the splitter is moved, and is generally used  in a stylesheet to apply a different appearance for each state."))
(DEF-XUL-ELEMENT "stack"
                 
                 "An element that renders its children on top of each other. The first element  if placed on the bottom, and each successive child is place above the  previous one. All elements are displayed at once. Child elements may also  be placed at absolute positions within the stack.        The left and top attributes on a child of the stack specify the position of  that element.

link:http://www.xulplanet.com/tutorials/xultu/stacks.html
src:http://www.xulplanet.com/reference/elemref/ref_stack.html")
(DEF-XUL-ELEMENT "statusbar"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_statusbar.html
src:nsIAccessibleProvider")
(DEF-XUL-ELEMENT "statusbarpanel"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_statusbarpanel.html
src:
  If the label of the statusbarpanel is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.
"
                 ("crop"
                  "If the label of the statusbarpanel is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("label"
                  "The label that will appear on the statusbarpanel. If this is left out, no text appears.")
                 ("src"
                  "The URL of the image to appear on the statusbarpanel. If this attribute is left  out, no image appears."))
(DEF-XUL-ELEMENT "stringbundle"
                 
                 "An element which can be used to load localized resources from property files.  Stringbundles should be placed inside a stringbundleset element.        A property file is a list of property key-value pairs each on a separate  line. The key and value is separated with an equals sign. For example, the  following defines two properties:         message.displayError=An error occured trying to display this message   message.nameAlreadyUsed=The name %s is already being used by another account.

link:http://www.xulplanet.com/tutorials/xultu/locprops.html
src:http://www.xulplanet.com/reference/elemref/ref_stringbundle.html"
                 ("src"
                  "The URL of the property file that contains the localized strings."))
(DEF-XUL-ELEMENT "tab"
                 
                 "A single tab which should be placed inside a tabs element. The user may click  a tab to bring the associated page of the tabbox to the front.

link:http://www.xulplanet.com/tutorials/xultu/tabpanel.html
src:http://www.xulplanet.com/reference/elemref/ref_tab.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the tab.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the tab will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("afterselected"
                  "This is set to true if the tab is immediately after the currently selected tab.  This is set automatically set when needed and you shouldn't adjust it manually.  This is primarily useful for themes so that they can adjust the appearance of the  area around the selected tab.")
                 ("beforeselected"
                  "This is set to true if the tab is immediately before the currently selected tab.  This is set automatically set when needed and you shouldn't adjust it manually.  This is primarily useful for themes so that they can adjust the appearance of the  area around the selected tab.")
                 ("crop"
                  "If the label of the tab is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("disabled"
                  "Indicates whether the tab is disabled or not. If this attribute is set to true,  the tab is disabled. This is usually drawn with the text in grey. If the tab is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the tab, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("image"
                  "The URL of the image to appear on the tab. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes.")
                 ("label"
                  "The label that will appear on the tab. If this is left out, no text appears.")
                 ("linkedpanel"
                  "The id of the linked tabpanel element that will be displayed when the tab  is selected. If this attribute is not used, the tab will be connected to the  panel at the corresponding index in the tabpanels element that the tab is  in its tabs container. However, if this attribute is used, this behavior  is overridden, and the tab will always be linked to a specific panel. This  might be used to avoid duplication by linking several tabs to one panel with  slight differences to the content adjusted in the select event.")
                 ("selected"
                  "This attribute is set to true if the tab is selected by default.")
                 ("validate"
                  "This attribute indicates whether to load the image from the cache or not.  This would be useful if the images are stored remotely or you plan on swapping  the image frequently. The following values are accepted, or leave out the  attribute entirely for default handling:"))
(DEF-XUL-ELEMENT "tabbrowser"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_tabbrowser.html
src:
  Set to true to enable autocomplete of fields.
"
                 ("autocompleteenabled"
                  "Set to true to enable autocomplete of fields.")
                 ("autocompletepopup"
                  "The id of a popup element used to hold autocomplete results for the tabbrowser.")
                 ("autoscroll"
                  "Set to false to disable autoscroll for this tabbrowser. If this attribute is set to  true or omitted, autoscroll will be enabled or depending on the user  preference 'general.autoScroll'.")
                 ("contentcontextmenu"
                  "A reference to the context menu for the content area in the tabbrowser.")
                 ("contenttooltip"
                  "A reference to the tooltip element to be used for the content area in the  tabbrowser.")
                 ("handleCtrlPageUpDown"
                  "If set to true or omitted, the tabbrowser will switch to the next tab when the  Control and Page Up or Page Down keys are pressed. If this attribute is set  to false, these keys do not navigate between tabs.")
                 ("onbookmarkgroup"
                  "This code executes when the user chooses the 'Bookmark This Group of Tabs'  command.")
                 ("onnewtab"
                  "This script will be called when the new tab button is clicked."))
(DEF-XUL-ELEMENT "tabbox"
                 
                 "A container used to display tabbed pages of elements to the user. The  tabbox should contain two children, the first a tabs element which contains  the tabs and the second a tabpanels element which contains the contents of  the pages. This is a type of box that defaults to vertical orientation.

link:http://www.xulplanet.com/tutorials/xultu/tabpanel.html
src:http://www.xulplanet.com/reference/elemref/ref_tabbox.html"
                 ("eventnode"
                  "Indicates from where keyboard navigation events are listened from. If this  attribute is not specified, events are listened to from the tabbox. Thus, if  this attribute is not used, the tabbox or an element inside it must have the  focus for the keyboard navigation to apply.")
                 ("handleCtrlPageUpDown"
                  "If set to true or omitted, the tabbox will switch to the next tab when the  Control and Page Up or Page Down keys are pressed. If this attribute is set  to false, these keys do not navigate between tabs.")
                 ("handleCtrlTab"
                  "If set to true or omitted, the tabbox will switch to the next tab when the  Control and Tab keys are pressed. If the Shift key is also held down, the  previous tab will be displayed. If this attribute is set to false, these  keys do not navigate between tabs."))
(DEF-XUL-ELEMENT "tabpanel"
                 
                 "A individual panel in a tabpanels element. This element is optional and you may  just use any other container in place of it.

link:http://www.xulplanet.com/tutorials/xultu/tabpanel.html
src:http://www.xulplanet.com/reference/elemref/ref_tabpanel.html")
(DEF-XUL-ELEMENT "tabpanels"
                 
                 "A container to hold the set of pages in a tabbox. The tabpanels element  should be placed in a tabbox. The children of the tabpanels element become  the panels of the tabbox. Usually, the children are tabpanel elements. By  clicking the first tab, the first panel will be displayed. By clicking the  second tab, the second panel will be displayed and so on.

link:http://www.xulplanet.com/tutorials/xultu/tabpanel.html
src:http://www.xulplanet.com/reference/elemref/ref_tabpanels.html"
                 ("selectedIndex"
                  "Gets and sets the index of the currently selected panel in the deck.  The first item is at index 0."))
(DEF-XUL-ELEMENT "tabs"
                 
                 "A row of tabs. A tabs element should be placed inside a tabbox and should  contain tab elements.

link:http://www.xulplanet.com/tutorials/xultu/tabpanel.html
src:http://www.xulplanet.com/reference/elemref/ref_tabs.html"
                 ("closebutton"
                  "If this attribute is set to true, the tabs row will have a new tab button  and close button on the ends. This feature is used by the tabbrowser to  add the capability to add new tabs and close existing tabs. You can set an  image to the new and close buttons by applying them to the 'tabs-newbutton'  and 'tabs-closebutton' classes respectively.")
                 ("disableclose"
                  "If tis attribute is true, the close button will be disabled.")
                 ("first-tab"
                  "This attribute will be set to true for the first tab. This attribute should  not be set manually, but is useful in a theme if the first tab should be  styled differently.")
                 ("last-tab"
                  "This attribute will be set to true for the last tab. This attribute should  not be set manually, but is useful in a theme if the last tab should be  styled differently.")
                 ("onclosetab"
                  "This script will be called when the close tab button is clicked.")
                 ("onnewtab"
                  "This script will be called when the new tab button is clicked.")
                 ("onselect"
                  "This event is sent to the tabs element when the tab is changed.")
                 ("setfocus"
                  "If true or omitted, the focus will be given to the first element in the  corresponding tabpanel when the tabs are navigated via the keyboard. If  this attribute is false, the focus does not change during navigation.")
                 ("tooltiptextnew"
                  "Used to set the text which appears in the tooltip when the user moves the  mouse over the new button in the tab row."))
(DEF-XUL-ELEMENT "template"
                 
                 "Used to declare a template for rule-based construction of elements. Elements  are constructed from a datasource. For more information see the rule element.

link:http://www.xulplanet.com/tutorials/xultu/templates.html
src:http://www.xulplanet.com/reference/elemref/ref_template.html"
                 ("container"
                  "May optionally be set to the variable to use as the container or reference  variable. If not specified, the variable specified in the uri attribute of the  content tag in the template's first rule is used.")
                 ("member"
                  "May optionally be set to the variable to use as the member variable. If not  specified, the variable specified in the uri attribute in the action body of  the template's first rule is used."))
(DEF-XUL-ELEMENT "textnode"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_textnode.html
src:
<p>  Normally when substituting RDF resources in template rules, you place the RDF  property name inside an attribute value preceded with 'rdf:'. In the case of the  textnode element, the entire node is replaced with text corresponding to the result  of the value attribute.   </p>  <p>  This is useful if you want to use the html element or other elements that have  text content inside them instead of in attributes. This element does nothing outside  of a template.  </p>
"
                 ("value"
                  "The text value to display. This value should be an RDF property."))
(DEF-XUL-ELEMENT "textbox"
                 
                 "A text input field in which the user can enter text. It is similar to the  HTML input element. Only one line of text is displayed by default. The  multiline attribute can be specified to display a field with multiple rows.

link:http://www.xulplanet.com/tutorials/xultu/inputs.html
src:http://www.xulplanet.com/reference/elemref/ref_textbox.html"
                 ("cols" "The number of columns in the textarea.")
                 ("disabled"
                  "Indicates whether the textbox is disabled or not. If this attribute is set to true,  the textbox is disabled. This is usually drawn with the text in grey. If the textbox is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the textbox, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("maxlength"
                  "The maximum number of characters that the textbox allows to be entered.")
                 ("multiline"
                  "If true, the textbox displays multiple lines. If the user presses ENTER, a new  line is started. If false, the textbox only allows entry of one line.")
                 ("onchange"
                  "This event is sent when the value of the textbox is changed.  The event is not sent until the focus is moved to another element.")
                 ("oninput"
                  "This event is sent when a user enters text in a textbox. This event  is only called when the text displayed would change, thus it is not called  when the user presses non-displayable keys.")
                 ("preference"
                  "Connects the textbox to a corresponding preference. This attribute only has  any effect when used inside a prefwindow. The value of the preference  will be updated to match the value property of the textbox.")
                 ("readonly"
                  "If set to true, then the user cannot modify the value of the textbox. However, the  value may still be modified by a script.")
                 ("rows" "The number of rows in the textarea.")
                 ("size"
                  "The number of characters that can be displayed in the textbox.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("timeout"
                  "For timed textboxes, the number of milliseconds before the timer fires a  command event. The timer starts after the user types a character. If the  user types another character, the timer resets.")
                 ("type"
                  "You can set the type attribute to one of the values below for a more  specialized type of textbox. Don't set the type if you wish to use a regular  textbox.")
                 ("value"
                  "The default value entered in the textbox. The attribute only holds the  default value and is never modified when the user enters text. To get the  updated value, use the value property.")
                 ("wrap"
                  "Set this attribute to the value 'off' to disable word wrapping in the textbox.  If this attribute is not specified, word wrapping is enabled."))
(DEF-XUL-ELEMENT "toolbar"
                 
                 "A container which typically contains a row of buttons. It is a type of box  that defaults to horizontal orientation. It can be collapsed with a grippy  when the toolbar is placed inside a toolbox. The toolbar should always have  an id attribute. Firefox supports toolbar customization, whereas the Mozilla  browser does not.

link:http://www.xulplanet.com/tutorials/xultu/toolbar.html
src:http://www.xulplanet.com/reference/elemref/ref_toolbar.html"
                 ("currentset"
                  "The current set of displayed items on the toolbar. This should be  set to a comma-separated list of item IDs from the toolbarpalette.")
                 ("customindex"
                  "This value is the index of the toolbar in the list of the custom  toolbars. The value is updated automatically by the toolbar  customization dialog.")
                 ("customizable"
                  "Set this attribute to true on toolbars that can be customized. This  causes the set of buttons to be persisted across sessions.")
                 ("defaultset"
                  "The default set of displayed items on the toolbar. This should be  set to a comma-separated list of item IDs from the toolbarpalette.  For example, 'back-button,forward-button,print-button'.")
                 ("grippyhidden"
                  "When set to true, the grippy will be hidden. When set to false, the default, the  grippy will be shown.")
                 ("grippytooltiptext"
                  "The text to appear on the tooltip for the toolbar's grippy when the toolbar is collapsed.  This would be used to label the grippy so that the user knows which toolbar it represents.")
                 ("toolbarname"
                  "The name of the toolbar, which is listed on the Show/Hide toolbars  menu."))
(DEF-XUL-ELEMENT "toolbarbutton"
                 
                 "A button that appears on a toolbar. It is equivalent to a regular button  except that it may be rendered differently. Typically, it is expected to  have an image.

link:http://www.xulplanet.com/tutorials/xultu/toolbar.html
src:http://www.xulplanet.com/reference/elemref/ref_toolbarbutton.html"
                 ("accesskey"
                  "This should be set to a letter that is used as a shortcut key. This letter  should be one of the characters that appears in the label text for the toolbarbutton.  This letter will typically be drawn underlined, although this behavior will  be platform and theme specific. When the user presses ALT (or a similar key  that varies on each platform) and the access key, the toolbarbutton will be activated  from anywhere in the window. Although the value is case insensitive, a  letter with the case matching the accesskey attribute will used if both  cases exist in the label.")
                 ("autoCheck"
                  "If this attribute is true, or left out, the checked state of the button will  be switched each time the button is pressed. If this attribute is false, the  checked state must be adjusted manually.")
                 ("checkState"
                  "This attribute may be used to create three state buttons, numbered 0, 1 and  2. When in state 0 or 1, pressing the button will switch to the opposite  state. When in state 2, pressing the button will switch to state 0. This  means that the button acts like a checkbox except that there is a third  state which must be set manually by adjusting the check state. If you wish  to have different behavior for how the states are adjusted, set the  autoCheck attribute to false and adjust the state with a script. The type  attribute must be set to 'checkbox' for buttons with a check state.  Constants for the possible values for this attribute are in the   nsIDOMXULButtonElement   interface.")
                 ("checked"
                  "Indicates whether the button is checked or not. This feature is used for  toggle buttons on a toolbar. You can associate two images, a normal image  and a depressed image, to a toolbar button to create the apperance of the  button being toggled. In the stylesheet, just have a style rule that checks  for the checked attribute.")
                 ("command"
                  "Set to the id of a command element that is being observed by the  element.")
                 ("crop"
                  "If the label of the toolbarbutton is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("dir"
                  "The direction in which the child elements of the toolbarbutton are placed.")
                 ("disabled"
                  "Indicates whether the toolbarbutton is disabled or not. If this attribute is set to true,  the toolbarbutton is disabled. This is usually drawn with the text in grey. If the toolbarbutton is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the toolbarbutton, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("dlgType"
                  "The dialog type of the button, used only when the button is in a dialog box.  You can use this feature to replace the standard dialog box buttons with custom  buttons, yet the dialog event methods will still function. For example, if the  dlgType is set to 'accept', the button will replace the dialog box's accept  button, which is usually labeled OK. Using this attribute on a button that is  not in a dialog box has no effect. The following values can be used as the dialog type:")
                 ("group"
                  "Buttons with the same value for their group attribute are put into the same  group. Only one button from each group can be checked at a time. If the user  selects one the radio buttons, the others in the group are unchecked.")
                 ("image"
                  "The URL of the image to appear on the toolbarbutton. If this is attribute is left  out, no image appears. The position of the image is determined by the dir  and orient attributes.")
                 ("label"
                  "The label that will appear on the toolbarbutton. If this is left out, no text appears.")
                 ("open"
                  "For the menu type buttons, the open attribute is set to true when the menu is  open. The open attribute is not present if the menu is closed.")
                 ("orient"
                  "Used to specify whether the children of the toolbarbutton are oriented horizontally or  vertically. The default value depends on the element. You can also use the  '-moz-box-orient' style property.")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence.")
                 ("type"
                  "The type of button. If this attribute is not present, a normal button is created.  You can set the type attribute to the value 'menu' to create a button with a  menu popup. This will typically cause the button to be displayed differently.")
                 ("validate"
                  "This attribute indicates whether to load the image from the cache or not.  This would be useful if the images are stored remotely or you plan on swapping  the image frequently. The following values are accepted, or leave out the  attribute entirely for default handling:"))
(DEF-XUL-ELEMENT "toolbargrippy"
                 
                 "The notch on the side of a toolbar which can be used to collapse and expand it.  This element is used internally by the toolbar and should only be used directly  when modifying its theme.

link:http://www.xulplanet.com/tutorials/xultu/toolbar.html
src:http://www.xulplanet.com/reference/elemref/ref_toolbargrippy.html")
(DEF-XUL-ELEMENT "toolbarpalette"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_toolbarpalette.html
src:
<p>  The item is a palette of available toolbar items. It is not displayed,  but is used by the toolbar customization dialog to display the list  of items. The children of the toolbarpalette should be the complete  list of toolbarbuttons and toolbaritems that can be added to the  toolbar. Do not add the various spacing items, as those are added  automatically.  </p>  <p>  You can add your own custom buttons to the Firefox browser by using  an overlay that overlays the toolbarpalette with the id  'BrowserToolbarPalette'.</p>
")
(DEF-XUL-ELEMENT "toolbarseparator"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_toolbarseparator.html
src:nsIAccessibleProvider")
(DEF-XUL-ELEMENT "toolbarspacer"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_toolbarspacer.html
src:nsIAccessibleProvider")
(DEF-XUL-ELEMENT "toolbarspring"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_toolbarspring.html
src:nsIAccessibleProvider")
(DEF-XUL-ELEMENT "toolbox"
                 
                 "A container for toolbars. It is a type of box but defaults to vertical  orientation. If a toolbar is placed inside a toolbox, a grippy is displayed  on its left or upper edge. The user may click the grippy to collapse the  toolbar. If multiple toolbars are placed in the same toolbox, they will  all collapse into the same row. The Firefox browser does not have  grippies so toolbars cannot be collapsed and expanded.

link:http://www.xulplanet.com/tutorials/xultu/toolbar.html
src:http://www.xulplanet.com/reference/elemref/ref_toolbox.html")
(DEF-XUL-ELEMENT "tooltip"
                 
                 "This element is used for the tooltip popups. For text only tooltips, this  element doesn't need to be used; instead you may just add a tooltiptext  attribute to an element.

link:http://www.xulplanet.com/tutorials/xultu/popups.html
src:http://www.xulplanet.com/reference/elemref/ref_tooltip.html"
                 ("crop"
                  "If the label of the tooltip is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("default"
                  "If true, the tooltip is used as the default popup for displaying tooltips in  the window.")
                 ("label"
                  "The label that will appear on the tooltip. If this is left out, no text appears.")
                 ("noautohide"
                  "If this attribute is set to false or omitted, the tooltip will automatically  disappear after a few seconds. If this attribute is set to true, this will  not happen and the tooltip will only hide when the user moves the mouse to  another element.")
                 ("onpopuphidden"
                  "This event is sent to a popup after it has been hidden.")
                 ("onpopuphiding"
                  "This event is sent to a popup when it is about to be hidden.")
                 ("onpopupshowing"
                  "This event is sent to a popup just before it is popped open. This handler is  usually used to dynamically set the contents when the user requests to display it.  Returning false from this event handler prevents the popup from appearing.")
                 ("onpopupshown"
                  "This is event is sent to a popup after it has been opened, much like the onload  event is sent to a window when it is opened.")
                 ("position"
                  "The position attribute determines where the popup appears relative to the element  the user clicked to invoke the popup. This allows you to place the menu on one side  on a button."))
(DEF-XUL-ELEMENT "tree"
                 
                 "A container which can be used to hold a tabular or hierarchical set of rows  of elements. The tree may contain any number of rows and any number of  columns. Each row of the tree may contain child rows which are displayed  indented from the parent. Unlike other elements, the data to display inside  the tree is not specified using tags, but is determined from a view object.  The view object implements the   nsITreeView   interface. The view is queried for the data to appear in the tree.   There are several ways in which trees are used, as listed below. The second  column lists the interfaces available via the tree's view property. The  third column indicates whether treeitem element are used.               Tree Type        View Interfaces        Has DOM Nodes        Description                  Content Tree                 nsITreeView ,         nsITreeContentView               Yes        This tree has treeitem elements placed within the treechildren          element. In this situation, a content view (which implements the          interface nsITreeContentView) which is a more specialized type of          view, uses the treeitem elements and their descendants to          determine the data to display in the tree. However, the treeitems          are not displayed directly; they are used only as data to the          content view. However, the content view will automatically update          the tree if treeitems are changed.                        RDF Tree                 nsITreeView ,         nsIXULTreeBuilder               No        This tree is generated from an RDF datasource. It is used when a          tree has a datasources attribute, and has 'dont-build-content' in          its flags attribute. For this tree, the data comes directly from           the RDF datasource. DOM treeitems are not created. Even though the          template uses treeitem elements to define the content, DOM nodes for          these elements are not created. This is the type that should be used          for RDF generated trees with lots of rows.                        RDF Content Tree                 nsITreeView ,         nsIXULTreeBuilder          nsITreeContentView               Yes        This tree is generated from an RDF datasource. It is similar to the          previous type but is used when the tree does not have          'dont-build-content' in its flags attribute. DOM treeitems are          created, so you can access the data using RDF functions or DOM          functions. This type is suitable for RDF generated trees with a          fairly small number of rows.                        Custom View Tree                 nsITreeView               No        For this tree you implement the nsITreeView interface yourself. The          tree's data is retrieved from this custom view. The custom view          should be attached to the tree by setting its view property.

link:http://www.xulplanet.com/tutorials/xultu/trees.html
src:http://www.xulplanet.com/reference/elemref/ref_tree.html"
                 ("disableKeyNavigation"
                  "If this attribute is not used, the user can navigate to specific items in  the tree by pressing the first the first letter of the item's label. This is  done incrementally, so pressing additional keys will select more specific  items. This feature may be disabled for a tree by setting this attribute to  true.")
                 ("disabled"
                  "Indicates whether the tree is disabled or not. If this attribute is set to true,  the tree is disabled. This is usually drawn with the text in grey. If the tree is  disabled, it does not respond to user actions. The element cannot be focused  and the command event will not fire. The element will still respond to mouse  events. To enable the tree, leave the attribute out entirely as opposed to  setting the value to false.")
                 ("enableColumnDrag"
                  "When set to 'true', the user may drag the column headers around to change the  order that they are displayed in.")
                 ("flags"
                  "Set this attribute to 'dont-build-content' for a template generated tree. For  trees that have their content placed directly inside the tree or have a custom  view object, this flag should not be used.")
                 ("hidecolumnpicker"
                  "When set to false, a drop-down will appear in the upper right corner of the  tree, which the user may use to show and hide columns. When set to true,  the column picker will be hidden. The default value is false.")
                 ("onselect"
                  "This event is sent to an tree when a row is selected, or whenever the  selection changes. The user can select multiple rows by holding down Shift or  Control and click on a row. The onselect event will be sent for each item added  or removed to the selection.")
                 ("rows" "The number of rows to display in the tree.")
                 ("seltype"
                  "Used to indicate whether multiple selection is allowed.")
                 ("statedatasource"
                  "Chrome XUL may specify an RDF datasource to use to store tree state  information. This is used to hold which tree items are open and which items  are collapsed. This information will be remembered for the next time the  XUL file is opened. If you do not specify this attribute, state information  will be stored in the local store (rdf:local-store).")
                 ("tabindex"
                  "The tab order of the element. The tab order is the order in which the focus  is moved when the user presses the Tab key. Elements with a higher tabindex are  later in the tab order sequence."))
(DEF-XUL-ELEMENT "treecell"
                 
                 "A single cell in a tree. This element should be placed inside a treerow. You  can set the text for the cell using the label attribute.

link:http://www.xulplanet.com/tutorials/xultu/trees.html
src:http://www.xulplanet.com/reference/elemref/ref_treecell.html"
                 ("label" "The label to appear on the cell.")
                 ("mode"
                  "For columns that are progress meters, this determines the type of progress meter  to use.")
                 ("properties"
                  "Sets the properties of the treecell, which can be used to style the cell.")
                 ("ref"
                  "Points to the treecol the cell is in. Usually you would not specify this  attribute as the tree will determine this by the position of the cell.")
                 ("src"
                  "The URL of the image to appear on the treecell. If this attribute is left  out, no image appears.")
                 ("value"
                  "A percentage value that specifies the amount of the progress meter that is  filled in. Because it is a percentage, it ranges from 0 to 100."))
(DEF-XUL-ELEMENT "treechildren"
                 
                 "This element is the body of the tree. For content trees, the content will  be placed inside this element. This element is also used to define container  rows in the tree.

link:http://www.xulplanet.com/tutorials/xultu/trees.html
src:http://www.xulplanet.com/reference/elemref/ref_treechildren.html"
                 ("alternatingbackground"
                  "If true, the background of the tree's rows will alternate between two  colors."))
(DEF-XUL-ELEMENT "treecol"
                 
                 "A column of a tree. It displays the column header and holds the size and other  information about the column. You can also place splitter elements between the  columns to allow column resizing. You should always place an id attribute on  a treecol element to ensure that the column positioning is handled properly.

link:http://www.xulplanet.com/tutorials/xultu/trees.html
src:http://www.xulplanet.com/reference/elemref/ref_treecol.html"
                 ("crop"
                  "If the label of the treecol is too small to fit in its given space, the  text will be cropped on the side specified by the crop attribute.  An ellipsis will be used in place of the cropped text. If the box  direction is reversed, the cropping is reversed.")
                 ("cycler"
                  "If true, then the column is a cycler column. In the case, clicking on a cell  in the column will alternate its state between on and off. This is used, for  example, in a mail window, for a column that indicates that the message is  read or unread with a small mark in the cell. If the cycler attribute is not  set, the cell is a regular text cell.")
                 ("dragging"
                  "This attribute will be set to true if the column is being dragged. This  attribute is set automatically; you shouldn't adjust it yourself.")
                 ("fixed"
                  "If true, the size of the column in the tree cannot be adjusted by the user.  Any splitters to either side will resize those columns while keeping the  fixed column at a constant size. If false or not specified, the user can  adjust the size of the column, typically by dragging the column header with  the mouse.")
                 ("hidden"
                  "This attribute, when set to true, hides the column. The user can show the  column by selecting it from a drop-down at the end of the column header row.")
                 ("hideheader"
                  "Set this to true to indicate that the tree column header should be displayed  without any column header styling.")
                 ("ignoreincolumnpicker"
                  "If true, the column does not appear in the column picker.")
                 ("label"
                  "The label that will appear on the treecol. If this is left out, no text appears.")
                 ("primary"
                  "If set to true, the column will have indentation and twisties drawn to the left  of it to indicate the hierarchy level of the rows. If no column has the primary  attribute set to true, the tree may still contain nested rows, although no  indication will be given to the user.")
                 ("sort"
                  "Set this to a RDF property to have the data in the column sorted based on  that property. The property will usually be the same as that of the cell  label.")
                 ("sortActive"
                  "This should be set to true for the column which should be sorted by default.")
                 ("sortDirection"
                  "Set this attribute to set the direction the column is sorted. The user may  change the sort direction by clicking the column headers.")
                 ("src"
                  "Set this attribute to have the tree column use an image for the header  instead of a label. Set this to the URL of an image to appear on the tree  column header. If this attribute is left out, no image appears and the  label is used instead. The class 'treecol-image' must be used on the  treecol element for the image to appear. You cannot have both an image and  a label.")
                 ("type"
                  "The type of tree column. The default is a text column that displays the content  as text."))
(DEF-XUL-ELEMENT "treecols"
                 
                 "A group of treecol elements. There should one and only one treecols element  in a tree.

link:http://www.xulplanet.com/tutorials/xultu/trees.html
src:http://www.xulplanet.com/reference/elemref/ref_treecols.html"
                 ("pickertooltiptext"
                  "The text for the tooltip on the column picker."))
(DEF-XUL-ELEMENT "treeitem"
                 
                 "A treeitem should be placed inside a treechildren element and should contain  treerow elements. The treeitem can be clicked by the user to select the row  of the tree. The treeitem contains a single row and all of what appear to the  user as that row's descendants.        In a template condition, you should use a treeitem instead of a content  element when the 'dont-build-content' flag is specified. Set the uri attribute  to the variable name to bind to a content node during matching.

link:http://www.xulplanet.com/tutorials/xultu/trees.html
src:http://www.xulplanet.com/reference/elemref/ref_treeitem.html"
                 ("container"
                  "Set to true if the element is to act as a container which can have child  elements. This would be used for folders. This will be set by the template  builder as needed.")
                 ("empty"
                  "Set to true if the element is a container that contains no children.")
                 ("label"
                  "For trees with only a single column, the label may be placed directly on the  treeitem without the need for a treerow and treecell inside.")
                 ("open"
                  "If true, the treeitem is open, allowing child elements to be displayed.")
                 ("uri"
                  "Used to specify the variable name for the content in a template condition.  When a rule is evaluated, it scans through each resource in the RDF  datasource looking for a match. Each resource is placed in the variable  specified in the uri attribute in turn. Then, the other rules are evaluated  for a match."))
(DEF-XUL-ELEMENT "treerow"
                 
                 "A single row in a tree. It should be placed inside a treeitem element.  Children of the treerow should be treecell elements. If child rows are  necessary, they should be placed in a treechildren element inside the parent  treeitem.

link:http://www.xulplanet.com/tutorials/xultu/trees.html
src:http://www.xulplanet.com/reference/elemref/ref_treerow.html"
                 ("properties"
                  "Sets the properties of the treerow, which can be used to style the row."))
(DEF-XUL-ELEMENT "treeseparator"
                 
                 "

link:http://www.xulplanet.com/reference/elemref/ref_treeseparator.html
src:
  Sets the properties of the treeseparator, which can be used to style the  separator.
"
                 ("properties"
                  "Sets the properties of the treeseparator, which can be used to style the  separator."))
(DEF-XUL-ELEMENT "triple"
                 
                 "A triple can be included inside a rule's conditions element. It is used check  for an assertion within a graph. If such an assertion exists, the rule may  match, assuming that all the conditions match. If the assertion does not exist,  the rule will not match. Both the subject and object attributes may be variables.        The subject of a triple is a RDF resource. In an RDF file this would usually be  an RDF Description element. The predicate would be a child element or property.  For example, for a bookmark resource, the name and URL would be predicates. They  should be specified in their full URI form. The object is the value of the  RDF property.

link:http://www.xulplanet.com/tutorials/xultu/advrules.html
src:http://www.xulplanet.com/reference/elemref/ref_triple.html")
(DEF-XUL-ELEMENT "vbox"
                 
                 "A container element which can contain any number of child elements. This is  equivalent to the box element, except it defaults to vertical orientation.

link:http://www.xulplanet.com/tutorials/xultu/boxes.html
src:http://www.xulplanet.com/reference/elemref/ref_vbox.html")
(DEF-XUL-ELEMENT "window"
                 
                 "Describes the structure of a top-level window. It is the root node of a XUL  document. It is by default a horizontally oriented box. As it is a box, all  box attributes can be used. By default, the window will have a  platform-specific frame around it.        To set an icon for the window, create a platform-specific icon file  <windowid>.ico and/or <windowid>.xpm and place or install these  files into the <mozilla-directory>/chrome/icons/default/ directory.  The <windowid> is the value of the id attribute on the window. This  allows you to have a different icon for each window.     For properties and methods, see the   XUL Window  object.

link:http://www.xulplanet.com/tutorials/xultu/window.html
src:http://www.xulplanet.com/reference/elemref/ref_window.html"
                 ("height"
                  "Set this property to use a specific height for the window. This property, along  with the width property, would usually be persisted with the persist attribute  so that the next time the user opens the window, the height and width of the  window will be the same as last time.")
                 ("hidechrome"
                  "Set this attribute to true to have the chrome including the titlebar hidden.")
                 ("id"
                  "The window id. This is used to identify the window and to construct the  filename for the icon for the window.")
                 ("screenX"
                  "The horizontal position that the window appears on the screen.")
                 ("screenY"
                  "The vertical position that the window appears on the screen.")
                 ("sizemode"
                  "The state of the window. The following values may be used:")
                 ("title" "The text to appear on the title bar of the window.")
                 ("width"
                  "Set this property to use a specific width for the window.")
                 ("windowtype"
                  "Set to a string which can be used to identify the type of window. This might be  used, for example, to distingush between a browser window and an editor window.  Some of Mozilla's window handling functions use this attribute to group windows  of the same type together."))
(DEF-XUL-ELEMENT "wizard"
                 
                 "This element is used to construct a step-by-step wizard found in some  applications to guide users through a task. It is used for a window with  several steps contained on several pages. This element provides the header  and buttons along the bottom, and also handles navigation between the pages.  Each page should be constructed using a wizardpage. The pages are  displayed in the order that they are placed in the wizard, unless you use  the next and pageid attributes on the pages to change the sequence. The  wizard will rendered in a manner suitable for the user's selected theme and  platform. In newer versions of Mozilla, a statusbar may be placed directly  inside the wizard element which will be shared among all pages.

link:http://www.xulplanet.com/tutorials/xultu/wizard.html
src:http://www.xulplanet.com/reference/elemref/ref_wizard.html"
                 ("firstpage" "True if the wizard is on the first page.")
                 ("lastpage" "True if the wizard is on the last page.")
                 ("onwizardback"
                  "This should be set to code which is called when the user presses the  Back button. Return true to allow the previous page to be displayed and  false to disallow moving back a page. Use the canRewind property to indicate  to the user (by disabling the Back button) that they cannot go back a page.")
                 ("onwizardcancel"
                  "This should be set to code which is called when the user presses the  Cancel button. Return true to allow the wizard to be cancelled, and close the  wizard, or return false to prevent the wizard from being cancelled. Use the  canCancel property to indicate to the user (by disabling the Cancel button)  that they cannot cancel.")
                 ("onwizardfinish"
                  "This should be set to code which is called when the user presses the  Finish button, which would appear in place of the Next button on the last page  of the wizard. Return true to allow the wizard to be closed, accepting any values  entered on the pages, or return false to prevent the wizard from being closed.  Use the canAdvance property to indicate to the user (by disabling the Finish button)  that they cannot end the wizard.")
                 ("onwizardnext"
                  "This should be set to code which is called when the user presses the  Next button. Return true to allow the next page to be displayed and  false to disallow moving to the next page. Use the canAdvance property to  indicate to the user (by disabling the Next button) that they cannot go to the  next page.")
                 ("pagestep" "The index of the current page.")
                 ("title"
                  "The title that appears at the top of the wizard. This is overriden by the label  attribute on the individual pages."))
(DEF-XUL-ELEMENT "wizardpage"
                 
                 "This element defines a page in a wizard. The content in the pages should be placed  inside the wizardpage element.

link:http://www.xulplanet.com/tutorials/xultu/wizard.html
src:http://www.xulplanet.com/reference/elemref/ref_wizardpage.html"
                 ("description"
                  "Set this attribute to a description to appear in the wizard header while the  page is being displayed.")
                 ("label"
                  "The title that appears on the top of the wizard while the page is displayed.")
                 ("next"
                  "Set to the page ID of the next page after this one. When set, the page with this  pageID is displayed when the Next button is pressed. This can be used to create  wizards that do not have a linear sequence. If one of the pages has a next  attribute, all of the pages should have one, except that last page.")
                 ("onpageadvanced"
                  "This should be set to code which is called when the user presses the Next  button while on the current page. Return true to allow the next page to be  displayed and false to disallow moving to the next page.")
                 ("onpagehide"
                  "The code in this attribute is called when the page is hidden, such as when  moving to another page. Return true to accept the page change and false to  prevent the page from changing. This code is called before the wizard's  onwizardnext and related functions.")
                 ("onpagerewound"
                  "This should be set to code which is called when the user presses the Back  button while on the current page. Return true to allow the previous page to  be displayed and false to disallow moving to the next page.")
                 ("onpageshow"
                  "The code in this attribute is called when the page is shown.")
                 ("pageid"
                  "This attribute should be set to a string that identifies the page's identifer  in the wizard. This is used with the next attribute. The wizard always starts  with the wizardpage that appears first in the wizard child list."))
(DEF-XUL-ELEMENT "bbox"
                 
                 "A horizontal box that is baseline aligned. It is equivalent to using  an hbox element with an align attribute set to baseline.

link:http://www.xulplanet.com/reference/elemref/ref_bbox.html
src:NIL")
(DEF-XUL-ELEMENT "page"
                 
                 "Similar to a window, except it should be used for XUL files that are  to be loaded into an iframe.

link:http://www.xulplanet.com/reference/elemref/ref_page.html
src:NIL")
(DEF-XUL-ELEMENT "scrollbox"
                 
                 "A box that has additional functions that can be used to scroll the content. Note  that a scrollbox doesn't cause scrollbars to be displayed. It is intended to be  used when an application wants to be able to adjust the scroll position itself.  If you just want scrollbars to appear, add style='overflow: auto;'. This may also  be used with any other container element.

link:http://www.xulplanet.com/reference/elemref/ref_scrollbox.html
src:NIL")
(DEF-XUL-ELEMENT "stringbundleset"
                 
                 "A container for stringbundle elements.

link:http://www.xulplanet.com/reference/elemref/ref_stringbundleset.html
src:NIL")
(DEF-XUL-ELEMENT "titlebar"
                 
                 "Used to represent a title bar. This element is used to create a custom  titlebar by placing the contents as children inside the titlebar. When  the titlebar is clicked and dragged, the window moves with it. Any  elements inside the titlebar do not receive events. The titlebar will send  a command event after the move is complete.

link:http://www.xulplanet.com/reference/elemref/ref_titlebar.html
src:NIL")
(DEF-XUL-ELEMENT "toolbaritem"
                 
                 "An item that appears on a toolbar. This element should wrap all items  that are not buttons, which should instead be created using the  toolbarbutton element. The element is used, for example, to hold  the url field.

link:http://www.xulplanet.com/reference/elemref/ref_toolbaritem.html
src:NIL")
(DEF-XUL-ELEMENT "toolbarset"
                 
                 "This element is used as a container for custom toolbars, which are added in  the custom toolbar dialog.

link:http://www.xulplanet.com/reference/elemref/ref_toolbarset.html
src:NIL")