# clog-tinykeys
clog-tinykeys is a CLOG plugin for defining application keyboard shortcuts to call server-side functions from client-side keyboard events. It uses jamiebuilds' tinykeys Javascript library to install listeners, and handle client-side recognition of keyboard events.

## Installation
Clone repo to your local Common Lisp sources directory, then `(asdf:load-system :clog-tinykeys)` or add as a dependency to your application's ASDF system definition.

## Get Started
**Load the plugin**:
``` common-lisp
(clog-tinykeys:load-tinykeys (clog:html-document body))
```

**Create some keybindings**:
``` common-lisp
;; Supply an ID to be associated with the created
;; keybindings. If no ID is supplied, SET-ON-KEYS
;; will return a generated one for later use.
(set-on-keys (body :id "global-keybindings")
  (("m /" :prevent-default t) #'open-search-mode)
  (("m e") #'open-edit-mode)
  (("Escape") #'close-mode))

;; Remove keybindings associated with ID
(set-on-keys (body :id "global-keybindings") nil)

;; The handler has an implicit progn when it
;; detects the absence of a function designator
(set-on-keys (input)
  (("Control+u")
   (to-uppercase input))
  (("Control+d")
   (to-downcase input))
  (("Escape")
   (setf (text-value input) "")
   (blur input)))
```

## Syntax
<pre>
<b>load-tinykeys</b> <i>clog-document</i> &key <i>load-only-once</i> <i>wait-timeout</i> <i>wait-for-load</i>
⇒ boolean
</pre>

<pre>
<b>set-on-keys</b>
  (<i>clog-obj</i> &key <i>id</i> <i>trigger</i> <i>stop-propagation</i> <i>prevent-default</i>)
    &rest { nil | ((<i>keys</i> &key <i>trigger</i> <i>stop-propagation</i> <i>prevent-default</i>) &body <i>handler</i>) }+
⇒ string
</pre>

## Scoped keybindings
tinykeys doesn't allow the creation of keybinding scopes, but a rudimentary form can be acheived by attaching keybindings to a DIV element and setting it's tab-index to -1 so that it can receive keyboard events. As long as that element or a child of that element, say an input, has focus then events will reach it.




