# clog-tinykeys
clog-tinykeys is a CLOG plugin for defining application keyboard shortcuts to call server-side functions from client-side keyboard events. It uses jamiebuilds' tinykeys Javascript library to install listeners, and handle client-side recognition of keyboard events.

## Use
Clone repo to your local Common Lisp sources directory, then `(asdf:load-system :clog-tinykeys)` or add as a dependency to your application's ASDF system definition.

## API
**load-tinykeys**:
``` common-lisp
(clog-tinykeys:load-tinykeys (clog:html-document body))
```

**set-on-keys**:
``` common-lisp
;; Supply an ID to be associated with the created
;; keybindings. If no ID is supplied, SET-ON-KEYS
;; will return a generated one for later use.
(set-on-keys (body :id "global-keybindings")
  (("m /" :prevent-default t) #'open-search-mode)
  (("m e") #'open-edit-mode)
  (("Escape" #'close-mode)))

;; Remove keybindings associated with ID
(set-on-keys (body :id "global-keybindings") nil)

;; The handler has an implicit progn when it
;; detects the absence of a function designator
(let ((id (set-on-keys (input)
            (("Control+u") (to-uppercase input))
            (("Control+d") (to-downcase input))
            (("Escape")
             (setf (text-value input) "")
             (blur input)))))
  (set-on-keys (input :id id) nil))
```



