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
;; Non-normative example showing supported declarations.
;; By default handlers will only be triggered when the event listener
;; and event target match. Override this behavior by setting
;; :always-active to t.
(clog-tinykeys:set-on-keys (body :always-active t)
  (:keys "f o o" :handler (lambda () (print 'foo)))
  (:keys "Control+o" :handler #'open :prevent-default t :stop-propagation t)
  ;; Since :always-active is set to t, specifying :always-active nil here
  ;; will restore default behavior just for this keybinding.
  (:keys "$mod+c" :handler #'show-count :always-active nil))
```



