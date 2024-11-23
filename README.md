# clog-tinykeys
clog-tinykeys is a CLOG plugin for defining application keyboard shortcuts to call server-side functions from client-side keyboard events. It uses jamiebuilds' tinykeys Javascript library to install listeners, and handle client-side recognition of keyboard events.

Load the tinykeys library (from jsdeliver) in the client:
``` common-lisp
(clog-tinykeys:load-tinykeys (clog:html-document body))
```


Define keyboard shortcuts/hotkeys:
``` common-lisp
(clog-tinykeys:set-on-keys body
  (t (:keys "t 1" :handler #'toggle-switch1)
     (:keys "t 2" :handler #'toggle-switch2)
     (:keys "t b" :handler #'toggle-both)
     (:keys "Control+o" :handler #'turn-on-both :prevent-default t)
     (:keys "Escape" :handler #'turn-off-both)))
```

Conditional keyboard shortcuts can be specified by providing a form that will be evaluated during their construction ('T' in the above example).

Some thought should be given to where keyboard shortcuts are attached. When nodes disapear from the DOM, CLOG will garbage collect objects associated with the element including custom event handlers, but this wont occur for long-lived nodes. It's probably a good idea to attach keyboard shortcuts closest to where they are defined as this can allow scoping, but also they will be cleaned up when the node is destroyed.


