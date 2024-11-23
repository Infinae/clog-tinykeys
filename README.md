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
(clog-tinykeys:set-on-keys body
  (t (:keys "f o o" :handler (lambda () (print 'foo)))
     (:keys "Control+o" :handler #'open :prevent-default t :stop-propagation t)))
```


## Example
``` common-lisp
(defparameter *switches* (make-hash-table*))

(defun switch (body)
  (setf (title (html-document body)) "Switch")
  (clog-tinykeys:load-tinykeys (html-document body))
  (setf (display body) "flex")
  (setf (flex-direction body) "horizontal")
  (setf (column-gap body) "2em")
  (labels ((update-switch (clog-obj)
             (setf (background-color clog-obj)
                   (if (gethash clog-obj *switches*)
                       "orange" "lightgray")))
           (create-switch (clog-obj)
             (let ((switch (create-div clog-obj)))
               (setf (width switch) "5em")
               (setf (height switch) "5em")
               (setf (display switch) "inline-block")
               (update-switch switch)
               switch)))
    (let ((switch1 (create-switch body))
          (switch2 (create-switch body)))
      (flet ((toggle-switch1 (&optional event data)
               (declare (ignorable event data))
               (let ((value (gethash switch1 *switches*)))
                 (setf (gethash switch1 *switches*) (not value)))
               (update-switch switch1)) 
             (toggle-switch2 (&optional event data)
               (declare (ignorable event data))
               (let ((value (gethash switch2 *switches*)))
                 (setf (gethash switch2 *switches*) (not value)))
               (update-switch switch2))
             (toggle-both (&optional event data)
               (declare (ignorable event data))
               (let ((value1 (gethash switch1 *switches*))
                     (value2 (gethash switch2 *switches*)))
                 (setf (gethash switch1 *switches*) (not value1))
                 (setf (gethash switch2 *switches*) (not value2)))
               (update-switch switch1)
               (update-switch switch2))
             (turn-off-both (&optional event data)
               (declare (ignorable event data))
               (setf (gethash switch1 *switches*) nil)
               (setf (gethash switch2 *switches*) nil)
               (update-switch switch1)
               (update-switch switch2))
             (turn-on-both (&optional event data)
               (declare (ignorable event data))
               (setf (gethash switch1 *switches*) t)
               (setf (gethash switch2 *switches*) t)
               (update-switch switch1)
               (update-switch switch2)))
        (clog-tinykeys:set-on-keys body
          (t (:keys "t 1" :handler #'toggle-switch1)
             (:keys "t 2" :handler #'toggle-switch2)
             (:keys "t b" :handler #'toggle-both)
             (:keys "Control+o" :handler #'turn-on-both :prevent-default t)
             (:keys "Escape" :handler #'turn-off-both)))))))
```



