(cl:defpackage #:clog-tinykeys
  (:use #:cl)
  (:export #:load-tinykeys
           #:set-on-keys))

(cl:in-package :clog-tinykeys)

(defparameter *tinykeys-format-string*
  "
clogTKSU['~~\1@*~~\A'] = (
    tinykeys.tinykeys(~~\0@*~~\A.get().at(0), { ~
~{~{
        '~A': (event) => {
            if (~:[false~;true~] || event.target === ~~\0@*~~\A.get().at(0)) {
~:[~;                event.preventDefault()~&~]~
~:[~;                event.stopPropagation()~&~]~:
                ~~\0@*~~\A.trigger('~~\1@*~~\A', '~A')
            }
        },~}~}
    })
)")

(defun load-tinykeys (document)
  (clog:load-script document "https://cdn.jsdelivr.net/npm/tinykeys@3.0.0/dist/tinykeys.umd.min.js")
  (clog:js-execute document "clogTKSU = {}"))

(defun generate-event-handler (plists)
  (loop for i from 0
        for plist in plists
        for id = (format nil "TK:~D" i)
        for handler = (getf plist :handler)
        collect `(setf (gethash ,id handlers) ,handler) into setfs
        finally (return `(let ((handlers (clog:make-hash-table* :test 'equal)))
                           ,@setfs
                           (lambda (&optional event data)
                             (declare (ignorable event data))
                             (let ((handler (gethash data handlers)))
                               (when handler
                                 (funcall handler))
                               nil))))))

(defun generate-script (plists always-active)
  (loop for i from 0
        for plist in plists
        for id = (format nil "TK:~D" i)
        for keys = (getf plist :keys)
        for prevent-default = (getf plist :prevent-default)
        for stop-propagation = (getf plist :stop-propagation)
        for always-active* = (if (get-properties plist '(:always-active))
                                (getf plist :always-active)
                                always-active)
        collect (list keys always-active* prevent-default stop-propagation id) into parameters
        finally (return (format nil *tinykeys-format-string* parameters))))

(defun tks-id-p (string)
  (let ((tmp (search "TKS:" string)))
    (if tmp (if (zerop tmp) t nil) nil)))

(defun normalise-id (id)
  (if id
      (if (tks-id-p id) id (format nil "TKS:~A" id))
      (format nil "TKS:~A" (clog:generate-id))))

(defun generate-keybindings (keybindings clog-obj id always-active)
  (let ((event-handler (generate-event-handler keybindings))
        (script (generate-script keybindings always-active)))
    (when (or id keybindings)
      `(let ((event-name (normalise-id ,id))
             ,@(when keybindings
                `((event-handler ,event-handler)
                  (script ,script))))
         ,@(when id
             `((clog:set-on-event-with-data (clog:connection-body ,clog-obj) event-name nil)
               (clog:js-execute ,clog-obj (format nil "clogTKSU['~@*~A']?.(); delete clogTKSU['~@*~A'];" event-name))))
         ,@(when keybindings
             `((clog:set-on-event-with-data (clog:connection-body ,clog-obj) event-name event-handler :cancel-event t)
               (clog:js-execute ,clog-obj (format nil script (clog:jquery ,clog-obj) event-name))))
         event-name))))

(defmacro set-on-keys ((clog-obj &key (id nil) (always-active nil)) &body keybindings)
  (when (and (= (length keybindings) 1) (eq (first keybindings) nil))
    (setf keybindings nil))
  (generate-keybindings keybindings clog-obj id always-active))
