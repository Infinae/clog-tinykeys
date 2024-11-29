(cl:defpackage #:clog-tinykeys
  (:use #:cl)
  (:export #:load-tinykeys
           #:set-on-keys))

(cl:in-package :clog-tinykeys)

(defparameter *tinykeys-format-string*
  "
clogTinykeysUnsubscribers['~~\1@*~~\A'] = (
    tinykeys.tinykeys(~~\0@*~~\A.get().at(0), { ~
~{~{
        '~A': (event) => {
            if (~[~
                      (event.target === ~~\0@*~~\A.get().at(0)) || $(document.activeElement).is(':not(input)')~
                      ~;true~
                      ~;event.target === ~~\0@*~~\A.get().at(0)~
             ~]) {
~:[~;                event.preventDefault()~&~]~
~:[~;                event.stopPropagation()~&~]~:
                ~~\0@*~~\A.trigger('~~\1@*~~\A', '~A')
            }
        },~}~}
    })
)")

(defun load-tinykeys (document)
  (clog:load-script document "https://cdn.jsdelivr.net/npm/tinykeys@3.0.0/dist/tinykeys.umd.min.js")
  (clog:js-execute document "clogTinykeysUnsubscribers = {}"))

(defun generate-event-handler (plists)
  (loop for i from 0
        for plist in plists
        for id = (format nil "TK:~D" i)
        for handler = (let ((handler-body (getf plist :handler-body)))
                        (cond ((null handler-body) '(lambda () nil))
                              ((and (= (length handler-body) 1)
                                    (search "#'" (write-to-string (first handler-body))))
                               `,(first handler-body))
                              (t `(lambda () ,@handler-body))))
        collect `(setf (gethash ,id handlers) ,handler) into setfs
        finally (return `(let ((handlers (clog:make-hash-table* :test 'equal)))
                           ,@setfs
                           (lambda (&optional event data)
                             (declare (ignorable event data))
                             (let ((handler (gethash data handlers)))
                               (when handler
                                 (funcall handler))
                               nil))))))

(defun generate-script (plists)
  (loop for i from 0
        for plist in plists
        for id = (format nil "TK:~D" i)
        for keys = (getf plist :keys)
        for trigger = (let ((value (getf plist :trigger)))
                        (cond ((eq value :default) 0)
                              ((eq value :always) 1)
                              ((eq value :is-target) 2)))
        for stop-propagation = (getf plist :stop-propagation)
        for prevent-default = (getf plist :prevent-default)
        collect (list keys trigger prevent-default stop-propagation id) into parameters
        finally (return (format nil *tinykeys-format-string* parameters))))

(defun normalise-id (id)
  (assert (or (stringp id) (and (keywordp id) (eq id :undefined))))
  (if (eq id :undefined)
      (format nil "TKS:~A" (clog:generate-id))
      (if (not (zerop (or (search "TKS:" id) -1)))
          (format nil "TKS:~A" id)
          id)))

(defun restructure-keybinding (keybinding)
  (destructuring-bind
      ((keys &key (stop-propagation :undefined) (prevent-default :undefined) (trigger :undefined)) &rest handler-body) keybinding
    (list :keys keys
          :trigger trigger
          :stop-propagation stop-propagation
          :prevent-default prevent-default
          :handler-body handler-body)))

(defun merge-value (keyword outer-value inner-value)
  (cond ((and (eq outer-value :undefined) (eq inner-value :undefined))
         (cond ((eq keyword :trigger) :default)
               ((eq keyword :stop-propagation) nil)
               ((eq keyword :prevent-default) nil)))
        ((eq inner-value :undefined) outer-value)
        (t inner-value)))

(defun merge-properties (outer-properties inner-properties)
  (loop for keyword in '(:trigger :stop-propagation :prevent-default)
        for outer-value = (getf outer-properties keyword)
        for inner-value = (getf inner-properties keyword)
        nconc (list keyword (merge-value keyword outer-value inner-value)) into result
        finally (return
                  (concatenate
                   'list
                   (list :keys (getf inner-properties :keys))
                   (list :handler-body (getf inner-properties :handler-body))
                   result))))

(defun generate-keybindings (clog-obj id parameters keybindings)
  (assert (not (null keybindings)) nil "Please enter NIL or one or more keybinding forms.")
  (let* ((keybindings (loop for kb in keybindings collect (restructure-keybinding kb)))
         (keybindings (loop for kb in keybindings collect (merge-properties parameters kb)))
         (event-handler (generate-event-handler keybindings))
         (script (generate-script keybindings)))
    (when (or id keybindings)
      `(let ((event-name (normalise-id ,id))
             ,@(when keybindings
                 `((event-handler ,event-handler)
                   (script ,script))))
         ,@(unless (or (not id) (keywordp id) (eq id :undefined))
             `((clog:set-on-event-with-data (clog:connection-body ,clog-obj) event-name nil)
               (clog:js-execute ,clog-obj (format nil "clogTinykeysUnsubscribers['~@*~A']?.(); delete clogTinykeysUnsubscribers['~@*~A'];" event-name))))
         ,@(when keybindings
             `((clog:set-on-event-with-data (clog:connection-body ,clog-obj) event-name event-handler :cancel-event t)
               (clog:js-execute ,clog-obj (format nil script (clog:jquery ,clog-obj) event-name))))
         event-name))))

(defmacro set-on-keys
    ((clog-obj
      &key
        (id :undefined)
        (trigger :undefined)
        (stop-propagation :undefined)
        (prevent-default :undefined))
     &body keybindings)
  (generate-keybindings
   clog-obj id
   (list :trigger trigger
         :stop-propagation stop-propagation
         :prevent-default prevent-default)
   keybindings))
