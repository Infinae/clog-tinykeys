(cl:defpackage #:clog-tinykeys
  (:use #:cl)
  (:export #:load-tinykeys
           #:set-on-keys))

(cl:in-package :clog-tinykeys)

(defparameter *tinykeys-format-string*
  "
tinykeys.tinykeys(~~\0@*~~\A.get().at(0), { ~
~{~{
    '~A': (event) => {
        if (~:[false~;true~] || event.target === ~~\0@*~~\A.get().at(0)) {
~:[~;            event.preventDefault()~&~]~
~:[~;            event.stopPropagation()~&~]~:
            ~~\0@*~~\A.trigger('~~\1@*~~\A', '~A')
        }
    },~}~}
})")

(defun load-tinykeys (document)
  (clog:load-script document "https://cdn.jsdelivr.net/npm/tinykeys@3.0.0/dist/tinykeys.umd.min.js"))

(defun generate-event-handler (plists)
  (loop for i from 0
        for plist in plists
        for id = (format nil "TK~D" i)
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
        for id = (format nil "TK~D" i)
        for keys = (getf plist :keys)
        for prevent-default = (getf plist :prevent-default)
        for stop-propagation = (getf plist :stop-propagation)
        for always-active* = (if (get-properties plist '(:always-active))
                                (getf plist :always-active)
                                always-active)
        collect (list keys always-active* prevent-default stop-propagation id) into parameters
        finally (return (format nil *tinykeys-format-string* parameters))))

(defun generate-condition-clause (clause clog-obj always-active)
  (let ((condition (first clause))
        (event-handler (generate-event-handler (rest clause)))
        (script (generate-script (rest clause) always-active))
        (js-always-active (if always-active "true" "false")))
    `(when ,condition
       (let ((event-name (format nil "TKS~D" (clog:generate-id)))
             (event-handler ,event-handler)
             (script ,script))
         (clog:set-on-event-with-data (clog:connection-body ,clog-obj) event-name event-handler :cancel-event t)
         (clog:js-execute ,clog-obj (format nil script (clog:jquery ,clog-obj) event-name))))))

(defmacro set-on-keys ((clog-obj &key (always-active nil)) &body clauses)
  (if (plusp (length clauses))
      `(progn ,@(loop for clause in clauses
                      collect (generate-condition-clause clause clog-obj always-active) into code
                      finally (return code)))
      nil))
