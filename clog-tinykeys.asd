;;; SPDX-License-Identifier: MIT-0

(asdf:defsystem :clog-tinykeys
  :version      "0.1.0"
  :description  "A tinykeys plugin for CLOG"
  :author       "Infinae"
  :license      "MIT-0"
  :components   ((:file "clog-tinykeys"))
  :depends-on   (#:clog))
