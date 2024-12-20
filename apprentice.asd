(asdf:defsystem :apprentice
  :class "extensible-inferred-system:comment-system"
  :defsystem-depends-on (:extensible-inferred-system)
  :description "Apprentice describes things in a helpful manner."
  :author "Peter von Etter"
  :license "LGPL-3.0"
  :version "0.0.1"
  :depends-on ("apprentice/apprentice"))
