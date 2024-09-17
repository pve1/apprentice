(asdf:defsystem :slime-apprentice
  :description "Slime apprentice describes things in a helpful manner."
  :author "Peter von Etter"
  :license "LGPL-3.0"
  :version "0.0.1"
  :components ((:file "package")
               (:file "slime-apprentice")
               (:file "describe-apprentice")
               (:file "value-apprentice")
               (:file "caching-apprentice")
               (:file "apropos-apprentice")
               (:file "grep-apprentice")
               (:file "apprentice-gathering"))
  :depends-on (:swank
               :eclector))
