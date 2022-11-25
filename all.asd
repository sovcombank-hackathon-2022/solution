(pushnew "~/sovcomtrade/reblocks-websocket/" asdf:*central-registry* :test #'equal)

(defsystem all
  :class :package-inferred-system
  :pathname ""
  :depends-on ("app"
               "passport"
               "rates"
               "accounts"
               "processing"
               "all/all"))
