(require 'load-relative)
(require-relative-list
 '("./dbgr/track-mode") "dbgr-")
(require-relative-list
   '("./dbgr/ruby/rbdbgr"
   "./dbgr/python/pydbgr"))
   
(provide-me)
