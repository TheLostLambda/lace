;;; A small file that loads the ASDF system and pulls in QL packages
(pushnew (uiop:getcwd) asdf:*central-registry* :test #'equal)
(ql:quickload :rove)
(asdf:load-system :llace)
