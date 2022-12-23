;;; A small file that loads the ASDF system and pulls in QL dependencies
(pushnew (uiop:getcwd) ql:*local-project-directories* :test #'equal)
(ql:quickload :llace/tests)
