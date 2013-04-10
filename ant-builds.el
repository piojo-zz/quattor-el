;;; ant-builds.el --- Build Ant projects, with some SCDB specifities

;; Author: Luis Fernando Muñoz Mejías <lfmunozmejias@gmail.com>
;; Keywords: local


;;; Distributed under the terms of the Apache License, version 2.0.
;;; See https://www.apache.org/licenses/LICENSE-2.0.html for more
;;; details.


;;; Commentary:

;; Functions

;;; Code:

(defun ant-builds (parameters)
  "Build an Ant project with PARAMETERS."
  (let ((default-directory
	  (locate-dominating-file default-directory "build.xml"))
	(compile-command (format "ant %s" parameters)))
    (when default-directory
      (call-interactively 'compile))))

(defun ant-run()
  "Runs ant with no parameters."
  (interactive)
  (ant-builds ""))

(defun ant-log()
  "Run the compiler with logging mode"
  (interactive)
  (ant-builds "-Dpan.logging=call"))

(defun ant-dbg()
  "Build an SCDB project with debugging output."
  (interactive)
  (ant-builds "-Dpan.debug.include=.*"))


(provide 'ant-builds)
;;; ant-builds.el ends here
