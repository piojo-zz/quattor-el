;;; mvn-compile.el --- Compile Maven projects.

;; Author: Luis Fernando Muñoz Mejías <lfmunozmejias@gmail.com>
;; Keywords:

;;; Distributed under the terms of the Apache License, version 2.0.
;;; See https://www.apache.org/licenses/LICENSE-2.0.html for more
;;; details.

;;; Commentary:

;; Compiles a Maven project from within an Emacs buffer.

;;; Code:


(defun mvn-compile (target)
  "Compile the given TARGET of a Maven project."
  (let ((default-directory
          (locate-dominating-file default-directory "pom.xml"))
        (compile-command (format "mvn clean %s" target)))
    (when default-directory
      (call-interactively 'compile))))

(defun mvn-package ()
  "Run the Maven package target."
  (interactive)
  (mvn-compile "package"))

(defun mvn-test ()
  "Run the Maven test target."
  (interactive)
  (mvn-compile "test"))

(provide 'mvn-compile)
;;; mvn-compile.el ends here
