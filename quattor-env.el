;;; quattor-env.el --- Environment variables useful during Quattor
;;; development.

;; Copyright (C) 2013 Luis Fernando Muñoz Mejías

;; Author: Luis Fernando Muñoz Mejías <lfmunozmejias@gmail.com>
;; Keywords: processes

;;; Distributed under the terms of the Apache License, version 2.0,
;;; see http://www.apache.org/licenses/ for more information.

;;; Commentary:

;; Environment variables useful for Quattor Perl developments.  You
;; may want to adjust them to your local setup.

;;; Code:

(setenv "PERL5LIB" (expand-file-name "~/quattor/perl"))

(defun enable-perl-coverage-reports ()
  "Set the environment variable for generating coverage reports."
  (interactive)
  (setenv "HARNESS_PERL_SWITCHES"
          "-MDevel::Cover=+ignore,/(test|LC|CCM|CAF|Test|dependency)/"))

(defun disable-perl-coverage-reports ()
  "Disable the generation of coverage reports.
Coverage reports are slow to generate.  We may wish to disable
them."
  (interactive)
  (setenv "HARNESS_PERL_SWITCHES" ""))

(provide 'quattor-env)
;;; env.el ends here
