;;; pan-mode.el --- Emacs mode for the Pan language

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Luis Fernando MuÃ±oz MejÃ­as <lfmunozmejias@gmail.com>
;; Keywords:languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Emacs mode for the Pan language.  You can find the Pan language
;; specification in
;; http://quattor.org/documentation/2012/06/19/documentation-pan-book.html

;;; Code:

(defvar pan-mode-hook nil)

(defvar pan-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Pan mode.")

(add-to-list 'auto-mode-alist '("\\.pan\\'" . pan-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . pan-mode))

(defconst pan-font-lock-basics
  (list (cons  (regexp-opt '("unique" "declaration" "object" "structure")
                           t)
               font-lock-builtin-face)
        (cons (regexp-opt '("template" "variable" "type" "function" "bind"
                            "prefix" "return" "if" "while" "for" "foreach"
                            "error" "debug")
                          t)
              font-lock-keyword-face)
        (cons (regexp-opt '("SELF" "TEMPLATE" "OBJECT" "FUNCTION")
                          t)
              font-lock-constant-face))
  "Minimal syntax highlighting.")

(defvar pan-font-lock-keywords pan-font-lock-basics
  "Default highlighting expressions for pan-mode.")

(defun pan-indent-line ()
  "Indent current line as Pan code.

The rules are like this:

- Beginning of buffer is indented at line 0.
- Foo."
  (interactive)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          cur-indent)
        (if (looking-at "^[ \t]*};") ; Check for rule 2
            (progn
              (save-excursion
                (forward-line -1)
                (setq cur-indent (- (current-indentation) tab-width)))
              (if (< cur-indent 0)
                  (setq cur-indent 0)))
          (save-excursion
            (while not-indented
              (forward-line -1)
              (if (looking-at "^[ \t]*};")
                  (progn
                    (setq cur-indent (current-indentation))
                    (setq not-indented nil))
                (if (looking-at "^.*{")
                    (progn
                      (setq cur-indent (+ (current-indentation) tab-width))
                      (setq not-indented nil))
                  (if (bobp)
                      (setq not-indented nil))))))))))

(defvar pan-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?@ ". 1b" st)
    (modify-syntax-entry ?{ ". 2b" st)
    (modify-syntax-entry ?} ". 3b" st)
    (modify-syntax-entry ?# "< c" st)
    (modify-syntax-entry ?# "> c" st)
    st))

(defun pan-mode ()
  "Major mode for editing Pan code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table pan-mode-syntax-table)
  (use-local-map pan-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(pan-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       'pan-indent-line)
  (setq major-mode 'pan-mode)
  (setq mode-name "Pan")
  (run-hooks 'pan-mode-hook))


(provide 'pan-mode)
;;; pan-mode.el ends here
