;;; pan-mode.el --- Emacs mode for the Pan language

;; Author: Luis Fernando Muñoz Mejías <lfmunozmejias@gmail.com>
;; Keywords:languages

;; Distributed under the terms of the Apache license version 2.0.  See
;; https://www.apache.org/licenses/LICENSE-2.0.html

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

(defconst pan-template-qualifiers
  '("unique" "structure" "object" "declaration")
  "Pan template qualifiers.")

(defconst pan-keywords
  '("template" "if" "else" "return" "prefix" "bind" "function" "variable" "with"
    "final"  "type" "include" "valid" "for" "foreach" "while")
  "Keywords in the pan language.")

(defconst pan-builtins
  '("nlist" "list" "append" "merge" "format" "debug" "error" "match" "matches"
    "create" "index" "length" "replace" "splice" "split" "substr" "to_lowercase"
    "to_uppercase" "traceback" "deprecated" "base64_decode" "base64_encode"
    "escape" "unescape" "first" "key" "next" "prepend" "splice" "is_boolean"
    "is_defined" "is_double" "is_list" "is_long" "is_nlist" "is_null"
    "is_number" "is_property" "is_resource" "is_string" "to_boolean" "to_double"
    "to_long" "to_string" "clone" "delete" "exists" "path_exists" "if_exists"
    "value")
  "Built-ins in the Pan language.")

(defconst pan-reserved-globals
  '("SELF" "OBJECT" "TEMPLATE" "FUNCTION" "true" "false" "null"))

(defconst pan-type-names
  '("long" "string" "double" "number" "boolean")
  "Names for the basic Pan types.")

(defvar pan-tab-width 4 "Width of a tab in Pan mode.")

(defvar pan-font-lock
  (list
   (cons (regexp-opt pan-keywords 'symbols) font-lock-keyword-face)
   (cons (regexp-opt pan-reserved-globals 'symbols) font-lock-constant-face)
   (cons (regexp-opt pan-template-qualifiers 'symbols) font-lock-variable-name-face)
   (cons (regexp-opt pan-type-names 'symbols) font-lock-type-face)
   (cons (regexp-opt pan-builtins 'symbols) font-lock-builtin-face))
  "Font-lock for the Pan language.")



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
  (let ((st (make-syntax-table java-mode-syntax-table)))
    (modify-syntax-entry ?@ ". 1" st)
    (modify-syntax-entry ?{ "(} 2" st)
    (modify-syntax-entry ?} "){ >" st)
    (modify-syntax-entry ?# "<b" st)
    (modify-syntax-entry ?\n ">b" st)
    (modify-syntax-entry ?' "\"")
    st))


;; (defun pan-mode ()
;;   "Major mode for editing Pan code."
;;   (interactive)
;;   (kill-all-local-variables)
;;   (set-syntax-table pan-mode-syntax-table)
;;   (use-local-map pan-mode-map)
;;   (set (make-local-variable 'font-lock-defaults)
;;        '(pan-font-lock-keywords))
;;   (set (make-local-variable 'indent-line-function)
;;        'pan-indent-line)
;;   (setq major-mode 'pan-mode)
;;   (setq mode-name "Pan")
;;   (run-hooks 'pan-mode-hook))

(define-derived-mode pan-mode java-mode "Pan"
  "Mode for editing Pan code."
  :syntax-table pan-mode-syntax-table
  (setq font-lock-defaults '(pan-font-lock)))


(provide 'pan-mode)
;;; pan-mode.el ends here
