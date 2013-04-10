# Emacs configurations for Quattor development and operations

This repository contains some Emacs extensions for running Maven
commands under Emacs and invoking the Pan compiler in SCDB-like
environments.

You may want to edit `quattor-env.el` and set the `PERL5LIB`
environment variable to your tastes.

Also, I currently don't enforce any key bindings.  You may want to
define your own.  Mine look like this:

```lisp
(global-set-key (kbd "M-p p") 'mvn-package)
(global-set-key (kbd "M-p t") 'mvn-test)
```
