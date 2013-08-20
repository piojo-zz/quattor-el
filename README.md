# Emacs configurations for Quattor development and operations

This repository contains some Emacs extensions for running Maven
commands under Emacs and invoking the Pan compiler in SCDB-like
environments.

I list the most important ones

## Extensions for Quattor development

They allow to run our usual Maven commands from within Emacs.  You may
want to edit `quattor-env.el` and set the `PERL5LIB` environment
variable to your tastes.

Also, I currently don't enforce any key bindings.  You may want to
define your own.  Mine look like this:

```lisp
(global-set-key (kbd "M-p p") 'mvn-package)
(global-set-key (kbd "M-p t") 'mvn-test)
```

## Pan mode

A work-in-progress major mode for the Pan language.

Currently it has two problems:

* Annotations are not ended correctly.  The current workaround is to
  end an annotation block with `@}`, so that Emacs thinks both
  delimiters are made of two characters.
* Complex types may not indented correctly.

## Snippets

See the snippets directory for more details.
