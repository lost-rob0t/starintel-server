((nil
  . ((eval . (progn
               ;; Nix-pure rule: don't load Quicklisp contribs.
               ;; This prevents Emacs from requesting slynk-quicklisp (and thus ASDF :quicklisp).
               (setq-local sly-contribs '(sly-fancy))
               ;; Optional: make connect UX less annoying if you run different ports.
               ;; (setq-local sly-default-lisp 'sbcl)
               )))))
