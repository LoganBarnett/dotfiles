(use-package jq-ts-mode
  :mode ("\\.jq\\'" . jq-ts-mode)
  :init
  ;; Emacs expects libtree-sitter-jq.{so,dylib}, but this grammar builds as
  ;; libjq.{so,dylib}.
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-load-name-override-list
                 '(jq "libjq" "tree_sitter_jq")
                 )
    )
  )
