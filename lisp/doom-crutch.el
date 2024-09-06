;; Bundle together all of the Doom stuff we still need into one place.  Most
;; files included are just blatant copies.


    (load-library "doom-plug")
    ;; This needs to come before the Doom keybindings, but I don't want to muck
    ;; with the original source file.  This can be bound up into a general
    ;; "doom-theft" module later.
    (load-library "doom-constants")
    ;; Same as constants.
    (load-library "doom-lib")
    ;; Same as constants.
    (load-library "doom-use-package")
    (load-library "doom-keybinds")
