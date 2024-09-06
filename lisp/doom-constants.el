;;; Custom features & global constants
;; Doom has its own features that its modules, CLI, and user extensions can
;; announce, and don't belong in `features', so they are stored here, which can
;; include information about the external system environment. Module-specific
;; features are kept elsewhere, however.
(defconst doom-features
  (pcase system-type
    ('darwin                           '(macos bsd))
    ((or 'cygwin 'windows-nt 'ms-dos)  '(windows))
    ((or 'gnu 'gnu/linux)              '(linux))
    ((or 'gnu/kfreebsd 'berkeley-unix) '(linux bsd)))
  "A list of symbols denoting available features in the active Doom profile.")

;; Convenience aliases for internal use only (may be removed later).
(defconst doom-system            (car doom-features))
(defconst doom--system-windows-p (eq 'windows doom-system))
(defconst doom--system-macos-p   (eq 'macos doom-system))
(defconst doom--system-linux-p   (eq 'linux doom-system))