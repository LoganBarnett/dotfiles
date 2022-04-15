;;; applescript-mode.el --- major mode for editing AppleScript source

;; Copyright (C) 2004  MacEmacs JP Project

;;; Credits:
;;
;; Copyright (C) 2003,2004 FUJIMOTO Hisakuni
;;   http://www.fobj.com/~hisa/w/applescript.el.html
;; Copyright (C) 2003 443,435 (Public Domain?)
;;   http://pc.2ch.net/test/read.cgi/mac/1034581863/
;; Copyright (C) 2004 Harley Gorrell <harley@mahalito.net>
;;   http://www.mahalito.net/~harley/elisp/osx-osascript.el

;; Author: sakito <sakito@users.sourceforge.jp>
;; URL: https://github.com/emacsorphanage/applescript-mode
;; Keywords: languages, tools
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

(defconst applescript-mode-version "$Revision$"
  "The current version of the AppleScript mode.")

(defconst applescript-mode-help-address "sakito@users.sourceforge.jp"
  "Address accepting submission of bug reports.")

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

;; AppleScript Mode

;;; Usage:
;; To use applescript-mode.el put the following line in your .emacs:
;; (autoload 'applescript-mode "applescript-mode"
;;   "Major mode for editing AppleScript source." t)
;; (add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))

;; Please use the SourceForge MacEmacs JP Project to submit bugs or
;; patches:
;;
;;     http://sourceforge.jp/projects/macemacsjp

;; FOR MORE INFORMATION:

;; There is some information on applescript-mode.el at

;;     http://macemacsjp.sourceforge.jp/documents/applescript-mode/


;;; Code:

;; user customize variables
(defgroup applescript nil
  "Support for AppleScript, <http://www.apple.com/applescript/>"
  :group 'languages
  :prefix "applescript-")

(defcustom applescript-osascript-command "osascript"
  "Execute AppleScripts and other OSA language scripts."
  :type 'string
  :group 'applescript)

(defcustom applescript-osacompile-command "osacompile"
  "Compile AppleScripts and other OSA language scripts."
  :type 'string
  :group 'applescript)

(defcustom applescript-osascript-command-args '("-ss")
  "*List of string arguments to be used when starting a osascript."
  :type '(repeat string)
  :group 'applescript)

(defcustom applescript-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[applescript-guess-indent-offset]' can usually guess a good value when
you're editing someone else's AppleScript code."
  :type 'integer
  :group 'applescript)

(defcustom applescript-continuation-offset 4
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a
Continuation sign terminated line.  Only those continuation lines
for a block opening statement are given this extra offset."
  :type 'integer
  :group 'applescript)

;; Face Setting

;; Face for true, false, me, it
(defvar applescript-pseudo-keyword-face 'applescript-pseudo-keyword-face
  "Face for pseudo keywords in AppleScript mode, like me, true, false.")
(make-face 'applescript-pseudo-keyword-face)

;; Face for command
(defvar applescript-command-face 'applescript-command-face
  "Face for command like copy, get, set, and error.")
(make-face 'applescript-command-face)

(declare-function do-applescript "")

(defun applescript-font-lock-mode-hook ()
  (or (face-differs-from-default-p 'applescript-pseudo-keyword-face)
      (copy-face 'font-lock-keyword-face 'applescript-pseudo-keyword-face))
  (or (face-differs-from-default-p 'applescript-command-face)
      (copy-face 'font-lock-keyword-face 'applescript-command-face)))
(add-hook 'font-lock-mode-hook 'applescript-font-lock-mode-hook)

(defvar applescript-font-lock-keywords
  (let (
        ;; expressions and control Statements
        (kw1 (regexp-opt
              '("and" "app" "application" "considering" "div"
                "else" "end" "exit" "is" "mod" "not" "on" "or"
                "if" "ignoring" "reopen" "repeat"
                "tell" "then" "to"
                "using[ \t]terms[ \t]from"
                "with[ \t]timeout" "with[ \t]transaction")))
        ;; commands
        (kw2 (regexp-opt
              '("ASCII[ \t]character" "ASCII[ \t]number" "activate" "AGStart"
                "beep"  "copy" "count" "choose[ \t]application"
                "choose[ \t]file" "choose[ \t]folder" "close[ \t]access"
                "current[ \t]date" "display[ \t]dialog" "get" "get[ \t]EOF"
                "info[ \t]for" "launch" "list[ \t]disks" "list[ \t]folder"
                "load[ \t]script" "log" "monitor[ \t]depth" "max[ \t]monitor[ \t]depth"
                "min[ \t]monitor[ \t]depth" "new[ \t]file" "offset"
                "open[ \t]for[ \t]access" "path[ \t]to" "random[ \t]number"
                "read" "round" "run" "run[ \t]script" "scripting[ \t]component"
                "set" "set[ \t]EOF" "set[ \t]monitor[ \t]depth" "set[ \t]volume"
                "start[ \t]log" "stop[ \t]log" "store[ \t]script"
                "time[ \t]to[ \t]GMT" "write")))
        ;; misc
        (kw3 (regexp-opt
              '("buttons" "default[ \t]answer" "default[ \t]button"
                "to[ \t]begining[ \t]of" "to[ \t]word" "starting[ \t]at"
                "with[ \t]icon" "write[ \t]permission"))))
    (list
     ;; keywords
     (cons (concat "\\b\\(" kw1 "\\)\\b[ \n\t(]") 1)
     (cons (concat "\\b\\(" kw2 "\\)\\b[ \n\t(]") 1)
     ;; kw3 not yet..
     (cons (concat "\\b\\(" kw3 "\\)\\b[ \n\t(]") 1)
     ;; functions
     '("\\bon[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     '("\\bto[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ;; pseudo-keywords
     '("\\b\\(it\\|me\\|my\\|true\\|false\\)\\b"
       1 applescript-pseudo-keyword-face))))
(put 'applescript-mode 'font-lock-defaults '(applescript-font-lock-keywords))

;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar applescript-mode-abbrev-table nil
  "Abbrev table in use in `applescript-mode' buffers.")
(define-abbrev-table 'applescript-mode-abbrev-table nil)

(defvar applescript-mode-hook nil
  "*Hook called by `applescript-mode'.")

(defvar applescript-mode-map ()
  "Keymap used in `applescript-mode' buffers.")
(if applescript-mode-map
    nil
  (setq applescript-mode-map (make-sparse-keymap))
  ;; Key bindings

  ;; subprocess commands
  (define-key applescript-mode-map "\C-c\C-c" 'applescript-execute-buffer)
  (define-key applescript-mode-map "\C-c\C-s" 'applescript-execute-string)
  (define-key applescript-mode-map "\C-c|" 'applescript-execute-region)

  ;; Miscellaneous
  (define-key applescript-mode-map "\C-c;" 'comment-region)
  (define-key applescript-mode-map "\C-c:" 'uncomment-region))

(defvar applescript-mode-syntax-table nil
  "Syntax table used in `applescript-mode' buffers.")
(when (not applescript-mode-syntax-table)
  (setq applescript-mode-syntax-table (make-syntax-table))

  (modify-syntax-entry ?\" "\"" applescript-mode-syntax-table)

  (modify-syntax-entry ?:  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\; "." applescript-mode-syntax-table)
  (modify-syntax-entry ?&  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\|  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?+  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?/  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?=  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?<  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?>  "." applescript-mode-syntax-table)
  (modify-syntax-entry ?$ "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\[ "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\] "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\{ "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\} "." applescript-mode-syntax-table)
  (modify-syntax-entry ?. "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\\ "." applescript-mode-syntax-table)
  (modify-syntax-entry ?\' "." applescript-mode-syntax-table)

  ;; a double hyphen starts a comment
  (modify-syntax-entry ?-  ". 12" applescript-mode-syntax-table)

  ;; comment delimiters
  (modify-syntax-entry ?\f  ">   " applescript-mode-syntax-table)
  (modify-syntax-entry ?\n  ">   " applescript-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()1" applescript-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" applescript-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23b" applescript-mode-syntax-table))

;; Utilities
(defmacro applescript-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  `(condition-case nil (progn (,@ body)) (error nil)))

(defsubst applescript-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (when (featurep 'xemacs)
    (and (boundp 'zmacs-region-stays)
         (setq zmacs-region-stays t))))

(defsubst applescript-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of on or to
  eod  -- end of on or to
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (applescript-beginning-of-handler 'either))
     ((eq position 'eod) (applescript-end-of-handler 'either))
     ;; Kind of funny, I know, but useful for applescript-up-exception.
     ((eq position 'bob) (goto-char (point-min)))
     ((eq position 'eob) (goto-char (point-max)))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (applescript-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position)))
    (prog1 (point) (goto-char here))))

;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
(defvar applescript-menu nil
  "Menu for AppleScript Mode.
This menu will get created automatically if you have the
`easymenu' package.  Note that the latest X/Emacs releases
contain this package.")

(and (require 'easymenu nil t)
     (easy-menu-define
       applescript-menu applescript-mode-map "AppleScript Mode menu"
       '("AppleScript"
         ["Comment Out Region"   comment-region (mark)]
         ["Uncomment Region"     uncomment-region (mark)]
         "-"
         ["Execute buffer"       applescript-execute-buffer t]
         ["Execute region"       applescript-execute-region (mark)]
         ["Execute string"       applescript-execute-string t]
         "-"
         ["Mode Version"         applescript-mode-version t]
         ["AppleScript Version"   applescript-language-version t])))

;;;###autoload
(define-derived-mode
  applescript-mode
  prog-mode "AppleScript"
  "Major mode for editing AppleScript files."
  (progn
    (message "in custom applescript derived mode")
  ;; set up local variables
  ;; (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  (make-local-variable 'fill-paragraph-function)
  ;;

  ;; Maps and tables
  (use-local-map applescript-mode-map)

  (set-syntax-table applescript-mode-syntax-table)

  ;; Names
  (setq major-mode 'applescript-mode
        mode-name "AppleScript"
        local-abbrev-table applescript-mode-abbrev-table
        font-lock-defaults '(applescript-font-lock-keywords)
        paragraph-separate "[ \t\n\f]*$"
        paragraph-start    "[ \t\n\f]*$"
        require-final-newline t
        comment-start "-- "
        comment-end   ""
        comment-start-skip "---*[ \t]*"
        comment-column 40)

  ;;  Support for outline-minor-mode
  (set (make-local-variable 'outline-regexp)
       "\\([ \t]*\\(on\\|to\\|if\\|repeat\\|tell\\|end\\)\\|--\\)")
  (set (make-local-variable 'outline-level) 'applescript-outline-level)

  ;; Run the mode hook.  Note that applescript-mode-hook is deprecated.
;;  (if applescript-mode-hook
;;      (run-hooks 'applescript-mode-hook)
;;    (run-hooks 'applescript-mode-hook))
   ))

(when (not (or (rassq 'applescript-mode auto-mode-alist)
               (push '("\\.applescript$" . applescript-mode) auto-mode-alist))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(applescript\\|scpt\\)\\'" . applescript-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("osascript" . applescript-mode))

;;; Subprocess commands

;; function use variables
(defconst applescript-output-buffer "*AppleScript Output*"
  "Name for the buffer to display AppleScript output.")
(make-variable-buffer-local 'applescript-output-buffer)

;; Code execution commands
(defun applescript-execute-buffer (&optional async)
  "Execute the whole buffer as an Applescript"
  (interactive "P")
  (applescript-execute-region (point-min) (point-max) async))

(defun applescript-execute-string (string &optional async)
  "Send the argument STRING to an AppleScript."
  (interactive "sExecute AppleScript: ")
  (with-current-buffer (get-buffer-create (generate-new-buffer-name applescript-output-buffer))
    (insert string)
    (applescript-execute-region (point-min) (point-max) async)))

(defun applescript-execute-region (start end &optional async)
  "Execute the region as an Applescript"
  (interactive "r\nP")
  (let ((region (buffer-substring start end))
        (applescript-current-win (selected-window)))
    (pop-to-buffer applescript-output-buffer)
    (insert (applescript-execute-code region))
    (select-window applescript-current-win)))

(defun applescript-execute-code (code)
  "pop to the AppleScript buffer, run the code and display the results."
  (applescript-decode-string
   (do-applescript (applescript-string-to-sjis-string-with-escape code))))

(defun applescript-mode-version ()
  "Echo the current version of `applescript-mode' in the minibuffer."
  (interactive)
  (message "Using `applescript-mode' version %s" applescript-mode-version)
  (applescript-keep-region-active))

(defun applescript-language-version()
  "Echo the current version of AppleScript Version in the minibuffer."
  (interactive)
  (message "Using AppleScript version %s"
           (applescript-execute-code "AppleScript's version"))
  (applescript-keep-region-active))

;; applescript-beginning-of-handler, applescript-end-of-handler,applescript-goto-initial-line not yet
(defun applescript-beginning-of-handler (sym) "Todo.")
(defun applescript-end-of-handler (sym) "Todo.")
(defun applescript-goto-initial-line () "Todo.")

;;  Support for outline.el
(defun applescript-outline-level ()
  "This is so that `current-column` DTRT in otherwise-hidden text"
  (let (buffer-invisibility-spec)
    (save-excursion
      (back-to-indentation)
      (current-column))))

;; for Japanese
(defun applescript-unescape-string (str)
  "delete escape char '\'"
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" str))

(defun applescript-escape-string (str)
  "add escape char '\'"
  (replace-regexp-in-string "\\\\" "\\\\\\\\" str))

(defun applescript-sjis-byte-list-escape (lst)
  (cond
   ((null lst) nil)
   ((= (car lst) 92)
    (append (list 92 (car lst)) (applescript-sjis-byte-list-escape (cdr lst))))
   (t (cons (car lst) (applescript-sjis-byte-list-escape (cdr lst))))))

(defun applescript-string-to-sjis-string-with-escape (str)
  "String convert to SJIS, and escape \"\\\" "
  (apply 'string
         (applescript-sjis-byte-list-escape
          (string-to-list
           (applescript-encode-string str)))))

(defun applescript-decode-string (str)
  "do-applescript return values decode sjis-mac"
  (decode-coding-string str 'sjis-mac))

(defun applescript-encode-string (str)
  "do-applescript return values encode sjis-mac"
  (encode-coding-string str 'sjis-mac))

(defun applescript-parse-result (retstr)
  "String convert to Emacs Lisp Object"
  (cond
   ;; as list
   ((string-match "\\`\\s-*{\\(.*\\)}\\s-*\\'" retstr)
    (let ((mstr (match-string 1 retstr)))
      (mapcar (lambda (item) (applescript-parse-result item))
              (split-string mstr ","))))

   ;; AERecord item as cons
   ((string-match "\\`\\s-*\\([^:\\\"]+\\):\\(.*\\)\\s-*\\'" retstr)
    (cons (intern (match-string 1 retstr))
          (applescript-parse-result (match-string 2 retstr))))

   ;; as string
   ((string-match "\\`\\s-*\\\"\\(.*\\)\\\"\\s-*\\'" retstr)
    (applescript-decode-string
     (applescript-unescape-string (match-string 1 retstr))))

   ;; as integer
   ((string-match "\\`\\s-*\\([0-9]+\\)\\s-*\\'" retstr)
    (string-to-number (match-string 1 retstr)))

   ;; else
   (t (intern retstr))))

(provide 'applescript-mode)
;;; applescript-mode.el ends here
