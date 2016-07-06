;;; sth.el --- STH code editing commands for Emacs

;; Add code like the following to your .emacs to install:
;; (autoload 'sth-mode "...path.to.wherever.you.put.this.file.../sth.el" nil t)
;; (setq auto-mode-alist (cons '("\\.sth\\'" . sth-mode)
;;                             auto-mode-alist))

;; Copyright (C) 1988,94,96,2000  Free Software Foundation, Inc.
;; Copyright (C) 2003, 2005, 2011, 2016 Tony Garnock-Jones <tonyg@lshift.net>

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'comint)
(require 'font-lock)
(require 'rx)
(require 'cl-lib)

;;---------------------------------------------------------------------------
;; State

(defvar sth-buffer)

;;---------------------------------------------------------------------------
;; Customization

(defcustom sth-program-name "./sth-main.rkt"
  "*Program invoked by the `run-sth' command."
  :type 'string
  :group 'sth)

(defcustom inferior-sth-mode-hook nil
  "*Hook for customising inferior-sth mode."
  :type 'hook
  :group 'sth)

;;---------------------------------------------------------------------------
;; REPL

(define-derived-mode inferior-sth-mode comint-mode "Inferior Something"
  "Major mode for interacting with an inferior Something process."
  ;; Customise in inferior-sth-mode-hook
  (setq comint-prompt-regexp "^\"[^\"\n]*\" *")
  (sth-mode-variables)
  (setq mode-line-process '(":%s")))

;;;###autoload
(defun run-sth (cmd)
  "Run an inferior Something process, input and output via buffer *sth*.
If there is a process already running in `*sth*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `sth-program-name').  Runs the hooks `inferior-sth-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run Something: " sth-program-name)
		       sth-program-name)))
  (if (not (comint-check-proc "*sth*"))
      (let ((cmdlist (scheme-args-to-list cmd)))
	(set-buffer (apply 'make-comint "sth" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-sth-mode)))
  (setq sth-program-name cmd)
  (setq sth-buffer "*sth*")
  (pop-to-buffer "*sth*"))

;;---------------------------------------------------------------------------
;; Font-lock, Indentation, and Mode

(defvar sth-mode-syntax-table (make-syntax-table)
  "Syntax table in use in sth-mode buffers.")

(modify-syntax-entry ?' "\"" sth-mode-syntax-table)
(modify-syntax-entry ?_ "_" sth-mode-syntax-table)
(modify-syntax-entry ?\n "> b" sth-mode-syntax-table)
(modify-syntax-entry ?\r "> b" sth-mode-syntax-table)
(modify-syntax-entry ?/ ". 12b" sth-mode-syntax-table)
(mapcar #'(lambda (x) (modify-syntax-entry x "w" sth-mode-syntax-table))
        '(?- ?_ ?$ ?! ?? ?* ?+))

(defvar sth-font-lock-keywords
  (list
   ;; '("%assemble\\>" . font-lock-warning-face)

   '("\\<[A-Z][^[:space:].\n]*\\>" . font-lock-type-face)

   '("\\(\\<def\\(-[^[:space:].\n]*\\>\\)?\\)\\s-*\\(\\<[^[:space:].\n]*\\>\\)"
     (1 font-lock-keyword-face)
     (3 font-lock-function-name-face))

   '("\\<def\\(-[^[:space:].\n]*\\>\\)" . font-lock-keyword-face)

   '("\\(:\\<[^[:space:].\n]*\\>\\)" . font-lock-constant-face)

   ;; '("'\\([^\\]\\|\\\\.\\)*?'" . nil)

   (cons (regexp-opt
          '(
            "float"
            "double"
            "word32"
            "word64"
            "integer"
            "boolean"
            "text"
            "set"
            "list"
            "dict"
            "not"
            "ref"
            )
          'words)
         'font-lock-builtin-face)

   (regexp-opt
    '(
      "import"
      "def"
      "method"
      "self"
      "let"
      "let*"
      "letrec"
      "binary"
      "cond"
      "when"
      "else"
      "match"
      "raise"
      "catch"
      "throw"
      "return"
      )
    'words)
   ))

(defun sth-mode-variables ()
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "//")
  (setq comment-end "")
  (setq comment-start-skip "// *")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sth-font-lock-keywords nil nil ())))

;;;###autoload
(define-derived-mode sth-mode prog-mode "STH"
  "Major mode for editing STH code."
  (sth-mode-variables))

;;---------------------------------------------------------------------------
;; Provides

(provide 'sth-mode)

;;; sth.el ends here
