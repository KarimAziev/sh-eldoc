;;; sh-eldoc.el --- Extra eldoc functions for sh -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/sh-eldoc
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extra eldoc functions for sh

;;; Code:


(defcustom sh-eldoc-flags-functions '(sh-eldoc-describe-if-statement)
  "List of functions to add with `elisp-eldoc-flags-add-eldoc-functions'."
  :group 'autofix
  :type '(repeat
          (radio
           (function-item :tag "Describe if statement"
                          sh-eldoc-describe-if-statement)
           (function :tag "Custom function"))))

(defun sh-eldoc-get-sexp ()
  "Inside or on vector return highlight position and vector."
  (save-excursion
    (or (when-let* ((sexp (sexp-at-point))
                    (vect (when (vectorp sexp)
                            sexp)))
          (cons 0 vect))
        (when-let* ((synt (nth 1 (syntax-ppss (point))))
                    (sexp (save-excursion
                            (goto-char synt)
                            (when-let ((sexp (sexp-at-point)))
                              (when (vectorp sexp)
                                sexp))))
                    (pos (point)))
          (goto-char synt)
          (forward-char 1)
          (let ((sexp-count 0)
                (own-pos)
                (bounds))
            (with-syntax-table emacs-lisp-mode-syntax-table
              (while
                  (setq bounds (ignore-errors
                                 (skip-chars-forward "\s\t")
                                 (forward-sexp)
                                 (prog1 (bounds-of-thing-at-point 'sexp)
                                   (skip-chars-forward "\s\t"))))
                (setq sexp-count (1+ sexp-count))
                (when (and (>= pos (car bounds))
                           (>= (cdr bounds) pos))
                  (setq own-pos (1- sexp-count)))))
            (cons own-pos sexp))))))

(defun sh-eldoc-describe-if-statement (&optional _callback &rest _ignored)
  "Show description for arguments inside if statement."
  (when-let*
      ((found (sh-eldoc-get-sexp))
       (pos (or (car found) 0))
       (vect (cdr found))
       (args (append vect nil))
       (descr-cell
        (pcase (car args)
          ('-a (cons [-a FILE] "True if FILE exists"))
          ('-b '([-b FILE] .
                 "True if FILE exists and is a block-special file"))
          ('-c
           '([-c FILE] .
             "True if FILE exists and is a character-special file"))
          ('-d
           '([-d FILE] . "True if FILE exists and is a directory"))
          ('-e '([-e FILE] . "True if FILE exists"))
          ('-f '([-f FILE] .
                 "True if FILE exists and is a regular file"))
          ('-g
           '([-g FILE] .
             "True if FILE exists and its SGID bit is set"))
          ('-h
           '([-h FILE] . "True if FILE exists and is a symbolic link"))
          ('-k
           '([-k FILE] .
             "True if FILE exists and its sticky bit is set"))
          ('-p
           '([-p FILE] .
             "True if FILE exists and is a named pipe (FIFO)"))
          ('-r '([-r FILE] . "True if FILE exists and is readable"))
          ('-s
           '([-s FILE] .
             "True if FILE exists and has a size greater than zero"))
          ('-t
           '([-t FD] .
             "True if file descriptor FD is open and refers to a terminal"))
          ('-u
           '([-u FILE] .
             "True if FILE exists and its SUID (set user ID) bit is set"))
          ('-w '([-w FILE] . "True if FILE exists and is writable"))
          ('-x '([-x FILE] . "True if FILE exists and is executable"))
          ('-O
           '([-O FILE] .
             "True if FILE exists and is owned by the effective user ID"))
          ('-G
           '([-G FILE] .
             "True if FILE exists and is owned by the effective group ID"))
          ('-L
           '([-L FILE] . "True if FILE exists and is a symbolic link"))
          ('-N
           '([-N FILE] .
             "True if FILE exists and has been modified since it was last read"))
          ('-S '([-S FILE] . "True if FILE exists and is a socket"))
          ('-o
           '([-o OPTIONNAME] .
             "True if shell option \"OPTIONNAME\" is enabled"))
          ('-z
           '([-z STRING] . "True of the length if \"STRING\" is zero"))
          ('-n
           '([-n STRING] .
             "or [ STRING ]	True if the length of STRING is non-zero"))
          (_
           (pcase (nth 1 args)
             ('-nt
              '([FILE1 -nt FILE2] .
                "True if FILE1 has been changed more recently than FILE2, or if FILE1 exists and FILE2 does not"))
             ('-ot
              '([FILE1 -ot FILE2] .
                "True if FILE1 is older than FILE2, or is FILE2 exists and FILE1 does not."))
             ('-ef
              '([FILE1 -ef FILE2]	.
                "True if FILE1 and FILE2 refer to the same device and inode numbers."))
             ('=
              '([STRING1 = STRING2]	.
                "True if the strings are equal. = may be used instead of == for strict POSIX compliance."))
             ('== '([STRING1 == STRING2]	. "True if the strings are equal"))
             ('>
              '([STRING1 > STRING2]	.
                "True if STRING1 sorts after STRING2 lexicographically in the current locale"))
             ('<
              '([STRING1 > STRING2]	.
                "True if STRING1 sorts after STRING2 lexicographically in the current locale")))))))
    (let* ((thing (when (car descr-cell)
                    (prin1-to-string (nth pos (append (car descr-cell) nil)))))
           (label (prin1-to-string (car descr-cell)))
           (descr (concat label " " (cdr descr-cell))))
      (when thing
        (setq descr (replace-regexp-in-string
                     (regexp-opt (list thing) 'symbols)
                     (propertize thing 'face
                                 'eldoc-highlight-function-argument)
                     descr)))
      (when descr
        (message descr)))))


;;;###autoload
(defun sh-eldoc-setup ()
  "Add more `eldoc-documentation-functions'."
  (interactive)
  (dolist (fn sh-eldoc-flags-functions)
    (add-hook 'eldoc-documentation-functions
              fn
              nil t)))


;;;###autoload
(defun sh-eldoc-unsetup ()
  "Remove `eldoc-documentation-functions', added by `sh-eldoc-setup'."
  (interactive)
  (dolist (fn sh-eldoc-flags-functions)
    (remove-hook 'eldoc-documentation-functions
                 fn
                 t)))

(provide 'sh-eldoc)
;;; sh-eldoc.el ends here