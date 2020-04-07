;;; declare-file.el --- Parse the `declare-file' form in Lisp languages -*- lexical-binding: t -*-
;;
;; Copyright 2020 Lassi Kortela
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lispunion/emacs-declare-file
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Package-Version: 0.1.0
;; Keywords: languages lisp
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Parse the `declare-file' form in Lisp languages.
;;
;;; Code:

(defun declare-file-parse ()
  "Parse the `declare-file' form in the current buffer.

Reads the current buffer using a lenient form of S-expression
syntax. If a (declare-file ...) form is found near the top,
returns that form as an Emacs Lisp object. If a `declare-file'
form is not found cannot be parsed, nil is returned."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (form (eof (gensym "eof-")))
        (while (not (or (eq eof form)
                        (and (consp form) (eql 'declare-file (car form)))))
          (setq form (condition-case _ (read (current-buffer))
                       (end-of-file eof)
                       (invalid-read-syntax eof))))
        (and (consp form)
             (eql 'declare-file (car form))
             form)))))

(defun declare-file-buffer-p ()
  (not (null (declare-file-parse))))

(defun declare-file-activate ()
  (let ((body (cdr (declare-file-parse))))
    (dolist (lang (cdr (assoc 'language body)))
      (cond ((equal lang 'clojure) (clojure-mode))
            ((equal lang 'clojurescript) (clojurescript-mode))
            ((equal lang 'common-lisp) (lisp-mode))
            ((equal lang 'emacs-lisp) (emacs-lisp-mode))
            ((equal lang 'newlisp) (newlisp-mode))
            ((equal lang 'racket) (racket-mode))
            ((equal lang 'scheme) (scheme-mode))))))

(setq magic-mode-alist
      (cons (cons 'declare-file-buffer-p 'declare-file-activate)
            (assoc-delete-all 'declare-file-buffer-p magic-mode-alist)))

(provide 'declare-file)

;;; declare-file.el ends here
