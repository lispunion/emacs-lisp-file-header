;;; sexp-file-header.el --- Parse the `file-header' form in Lisp languages -*- lexical-binding: t -*-

;; Copyright 2020, 2023 Lassi Kortela
;; SPDX-License-Identifier: ISC

;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lispunion/emacs-sexp-file-header
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0.1.0
;; Keywords: languages lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Parse the `file-header' form in Lisp languages.

;;; Code:

(require 'lisp-local)

(defun sexp-file-header-parse ()
  "Parse the `file-header' form in the current buffer.

Reads the current buffer using a lenient form of S-expression
syntax. If a (file-header ...) form is found near the top,
returns that form as an Emacs Lisp object. If a `file-header'
form is not found cannot be parsed, nil is returned."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (form (eof (gensym "eof-")))
        (while (not (or (eq eof form)
                        (and (consp form) (eql 'file-header (car form)))))
          (setq form (condition-case _ (read (current-buffer))
                       (end-of-file eof)
                       (invalid-read-syntax eof))))
        (and (consp form)
             (eql 'file-header (car form))
             form)))))

(defun sexp-file-header-buffer-p ()
  "Return non-nil if the current buffer has a (file-header ...) form."
  (not (null (sexp-file-header-parse))))

(defvar sexp-file-header--languages
  '((clojure clojure-mode)
    (clojurescript clojurescript-mode)
    (common-lisp lisp-mode)
    (emacs-lisp emacs-lisp-mode)
    (newlisp newlisp-mode)
    (racket racket-mode)
    (scheme scheme-mode)))

(defun sexp-file-header--apply-language (body)
  "Apply the `language' section of (file-header BODY ...)."
  (dolist (language-name (cdr (assoc 'language body)))
    (dolist (entry sexp-file-header--languages)
      (when (equal language-name (elt entry 0))
        (let ((function-name (elt entry 1)))
          (when (fboundp function-name)
            (funcall function-name)))))))

(defun sexp-file-header--apply-indent (body)
  "Apply the `indent' section of (file-header BODY ...)."
  (let ((indent (cdr (assoc 'indent body))))
    (dolist (indent-form indent)
      (when (and (listp indent-form)
                 (= 2 (length indent-form))
                 (symbolp (car indent-form))
                 (integerp (cadr indent-form))
                 (>= (cadr indent-form) 0))
        (lisp-local-set-indent (car indent-form) (cadr indent-form))))))

(defun sexp-file-header--apply (body)
  "Apply (file-header BODY ...)."
  (sexp-file-header--apply-language body)
  (sexp-file-header--apply-indent body))

(defun sexp-file-header-apply ()
  "Apply `file-header' from current buffer."
  (let ((body (cdr (sexp-file-header-parse))))
    (sexp-file-header--apply body)
    (not (null body))))

(add-to-list 'magic-mode-alist
             (cons 'sexp-file-header-buffer-p
                   'sexp-file-header-apply))

(provide 'sexp-file-header)

;;; sexp-file-header.el ends here
