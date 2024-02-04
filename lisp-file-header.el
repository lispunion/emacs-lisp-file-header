;;; lisp-file-header.el --- Read the `file-header' form in Lisp languages -*- lexical-binding: t -*-

;; Copyright 2020, 2023, 2024 Lassi Kortela
;; SPDX-License-Identifier: ISC

;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lispunion/emacs-lisp-file-header
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0.1.0
;; Keywords: languages lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Read the `file-header' form in Lisp languages.

;;; Code:

(require 'lisp-local)

(defun lisp-file-header-read ()
  "Read the `file-header' form in the current buffer.

Reads the current buffer using a lenient form of S-expression
syntax. If a (file-header ...) form is found near the top,
returns that form as an Emacs Lisp object. If a `file-header'
form is not found or cannot be read, nil is returned."
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

(defun lisp-file-header-buffer-p ()
  "Return non-nil if the current buffer has a (file-header ...) form."
  (not (null (lisp-file-header-read))))

(defvar lisp-file-header--languages
  '((clojure clojure-mode)
    (clojurescript clojurescript-mode)
    (common-lisp lisp-mode)
    (emacs-lisp emacs-lisp-mode)
    (newlisp newlisp-mode)
    (racket racket-mode)
    (scheme scheme-mode)))

(defun lisp-file-header--apply-language (body)
  "Apply the `language' section of (file-header BODY ...)."
  (dolist (language-name (cdr (assoc 'language body)))
    (dolist (entry lisp-file-header--languages)
      (when (equal language-name (elt entry 0))
        (let ((function-name (elt entry 1)))
          (when (fboundp function-name)
            (funcall function-name)))))))

(defun lisp-file-header--apply-indent (body)
  "Apply the `indent' section of (file-header BODY ...)."
  (let ((indent (cdr (assoc 'indent body))))
    (dolist (indent-form indent)
      (when (and (listp indent-form)
                 (= 2 (length indent-form))
                 (symbolp (car indent-form))
                 (integerp (cadr indent-form))
                 (>= (cadr indent-form) 0))
        (lisp-local-set-indent (car indent-form) (cadr indent-form))))))

(defun lisp-file-header--apply (body)
  "Apply (file-header BODY ...)."
  (lisp-file-header--apply-language body)
  (lisp-file-header--apply-indent body))

(defun lisp-file-header-apply ()
  "Apply `file-header' from current buffer."
  (let ((body (cdr (lisp-file-header-read))))
    (lisp-file-header--apply body)
    (not (null body))))

(add-to-list 'magic-mode-alist
             (cons 'lisp-file-header-buffer-p
                   'lisp-file-header-apply))

(provide 'lisp-file-header)

;;; lisp-file-header.el ends here
