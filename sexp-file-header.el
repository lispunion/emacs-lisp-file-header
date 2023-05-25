;;; sexp-file-header.el --- Parse the `file-header' form in Lisp languages -*- lexical-binding: t -*-

;; Copyright 2020, 2023 Lassi Kortela
;; SPDX-License-Identifier: ISC

;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lispunion/emacs-sexp-file-header
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Package-Version: 0.1.0
;; Keywords: languages lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Parse the `file-header' form in Lisp languages.

;;; Code:

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
  (not (null (sexp-file-header-parse))))

(defun sexp-file-header--apply-language (body)
  (dolist (lang (cdr (assoc 'language body)))
    (cond ((equal lang 'clojure) (clojure-mode))
          ((equal lang 'clojurescript) (clojurescript-mode))
          ((equal lang 'common-lisp) (lisp-mode))
          ((equal lang 'emacs-lisp) (emacs-lisp-mode))
          ((equal lang 'newlisp) (newlisp-mode))
          ((equal lang 'racket) (racket-mode))
          ((equal lang 'scheme) (scheme-mode)))))

(defun sexp-file-header-apply ()
  (let ((body (cdr (sexp-file-header-parse))))
    (sexp-file-header--apply-language body)
    (not (null body))))

(setq magic-mode-alist
      (cons (cons 'sexp-file-header-buffer-p
                  'sexp-file-header-apply)
            (assoc-delete-all 'sexp-file-header-buffer-p
                              magic-mode-alist)))

(provide 'sexp-file-header)

;;; sexp-file-header.el ends here
