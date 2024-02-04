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

(defun lisp-file-header--read-atmosphere ()
  (let ((done nil))
    (while (not done)
      (if (or (looking-at "[ \n\r\t]+")
              (looking-at ";.*$"))
          (goto-char (match-end 0))
        (setq done t)))))

(defun lisp-file-header--read-string ()
  (let ((result ""))
    (while (not (looking-at "\""))
      (when (looking-at "$")
        (signal 'invalid-read-syntax '("Multi-line string")))
      (when (looking-at "[^\"\\\n\r]+")
        (setq result (concat result (match-string-no-properties 0)))
        (goto-char (match-end 0)))
      (when (looking-at "\\\\\\(.\\)")
        (setq result (concat result (match-string-no-properties 1)))
        (goto-char (match-end 0))))
    (goto-char (match-end 0))
    result))

(defun lisp-file-header--read-list ()
  (let ((result '())
        (done nil))
    (while (not done)
      (lisp-file-header--read-atmosphere)
      (cond ((looking-at ")")
             (goto-char (match-end 0))
             (setq done t))
            (t
             (setq result (cons (lisp-file-header--read-datum)
                                result)))))
    (reverse result)))

(defun lisp-file-header--read-datum ()
  (cond ((looking-at "\"")
         (goto-char (match-end 0))
         (lisp-file-header--read-string))
        ((looking-at "(")
         (goto-char (match-end 0))
         (lisp-file-header--read-list))
        ((looking-at ")")
         (signal 'invalid-read-syntax '(")")))
        ((looking-at "[A-Za-z0-9*+-]+")
         (let ((object (car (read-from-string
                             (match-string-no-properties 0)))))
           (goto-char (match-end 0))
           object))
        ((looking-at ".")
         (signal 'invalid-read-syntax
                 (list (match-string-no-properties 0))))
        (t
         (signal 'end-of-file '()))))

(defun lisp-file-header--read ()
  (let ((rounds 5)
        (result nil))
    (while (> rounds 0)
      (lisp-file-header--read-atmosphere)
      (let ((datum (condition-case _ (lisp-file-header--read-datum)
                     ((invalid-read-syntax end-of-file)
                      (setq rounds 0)
                      nil))))
        (cond ((and (consp datum)
                    (eql 'file-header (car datum)))
               (setq result datum)
               (setq rounds 0))
              (t
               (setq rounds (1- rounds))))))
    result))

(defun lisp-file-header-read ()
  "Read the `file-header' form in the current buffer.

Reads the current buffer using a lenient form of S-expression
syntax. If a (file-header ...) form is found near the top,
returns that form as an Emacs Lisp object. If a `file-header'
form is not found or cannot be read, nil is returned."
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (search-forward "file-header" 5000 t)
          (goto-char (point-min))
          (lisp-file-header--read))))))

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
