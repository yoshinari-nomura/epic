;;; epic-helm.el --- Sample Helm settings for Epic

;; Copyright (C) 2011-2015 Yoshinari Nomura.
;; All rights reserved.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2011-06-23
;; Keywords: evernote, applescript, epic

;;; Commentary:

;; Sample helm settings for Epic.

;;; Code:

(require 'epic)

(defvar helm-c-source-evernote-tags
  '((name . "Evernote Tags")
    (candidates . epic-tags)
    (migemo)
    (action
     ("Insert Tag Name" .
      (lambda (candidate) (insert "#" candidate) candidate)))
    ))

(defvar helm-c-source-evernote-notebooks
  '((name . "Evernote Notebooks")
    (candidates . epic-notebooks)
    (migemo)
    (action
     ("Pop To Notebook in Evernote" .
      (lambda (candidate)
        (epic-open-notebook-in-collection-window candidate)))
     ("Insert Notebook Name" .
      (lambda (candidate)
        (insert "@" candidate) candidate)))
    ))

(defvar helm-c-source-evernote-notebooks-in-stack
  '((name . "Evernote Notebooks In Stack")
    (candidates . epic-find-notebook-titles-in-stack)
    (migemo)
    (action
     ("Pop To Notebook in Evernote" .
      (lambda (candidate)
        (epic-open-notebook-in-collection-window candidate)))
     ("Insert Notebook Name" .
      (lambda (candidate)
        (insert "@" candidate) candidate)))
    ))


(declare-function helm "helm")

;;;###autoload
(defun epic-helm ()
  "Insert or jump using helm.el package.
Insert the name of selected tag of Evernote with the prefix of `#'.
Insert the name of selected notebook of Evernote with the prefix of `@'.
Pop to Evernote App and open the selected notebook."
  (interactive)
  (helm
   '(
     helm-c-source-evernote-tags
     helm-c-source-evernote-notebooks-in-stack
     helm-c-source-evernote-notebooks
     )))

(provide 'epic-helm-config)

;;; Copyright Notice:

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; epic-helm-config.el ends here
