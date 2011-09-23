;;; epic.el -- Evernote Picker

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2011-06-23
;; Revised: 2011-06-24

;;; Commentay:

;; Epic is a small elisp to access Evernote process via AppleScript.
;; After load this file, you can:
;;
;;   + drag notes in Evernote to an org-mode buffer.
;;   + use M-x ``epic-anything'' for completion of Tag and Notebook.
;;   + use M-x ``epic-insert-selected-note-as-org-links''
;;     for insertion of org-style links.
;;   + use M-x ``epic-create-note-from-region''
;;     for creation of a new note in your local Evernote.
;; 
;; Note for setup:
;;   Since the current Evernote (2.2) does not have any interface to
;;   acquire note-links in the form of ``evernote://...'',
;;   Epic sends Control-L to make Evernote put the links to clipboard.
;;   So, you have to bind Control-L to ``copy note link'' within Evernote.
;;   Please set up your Mac referring to:
;;    http://docs.info.apple.com/article.html?path=Mac/10.5/en/8564.html
;;
;; The updated version might be available from:
;;   http://github.com/yoshinari-nomura/epic

;;; Code:

(require 'htmlize)

;;
;; Get info from Evernote
;;

(defvar epic-cache-notebooks nil)
(defvar epic-cache-tags      nil)

(defun epic-notebooks ()
  "Get name list of notebooks in Evernote DB"
  (or epic-cache-notebooks
      (setq epic-cache-notebooks
            (epic/get-name-list "notebooks"))))

(defun epic-tags ()
  "Get name list of tags in Evernote DB"
  (or epic-cache-tags
      (setq epic-cache-tags
            (epic/get-name-list "tags"))))

(defun epic/get-name-list (obj-name)
  "Get name list of tags or notebooks in Evernote DB. OBJ-NAME must be ``tags'' or ``notebook''"
  (split-string
   (substring
    (do-applescript (format "
      tell application \"Evernote\"
        set retval to \"\"
        set aList to %s
        repeat with x in aList
          set retval to (retval & (name of x) & \"\n\")
        end repeat
      end tell
      " obj-name)) 0 -1)
   "\n"))

(defun epic-selected-note-titles ()
  (sit-for 0.1) ;; required in case called as DnD-callbacks.
  (split-string
   (substring
    (do-applescript "
      tell application \"Evernote\"
        set noteList  to selection
        set noteTitle to \"\"
        repeat with n in noteList
          set noteTitle to (noteTitle & (title of n) & \"\n\")
        end repeat
      end tell
      ") 0 -1)
   "\n"))

(defun epic-selected-note-uris ()
  (do-applescript "
    tell application \"System Events\"
      set frontApp to name of first application process whose frontmost is true
      tell process \"Evernote\"
        activate
        set frontmost to true
        delay 0.1
        keystroke \"l\" using {control down}
      end tell
    end tell
    tell application frontApp
      activate
    end tell
    ")
  (split-string (ns-get-pasteboard)))

(defun epic-selected-note-list ()
  "Get selected notes as a list of (uri . title) cons cell.
like: (("title1" . "evernote:///.....") ("title2" . "evernote:///..."))"
  (let ((uris   (epic-selected-note-uris))
        (titles (epic-selected-note-titles))
        (result '()))
    (while (and (car uris) (car titles))
      (setq result (cons (cons (car uris) (car titles)) result))
      (setq uris   (cdr uris))
      (setq titles (cdr titles)))
    result))

;;
;; Completing read for Tags, Notebooks
;;

(defun epic-read-notebook (&optional default)
  (interactive)
  (epic/completing-read "Notebook: " (epic-notebooks)
                        'epic-notebook-history (or default "")))

(defun epic-read-tag (&optional default)
  (interactive)
  (epic/completing-read "Tag: " (epic-tags) 'epic-tag-history (or default "")))

(defun epic-read-tag-list ()
  (interactive)
  (let (tag (tag-list '()))
    (while (not (string= "" 
                         (setq
                          tag
                          (epic/completing-read
                           "Add tag (finish to blank): " (epic-tags) 'epic-tag-history ""))))
      (setq tag-list (cons tag tag-list))
      (setq tag ""))
    (nreverse tag-list)))
      
(defun epic-read-title (&optional default)
  (interactive)
  (read-string "Title: " default 'epic-title-history default))

(defun epic/completing-read (prompt collection hist &optional default)
  (let ((anything-use-migemo t))
    (completing-read prompt collection nil 'force
                     nil hist default)))

;;
;; Misc
;;

(defun epic/as-quote (obj)
  "Make AppleScript literals (List and String) from lisp OBJ."
  (cond
   ((stringp obj)
    (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" obj)))
   ((listp obj)
    (concat "{" (mapconcat 'epic/as-quote obj ", ") "}"))
   ))

(defun epic/as-option (opt-name opt-value)
  "Make AppleScript optional OPT-NAME phrase if OPT-VALUE is not blank."
  (if (or (null opt-value)
          (and (stringp opt-value)
               (string= opt-value "")))
      ""
    (format "%s %s" opt-name (epic/as-quote opt-value))))

;;
;; Create note
;;

(defun epic-create-note-from-region (beg end title notebook tags)
  (interactive 
   (list (region-beginning) (region-end)
         (epic-read-title)
         (epic-read-notebook)
         (epic-read-tag-list)))
  (let* ((htmlize-output-type 'font)
         (htmlbuf   (htmlize-region beg end))
         (temp-file (make-temp-file "epic" nil ".html")))
    (unwind-protect
	(with-current-buffer htmlbuf
          (write-region nil nil temp-file nil 'silent)
          (epic-create-note-from-file temp-file title notebook tags))
      (kill-buffer htmlbuf)
      (delete-file temp-file)
      )))

(defun epic-create-note-from-file (file-name title &optional notebook tags)
  (do-applescript (format "
    tell application \"Evernote\"
      set aNote to (create note from file %s title %s %s %s)
      open note window with aNote
      activate
    end tell
    " (epic/as-quote file-name)
    (epic/as-quote title)
    (epic/as-option "notebook" notebook)
    (epic/as-option "tags" tags))))

;;
;; Org-mode support
;;

(defun epic/zipup-to-org-links (uris titles)
  (let ((result ""))
    (while (and (car uris) (car titles))
      (setq result
            (concat result (format "[[%s][%s]]\n" (car uris) (car titles))))
      (setq uris   (cdr uris))
      (setq titles (cdr titles)))
    result))

(defun epic-insert-selected-note-as-org-links ()
  "Insert org-style links to the selected notes in Evernote."
  (interactive)
  (insert (epic/zipup-to-org-links
           (epic-selected-note-uris)
           (epic-selected-note-titles))))

;;
;; By typing C-cC-o (org-open-at-point) on an org-link,
;; you can open a corresponding note in your desktop Evernote app.
;; 
(defun epic-org-evernote-open (path)
  (browse-url (concat "evernote:" path)))
(org-add-link-type "evernote" 'epic-org-evernote-open)

;;
;; Evernote + OrgMode -- DnD settings
;;

(define-key global-map [ns-drag-text] 'epic-ns-insert-text)

(defun epic-ns-insert-text ()
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (string-match "^evernote:" ns-input-text))
      (insert (epic/zipup-to-org-links
               (split-string ns-input-text " ")
               (epic-selected-note-titles)))
    (dnd-insert-text (get-buffer-window) 'copy ns-input-text))
  (setq ns-input-text nil))

;;
;; Anything support
;;

(setq anything-c-source-evernote-tags
  '((name . "Evernote Tags")
    (candidates . epic-tags)
    (migemo)
    (action . (lambda (candidate) (insert "#" candidate) candidate))
    ))

(setq anything-c-source-evernote-notebooks
  '((name . "Evernote Notebooks")
    (candidates . epic-notebooks)
    (migemo)
    (action . (lambda (candidate) (insert "@" candidate) candidate))
    ))

(defun epic-anything-tags ()
  (interactive)
  (anything '(anything-c-source-evernote-tags)))

(defun epic-anything-notebooks ()
  (interactive)
  (anything '(anything-c-source-evernote-notebooks)))

(defun epic-anything ()
  (interactive)
  (anything '(
     anything-c-source-evernote-tags
     anything-c-source-evernote-notebooks
     )))

;;; Copyright Notice:

;; Copyright (C) 2011 Yoshinari Nomura.
;; All rights reserved.

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

;;; epic.el ends here
