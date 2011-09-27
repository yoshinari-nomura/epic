;;; epic.el -- Evernote Picker

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2011-06-23
;; Revised: 2011-06-24

;;; Commentay:

;; * What is Epic ? 
;;
;;   Epic is a small elisp to access Evernote process via AppleScript.
;;   The updated version might be available from:
;;     http://github.com/yoshinari-nomura/epic
;;
;;   Epic has these functions:
;;
;;   - For org-mode users:
;;     + Org-mode becomes to recognize evernote:// links.
;;     + You can drag notes in Evernote app to an org-mode buffer.
;;     + ``epic-insert-selected-note-as-org-links''
;;        for insertion of org-style links.
;;
;;   - Completing read for tags and notebooks:
;;     + ``epic-anything'' :: insert tag and notebook using anything.el package.
;;     + ``epic-read-notebook'', ``epic-read-tag'', `epic-read-tag-list'' :: 
;;       for the completion of tags and notebooks.
;;
;;   - Creation of note articles:
;;     + ``epic-create-note-from-region'', ``epic-create-note-from-file'' ::
;;       for creation of a new note in your local Evernote.
;;
;;   - For Mew users:
;;     + ``epic-mew-forward-to-evernote'' :: 
;;        Nifty mail forwarder.
;;        You need to set the vars: ``epic-evernote-mail-address'',
;;        ``epic-evernote-mail-headers''
;;     + ``epic-mew-create-note'' :: 
;;        Import a mail article into the local Evernote app.
;;
;; * Setting Example
;;
;;   : (require 'epic)
;;   : (define-key global-map [(control ?:)] 'epic-anything)
;;   : (define-key mew-summary-mode-map "r" 'epic-mew-create-note)
;;   : (define-key mew-summary-mode-map "e" 'epic-mew-forward-to-evernote)
;;   : (setq epic-evernote-mail-address "??????@???.evernote.com")
;;
;; * Note for setup
;;
;;   Since the current Evernote (2.2) does not have any interface to
;;   acquire note-links in the form of ``evernote://...'',
;;   Epic sends Control-L to make Evernote put the links to clipboard.
;;   So, you have to bind Control-L to ``copy note link'' within Evernote.
;;   Please set up your Mac referring to:
;;     http://docs.info.apple.com/article.html?path=Mac/10.5/en/8564.html

;;; Code:

(require 'htmlize)

;;
;; Get info from Evernote
;;

;; XXX: some cache control to be added.
(defvar epic-cache-notebooks nil)
(defvar epic-cache-tags      nil)

(defun epic-notebooks ()
  "Return the name list of notebooks in Evernote."
  (or epic-cache-notebooks
      (setq epic-cache-notebooks
            (epic/get-name-list "notebooks"))))

(defun epic-tags ()
  "Return the name list of tags in Evernote."
  (or epic-cache-tags
      (setq epic-cache-tags
            (epic/get-name-list "tags"))))

(defun epic/get-name-list (obj-name)
  "Return the name list of tags or notebooks in Evernote.
 OBJ-NAME must be ``tags'' or ``notebook''"
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
  "Return the titles of selected notes in Evernote."
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
  "Return the URI list of the selected notes in Evernote.
 URIs are in the form of evernote://...
 Evernote seems to add URIs to their notes on syncing with its cloud server.
 Therefore, this function does not work with a note which is not never
 synced before."
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
  "Return selected notes as a list of (uri . title) cons cell
 like: (("title1" . "evernote:///.....") ("title2" . "evernote:///..."))."
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
  "Completing read for notebooks of Evernote.
 This is supposed to work better with anything.el package."
  (interactive)
  (epic/completing-read "Notebook: " (epic-notebooks)
                        'epic-notebook-history (or default "")))

(defun epic-read-tag (&optional default)
  "Completing read for tags of Evernote.
 This is supposed to work better with anything.el package."
  (interactive)
  (epic/completing-read "Tag: " (epic-tags) 'epic-tag-history (or default "")))

(defun epic-read-tag-list ()
  "Completing read for tags of Evernote.
 This repeats ``epic-read-tag'' until the input is blank, and returns
 the tags in list-form."
  (interactive)
  (let (tag (tag-list '()))
    (while (not (string= "" (setq tag
                                  (epic/completing-read
                                   "Add tag (finish to blank): "
                                   (epic-tags)
                                   'epic-tag-history ""))))
      (setq tag-list (cons tag tag-list))
      (setq tag ""))
    (nreverse tag-list)))
      
(defun epic-read-title (&optional default)
  "Same as ``read-string'' with the exception of a descriptive prompt."
  (interactive)
  (read-string "Title: " default 'epic-title-history default))

(defun epic/completing-read (prompt collection hist &optional default)
  "Completing read for getting along with migemo and anything.el package."
  (let ((anything-use-migemo t))
    (completing-read prompt collection nil 'force
                     nil hist default)))

;;
;; Misc
;;

(defun epic/as-quote (obj)
  "Make AppleScript literals (List or String) from lisp OBJ."
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
  "Create a note article of Evernote from the text between BEG to END.
 Set TITLE (string), NOTEBOOK (stirng), and TAGS (list of string)
 to the article, and store it to Evernote."
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
  "Create a note aricle of Evernote from the FILE-NAME.
 Set TITLE (stirng), NOTEBOOK (string), and TAGS (list of string)
 to the article, and store it to Evernote."
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
  "Take two lists and zip up them to be org-style links like:
    [[URI1][TITLE1]] LF [[URI2][TITLE2]]..."
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
  "Insert a name of tag of Evernote with the prefix of `#'
 using anything.el package."
  (interactive)
  (anything '(anything-c-source-evernote-tags)))

(defun epic-anything-notebooks ()
  "Insert a name of notebook of Evernote with the prefix of `@'
 using anything.el package."
  (interactive)
  (anything '(anything-c-source-evernote-notebooks)))

(defun epic-anything ()
  "Insert a tag or notebook of Evernote with the prefix of `#' or `@'
 using anything.el package."
  (interactive)
  (anything '(
     anything-c-source-evernote-tags
     anything-c-source-evernote-notebooks
     )))

;;
;; Mew support
;;

(defun epic-mew-create-note (title notebook tags)
  "Import a mail article into the local Evernote app.
 The mail article must be selected and displayed
 by typing ``.'' (mew-summary-analyze-again) in the mew-summary buffer."
  (interactive 
   (list (epic-read-title (nth 4 (epic/mew-get-message-info)))
         (epic-read-notebook)
         (epic-read-tag-list)))
  (when (memq major-mode '(mew-summary-mode mew-virtual-mode))
    (let* ((msgnum (mew-summary-message-number2))
           (folder-name (mew-summary-folder-name)))
      (save-window-excursion
        (mew-summary-set-message-buffer folder-name msgnum)
        (epic-create-note-from-region
         (window-start (get-buffer-window))
         (point-max) title notebook tags)))))

(defun epic/mew-get-message-info ()
  (interactive)
  (when (memq major-mode '(mew-summary-mode mew-virtual-mode))
      (let ((msgnum (mew-summary-message-number2))
            (folder (mew-summary-folder-name))
            (window) (begin) (end) (subject))
        (save-window-excursion
          (mew-summary-set-message-buffer folder msgnum)
          (setq window  (get-buffer-window))
          (setq begin   (window-start window))
          (setq end     (point-max))
          (setq subject (mew-header-get-value mew-subj:)))
        (list folder msgnum begin end subject))))

;;
;; Send email forwarding to Evernote server by using Mew.
;;

(defvar epic-evernote-mail-address
  "your-evernote-importer-address0@???.evernote.com"
  "Evernote importer address assigned your evernote account.")

(defvar epic-evernote-mail-headers
  '("Message-Id:"
    "Subject:"
    "From:"
    "To:"
    "Cc:"
    "Date:")
  "Mail headers which need to be remained in the head of created note."
  )

(defun epic/mew-get-message-header-as-string (folder-name msgnum)
  (save-window-excursion
    (mew-summary-set-message-buffer folder-name msgnum)
    (mapconcat '(lambda (header)
                  (let (value)
                    (if (setq value (mew-header-get-value header))
                        (format "%s %s\n" header value))))
               epic-evernote-mail-headers
               "")))

(defun epic-mew-forward-to-evernote ()
  "Foward a mail to Evernote with the original headers."
  (interactive)
  (when (memq major-mode '(mew-summary-mode mew-virtual-mode))
    (let* ((msgnum (mew-summary-message-number2))
           (folder-name (mew-summary-folder-name))
           (mew-forward-string "")
           (mew-ask-send nil)
           (headers (epic/mew-get-message-header-as-string folder-name msgnum)))
      (mew-summary-forward)
      (mew-header-replace-value "To:" epic-evernote-mail-address)
      (mew-header-goto-body)
      (insert headers)
      (insert " \n")
      (goto-char (point-min))
      (search-forward-regexp "^Subject: " nil t)
      ;; (mew-draft-send-message)
      )))

(provide 'epic)

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
