;;; epic.el --- Evernote Picker

;; Copyright (C) 2011, 2012 Yoshinari Nomura.
;; All rights reserved.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2011-06-23
;; Revised: 2012-08-28
;; Version: 0.1
;; Package-Requires: ((htmlize "1.47"))
;; Keywords: evernote, applescript

;;; Commentary:

;; * What is Epic ?
;;
;;   Epic is a small elisp to access Evernote process via AppleScript.
;;   Epic has these functions:
;;
;;   - Completing read for tags and notebooks:
;;     + ``epic-read-notebook'', ``epic-read-tag'', ``epic-read-tag-list'' ::
;;       for the completion of tags and notebooks.
;;
;;   - Creation of note articles:
;;     + ``epic-create-note-from-region'', ``epic-create-note-from-file'' ::
;;       for creation of a new note in your Evernote.app.
;;
;;   - For Mew users:
;;     + ``epic-mew-forward-to-evernote'' ::
;;        Nifty mail forwarder.
;;        You need to set the vars: ``epic-evernote-mail-address'',
;;        ``epic-evernote-mail-headers''
;;     + ``epic-mew-create-note'' ::
;;        Import a mail article into the local Evernote.app.
;;
;;   - For Org-mode users:
;;     With orglue.el (https://github.com/yoshinari-nomura/orglue)
;;     + Org-mode becomes to recognize evernote:// links.
;;     + You can drag notes in Evernote.app to an org-mode buffer.
;;     + ``epic-insert-selected-note-as-org-links''
;;        for insertion of org-style links.
;;
;; * Setting Example
;;
;;   : (require 'epic)
;;   : (define-key global-map [(control ?:)] 'epic-anything)
;;   : (define-key mew-summary-mode-map "r" 'epic-mew-create-note)
;;   : (define-key mew-summary-mode-map "e" 'epic-mew-forward-to-evernote)
;;   : (setq epic-evernote-mail-address "??????@???.evernote.com")
;;
;; * Contact Info
;;
;;   The updated version might be available from:
;;     http://github.com/yoshinari-nomura/epic

;;; Code:

(require 'htmlize)

;;;
;;; customizable variables
;;;

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

;; XXX: some cache control to be added.
(defvar epic-cache-notebooks nil)
(defvar epic-cache-tags      nil)

(defvar epic-default-evernote-stack "Projects")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notebooks

(defun epic-create-notebook (name)
  (interactive "sNew notebook name: ")
  (if (epic-notebook-exists-p name)
      (message "Notebook ``%s'' is already exists." name)
    (do-applescript (format "
      tell application \"Evernote\"
        create notebook %s
      end tell
      " (epic/as-quote name)))
    (message "Notebook ``%s'' is created." name)))

(defun epic-notebooks ()
  "Return the name list of notebooks in Evernote."
  (or epic-cache-notebooks
      (setq epic-cache-notebooks
            (epic/get-name-list "notebooks"))))

(defun epic-find-notebook-titles-in-stack (&optional stack-name)
  (epic-find-notebook-titles
   (concat "stack:" (or stack-name epic-default-evernote-stack))))
           
(defun epic-read-notebook (&optional default)
  "Completing read for notebooks of Evernote.
 This is supposed to work better with anything.el package."
  (interactive)
  (epic/completing-read "Notebook: " (epic-notebooks)
                        'epic-notebook-history (or default "")))

(defun epic-notebook-exists-p (name)
  (= 1 ;; XXX: current do-applescript can't return bool.
     (do-applescript (format "
       tell application \"Evernote\"
         if (notebook named %s exists) then
           return 1
         end if
         return 0 
       end tell
       " (epic/as-quote name)))))

(defun epic-rename-notebook (old-name new-name)
  (if (and (epic-notebook-exists-p old-name)
           (not (epic-notebook-exists-p new-name)))
      (do-applescript (format "
        tell application \"Evernote\"
          set name of notebook %s to %s
        end tell
        " (epic/as-quote old-name) (epic/as-quote new-name)))))

(defun epic-find-notebook-titles (query-string)
  (epic/split-lines
    (do-applescript (format "
      tell application \"Evernote\"
        set noteList to find notes %s
        set notebookTitles to {}
        set retstring to \"\"
        repeat with n in noteList
          set notebookname to (name of (notebook of n))
          if (notebookname is not in notebookTitles) then
            set notebookTitles to notebookTitles & notebookname
          end if
        end repeat
        repeat with n in notebookTitles
          set retstring to retstring & n & \"\n\"
        end repeat
        retstring
      end tell
      " (epic/as-quote query-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes

(defun epic-selected-note-uris ()
  "Return the URI list of the selected notes in Evernote.
 URIs are in the form of evernote://...
 Evernote seems to add URIs to their notes on syncing with its cloud server.
 Therefore, this function does not work with a note which is not never
 synced before."
  (epic/split-lines
   (do-applescript "
      tell application \"Evernote\"
        set noteList  to selection
        set noteLink to \"\"
        repeat with n in noteList
          set noteLink to (noteLink & (note link of n) & \"\n\")
        end repeat
      end tell
      ")))

(defun epic-nullify-selected-note ()
  (do-applescript "
      tell application \"Evernote\"
        set noteList  to selection
        set noteLink to \"\"
        repeat with n in noteList
          set (HTML content of n) to \"\"
        end repeat
      end tell
      "))

(defun epic-selected-note-list ()
  "Return selected notes as a list of (uri . title) cons cell
 like: ((\"title1\" . \"evernote:///.....\") (\"title2\" . \"evernote:///...\"))."
  (let ((uris   (epic-selected-note-uris))
        (titles (epic-selected-note-titles))
        (result '()))
    (while (and (car uris) (car titles))
      (setq result (cons (cons (car uris) (car titles)) result))
      (setq uris   (cdr uris))
      (setq titles (cdr titles)))
    result))

(defun epic-selected-note-titles ()
  "Return the titles of selected notes in Evernote."
  (sit-for 0.1) ;; required in case called as DnD-callbacks.
  (epic/split-lines
   (epic/as-tell-evernote "
     set noteList  to selection
     set noteTitle to \"\"
     repeat with n in noteList
       set noteTitle to (noteTitle & (title of n) & \"\n\")
     end repeat
     ")))

(defun epic-find-note-titles (query-string)
  "Return the titles of selected notes in Evernote.
query string overview: http://dev.evernote.com/documentation/cloud/chapters/search_grammar.php
"
  (sit-for 0.1) ;; required in case called as DnD-callbacks.
  (epic/split-lines
    (do-applescript (format "
      tell application \"Evernote\"
        set noteList to find notes %s
        set noteTitle to \"\"
        repeat with n in noteList
          set noteTitle to (noteTitle & (title of n) & \"\n\")
        end repeat
      end tell
      " (epic/as-quote query-string)))))

(defun epic-find-note-by-url (note-url)
  (do-applescript (format "
    tell application \"Evernote\"
      set aNote to find note %s
      if (exists aNote)
        open note window with aNote
        activate
        return note link of aNote as string
      end if
    end tell
    " (epic/as-quote note-url))))

(defun epic-note-get-tags (note-url)
  (epic/split-lines
     (do-applescript (format "
       tell application \"Evernote\"
         set aNote to find note %s
         set aList to \"\"
         if (exists aNote)
           set aTagList to (tags of aNote)
           if (exists aTagList)
             repeat with n in aTagList
               set aList to (aList & (name of n) & \"\n\")
             end repeat
             return aList
           end if
         end if
       end tell
       " (epic/as-quote note-url)))))

(defun epic-find-note-attachments (note-url)
  (do-applescript (format "
    tell application \"Evernote\"
      set aNote to find note %s
      set aList to \"\"
      if (exists aNote) and (exists attachments of aNote)
        repeat with n in (attachments of aNote)
          write n to \"/tmp/attachment-\" & (hash of n)
          set aList to (aList & (filename of n) & \"\n\")
        end repeat
        return aList
      end if
    end tell
    " (epic/as-quote note-url))))

(defun epic-export-note (note-url filename &optional export-tags format)
  (do-applescript (format "
    tell application \"Evernote\"
      set aNote to find note %s
      if (exists aNote)
        export {aNote} to %s %s %s
      end if
    end tell
    " (epic/as-quote note-url)
    (epic/as-quote filename)
    (epic/as-option "tags" (or export-tags 'true))
    (epic/as-option "format" (or format 'HTML)))))


(defun epic-read-note-title (&optional default)
  "Same as ``read-string'' with the exception of a descriptive prompt."
  (interactive)
  (read-string "Title: " default 'epic-title-history default))

(defun epic-create-note-from-region (beg end title notebook tags)
  "Create a note article of Evernote from the text between BEG to END.
 Set TITLE (string), NOTEBOOK (stirng), and TAGS (list of string)
 to the article, and store it to Evernote."
  (interactive
   (list (region-beginning) (region-end)
         (epic-read-note-title)
         (epic-read-notebook)
         (epic-read-tag-list)))
  (let* ((htmlize-output-type 'font)
         (htmlbuf (htmlize-region beg end))
         (temp-file (make-temp-file "epic" nil ".html")))
    (unwind-protect
	(with-current-buffer htmlbuf
          (write-region nil nil temp-file nil 'silent)
          (epic-create-note-from-file temp-file title notebook tags)
          (message "OK: %s" temp-file)
          )
      (kill-buffer htmlbuf)
      (delete-file temp-file)
      )))

(defun epic-create-note-from-file (file-name title &optional notebook tags attachments)
  "Create a note aricle of Evernote from the FILE-NAME.
 Set TITLE (stirng), NOTEBOOK (string), and TAGS (list of string)
 to the article, and store it to Evernote."
  (do-applescript (format "
    tell application \"Evernote\"
      set aNote to (create note from file %s title %s %s %s %s)
      open note window with aNote
      activate
    end tell
    " (epic/as-quote file-name)
    (epic/as-quote title)
    (epic/as-option "notebook" notebook)
    (epic/as-option "tags" tags)
    (epic/as-option "attachments" attachments))))

(defun epic-create-note-from-html-string (html-string title &optional notebook tags attachments)
  "Create a note aricle of Evernote from the FILE-NAME.
 Set TITLE (stirng), NOTEBOOK (string), and TAGS (list of string)
 to the article, and store it to Evernote."
  (do-applescript (format "
    tell application \"Evernote\"
      set aNote to (create note with html %s title %s %s %s %s)
      open note window with aNote
      activate
      return (note link of aNote) as string
    end tell
    " (epic/as-quote html-string)
    (epic/as-quote title)
    (epic/as-option "notebook" notebook)
    (epic/as-option "tags" tags)
    (epic/as-option "attachments" attachments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tags

(defun epic-tags ()
  "Return the name list of tags in Evernote."
  (or epic-cache-tags
      (setq epic-cache-tags
            (epic/get-name-list "tags"))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI Control of Evernote App

;; XXX: I don't want to open another collection window.
;; hint for fix?: http://discussion.evernote.com/topic/9621-applescript-to-open-specific-notebook/
(defun epic-open-collection-window-old (query-string)
  (if (string-match "^evernote://" query-string)
      (do-applescript (format "
        do shell script (\"open -g \" & %s)
        delay 1
        tell application \"Evernote\"
          set noteList to selection
          repeat with n in noteList
            set noteTitle to (title of n)
            set notebookname to (name of (notebook of n))
          end repeat
          open collection window with query string \"intitle:\\\"\" & noteTitle & \"\\\" notebook:\\\"\" & notebookname & \"\\\"\"
          activate
        end tell
        -- \"notebook:\\\"\" & notebookname & \"\\\"\"
        " (epic/as-quote query-string)))
    (do-applescript (format "
      tell application \"Evernote\"
        open collection window with query string %s
        activate
      end tell
      " (epic/as-quote query-string)))))

(defun epic-open-collection-window (query-string)
  (if (string-match "^evernote://" query-string)
      (do-applescript (format "
        do shell script (\"open \" & %s)
        " (epic/as-quote query-string)))
    (do-applescript (format "
      tell application \"Evernote\"
        open collection window with query string %s
        activate
      end tell
      " (epic/as-quote query-string)))))

(defun epic-open-notebook-in-collection-window (notebook-name)
  (interactive "sNotebook name: ")
  (epic-open-collection-window
   (format "notebook:\"%s\"" notebook-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mew support

(defun epic-mew-create-note (title notebook tags)
  "Import a mail article into the local Evernote app.
 The mail article must be selected and displayed
 by typing ``.'' (mew-summary-analyze-again) in the mew-summary buffer."
  (interactive
   (list (epic-read-note-title (nth 4 (epic/mew-get-message-info)))
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

(defun epic/mew-get-message-header-as-string (folder-name msgnum)
  (save-window-excursion
    (mew-summary-set-message-buffer folder-name msgnum)
    (mapconcat (lambda (header)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for Applescript

(defmacro epic/as-tell-evernote (body &rest params)
  `(do-applescript
    (format
     (concat "tell application \"Evernote\"\n"
             ,body
             "end tell\n")
     ,@params)))

(defun epic/get-name-list (obj-name)
  "Return the name list of tags or notebooks in Evernote.
 OBJ-NAME must be ``tags'' or ``notebooks''"
  (epic/split-lines
    (do-applescript (format "
      tell application \"Evernote\"
        set retval to \"\"
        set aList to %s
        repeat with x in aList
          set retval to (retval & (name of x) & \"\n\")
        end repeat
      end tell
      " obj-name))))

(defun epic/as-quote (obj)
  "Make AppleScript literals from lisp OBJ (list, string, integer, symbol)."
  (cond
   ((stringp obj) ;; (["\]) -> \$1
    (format "\"%s\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" obj)))
   ((listp obj)
    (concat "{" (mapconcat 'epic/as-quote obj ", ") "}"))
   ((eq t obj)
    "true")
   (t ;; integer or symbol assumed
    (format "%s" obj))
   ))

(defun epic/as-option (opt-name opt-value)
  "Make AppleScript optional OPT-NAME phrase if OPT-VALUE is not blank."
  (if (or (null opt-value)
          (and (stringp opt-value)
               (string= opt-value "")))
      ""
    (format "%s %s" opt-name (epic/as-quote opt-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for debug

(defun epic/as-quote-test (obj)
  (do-applescript (format "return %s" (epic/as-quote obj))))

(defconst epic/as-quote-script
"
on escape_string(str)
  set text item delimiters to \"\"
  set escaped to \"\"
  repeat with c in text items of str
    if c is in {quote, \"\\\\\"} then
      set c to \"\\\\\" & c
    end if
    set escaped to escaped & c
  end repeat
end escape_string

on emacs_converter(obj)
  set c to (class of obj)
  if c is in {real, integer, number} then
    return obj
  else if c is in {text, string} then
    return quote & escape_string(obj) & quote
  else if obj = {} then
    return \"()\"
  else if c is in {boolean}
    if obj then
      return \"t\"
    else
      return \"nil\"
    end if
  else if c is in {list} then
    set res to \"(\"
    repeat with e in obj
      set res to (res & emacs_converter(e) & \" \")
    end repeat
    set res to (res & \")\")
    return res
  end if
end emacs_converter
")

(defun epic-test (obj)
  (do-applescript 
   (concat epic/as-quote-script
           (format "emacs_converter(%s)" (epic/as-quote obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc

(defun epic/chomp (str &optional LF)
  (if (string= (substring str -1) (or LF "\n"))
      (substring str 0 -1)
    str))

(defun epic/split-lines (lines &optional LF)
  (and lines (split-string (epic/chomp lines) (or LF "\n"))))
      
(defun epic/completing-read (prompt collection hist &optional default)
  "Completing read for getting along with migemo and anything.el package."
  (let ((anything-use-migemo t))
    (completing-read prompt collection nil 'force
                     nil hist default)))

(provide 'epic)

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

;;; epic.el ends here
