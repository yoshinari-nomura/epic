(require 'epic)

(defvar anything-c-source-evernote-tags
  '((name . "Evernote Tags")
    (candidates . epic-tags)
    (migemo)
    (action
     "Insert Tag Name" .
     (lambda (candidate) (insert "#" candidate) candidate))
    ))

(defvar anything-c-source-evernote-notebooks
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

;; sample
(defun epic-anything ()
  "Using anything.el package:
1. Insert the name of selected tag of Evernote with the prefix of `#'.
2. Insert the name of selected notebook of Evernote with the prefix of `@'.
3. Pop to Evernote App and open the selected notebook.
"
  (interactive)
  (anything
   '(
     anything-c-source-evernote-tags
     anything-c-source-evernote-notebooks
     )))

(provide 'epic-anything-config)
