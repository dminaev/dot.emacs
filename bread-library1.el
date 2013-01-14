;;; bread-library.el --- e-library browser for Emacs

(defconst bread-library-version "0.2" "bread-library version.")

;; Author: Dmitri Minaev <minaev@gmail.com>
;; Version: 0.2
;; Keywords: ebooks reading library catalogue
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:


(provide 'bread-library)

(defvar bread-library-db '()
  "list of books. Every entry  is a list that includes: 1.path to
  the  file 2.author  3.title 4.tags  5.state  6.genre 7.priority
  8.notes ")

(defvar bread-library-file "~/.bread-library"
  "file where the Bread library is stored")

(defvar bread-library-sort 'author
  "Defines grouping for the catalog:
 'title: simple list of books;
 'author: books are grouped by author;
 'genre: books are grouped by genre and author")

(defconst bread-library-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x x") 'bread-library-kill-buffer)
    (define-key map (kbd "C-x w") 'bread-library-rebuild-library)
    (define-key map (kbd "C-x g g") 'bread-library-by-genre)
    (define-key map (kbd "C-x g a") 'bread-library-by-author)
    (define-key map (kbd "C-x g t") 'bread-library-by-title)
    map)
  "keymap for Bread library mode")

(defun bread-library-browse ()
  "Main function that presents the catalog"
  (interactive)
  (message "Loading the library database from file...")
  (switch-to-buffer (get-buffer-create "Bread Library"))
  (message "Loading the library database from file... done")
  (bread-library-display-library)
  (org-mode)
  (add-hook 'after-change-functions 'bread-library-mark-entry-dirty nil t)
  (bread-library-mode))
;  (toggle-read-only))

(define-minor-mode bread-library-mode
  "Defines Bread Library minor mode. Add library-specific keybindings"
  :group bread-library)

(defun bread-library-load-db ()
  "Loads the list of books from disk file to the variable bread-library-db"
  (if (file-exists-p bread-library-file)
      (with-temp-buffer
	(insert-file-contents bread-library-file)
	(setq bread-library-db (read (current-buffer))))
    (setq bread-library-db '())))

(defun bread-library-by-genre ()
  "Sets the grouping to 'genre and redraws the catalog"
  (interactive)
  (unless (eq bread-library-sort 'genre)
    (bread-library-save-db (bread-library-org-to-sexp (current-buffer)))
    (setq bread-library-sort 'genre)
    (erase-buffer)
    (bread-library-display-library)))

(defun bread-library-by-author ()
  "Sets the grouping to 'author and redraws the catalog"
  (interactive)
  (unless (eq bread-library-sort 'author)
    (bread-library-save-db (bread-library-org-to-sexp (current-buffer)))
    (setq bread-library-sort 'author)
    (erase-buffer)
    (bread-library-display-library)))

(defun bread-library-by-title ()
  "Sets the grouping to 'title and redraws the catalog"
  (interactive)
  (unless (eq bread-library-sort 'title)
    (bread-library-save-db (bread-library-org-to-sexp (current-buffer)))
    (setq bread-library-sort 'title)
    (erase-buffer)
    (bread-library-display-library)))

(defun bread-library-display-library ()
  "Prepares the settings for Org-mode and calls rendering code for every book."
  (bread-library-load-db)
  (insert "#+SEQ_TODO: TOREAD READING POSTPONED | DONE THROWNAWAY
#+PRIORITIES: 1 9 5
#+OPTIONS: f:nil toc:2
#+TAGS: autobiographical(a) fiction(f) nonfiction(n) modernclassics(m) crypto math science(s) scifi(c) philosophy history(h) geography(g) politics(p) fairytale(t) kids(k)\n")
;  (insert "* All books:\n")
  (cond ((eq bread-library-sort 'title)
	 (message "by title")
	 (setq bread-library-db (sort bread-library-db #'(lambda (i1 i2) (string< (nth 1 i1) (nth 2 i2)))))
	 (mapcar 'bread-library-render-entry-lvl1 bread-library-db))
	((eq bread-library-sort 'author)
	 (message "by author/title")
;	  (setq bread-library-db (sort bread-library-db 'bread-library-by-author-sorter)))
	 (mapcar 'bread-library-render-author-lvl1 (bread-group-by-author bread-library-db)))
	((eq bread-library-sort 'genre)
	 (message "by genre/author/title")
	 (mapcar 'bread-library-render-genre-lvl1 (bread-group-by-genre bread-library-db)))
	(t (message "by default") t))
  (insert "\n*")
  (goto-char (point-min))
  (org-shifttab))

(defun bread-library-kill-buffer ()
  "Saves the library to the file and quits."
  (interactive)
;  (setq bread-library-db (bread-library-org-to-sexp (current-buffer)))
  (bread-library-save-db (bread-library-org-to-sexp (current-buffer)))
  (kill-buffer (get-buffer "Bread Library")))

(defun bread-library-save-db (db)
    "Save the library database to a file."
;    (interactive)
    (message "Saving Bread library database...")
    (with-temp-buffer
      (insert "; 1.path 2.author 3.title 4.tags 5.state 6.genre 7.priority 8.description")
;      (sort bread-library-db 'bread-library-by-author-sorter)
      (print db (current-buffer))
      (write-file bread-library-file))
;    (message "Saved database: %S" bread-library-db)
    (message "Saving Bread library database...done"))

(defun bread-library-add-book (file)
  "Attempts  to   get  metadata  from  file,   then  prompts  for
confirmation (or  modification) of these metadata,  then adds the
book to the database and saves it.  Intended use: from dired."
  (if (assoc file bread-library-db)
      (error "File is already in the database")
    (progn
      (let ((metadata (bread-get-metadata file)))
	(let ((filename (nth 0 metadata))
	      (author (read-from-minibuffer 
		       "Author: "
		       (nth 1 metadata)))
	      (title (read-from-minibuffer 
		      "Title: "
		      (nth 2 metadata)))
	      (genre (read-from-minibuffer "Genre: " (nth 3 metadata)))
	      (tags (read-from-minibuffer "Tags (separated and surrounded by colons): " ":"))
	      (desc (nth 4 metadata)))
	  (setq bread-library-db (cons 
				  (list filename author title tags "TOREAD" genre nil desc)
				  bread-library-db))))
      (bread-library-save-db bread-library-db))))

(defun bread-get-metadata (file)
  "Extracts metadata  from files,  depending on the  filetype, or
prepares the metadata from the path to the file, if other methods
are not available for this type of files."
;  (message "Getting metadata from %S" file)
  (let ((filetype (file-name-extension file))
	(filedir (file-name-nondirectory (directory-file-name (file-name-directory file))))
	(filesansext (file-name-sans-extension (file-name-nondirectory file)))
	(filepreviousdir (file-name-nondirectory 
			  (directory-file-name 
			   (file-name-directory 
			    (directory-file-name (file-name-directory file)))))))
;    (message "Filetype: %s" filetype)
    (cond ((string-match "[fF][bB]2" filetype)
	   (with-temp-buffer
	     (insert-file-contents file nil 0 8192)
	     (goto-char (point-min))
	     (search-forward "<title-info>")
	     (backward-char 12)
	     (let ((title-info (xml-parse-tag)))
;	       (message "title-info: %S" title-info)
	       (let ((authors "") (title "") (genre ""))
		 (mapc 'bread-library-extract-names title-info)
;		 (message "authors: %s" authors)
		 (setq title (caddr (assoc 'book-title title-info)))
;		 (message "title: %s" title)
		 (setq genre (caddr (assoc 'genre title-info)))
;		 (message "genre: %s" genre)
		 (setq desc 
		       (org-trim 
			(mapconcat 
			 'bread-library-format-fb2description 
			 (assoc 'annotation title-info) "")))
;		 (message "description: %s" desc)
		 (list file authors title genre desc)))))
	  ((string-match "[pP][dD][fF]" filetype) 
	   (with-temp-buffer
	     (insert (shell-command-to-string (concat "pdfinfo " file)))
;	     (message "pdfinfo: %s" (shell-command-to-string (concat "pdfinfo " file)))
	     (goto-char (point-min))
	     (let ((authors filedir) (title filesansext))
	       (when (re-search-forward "Title: \\(.*\\)" nil t)
;		 (message "title found: %s" (match-string 1))
		 (setq title (org-trim (match-string 1))))
	       (goto-char (point-min))
	       (when (re-search-forward "Author: \\(.*\\)" nil t)
;		 (message "author found: %s" (match-string 1))
		 (setq authors (org-trim (match-string 1))))
	       (list file authors title filepreviousdir ""))))
	  (t 
	   (list file filedir filesansext  filepreviousdir "")))))

(defun bread-library-format-fb2description (e)
  (let ((d ""))
    (cond 
     ;; e = nil
     ((not e) 
      d)
     ;; e = string
     ((stringp e)
      (concat (org-trim e) "\n"))
     ;; e = list
     ((listp e)
      (if (listp (cdr e))
	  ;; e = proper list
	    (mapconcat 'bread-library-format-fb2description e "")
	  ;; e = dotted pair
	  d))
     (t d))))

(defun bread-library-extract-names (s)
  (when (and (listp s) (eq (car s) 'author))
;    (message "s: %S" s)
    (unless (equal authors "")
      (setq authors (concat authors "; ")))
    (let ((fname (nth 2 (assoc 'first-name s)))
	  (lname (nth 2 (assoc 'last-name s))))
      (if (and fname lname)
	  (setq authors (concat authors lname ", " fname))
	(setq authors (concat authors lname fname))))))

(defun bread-library-render-entry-lvl1 (entry)
  (bread-library-render-entry entry "*"))
(defun bread-library-render-entry-lvl2 (entry)
  (bread-library-render-entry entry "**"))
(defun bread-library-render-entry-lvl3 (entry)
  (bread-library-render-entry entry "***"))
(defun bread-library-render-author-lvl1 (author)
  (bread-library-render-author author "*"))
(defun bread-library-render-author-lvl2 (author)
  (bread-library-render-author author "**"))


(defun bread-library-render-entry (entry lvl)
  "Renders an entry to the Org-mode syntax."
  (let ((file (nth 0 entry))
	(author (nth 1 entry))
	(title (nth 2 entry))
	(tags (nth 3 entry))
	(state (nth 4 entry))
	(genre (nth 5 entry))
	(priority (nth 6 entry))
	(desc (nth 7 entry)))
    (insert (concat "\n" lvl " " state (when priority (format " [#%s]" priority)) 
		    " " title ". " author "             " tags  
		    "\n:PROPERTIES:\n:title: " title 
		    "\n:author: " author
		    "\n:url: " file 
		    "\n:genre: " genre)
		    "\n:END:\n  [[file:" file "][" file "]]\n")
    (bread-library-render-description desc)))

(defun bread-library-render-description (d)
  (when (> (length d) 0)
    (let ((pt (point)))
      (insert "\n")
      (insert d)
      (fill-region pt (point))
      (insert "\n"))))

(defun bread-library-render-author (author lvl)
  (insert (concat "\n" lvl " " (nth 0 author)))
  (cond ((= (length lvl) 1)
	 (mapcar 'bread-library-render-entry-lvl2 (cdr author)))
	((= (length lvl) 2)
	  (mapcar 'bread-library-render-entry-lvl3 (cdr author)))))

(defun bread-library-render-genre-lvl1 (genre)
  (insert (concat "\n* " (nth 0 genre)))
  (mapcar 'bread-library-render-author-lvl2 (cdr genre)))

;; (defun bread-library-edit-entry ()
;;   (interactive)
;;   (let ((props (org-entry-properties)))
;;     (message "Properties: %S" props)
;;     (let ((url (cdr (assoc "url" props)))
;; 	  (author (cdr (assoc "author" props)))
;; 	  (title (cdr (assoc "title" props)))
;; 	  (genre (cdr (assoc "genre" props)))
;; 	  (tags (cdr (assoc "TAGS" props)))
;; 	  (state (cdr (assoc "TODO" props))))
;; ;      (toggle-read-only -1)
;;       (let ((new-author (read-from-minibuffer "Author: " author))
;; 	    (new-title (read-from-minibuffer  "Title: " title))
;; 	    (new-tags (read-from-minibuffer "Tags: " tags))
;; 	    (new-genre (read-from-minibuffer "Genre: " genre))
;; 	    (new-state (read-from-minibuffer "State: " state)))
;; 	(setcdr (assoc url bread-library-db) (list new-author new-title new-tags new-state new-genre ""))
;; 	(bread-library-save-db)
;; 	(org-cut-subtree)
;; 	(cond ((eq bread-library-sort 'title)
;; 	       (insert "\n* "))
;; 	      ((eq bread-library-sort 'author)
;; 	       (insert "\n** "))
;; 	      ((eq bread-library-sort 'genre)
;; 	       (insert "\n*** ")))
;; 	(insert (concat new-state " " new-title ". " 
;; 			new-author "             " new-tags  "\n:PROPERTIES:\n:title: " 
;; 			new-title "\n:author: " new-author
;; 			"\n:url: " url "\n:genre: " genre "\n:END:\n  file:" url "\n"))
;; 	(toggle-read-only 1)))))

	
(defun bread-group-by-author (list)
  "Returns a list of books grouped by authors."
  (let (value)
    (dolist (item list value)
      (let ((author-is-known (assoc (nth 1 item) value)))
	(if author-is-known
	    (setcdr author-is-known (append (list item) (cdr author-is-known)))
	  (setq value (cons (list (nth 1 item) item) value)))))
    (sort value #'(lambda (i1 i2) (string< (nth 0 i1) (nth 0 i2))))))


(defun bread-group-by-genre (list)
  "Returns a list of books grouped by genre and by authors."
  (let (value)
    (dolist (item list value)
      (let ((genre-is-known (assoc (nth 5 item) value)))
	(if genre-is-known
	    (let ((author-is-known (assoc (nth 1 item) (cdr genre-is-known))))
	      (if author-is-known
		  (setcdr author-is-known (append (list item) (cdr author-is-known)))
		(setcdr genre-is-known (cons (list (nth 1 item) item) (cdr genre-is-known)))))
	  (setq value (cons (list (nth 5 item) (list (nth 1 item) item)) value)))))
    (sort value #'(lambda (i1 i2) (string< (nth 0 i1) (nth 0 i2))))))
;    (mapcar #'(lambda (g) (setcdr g (sort (cdr g) #'(lambda (i1 i2) (string< (first i1) (first i2)))))) 
;	    value)))


(defun bread-library-org-to-sexp (buffer)
  "Converts the Org-mode buffer into sexps"
  (save-excursion
    (save-restriction
      (setq case-fold-search nil)
      (widen)
      (let ((result nil))
	(goto-char (point-min))
	(while (search-forward ":PROPERTIES:" nil t)
	  (let ((props (org-entry-properties)))
	    (let ((url (cdr (assoc "url" props)))
		  (author (cdr (assoc "author" props)))
		  (title (cdr (assoc "title" props)))
		  (genre (cdr (assoc "genre" props)))
		  (tags (cdr (assoc "ALLTAGS" props)))
		  (state (cdr (assoc "TODO" props)))
		  (priority (cdr (assoc "PRIORITY" props)))
		  (desc ""))
	      (re-search-forward ":END:\n.*\n\n\\([[:print:][:space:]]*?\\)\\(^\\*\\)" nil t)
	      (when (> (length (match-string 1)) 0)
		(setq desc (org-trim (match-string 1))))
		(set-text-properties 0 (length desc) nil desc)
	      (setq result (cons (list url author title tags state genre priority desc) result)))))
	result))))

(defun bread-library-harvester ()
  "Walks through the given directory and adds the files to the library"
  (interactive)
  (bread-library-load-db)
  (let ((dir (read-directory-name "Enter the directory name: ")))
    (mapc 'bread-library-add-book-silently (directory-files dir t nil t)))
  (bread-library-save-db bread-library-db)
  (beep))

(defun bread-library-add-book-silently (file)
;  (message "harvester found file %s" file)
  (unless (assoc file bread-library-db)
    (cond ((equal file (concat (file-name-directory file) ".")))
;	   (message "%s discarded" file))
	  ((equal file (concat (file-name-directory file) "..")))
;	   (message "%s discarded" file))
	  ((file-directory-p file)
;	   (message "%s is a directory, recursing..." file)
	   (mapc 'bread-library-add-book-silently (directory-files file t nil t)))
	  ((file-regular-p file)
;	   (message "auto-adding %s" file)
	   (when (or (equal (file-name-extension file) "fb2")
		     (equal (file-name-extension file) "pdf")
		     (equal (file-name-extension file) "djvu")
		     (equal (file-name-extension file) "doc")
		     (equal (file-name-extension file) "rtf")
		     (equal (file-name-extension file) "txt"))
;	     (message "the file is %s" (file-name-extension file))
	     (let ((metadata (bread-get-metadata file)))
	       (let ((authors (nth 1 metadata))
		     (title (nth 2 metadata))
		     (genre (nth 3 metadata))
		     (desc (nth 4 metadata)))
		 (setq bread-library-db (cons 
					 (list file authors title ":auto:" "TOREAD" genre nil desc)
					 bread-library-db)))))))))

(defun bread-library-mark-entry-dirty (beg end length)
  "This function  is called from  after-change-functions hook and
  marks the current entry as `dirty', to save it later."
  (save-excursion
    (unless (org-entry-get nil "D" "t")
      (org-entry-put nil "D"  "t"))))