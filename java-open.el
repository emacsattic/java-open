;;; java-open.el --- open source file of Java class name under point

;; Copyright (C) 2000 rajeev k

;; Author: rajeev1998@yahoo.com
;; Maintainer:
;; Created: 29 Jul 2000
;; Keywords: java convenience

;; This code is free; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;;; Commentary:
;;
;; Purpose
;; -------
;; java-open adds the ability to open the source file corresponding to a 
;; Java class by placing the cursor on the class name and pressing a key.
;; No TAGS file or other form of preprocessing is needed.
;;
;; Usage
;; -----
;; Example of lines to be added to your .emacs:
;;
;;     (require 'java-open)
;;
;;     ; java-open-source-path is similar in function to CLASSPATH
;;     (setq java-open-source-path '("L:/myprog/source" "M:/jdk1.3/src"))
;;
;;     ; keyboard shortcuts
;;     (global-set-key [f10] 'java-open-class-at-point)
;;     (global-set-key [f11] 'java-open-base-class-source)
;;
;; How it works
;; ------------
;; This code works by searching the current buffer for an import declaration
;; corresponding to the class name.  If an import declaration is found then
;; the filename and relative path is deduced from the import declaration.
;; Then the directories specified in the java-open-source-path are searched
;; for the relative path and file, and the file opened if found.
;;
;; If no import declaration is found then the current directory is searched
;; before reporting an error.
;;
;; Limitations
;; -----------
;; java-open can only open source files if you point to a class name.  It
;; cannot look for a class that contains the method name under the cursor.
;;

;;; History:
;;
;; Aug-05-2000 - added improvements suggested by
;;               Kalle Olavi Niemitalo <tosi@stekt.oulu.fi>  

;;; Code:

(provide 'java-open)

(defvar java-open-source-path '()
  "List of directories to search for source files.")

;; Search for filename in java-open-source-path and open it if found.
;; Return filename if successful; nil otherwise.
(defun java-open-file-in-path (filename)
  (let ((scan java-open-source-path))
    (catch 'found
      (while scan
        (let* ((dir (car scan))
               (full-name (concat dir "/" filename)))
          (if (file-readable-p full-name)
              (progn (find-file full-name)
                     (throw 'found full-name))))
        (setq scan (cdr scan))))))

;; Given a fully qualified class name, open source file corresponding
;; to the class name.
;; Returns value returned by java-open-file-in-path.
(defun java-open-class (full-class-name)
  (let ((line full-class-name))
    ;; replace all '.' with '/'
    (while (string-match "\\." line)
      (setq line (replace-match "/" t 1 line)))
    (java-open-file-in-path (concat line ".java"))))

;; Open source file corresponding to single-type-import declaration
;; under point.
(defun java-open-import-at-point ()
  (let (line)
    (save-excursion
      ;; get the line at point
      (setq line (buffer-substring
                  (progn (beginning-of-line) (point))
                  (progn (end-of-line) (point))))
      ;; extract class name
      (if (string-match "import \\(.*\\);" line)
          (setq line (replace-match "\\1" t nil line))
        (error "Unable to determine name of class being imported"))
      (let ((filename (java-open-class line)))
        (if (eq filename nil)
            (error "Can't find source for %s" line)
          (message "Opened %s" filename))))))

;; Given an unqualified class name, search all type-import-on-demand
;; declarations (wide imports) for the class, and open its source file.
(defun java-search-wide-imports (class-name)
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward (concat "^import .*\\.\\*;") nil t)
        (let ((line (buffer-substring
                     (progn (beginning-of-line) (point))
                     (progn (end-of-line) (point)))))
          (if (string-match "import \\(.*\\.\\)\\*;" line)
              (progn (setq line (replace-match "\\1" t nil line))
                     (let ((filename (java-open-class 
                                      (concat line class-name))))
                       (if (not (eq filename nil))
                           (progn (message "Opened %s" filename)
                                  (throw 'found t)))))))))))

(defun java-open-class-at-point ()
  "Open source file corresponding to Java class name at point."
  (interactive)
  (save-excursion
    ;; get class name
    (let ((class-name (current-word)))
      ;; search for single-type-import corresponding to class name
      (goto-char (point-min))
      (if (re-search-forward (concat "^import .*\\." class-name ";") nil t)
          (java-open-import-at-point)       ; found import declaration
        ;; no single-type-import declaration found -- try wide imports
        (if (not (java-search-wide-imports class-name))
            ;; maybe it is in java.lang package
            (let ((filename (java-open-class (concat "java.lang." class-name))))
              (if (not (eq filename nil))
                  (message "Opened %s" filename)
                ;; look for file in current directory
                (let ((fname (concat class-name ".java")))
                  (if (file-readable-p fname)
                      (progn (find-file fname)      ; open file in current dir
                             (message "Opened %s" (expand-file-name fname)))
                    (error "No import declaration found for \"%s\""
                           class-name))))))))))

(defun java-open-base-class-source ()
  "Open source file of base class of class defined in current buffer."
  (interactive)
  (let ((class-name
        (file-name-sans-extension 
         (file-name-nondirectory (buffer-file-name)))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (concat "class " class-name 
                   " extends *[a-zA-Z_][a-zA-Z0-9_]*") nil t)
          (java-open-class-at-point)
        (error "Failed to determine base class of \"%s\"" class-name)))))

(provide 'java-open)

;;; java-open.el ends here
