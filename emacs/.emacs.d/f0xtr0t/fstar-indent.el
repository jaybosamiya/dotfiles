(require 'subr-x)
(require 'fstar-mode)

(defun fstar-indent-file-to-string (file)
  (string-trim
   (with-temp-buffer (insert-file-contents file)
		     (buffer-string))))

(defun fstar-indent-string (str)
  (let*
      ((strfile (concat (expand-file-name (make-temp-name "fstar-indent")
					  temporary-file-directory) ".fst"))
       (errfile (expand-file-name (make-temp-name "fstar-indent-error")
				  temporary-file-directory))
       (has-module-name (string-match-p "^module [A-Za-z0-9]+$" str))
       (added-module-line "module TempModuleName")
       (str (if has-module-name
		str
	      (concat added-module-line "\n" str))))
    (with-temp-buffer
      (insert  str)
      (write-region (point-min) (point-max) strfile)
      (let*
	  ((indented-str
	    (with-output-to-string
	      (call-process "fstar" nil (list standard-output errfile) nil
			    "--indent" strfile))))
	(when (file-exists-p errfile)
	  (let* ((err-msg (fstar-indent-file-to-string errfile)))
	    (delete-file errfile)
	    (when (string-match-p "error was reported (see above)" err-msg)
	      (delete-file strfile)
	      (error "Can't indent:\n%s" err-msg))))
	(delete-file strfile)
	(message "Finished indenting")
	(string-trim
	 (if has-module-name
	     indented-str
	   (replace-regexp-in-string added-module-line "" indented-str)))))))

(defun fstar-indent-region (start end)
  (interactive "r")
  (save-excursion
    (let*
	((saved-point (point))
	 (start-point (progn (goto-char start) (line-beginning-position)))
	 (end-point (progn (goto-char end) (line-end-position)))
	 (str (buffer-substring-no-properties start-point end-point))
	 (indented-str (condition-case v
			   (fstar-indent-string str)
			 (error (error "%s" (cadr v))))))
      (if (not (string= str indented-str))
	  (progn
	    (delete-region start-point end-point)
	    (insert indented-str)
	    (goto-char saved-point)
	    t)
	(progn
	  (message "Already indented correctly")
	  nil)))))

(defun fstar-indent-buffer ()
  (interactive)
  (let* ((saved-line-number (line-number-at-pos)))
    (if (fstar-indent-region (point-min) (point-max))
	(progn
	  (goto-char 1)
	  (forward-line (1- saved-line-number))))))

(defun fstar-indent-subp ()
  (interactive)
  (let* ((saved-point (point)))
    (if (and (bolp) (not (eolp)))
	(forward-char))
    (let* ((saved-line-number (line-number-at-pos))
	   (start (fstar-subp-previous-block-start))
	   (start. (skip-chars-forward " \n\t"))
	   (end (fstar-subp-next-block-end))
	   (end. (skip-chars-backward " \n\t")))
      (if (fstar-indent-region (+ start start.) (+ end end.))
	  (progn
	    (goto-char 1)
	    (forward-line (1- saved-line-number)))
	(progn
	  (message "Already indented correctly")
	  (goto-char saved-point))))))

(provide 'fstar-indent)
