(in-package #:cl-user)
(defpackage #:pass
  (:use :cl :uiop/image :uiop/filesystem :uiop/os :uiop/pathname
	:uiop/run-program)
  (:export :main
	   :*compiled*))
(in-package #:pass)

(require :getopt)
(require :cl-ppcre)
(require :utilities.print-tree)

(defparameter *opts* '(("h" :none)
		       ("p" :required)
		       ("e" :none)
		       ("m" :none)
		       ("c" :none)
		       ("f" :none)
		       ("n" :none)
		       ("i" :none)
		       ("r" :none)))
(defvar *hflag* nil)
(defvar *pflag* nil)
(defvar *eflag* nil)
(defvar *mflag* nil)
(defvar *cflag* nil)
(defvar *fflag* nil)
(defvar *nflag* nil)
(defvar *iflag* nil)
(defvar *rflag* nil)
(defvar *prefix* (concatenate 'string (uiop:getenv "HOME") "/.password-store/"))
(defvar *gpg-id* (concatenate 'string *prefix* ".gpg-id"))
(defvar *git-dir* (concatenate 'string *prefix* ".git"))
(defvar *gpg-recipient* "")
(defvar *editor* "")
(defvar *compiled* nil)

(let ((e (uiop:getenv "EDITOR")))
  (setf *editor* (if e e "vi")))

(defun is-compiled ()
  (eq *compiled* t))

(defun usage ()
  (format t "this is usage ~A" uiop:*command-line-arguments*)
  ;(format t "usage: ~A [-h] [-o filename] url~%" (car sb-ext:*posix-argv*))
;  (format t "options:~%")
;  (format t "    -h            print this help message~%")
;  (format t "    -o filename   write url to filename~%~%")
  (uiop:quit 0))

(defun err (msg &optional die)
  (warn msg)
  (if die
      (uiop:quit 1)))

(defun prefix-exists ()
  (if (uiop:directory-exists-p (make-pathname :name *prefix*)) t nil))

(defun gpg-id-exists ()
  (if (probe-file *gpg-id*) t nil))

(defun git-dir-exists ()
  (if (uiop:directory-exists-p (make-pathname :name *prefix*)) t nil))

(defun change-prefix (new)
  (if new
      (progn
	(setf *prefix* new)
	(setf *gpg-id* (concatenate 'string *prefix* ".gpg-id"))
	(setf *git-dir* (concatenate 'string *prefix* ".git"))

	(if (prefix-exists)
	    (uiop:chdir *prefix*)))))

(defun exec (command)
  (uiop:run-program command :output :interactive
		    :error-output :interactive))

(defun git-commit (message)
					;(uiop:run-program `("git" "commit" "-Sm" ,message) :output :interactive)
  (format t "~A~%" message)
  (exec `("git" "commit" "-S" ,*gpg-recipient* "-m" ,message)))
(defun git-add (file &optional message)
					;(uiop:run-program `("git" "add" ,file) :output :interactive)
  (exec `("git" "add" "-f" ,file))
  (git-commit (if message message (concatenate 'string "Added " file))))

(defun is-prefix-initialized ()
  (and (prefix-exists) (gpg-id-exists) (git-dir-exists)))

(defun check-sneaky-paths (path)
  path)

(defun parse-argv (argv)
  (multiple-value-bind (args opts)
      (getopt:getopt argv *opts*)
    (dolist (opt opts)
      (case (car opt)
	("h" (setq *hflag* t))
	("p" (setq *pflag* t))
	("e" (setq *eflag* t))
	("m" (setq *mflag* t))
	("c" (setq *cflag* t))
	("f" (setq *fflag* t))
	("f" (setq *nflag* t))
	("n" (setq *nflag* t))
	("i" (setq *iflag* t))
	("r" (setq *rflag* t))))
    (cdr args)))

(defun reencrypt-path (path)
  path)

(defun cmd-init (args)
  (if (null args)
      (progn
	(format t "Usage: pass init [--path=subfolder] gpg-id...")
	(uiop:quit 1))
      (setf *gpg-recipient* (car args)))
  (if *pflag*
      (change-prefix *pflag*))
  (if (prefix-exists)
      (err "Subfolder already exists!" t))
  (if (gpg-id-exists)
      (err "gpg-id already exists!" t))
  (format t "Creating directory...~%")
  (ensure-directories-exist (concatenate 'string *prefix* "/"))
  (format t "Changing directory...~%")
  (uiop:chdir *prefix*)
  ;; This isn't necessary when we have implemented reencrypt-path
  (with-open-file (stream *gpg-id*
			  :direction :output
			  :if-exists :error
			  :if-does-not-exist :create)
    (write-line (car args) stream))
  (format t "Creating git repository...~%")
  (uiop:run-program '("git" "init"))
  (reencrypt-path *prefix*)
  (git-add *gpg-id*)
  t)

(defun cmd-show (args)
  (if (null args)
      (setf args ""))
  (cond
    ((probe-file (concatenate *prefix* "/" (car args))) (format t "TODO"))
    ((uiop:directory-exists-p (make-pathname :name (concatenate *prefix* (car args)))) (format t "TODO"))
    (t (format t "WUT")))
  (format t "show ~A" args))

(defun cmd-grep (args)
  (format t "grep ~A" args))

(defun cmd-insert (args)
  (format t "insert ~A" args))

(defun cmd-edit (args)
  (format t "edit ~A" args))

(defun cmd-generate (args)
  (format t "generate ~A" args))

(defun cmd-rm (args)
  (format t "rm ~A" args))

(defun cmd-mv (args)
  (format t "mv ~A" args))

(defun cmd-cp (args)
  (format t "cp ~A" args))

(defun cmd-git (args)
  (format t "git ~A" args))

(defun cmd-version (args)
  (format t "version ~A" args))

(defun cmd-help ()
  (usage))

(defun main (argv)
  (uiop:setup-command-line-arguments)
  (if (not (is-compiled))
      (setf argv (append '("pass") uiop:*command-line-arguments*)))
  (let* ((args (parse-argv argv))
	 (cmd (car args))
	 (params (cdr args)))
    (cond ((string-equal cmd "init") (cmd-init params))
	  ((string-equal cmd "list") (cmd-show params))
	  ((string-equal cmd "show") (cmd-show params)))
    (case (car args)
      ("init" (cmd-init (cdr args)))
      ;("ls" (cmd-ls (cdr args)))
      ;("list" (cmd-ls (cdr args)))
      ;("show" (cmd-ls (cdr args)))
					;("find" (cmd-find (cdr args)))
      ;("show" (cmd-show (cdr args)))
      ;("grep" (cmd-grep (cdr args)))
      ;("insert" (cmd-insert (cdr args)))
      ;("edit" (cmd-edit (cdr args)))
      ;("generate" (cmd-generate (cdr args)))
      ;("rm" (cmd-rm (cdr args)))
      ;("mv" (cmd-mv (cdr args)))
      ;("cp" (cmd-cp (cdr args)))
      ;("copy" (cmd-cp (cdr args)))
      ;("git" (cmd-git (cdr args)))
      ;("versoin" (cmd-version (cdr args)))
      ;("help" (cmd-help))
      ))
  (uiop:quit 0))

;(main nil)

(defun traverse-directory (dir)
  (let ((tree `(,dir)))
    ;; First we collect subdirectories
    (loop for d on (uiop:subdirectories dir) do
	 (push (traverse-directory (car d)) tree))
    (loop for f on (uiop:directory-files dir) do
	 (push (car f) tree))
    (reverse tree)))

(defun print-tree (tree &optional (level 0) (first t))
  (let ((iterations (- (list-length tree) 1)))
    (loop for node in tree
       for cnt from 0 to iterations
       do
	 (if (not first)
	     (progn
	       (cond
		 ((= level 1)
		  (if (< cnt iterations)
		      (progn
			(princ "|-- "))
		      (progn
			(princ "`-- "))))
		 (t
		  (loop repeat (- level 1) do (princ "|   "))
		  (if (< cnt iterations)
		      (progn
			(princ "|-- "))
		      (progn
			(princ "`-- ")))))))
	 (if (listp node)
	     (progn
	       (format t "~A~%" (car node))
	       (print-tree (cdr node) (+ level 1) nil))
	     (progn
	       (format t "~A~%" node))))))

(defun print-directory-tree (dir)
  (print-tree `(,(traverse-directory dir))))

;(print-directory-tree "~/temp/")
