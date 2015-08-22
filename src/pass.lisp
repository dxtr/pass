(in-package #:cl-user)
(defpackage #:pass
  (:use :cl :uiop/image :uiop/filesystem :uiop/os :uiop/pathname
	:uiop/run-program)
  (:export :main
	   :*compiled*))
(in-package #:pass)

;; blah blah blah.
;(require :uiop/image)
(require :getopt)
(require :cl-ppcre)
(require :utilities.print-tree)

(defun getenv (name)
  (uiop:getenv name))

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
(defvar *prefix* (uiop:merge-pathnames* (getenv "HOME") "/.password-store"))
(defvar *gpg-id* (uiop:merge-pathnames* *prefix* "/.gpg-id"))
(defvar *git-dir* (uiop:merge-pathnames* *prefix* "/.git"))
(defvar *gpg-recipient* "")
(defvar *editor* "")
(defvar *compiled* nil)

(let ((e (getenv "EDITOR")))
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
  (if (not (null die))
      #+sbcl (sb-ext:exit :code 1)
      #+ccl (ccl:quit)))

(defun change-prefix (new)
  (if new
      (progn
	(setf *prefix* new)
	(setf *gpg-id* (concatenate 'string *prefix* "/.gpg-id"))
	(setf *git-dir* (concatenate 'string *prefix* "/.git"))

	;; TODO: Change cwd if possible
	)))

(defun prefix-exists ()
  (if (uiop:directory-exists-p (make-pathname :name *prefix*)) t nil))

(defun gpg-id-exists ()
  (if (probe-file *gpg-id*) t nil))

(defun git-dir-exists ()
  (if (uiop:directory-exists-p (make-pathname :name *prefix*)) t nil))

(defun git-commit (message)
  (uiop:run-program `("git" "commit" "-Sm" ,message)))

(defun git-add-file (file message)
  (uiop:run-program `("git" "add" ,file))
  (git-commit message))

(defun is-prefix-initialized ()
  (and (prefix-exists) (gpg-id-exists) (git-dir-exists)))

(defun check-sneaky-paths (path)
  nil)

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
  nil)

(defun cmd-init (args)
  (if (null args)
      (progn
	(format t "Usage: pass init [--path=subfolder] gpg-id...")
	(uiop:quit 1)))
  (if (not (null *pflag*))
      (change-prefix *pflag*))
  (if (is-prefix-initialized)
      (progn
	(format t "~A is already initialized!" *prefix*)
	(uiop:quit 1)))
  (ensure-directories-exist (concatenate 'string *prefix* "/"))
  (chdir *prefix*)
  ;; This isn't necessary when we have implemented reencrypt-path
  (with-open-file (stream *gpg-id*
			  :direction :output
			  :if-exists :error
			  :if-does-not-exist :create)
    (write-line (car args) stream))
  (uiop:run-program '("git" "init"))
  (reencrypt-path (*prefix)))

(defun cmd-ls (args)
  (format t "list ~A" args))

(defun cmd-show (args)
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
    (format t "args: ~A~%" args)
    (cond ((string-equal cmd "init") (cmd-init params))
	  ((string-equal cmd "list") (format t "list"))
	  ((string-equal cmd "show") (format t "show")))
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

(main nil)
