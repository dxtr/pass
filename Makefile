CL=sbcl

all:
	buildapp --asdf-tree ~/quicklisp/dists/quicklisp/software \
		--logfile build.log \
		--load-system pass \
		--eval "(defun main (argv) (setf pass:*compiled* t) (pass:main argv))" \
		--entry main \
		--output pass

clean:
	rm pass
