.DEFAULT_GOAL := usage

usage:
	@ echo "make ssh-sbcl # "
	@ echo "make ssh-ccl # "

ssh-sbcl:
	@ rm -f dist/sbcl/ssh-sbcl || true
	@ mkdir -p dist/sbcl || true
	@ cat deliver-sshd.lisp|sbcl --dynamic-space-size 2048

ssh-ccl:
	@ rm -f dist/ccl/ssh-sbcl || true
	@ mkdir -p dist/ccl || true
	@ cat deliver-sshd.lisp|ccl64
