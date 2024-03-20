LISP ?= sbcl

build:
	$(LISP) \
		--load vm.asd \
		--eval '(ql:quickload :vm)' \
		--eval '(asdf:make :vm)' \
		--eval '(quit)'

clean:
	rm vm *.fasl
