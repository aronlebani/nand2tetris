LISP ?= sbcl

build:
	$(LISP) \
		--load assembler.asd \
		--eval '(ql:quickload :assembler)' \
		--eval '(asdf:make :assembler)' \
		--eval '(quit)'

clean:
	rm assembler *.fasl
