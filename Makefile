build:
	dune build ./test/test.exe
	dune build ./bench/bench_test.exe
	dune build ./reference/reference.exe

bench: build ./bench/bench.ml
	python3 bench_test.py

reference: build ./reference/reference.ml
	cp _build/default/reference/reference.exe .
	python3 reference_test.py

test: build ./test/test.ml
	dune exec ./test/test.exe .

install:
	dune install
