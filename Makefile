build: src/*
	mkdir -p bin/
	gcc src/vm.c -o bin/a.out

.PHONY: run
run: build
	./bin/a.out images/rogue.obj
