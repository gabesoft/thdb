
setup:
	stack setup

build:
	stack build

repl:
	stack ghci

exec: build
	stack exec -- thdb-exe