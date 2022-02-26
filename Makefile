STACK = stack
EXE   = LambdaCalcToolkit-exe

TEST_NUM_THREADS = 12
TEST_TIMEOUT= 15

.PHONY: all

.SILENT:

all: build test_quiet

verbose: build test

build:
	$(STACK) build

exec: build
	$(STACK) exec $(EXE) $(target)

test: build
	$(STACK) test --test-arguments="--timeout=$(TEST_TIMEOUT) --num-threads $(TEST_NUM_THREADS)"

test_quiet: build
	$(STACK) test --test-arguments="--timeout=$(TEST_TIMEOUT) --num-threads $(TEST_NUM_THREADS) --hide-successes"

clean: clean_test
	$(STACK) clean

purge: clean_test
	$(STACK) purge

clean_test:
	rm -rf test/output/compiler/*
