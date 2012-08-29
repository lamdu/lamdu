all: run_test_typed

build: build_test_typed build_benchmark_typed

run_test_typed: build_test_typed
	./test_Typed

build_test_typed:
	ghc -Wall --make test_Typed -i../codeedit -i../bottlelib

run_benchmark_typed: build_benchmark_typed
	./benchmark_Typed

build_benchmark_typed:
	ghc -Wall --make benchmark_Typed -i../codeedit -i../bottlelib
