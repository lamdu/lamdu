all: run_test_typed

run_test_typed:
	ghc -Wall --make test_Typed -i../codeedit -i../bottlelib
	./test_Typed

run_benchmark_typed:
	ghc -Wall --make benchmark_Typed -i../codeedit -i../bottlelib
	./benchmark_Typed
