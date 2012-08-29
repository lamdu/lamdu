all: run_test_typed

build: build_test_typed build_benchmark_typed

run_test_typed: build_test_typed
	./test_Typed

build_test_typed:
	ghc -O2 -Wall --make test_Typed -i../codeedit -i../bottlelib

run_benchmark_typed: build_benchmark_typed
	./benchmark_Typed

build_benchmark_typed:
	ghc -O2 -Wall --make benchmark_Typed -i../codeedit -i../bottlelib

run_profile_typed: build_profile_typed
	./profile_Typed

build_profile_typed:
	ghc -Wall -auto-all -caf-all -rtsopts --make benchmark_Typed -i../codeedit -i../bottlelib
