all: run_test_infer

build: build_cabal build_test_infer build_benchmark_infer build_profile_infer

run_test_infer: build_test_infer
	./test_Infer

build_test_infer:
	ghc -O2 -Wall --make test_Infer -i../src -i../bottlelib

run_benchmark_infer: build_benchmark_infer
	./benchmark_Infer

build_benchmark_infer:
	ghc -O2 -Wall --make benchmark_Infer -i../src -i../bottlelib

run_profile_infer: build_profile_infer
	./profile_Infer +RTS -p

build_profile_infer: build_benchmark_infer
	ghc -O2 -prof -hisuf p_hi -osuf p_o -Wall -auto-all -caf-all -rtsopts --make profile_Infer -i../src -i../bottlelib

build_cabal:
	make -C ..
