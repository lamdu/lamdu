GHC=ghc
GHC_FLAGS=-O2 -Wall -Werror -XScopedTypeVariables

all: run_test_infer

build: build_cabal build_test_stuff

build_test_stuff: build_test_infer build_benchmark_infer build_profile_infer

run_test_infer:
	runghc -i.. -i../bottlelib test_Infer.hs

run_optimized_test_infer: build_test_infer
	./test_Infer

build_test_infer:
	${GHC} ${GHC_FLAGS} --make test_Infer -i.. -i../bottlelib

build_benchmark_infer:
	${GHC} ${GHC_FLAGS} --make benchmark_Infer -i.. -i../bottlelib

build_profile_infer: build_benchmark_infer
	${GHC} ${GHC_FLAGS} -prof -hisuf p_hi -osuf p_o -Wall -auto-all -caf-all -rtsopts --make profile_Infer -i.. -i../bottlelib

run_benchmark_infer: build_benchmark_infer
	./benchmark_Infer

run_profile_infer: build_profile_infer
	./profile_Infer +RTS -p

build_cabal:
	make -C ..
