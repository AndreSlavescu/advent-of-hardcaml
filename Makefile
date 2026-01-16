.PHONY: lint format lint-cpp lint-ocaml format-cpp format-ocaml

CXX ?= g++

CPP_FILES := $(shell find . -type f \( -name '*.cpp' -o -name '*.h' \) -not -path '*/_build/*' -not -path '*/obj_dir/*' -not -path './.git/*')
CPP_UNUSED_SRCS := $(shell find . -type f -name '*.cpp' -not -path '*/_build/*' -not -path '*/obj_dir/*' -not -path './.git/*' -not -path '*/verilator/*')
OCAML_FILES := $(shell find . -type f \( -name '*.ml' -o -name '*.mli' \) -not -path '*/_build/*' -not -path './.git/*')
DUNE_DIRS := $(shell find . -name dune-project -not -path './.git/*' -exec dirname {} \;)

lint: lint-cpp lint-ocaml

format: format-cpp format-ocaml

lint-cpp:
	@command -v clang-format >/dev/null 2>&1 || { echo "clang-format not found"; exit 1; }
	@command -v $(CXX) >/dev/null 2>&1 || { echo "$(CXX) not found"; exit 1; }
	@if [ -z "$(CPP_FILES)" ]; then echo "No *.cpp files found"; exit 0; fi
	clang-format --dry-run --Werror $(CPP_FILES)
	@if [ -n "$(CPP_UNUSED_SRCS)" ]; then $(CXX) -std=c++20 -Wall -Wextra -Werror -Wunused -fsyntax-only $(CPP_UNUSED_SRCS); fi

format-cpp:
	@command -v clang-format >/dev/null 2>&1 || { echo "clang-format not found"; exit 1; }
	@if [ -z "$(CPP_FILES)" ]; then echo "No *.cpp files found"; exit 0; fi
	clang-format -i $(CPP_FILES)

lint-ocaml:
	@command -v ocamlformat >/dev/null 2>&1 || { echo "ocamlformat not found"; exit 1; }
	@if [ -z "$(OCAML_FILES)" ]; then echo "No *.ml or *.mli files found"; exit 0; fi
	ocamlformat --check --profile=janestreet --enable-outside-detected-project $(OCAML_FILES)
	@command -v dune >/dev/null 2>&1 || { echo "dune not found"; exit 1; }
	@if [ -z "$(DUNE_DIRS)" ]; then echo "No dune projects found"; else \
		for d in $(DUNE_DIRS); do \
			(cd $$d && OCAMLPARAM="_,warn-error=+26+27+32+33+34+35+36+37+38+39+60+66+67+69+71" dune build); \
		done; \
	fi

format-ocaml:
	@command -v ocamlformat >/dev/null 2>&1 || { echo "ocamlformat not found"; exit 1; }
	@if [ -z "$(OCAML_FILES)" ]; then echo "No *.ml or *.mli files found"; exit 0; fi
	ocamlformat --profile=janestreet --enable-outside-detected-project -i $(OCAML_FILES)
