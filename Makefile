.PHONY: lint format lint-cpp lint-ocaml format-cpp format-ocaml

CPP_FILES := $(shell find . -type f \( -name '*.cpp' -o -name '*.h' \) -not -path '*/_build/*' -not -path '*/obj_dir/*' -not -path './.git/*')
OCAML_FILES := $(shell find . -type f \( -name '*.ml' -o -name '*.mli' \) -not -path '*/_build/*' -not -path './.git/*')

lint: lint-cpp lint-ocaml

format: format-cpp format-ocaml

lint-cpp:
	@command -v clang-format >/dev/null 2>&1 || { echo "clang-format not found"; exit 1; }
	@if [ -z "$(CPP_FILES)" ]; then echo "No *.cpp files found"; exit 0; fi
	clang-format --dry-run --Werror $(CPP_FILES)

format-cpp:
	@command -v clang-format >/dev/null 2>&1 || { echo "clang-format not found"; exit 1; }
	@if [ -z "$(CPP_FILES)" ]; then echo "No *.cpp files found"; exit 0; fi
	clang-format -i $(CPP_FILES)

lint-ocaml:
	@command -v ocamlformat >/dev/null 2>&1 || { echo "ocamlformat not found"; exit 1; }
	@if [ -z "$(OCAML_FILES)" ]; then echo "No *.ml or *.mli files found"; exit 0; fi
	ocamlformat --check --profile=janestreet --enable-outside-detected-project $(OCAML_FILES)

format-ocaml:
	@command -v ocamlformat >/dev/null 2>&1 || { echo "ocamlformat not found"; exit 1; }
	@if [ -z "$(OCAML_FILES)" ]; then echo "No *.ml or *.mli files found"; exit 0; fi
	ocamlformat --profile=janestreet --enable-outside-detected-project -i $(OCAML_FILES)
