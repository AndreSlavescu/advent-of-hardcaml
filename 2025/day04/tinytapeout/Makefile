DESIGN_NAME = tt_um_day04
PDK_ROOT ?= $(HOME)/.volare

DOCKER_CMD = docker run --rm \
	-v $(PWD):/work \
	-v $(PDK_ROOT):/pdk \
	-e PDK_ROOT=/pdk \
	-e PDK=sky130A \
	-w /work \
	ghcr.io/efabless/openlane2:latest

.PHONY: all clean verify synth

all: synth

src/day04.v: ../hardcaml/verilator/day04.v
	mkdir -p src && cp $< $@

verify: src/day04.v
	python3 verify.py

synth: src/day04.v
	$(DOCKER_CMD) openlane config.json --flow Classic

clean:
	rm -rf runs/ src/day04.v
