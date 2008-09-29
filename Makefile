# Targets: 

ML_ISA_SRC_FILES = $(shell ls isa_src/*.ML)
ML_ISAP_SRC_FILES = $(shell ls isap_src/*.ML)
ML_PARSER_SRC_FILES = $(shell ls parser/*.ML)
ML_SYSTEM_FILES = $(shell ls ML-Systems/*.ML)



POLYML=poly
POLYML_SYSTEM_HEAP=polyml-5.2.polyml-heap
POLYML_ISA_HEAP=isalib.polyml-heap
POLYML_ISAP_HEAP=isaplib.polyml-heap

default: heaps/$(POLYML_ISAP_HEAP)

################

# make polyml heap

heaps/polyml-5.2.polyml-heap: $(ML_SYSTEM_FILES)
	echo 'use "ML-Systems/polyml.ML"; SaveState.saveState "heaps/$(POLYML_SYSTEM_HEAP)"; quit();' | $(POLYML)

heaps/polyml-5.1.polyml-heap: $(ML_SYSTEM_FILES)
	echo 'use "ML-Systems/polyml-5.1.ML"; SaveState.saveState "heaps/$(POLYML_SYSTEM_HEAP)"; quit();' | $(POLYML)

heaps/$(POLYML_ISA_HEAP): heaps/$(POLYML_SYSTEM_HEAP) $(ML_ISA_SRC_FILES)
	echo 'PolyML.SaveState.loadState "heaps/$(POLYML_SYSTEM_HEAP)"; cd "isa_src"; use "ROOT.ML"; cd ".."; PolyML.SaveState.saveState "heaps/$(POLYML_ISA_HEAP)"; quit();' | $(POLYML)
	@echo "Built polyml heap: $(POLYML_ISA_HEAP)"

heaps/$(POLYML_ISAP_HEAP): heaps/$(POLYML_ISA_HEAP) $(ML_ISAP_SRC_FILES) $(ML_PARSER_SRC_FILES)
	echo 'PolyML.SaveState.loadState "heaps/$(POLYML_ISA_HEAP)"; cd "isap_src"; use "ROOT.ML"; cd "../parser"; use "ROOT.ML"; cd ".."; PolyML.SaveState.saveState "heaps/$(POLYML_ISAP_HEAP)"; quit();' | $(POLYML)
	@echo "Built polyml heap: $(POLYML_ISAP_HEAP)"

run-$(POLYML_ISAP_HEAP): heaps/$(POLYML_ISAP_HEAP)
	./bin/polyml-isaplib

run-$(POLYML_ISA_HEAP): heaps/$(POLYML_ISA_HEAP)
	./bin/polyml-isalib

run-isap: run-$(POLYML_ISAP_HEAP)
run-isa: run-$(POLYML_ISA_HEAP)
run: run-$(POLYML_ISAP_HEAP)

clean: 
	rm -f heaps/*.polyml-heap

#	@if test -e heaps/*.polyml-heap; then rm -f heaps/*.polyml-heap; echo "Removed heaps, now clean."; else echo "No heaps to remove, already clean."; fi