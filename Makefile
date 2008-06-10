# Targets: 

ML_ISA_SRC_FILES = $(shell ls isa_src/*.ML)
ML_ISAP_SRC_FILES = $(shell ls isap_src/*.ML)
ML_SYSTEM_FILES = $(shell ls ML-Systems/*.ML)



POLYML=poly
POLYML_SYSTEM_HEAP=heaps/polyml-5.2.polyml-heap
POLYML_ISA_HEAP=heaps/isalib.polyml-heap
POLYML_ISAP_HEAP=heaps/isaplib.polyml-heap

default: $(POLYML_ISAP_HEAP)

################

# make polyml heap

heaps/polyml-5.2.polyml-heap: $(ML_SYSTEM_FILES)
	echo 'use "ML-Systems/polyml.ML"; SaveState.saveState "$(POLYML_SYSTEM_HEAP)"; quit();' | $(POLYML)

heaps/polyml-5.1.polyml-heap: $(ML_SYSTEM_FILES)
	echo 'use "ML-Systems/polyml-5.1.ML"; SaveState.saveState "$(POLYML_SYSTEM_HEAP)"; quit();' | $(POLYML)

$(POLYML_ISA_HEAP): $(POLYML_SYSTEM_HEAP) $(ML_ISA_SRC_FILES)
	echo 'PolyML.SaveState.loadState "$(POLYML_SYSTEM_HEAP)"; use "isa_src/ROOT.ML"; PolyML.SaveState.saveState "$(POLYML_ISA_HEAP)"; quit();' | $(POLYML)
	@echo "Built polyml heap: $(POLYML_ISA_HEAP)"

$(POLYML_ISAP_HEAP): $(POLYML_ISA_HEAP) $(ML_ISAP_SRC_FILES)
	echo 'PolyML.SaveState.loadState "$(POLYML_ISA_HEAP)"; use "isap_src/ROOT.ML"; PolyML.SaveState.saveState "$(POLYML_ISAP_HEAP)"; quit();' | $(POLYML)
	@echo "Built polyml heap: $(POLYML_ISAP_HEAP)"

clean: 
	rm -f heaps/*.polyml-heap

#	@if test -e heaps/*.polyml-heap; then rm -f heaps/*.polyml-heap; echo "Removed heaps, now clean."; else echo "No heaps to remove, already clean."; fi