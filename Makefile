# Targets: 

ML_BASIC_SRC_FILES = $(shell ls basics/*.ML General/*.ML project/*.ML)
ML_NAMES_SRC_FILES = $(shell ls names/*.ML names/test/*.ML)
ML_GRAPH_SRC_FILES = $(shell ls graph/*.ML graph/test/*.ML)
ML_SEARCH_SRC_FILES = $(shell ls search/*.ML)
ML_PARSER_SRC_FILES = $(shell ls parser/*.ML parser/examples/*.ML)
ML_SYSTEM_FILES = $(shell ls ML-Systems/*.ML)
ML_ALL_FILES= $(ML_SYSTEM_FILES) $(ML_PARSER_SRC_FILES) $(ML_GRAPH_SRC_FILES) $(ML_NAMES_SRC_FILES) $(ML_BASIC_SRC_FILES)


POLYML=poly
POLYML_SYSTEM_HEAP=polyml.polyml-heap
POLYML_BASIC_HEAP=basic.polyml-heap
POLYML_NAMES_HEAP=names.polyml-heap
POLYML_GRAPH_HEAP=graph.polyml-heap
POLYML_SEARCH_HEAP=search.polyml-heap
POLYML_ALL_HEAP=all.polyml-heap

default: heaps/$(POLYML_ALL_HEAP)

################

# make polyml heap

heaps/$(POLYML_SYSTEM_HEAP): $(ML_SYSTEM_FILES)
	mkdir -p heaps
	echo 'PolyML.use "ML-Systems/polyml.ML"; PolyML.fullGC (); SaveState.saveState "heaps/$(POLYML_SYSTEM_HEAP)"; OS.Process.exit OS.Process.success;' | $(POLYML)

heaps/$(POLYML_BASIC_HEAP): heaps/$(POLYML_SYSTEM_HEAP) $(ML_BASIC_SRC_FILES)
	mkdir -p heaps
	echo 'PolyML.SaveState.loadState "heaps/$(POLYML_SYSTEM_HEAP)"; do_and_exit_or_die (fn () => (cd "basic"; PolyML.use "ROOT.ML"; cd ".."; PolyML.fullGC (); PolyML.SaveState.saveState "heaps/$(POLYML_BASIC_HEAP)"));' | $(POLYML)
	@echo "Built polyml heap: $(POLYML_BASIC_HEAP)"

heaps/$(POLYML_ALL_HEAP): $(ML_ALL_FILES) 
	mkdir -p heaps
	echo 'use "ROOT.ML"; PolyML.fullGC (); PolyML.SaveState.saveState "heaps/$(POLYML_ALL_HEAP)";' | $(POLYML)
	@echo "Built polyml heap: $(POLYML_ALL_HEAP)"

run-$(POLYML_BASIC_HEAP): heaps/$(POLYML_SYSTEM_HEAP) $(ML_BASIC_SRC_FILES)
	(echo 'PolyML.SaveState.loadState "heaps/$(POLYML_SYSTEM_HEAP)"; cd "basic"; use "ROOT.ML"; cd "..";'; cat) | $(POLYML)

run-$(POLYML_ALL_HEAP): $(ML_ALL_FILES)
	(echo 'PolyML.SaveState.loadState "heaps/$(POLYML_BASIC_HEAP)"; use "ROOT.ML";'; cat) | $(POLYML)

run-all: run-$(POLYML_ALL_HEAP)
run-basic: run-$(POLYML_BASIC_HEAP)
run: run-$(POLYML_ALL_HEAP)

clean: 
	rm -f heaps/*.polyml-heap
	find -type d -name .polysave | xargs rm -rf

#	@if test -e heaps/*.polyml-heap; then rm -f heaps/*.polyml-heap; echo "Removed heaps, now clean."; else echo "No heaps to remove, already clean."; fi
