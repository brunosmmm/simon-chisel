IMPLEMENTED_MODULES=SimonCore SimonKeyExpander SimonRound RotateUnit
GENERATED_FILES=$(foreach MOD,$(IMPLEMENTED_MODULES),$(MOD)_*.v $(MOD).fir $(MOD).anno.json)

%.v:
	sbt "runMain SimonAcc.$(*F)Driver --split-modules"

clean:
	rm -rf $(GENERATED_FILES)

.PHONY: clean
