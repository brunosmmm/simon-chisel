IMPLEMENTED_MODULES=SimonCore SimonKeyExpander SimonRound
GENERATED_FILES=$(foreach MOD,$(IMPLEMENTED_MODULES),$(MOD).v $(MOD).fir $(MOD).anno.json)

%.v:
	sbt "runMain SimonAcc.$(*F)Driver --split-modules"

clean:
	rm -rf $(GENERATED_FILES)

.PHONY: clean
