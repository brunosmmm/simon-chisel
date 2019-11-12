IMPLEMENTED_MODULES=SimonCore SimonKeyExpander SimonRound RotateUnit
GENERATED_FILES=$(foreach MOD,$(IMPLEMENTED_MODULES),$(MOD)*.v $(MOD).fir $(MOD).anno.json)
TARGET?=.

%.v:
	mkdir -p $(TARGET)/$(*F) &&\
	sbt "runMain SimonAcc.$(*F)Driver --split-modules --target-dir $(TARGET)/$(*F)"

clean:
	rm -rf $(foreach MOD,$(IMPLEMENTED_MODULES),$(TARGET)/$(MOD))

.PHONY: clean
