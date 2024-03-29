# --- Source names ---
MODULES = Focal_Test_Utils
PROGS = testExample testMemoryTransfer testPinnedMemory \
        testKernelSetup testLocalMem testSubBuffers testQueuePool \
		testEvents testProfiling testFileSource
REPORT = testSummary

# --- Output Directories ---
FOCAL_DIR ?= ..
OPENCL_DIR ?= /usr/lib/
BINDIR = $(FOCAL_DIR)/bin/
OBJDIR = $(FOCAL_DIR)/obj/
MODDIR = $(FOCAL_DIR)/mod/
OUTDIR = test_outputs

# --- Targets ---
MODOBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(MODULES)))
PROGOBJS =$(addprefix $(OBJDIR), $(addsuffix .o, $(PROGS)))
TESTOUT = $(addprefix $(OUTDIR)/, $(addsuffix .out, $(PROGS)))
KERNELOBJS = $(addprefix $(OBJDIR), $(addsuffix .cl.o, $(PROGS)) )
EXEC = $(addprefix $(BINDIR), $(PROGS))

# --- Main build target ---
all: $(EXEC) $(REPORT)
	@cat $(REPORT)
	@test $$(grep "EXIT_" test_outputs/* --binary-files=text | grep -c -v "EXIT_0\|EXIT_101") -eq 0

coverage: all

BUILD:=debug
include $(FOCAL_DIR)/make.include

# --- Compiler Flags ---
FFLAGS:=$(FOCAL_FFLAGS) 


# --- Link Flags ---
ifeq ($(BUILD), release)
    FOCAL_LFLAGS ?= -L$(FOCAL_DIR)/lib -lFocal
else ifeq ($(BUILD), debug)
    FOCAL_LFLAGS ?= -L$(FOCAL_DIR)/lib -lFocaldbg
else
    $(error unrecognized build.)
endif
OPENCL_LFLAGS ?= -g -L$(OPENCL_DIR) -lOpenCL
LFLAGS = $(FOCAL_LFLAGS) $(OPENCL_LFLAGS)

coverage: FOCAL_FFLAGS+=--coverage
coverage: LFLAGS+=--coverage



# --- Cleanup (reset) ---
clean:
	rm -f $(PROGOBJS)
	rm -f $(KERNELOBJS)
	rm -f $(MODOBJS)
	rm -f $(EXEC)
	rm -f $(TESTOUT)

# Programs depend on modules
$(PROGOBJS): $(MODOBJS)

# Module depends on focal
$(MODOBJS): $(FOCAL_LIB_OBJS)

# Recipe to link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o %.cl.o) $(MODOBJS) | $(BINDIR)
	$(FC) $^ $(LFLAGS) -o $@

# Recipe to compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Separate recipe for Focal library objects
# (needed for coverage flags)
$(OBJDIR)Focal_%.o: Focal_%.f90
	$(FC) $(FOCAL_FFLAGS) -c $< -o $@


# Recipe to 'compile' kernel source into a binary object
$(OBJDIR)%.cl.o: %.cl
	-mkdir build_$*_cl
	cp $< build_$*_cl/fclKernels.cl
	cd build_$*_cl; ld -r -b binary fclKernels.cl -o ../$@; rm fclKernels.cl
	rm -rf build_$*_cl

$(OUTDIR)/%.out: $(BINDIR)% | $(OUTDIR)
	@echo Running test: $*
	@-$< > $@ 2>&1 ; echo "$* : EXIT_$$?" >> $@ || true


$(REPORT): $(TESTOUT)
	@echo Generating report...
	@echo "*** Test outputs ***" > $@
	@grep -T --binary-files=text . $^ | sed 's/$(OUTDIR)//g;s/.out//g;s/\///g' >> $@
	@echo "------------------------------------------------------" >> $@
	@echo "          Focal library test suite results" >> $@
	@echo "------------------------------------------------------" >> $@
	@echo "Test environment:" >> $@
	@echo "$$(uname -a)" >> $@
	@echo "$$($(FC) --version)" >> $@
	@echo "------------------------------------------------------" >> $@
	@echo "NO_OF_TESTS: $$(ls -A test_outputs/* | wc -l)" >> $@
	@echo "TEST_SUCCESS: $$(cat $^ | grep -c EXIT_0)" >> $@
	@echo "TEST_FAILED: $$(grep "EXIT_" $^ --binary-files=text | grep -c -v "EXIT_0\|EXIT_101")" >> $@
	@echo "TEST_NOT_RUN: $$(cat $^ | grep -c EXIT_101)" >> $@
	@echo "" >> $@
	@echo "*** Failed tests ***" >> $@
	@grep -L "EXIT_0\|EXIT_101" $^ | sed 's/$(OUTDIR)//g;s/.out//g;s/\///g' >> $@
	@echo "" >> $@
	@echo "*** Tests not run ***" >> $@
	@grep -l EXIT_101 $^ | sed 's/$(OUTDIR)//g;s/.out//g;s/\///g' >> $@
	@echo "" >> $@
	@echo "------------------------------------------------------" >> $@

$(OUTDIR):
	mkdir $(OUTDIR)

$(BINDIR):
	mkdir $(BINDIR)

# Don't cleanup kernels object
.SECONDARY: $(KERNELOBJS)

.PHONY: all coverage clean
