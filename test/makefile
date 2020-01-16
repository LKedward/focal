# --- Source names ---
MODULES = Focal_Test_Utils
PROGS = testExample testMemoryTransfer testKernelSetup testLocalMem
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
	@sed '/Test outputs/q' $(REPORT)
	@echo  \(Open file: $(REPORT) to see full test outputs\)

BUILD:=debug
include $(FOCAL_DIR)/make.include

# --- Compiler Flags ---
FFLAGS=$(FOCAL_FFLAGS)

# --- Link Flags ---
ifeq ($(BUILD), release)
    FOCAL_LFLAGS ?= -L$(FOCAL_DIR)/lib -lfocal
else ifeq ($(BUILD), debug)
    FOCAL_LFLAGS ?= -L$(FOCAL_DIR)/lib -lfocaldbg
else
    $(error unrecognized build.)
endif
OPENCL_LFLAGS ?= -g -L$(OPENCL_DIR) -lOpenCL
LFLAGS = $(FOCAL_LFLAGS) $(OPENCL_LFLAGS)


# --- Cleanup (reset) ---
clean:
	rm -f $(PROGOBJS)
	rm -f $(KERNELOBJS)
	rm -f $(MODOBJS)
	rm -f $(EXEC)
	rm -f $(TESTOUT)

# Programs depend on modules
$(PROGOBJS): $(FOCAL_LIB_OBJS) $(MODOBJS)

# Recipe to link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o %.cl.o) $(MODOBJS)
	$(FC) $^ $(LFLAGS) -o $@

# Recipe to compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

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
	@echo "------------------------------------------------------" > $@
	@echo "          Focal library test suite results" >> $@
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
	@echo "*** Test outputs ***" >> $@
	@grep -T --binary-files=text . $^ | sed 's/$(OUTDIR)//g;s/.out//g;s/\///g' >> $@
	@echo "------------------------------------------------------" >> $@

$(OUTDIR):
	mkdir $(OUTDIR)

# Don't cleanup kernels object
.SECONDARY: $(KERNELOBJS)
