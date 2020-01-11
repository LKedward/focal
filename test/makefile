# --- Source names ---
MODULES = Focal_Test_Utils
PROGS = testExample testMemoryTransfer testKernelSetup
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

# Error code substitution command
EC_CMD = $(shell awk -F : '/^[^\#]/ {gsub("\"","");print "s/" $$1 "/" $$2 "/g"}' ORS=";" error_codes)

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
$(BINDIR)%: $(addprefix $(OBJDIR), %.o testKernels.cl.o) $(MODOBJS)
	$(FC) $^ $(LFLAGS) -o $@

# Recipe to compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Recipe to 'compile' kernel source into a binary object
$(OBJDIR)%.cl.o: %.cl
	cp $< fclKernels.cl
	ld -r -b binary fclKernels.cl -o $@
	rm fclKernels.cl
	nm $@

$(OUTDIR)/%.out: $(BINDIR)% | $(OUTDIR)
	@echo Running test: $*
	@-$< > $@ 2>&1 ; echo "$* : EXIT_$$?" >> $@ || true
	@sed '$(EC_CMD)' -i $@


$(REPORT): $(TESTOUT)
	@echo Generating report...
	@echo "------------------------------------------------------" > $@
	@echo "          Focal library test suite results" >> $@
	@echo "------------------------------------------------------" >> $@
	@echo "TEST_SUCCESS: $$(cat $^ | grep -c TEST_SUCCESS)" >> $@
	@echo "TEST_FAILED: $$(cat $^ | grep -c "EXIT_\|TEST_FAILED")" >> $@
	@echo "TEST_NOT_RUN: $$(cat $^ | grep -c TEST_NOT_RUN)" >> $@
	@echo "" >> $@
	@echo "*** Failed tests ***" >> $@
	@grep -l "EXIT_\|TEST_FAILED" $^ | sed 's/$(OUTDIR)//g;s/.out//g;s/\///g' >> $@
	@echo "" >> $@
	@echo "*** Tests not run ***" >> $@
	@grep -l TEST_NOT_RUN $^ | sed 's/.out//g' >> $@
	@echo "" >> $@
	@echo "*** Test outputs ***" >> $@
	@grep -T . $^ | sed 's/$(OUTDIR)//g;s/.out//g;s/\///g' >> $@
	@echo "------------------------------------------------------" >> $@

$(OUTDIR):
	mkdir $(OUTDIR)

# Don't cleanup kernels object
.SECONDARY: $(OBJDIR)testKernels.cl.o
