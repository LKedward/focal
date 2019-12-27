# --- Source names ---
PROGS = platform_query sum nbody

# --- Output Directories ---
FOCAL_DIR ?= ..
OPENCL_DIR ?= /usr/lib/
BINDIR = $(FOCAL_DIR)/bin/
OBJDIR = $(FOCAL_DIR)/obj/
MODDIR = $(FOCAL_DIR)/mod/

# --- Targets ---
PROGOBJS =$(addprefix $(OBJDIR), $(addsuffix .o, $(PROGS)))
KERNELOBJS = $(addprefix $(OBJDIR), $(addsuffix .cl.o, $(PROGS)) )
EXEC = $(addprefix $(BINDIR), $(PROGS))


# --- Main build target ---
all: $(EXEC)

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
	rm -f $(EXEC)

# Programs depend on modules
$(PROGOBJS): $(FOCAL_LIB_OBJS)

# Recipe to link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o %.cl.o)
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


