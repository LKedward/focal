PROGS = platform_query sum
KERNELS = sum.cl
# --- Directories ---
FOCAL_DIR ?= ..
OPENCL_DIR ?= /usr/lib/
BINDIR = $(FOCAL_DIR)/bin/
OBJDIR = $(FOCAL_DIR)/obj/
MODDIR = $(FOCAL_DIR)/mod/

# --- Targets ---
FOCAL_LIB = $(addprefix $(FOCAL_DIR)/lib/, libfocal.a libfocaldbg.a)
EXEC = $(addprefix $(BINDIR), $(PROGS))
DIRS = $(MODDIR) $(BINDIR) $(OBJDIR)

# --- Compiler Flags ---
include $(FOCAL_DIR)/make.compiler

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

# --- Recipes ---
all: $(DIRS) $(EXEC)

clean:
	rm -f $(EXEC)

# Executables are dependent on focal libs
#$(EXEC): $(FOCAL_LIB)

# Link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o) fclKernels.o 
	$(FC) $^ $(LFLAGS) -o $@

# Compile fortran objects
$(OBJDIR)%.o: %.f90 $(FOCAL_LIB)
	$(FC) $(FFLAGS) -c $< -o $@

# Compile kernel binary resource
fclKernels.o: fclKernels.cl
	ld -r -b binary fclKernels.cl -o $@
	nm $@

# Concatenate kernel files
fclKernels.cl: $(KERNELS)
	cat $^ > fclKernels.cl
$(DIRS):
	mkdir $@
