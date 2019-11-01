PROGS = platform_query sum
KERNELS = sum.cl
# --- Directories ---
FOCAL_DIR ?= ..
OPENCL_DIR ?= /usr/lib/
BINDIR = $(FOCAL_DIR)/bin/
OBJDIR = $(FOCAL_DIR)/obj/
MODDIR = $(FOCAL_DIR)/mod/

# --- Targets ---
FOCAL_LIB = $(FOCAL_DIR)/lib/libfocal.a
EXEC = $(addprefix $(BINDIR), $(PROGS))
DIRS = $(MODDIR) $(BINDIR) $(OBJDIR)

# --- Link Flags ---
FOCAL_LFLAGS ?= -L$(FOCAL_DIR)/lib -lfocal
OPENCL_LFLAGS ?= -L$(OPENCL_DIR) -lOpenCL
LFLAGS = $(FOCAL_LFLAGS) $(OPENCL_LFLAGS)

# --- Compiler Flags ---
include $(FOCAL_DIR)/make.compiler

# --- Recipes ---
all: $(DIRS) $(EXEC)

clean:
	rm -f $(EXEC)

# Link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o) fclKernels.o $(FOCAL_LIB)
	$(FC) $^ $(LFLAGS) -o $@

# Compile fortran objects
$(OBJDIR)%.o: %.f90 
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
