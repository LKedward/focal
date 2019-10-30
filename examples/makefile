PROGS = platform_query sum

# --- Directories ---
FOCAL_DIR ?= ..
OPENCL_DIR ?= /usr/lib/
BINDIR = $(FOCAL_DIR)/bin/
OBJDIR = $(FOCAL_DIR)/obj/
MODDIR = $(FOCAL_DIR)/mod/

# --- Targets ---
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
$(BINDIR)%: $(addprefix $(OBJDIR), %.o)
	$(FC) $^ $(LFLAGS) -o $@

# Compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(DIRS):
	mkdir $@
