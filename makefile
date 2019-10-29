# --- Configuration ---

# Directories
MODDIR = mod/
BINDIR = bin/
OBJDIR = obj/
DOCDIR = doc/
PREFIX ?= /usr/local/

# Source directories
vpath %.f90 src
vpath %.f90 examples
vpath %.f90 src/external

# Source files
PROGS = platform_query sum
BASE = Focal clfortran Quicksort
MODULES =
EXTERNAL =
SRCS = Error Memory Query Setup Utils

# Compiler
PLATFORM ?= gnu
BUILD ?= release

# Libraries
LD_LIBRARY_OPENCL="/c/Program Files/NVIDIA GPU Computing Toolkit/CUDA/v10.1/lib/x64/"

# --- End Configuration ---

# Objects

MODULE_HEADERS = $(addsuffix _h, $(MODULES))
BASE_OBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(BASE)) )
OBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(MODULES) $(EXTERNAL) $(SRCS) ) )
PROG_OBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(PROGS) ) )
HEADER_OBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(MODULE_HEADERS)) )
EXEC = $(addprefix $(BINDIR), $(PROGS))
EXECINSTALL = $(addprefix $(PREFIX)bin/, $(PROGS))
DIRS = $(MODDIR) $(BINDIR) $(OBJDIR) $(DOCDIR)


# Compiler standard flags

LFLAGS= -L$(LD_LIBRARY_OPENCL) -lopenCL
ifeq ($(PLATFORM), gnu)
	FC=gfortran
	FFLAGS += -std=f2008 -fimplicit-none -J$(MODDIR)
	FFLAGS_LEGACY = -fimplicit-none -J$(MODDIR)

else ifeq ($(PLATFORM), intel)
	FC=ifort
	FFLAGS += -stand:f08 -module:$(MODDIR)
	FFLAGS_LEGACY = $(FFLAGS)

else
  $(error unrecognized platform.)
endif


# Compile debug flags
ifeq ($(PLATFORM)-$(BUILD), gnu-debug)
	FFLAGS += -g -Og -C -Wall -fbounds-check -fbacktrace -fno-realloc-lhs -ffpe-trap=invalid,zero,overflow

else ifeq ($(PLATFORM)-$(BUILD), intel-debug)
	FFLAGS += -g -O0 -check all -debug all -traceback -fpe0

else ifeq ($(PLATFORM)-$(BUILD), gnu-release)
	FFLAGS += -O3 -flto

else ifeq ($(PLATFORM)-$(BUILD), intel-release)
	FFLAGS += -O3 -ipo

else
  $(error unrecognized build target.)
endif


# --- Recipes ---
all: $(DIRS) $(EXEC)

install: all $(EXECINSTALL)

uninstall:
	rm -f $(addprefix $(PREFIX)bin/, $(PROGS))

doc: $(DOCDIR)index.html

clean:
	rm -f $(OBJDIR)*.o
	rm -f $(MODDIR)*.mod
	rm -f $(MODDIR)*.smod
	rm -f $(EXEC)

docclean:
	rm -rf $(DOCDIR)*

# Install programs
$(PREFIX)bin/%: $(BINDIR)%
	cp "$<" "$@"

# Link objects
$(BINDIR)%: $(BASE_OBJS) $(OBJS) $(HEADER_OBJS) $(addprefix $(OBJDIR), %.o)
	$(FC) $^ $(LFLAGS) -o $@

# Compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Compile legacy fortran objects
$(OBJDIR)%.o: %.f
	$(FC) $(FFLAGS_LEGACY) -c $< -o $@

# Program objects depend on code modules
$(PROG_OBJS): $(OBJS)

# Code modules depend on base modules
$(OBJS): $(BASE_OBJS)

# # Code modules depend on interface modules
# $(OBJS): $(HEADER_OBJS)

# # Interface modules depend on base module
# $(HEADER_OBJS): $(BASE_OBJS)

$(DOCDIR)index.html: $(addsuffix .f90, $(PROGS) $(MODULES) $(MODULE_HEADERS))
	ford ford.md

$(DIRS):
	mkdir $@
