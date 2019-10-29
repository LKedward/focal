# --- Configuration ---

# Directories
MODDIR = mod/
BINDIR = bin/
OBJDIR = obj/
DOCDIR = doc/
LIBDIR = lib/
PREFIX ?= /usr/local/

# Source directories
vpath %.f90 src
vpath %.f90 examples
vpath %.f90 external
vpath %.f90 external/clfortran

# Source files
PROGS = platform_query sum
BASE = Focal clfortran Quicksort
SRCS = Error Memory Query Setup Utils
LIBS = focal

# Compiler
PLATFORM ?= gnu
BUILD ?= release

# --- End Configuration ---

# Objects

BASE_OBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(BASE)) )
OBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(MODULES) $(SRCS) ) )
PROG_OBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(PROGS) ) )
EXEC = $(addprefix $(BINDIR), $(PROGS))
LIB_OBJS = $(addprefix $(LIBDIR)lib, $(addsuffix .a, $(LIBS)) )
EXECINSTALL = $(addprefix $(PREFIX)bin/, $(PROGS))
LIBINSTALL = $(addprefix $(PREFIX)lib/lib, $(addsuffix .a, $(LIBS)) )
DIRS = $(MODDIR) $(BINDIR) $(OBJDIR) $(DOCDIR) $(LIBDIR) $(PREFIX)lib


# Compiler standard flags

LFLAGS=  -L$(LIBDIR) -lfocal -L$(OPENCL_LIBRARY_PATH) -lOpenCL
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
all: $(DIRS) $(EXEC) $(LIB_OBJS)

install: all $(LIBINSTALL)

uninstall:
	rm -f $(addprefix $(PREFIX)bin/, $(PROGS))
	rm -f $(LIBINSTALL)

doc: $(DOCDIR)index.html

clean:
	rm -f $(OBJDIR)*.o
	rm -f $(MODDIR)*.mod
	rm -f $(MODDIR)*.smod
	rm -f $(LIBDIR)*.a
	rm -f $(EXEC)

docclean:
	rm -rf $(DOCDIR)*

# Install libraries
$(PREFIX)lib/%: $(LIBDIR)%
	cp "$<" "$@"

# Link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o)
	$(FC) $^ $(LFLAGS) -o $@

# Generate libraries
$(LIBDIR)%: $(BASE_OBJS) $(OBJS)
	$(AR) -cq $@ $^

# Compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Compile legacy fortran objects
$(OBJDIR)%.o: %.f
	$(FC) $(FFLAGS_LEGACY) -c $< -o $@

# Program objects depend on libraries
$(PROG_OBJS): $(LIB_OBJS)

# Library objects depend on code modules
$(LIB_OBJS): $(OBJS)

# Code modules depend on base modules
$(OBJS): $(BASE_OBJS)

$(DOCDIR)index.html: $(addsuffix .f90, $(OBJS) )
	ford ford.md

$(DIRS):
	mkdir $@
