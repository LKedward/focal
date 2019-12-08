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
PROGS =
BASE = Focal clfortran Quicksort
SRCS = Error Memory Query Setup Utils
LIBS = focal focaldbg

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


# --- Link Flags ---
OPENCL_DIR ?= /usr/lib/
LFLAGS=  -L$(LIBDIR) -lfocal -L$(OPENCL_DIR) -lOpenCL

# --- Compiler flags ---
include make.compiler

# --- Recipes ---
all: $(DIRS) $(EXEC) $(LIB_OBJS)

examples: $(LIB_OBJS)
	cd examples; make $(MAKEFLAGS)

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
	rm -f $(BINDIR)*

docclean:
	rm -rf $(DOCDIR)*

# Install libraries
$(PREFIX)lib/%: $(LIBDIR)%
	cp "$<" "$@"

# Link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o)
	$(FC) $^ $(LFLAGS) -o $@

# Generate release library
$(LIBDIR)libfocal.a: $(BASE_OBJS) $(OBJS) $(OBJDIR)NoDebug.o
	rm -f $@
	$(AR) -cq $@ $^

# Generate debug library
$(LIBDIR)libfocaldbg.a: $(BASE_OBJS) $(OBJS) $(OBJDIR)Debug.o
		rm -f $@
		$(AR) -cq $@ $^

# Compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

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

.PHONY: all examples install uninstall doc clean docclean
