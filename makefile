
FOCAL_DIR =.

# --- Main build target ---
all: focal

# --- Include Focal non-recursive sub makefile ----
include $(FOCAL_DIR)/make.include

# --- Cleanup (reset) ---
clean: focal_clean

examples: $(LIB_OBJS)
	cd examples; make

test: $(LIB_OBJS)
	cd test; make

.PHONY: all clean examples test
