
FOCAL_DIR =.

# --- Main build target ---
all: focal

# --- Include Focal non-recursive sub makefile ----
include $(FOCAL_DIR)/make.include

# --- Cleanup (reset) ---
clean: focal_clean

examples: $(LIB_OBJS)
	$(MAKE) -C examples

test: $(LIB_OBJS)
	$(MAKE) -C test

.PHONY: all clean examples test
