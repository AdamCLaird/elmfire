## Makefile
## Compiles Fortran scripts for all test runs
## Updated: 07-24-2025

FC := mpifort
FFLAGS := -O0 -g -Wall -cpp -ffree-line-length-none -D_WUI \
          -Wno-conversion \
          -Wno-unused-dummy-argument \
          -Wno-character-truncation \
          -Wno-unused-variable \
          -Wno-unused-label

BIN_DIR := bin
VPATH := ../source \
	./00-unit-independent \
		./00-unit-independent/elmfire_level_set \
		./00-unit-independent/elmfire_spread_rate \
		./00-unit-independent/elmfire_subs \
	./01-unit-dependent \
		./01-unit-dependent/elmfire_level_set \
		./01-unit-dependent/elmfire_spread_rate \
	./02-component \
    ./03-integration \
	./04-regression \
	./05-performance

# List of test scripts by dir (append for new tests)
00-UNIT_INDEPENDENT/ELMFIRE_LEVEL_SET := \
	test_calc_cfl \
	test_calc_normal_vectors \
	test_half_superbee \
	test_rk2_integrate

00-UNIT_INDEPENDENT/ELMFIRE_SPREAD_RATE := \
	test_ellipse_ucb \
	test_hamada \
	test_surface_spread_rate

00-UNIT_INDEPENDENT/ELMFIRE_SUBS := \
	test_icol_from_x

01-UNIT_DEPENDENT/ELMFIRE_LEVEL_SET := \
	test_limit_gradients \
	test_tag_band

01-UNIT_DEPENDENT/ELMFIRE_SPREAD_RATE :=

02-COMPONENT :=

03-INTEGRATION :=

04-REGRESSION :=

05-PERFORMANCE :=

# Compile all test files
ALL_TEST_NAMES := \
	$(00-UNIT_INDEPENDENT/ELMFIRE_LEVEL_SET) \
	$(00-UNIT_INDEPENDENT/ELMFIRE_SPREAD_RATE) \
	$(00-UNIT_INDEPENDENT/ELMFIRE_SUBS) \
	$(01-UNIT_DEPENDENT/ELMFIRE_LEVEL_SET) \
	$(01-UNIT_DEPENDENT/ELMFIRE_SPREAD_RATE) \
	$(02-COMPONENT) \
	$(03-INTEGRATION) \
	$(04-REGRESSION) \
	$(05-PERFORMANCE)
ALL_TESTS := $(addprefix $(BIN_DIR)/, $(ALL_TEST_NAMES))

# List of ELMFIRE source files
SOURCE := \
  elmfire_vars.f90 \
  elmfire_namelists.f90 \
  elmfire_subs.f90 \
  elmfire_spotting.f90 \
  elmfire_spotting_superseded.f90 \
  elmfire_io.f90 \
  elmfire_spread_rate.f90 \
  elmfire_level_set.f90
SOURCE_OBJ := $(addprefix $(BIN_DIR)/, $(SOURCE:.f90=.o))

# Make bin directory
$(BIN_DIR):
	mkdir -p $@

# Compile source files into object files
$(BIN_DIR)/%.o: %.f90 | $(BIN_DIR)
	cd $(BIN_DIR) && $(FC) $(FFLAGS) -J. -I. -c ../$< -o $(@F)

# Compile any test binary from any VPATH subdir
$(BIN_DIR)/%: %.f90 $(SOURCE_OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) -I$(BIN_DIR) -o $@ $< $(SOURCE_OBJ)

# Build all
all: $(ALL_TESTS)

# Run all test binaries
run: all
	@for test in $(ALL_TEST_NAMES); do \
	  echo " "; \
	  echo "Running $$test..."; \
	  ./$(BIN_DIR)/$$test || exit 1; \
	done

# Clean and recreate bin dir
clean:
	rm -rf $(BIN_DIR)
	mkdir -p $(BIN_DIR)

.PHONY: build_objects
build_objects: $(SOURCE_OBJ)