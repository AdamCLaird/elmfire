## Makefile
## Compiles Fortran scripts for all test runs
## Updated: 07-22-2025

FC := mpifort
FFLAGS := -O0 -g -Wall -cpp -ffree-line-length-none -D_WUI\
          -Wno-conversion \
          -Wno-unused-dummy-argument \
          -Wno-character-truncation \
          -Wno-unused-variable \
          -Wno-unused-label
SCRATCH_DIR := scratch
VPATH := ../source ./unit-independent ./unit-dependent ./component \
./integration ./regression

# List of test scripts broken down by test type
UNIT_INDEPENDENT := \
test_calc_cfl \
test_calc_normal_vectors \
test_ellipse_ucb \
test_half_superbee \
test_rk2_integrate 
UNIT_DEPENDENT   := \
test_tag_band
COMPONENT := 
INTEGRATION := 
REGRESSION := 

ALL_TEST_NAMES := $(UNIT_INDEPENDENT) $(UNIT_DEPENDENT) # $(COMPONENT) $(INTEGRATION) $(REGRESSION)
ALL_TESTS := $(addprefix $(SCRATCH_DIR)/, $(ALL_TEST_NAMES))

SOURCE := \
  elmfire_vars.f90 \
  elmfire_namelists.f90 \
  elmfire_subs.f90 \
  elmfire_spotting.f90 \
  elmfire_spotting_superseded.f90 \
  elmfire_io.f90 \
  elmfire_spread_rate.f90 \
  elmfire_level_set.f90

$(SCRATCH_DIR):
	mkdir -p $@

SOURCE_OBJ := $(addprefix $(SCRATCH_DIR)/, $(SOURCE:.f90=.o))

# Build rule for test binaries
$(SCRATCH_DIR)/%: unit-independent/%.f90 $(SOURCE_OBJ) | $(SCRATCH_DIR)
	$(FC) $(FFLAGS) -I$(SCRATCH_DIR) -o $@ $< $(SOURCE_OBJ)

$(SCRATCH_DIR)/%: unit-dependent/%.f90 $(SOURCE_OBJ) | $(SCRATCH_DIR)
	$(FC) $(FFLAGS) -I$(SCRATCH_DIR) -o $@ $< $(SOURCE_OBJ)

$(SCRATCH_DIR)/%: component/%.f90 $(SOURCE_OBJ) | $(SCRATCH_DIR)
	$(FC) $(FFLAGS) -I$(SCRATCH_DIR) -o $@ $< $(SOURCE_OBJ)

$(SCRATCH_DIR)/%: integration/%.f90 $(SOURCE_OBJ) | $(SCRATCH_DIR)
	$(FC) $(FFLAGS) -I$(SCRATCH_DIR) -o $@ $< $(SOURCE_OBJ)

$(SCRATCH_DIR)/%: regression/%.f90 $(SOURCE_OBJ) | $(SCRATCH_DIR)
	$(FC) $(FFLAGS) -I$(SCRATCH_DIR) -o $@ $< $(SOURCE_OBJ)

# Compile source files into object files
$(SCRATCH_DIR)/%.o: %.f90 | $(SCRATCH_DIR)
	cd $(SCRATCH_DIR) && $(FC) $(FFLAGS) -J. -I. -c ../$< -o $(@F)

# Build all
all: $(ALL_TESTS)

# Run all test binaries
run: all
	@for test in $(ALL_TEST_NAMES); do \
	  echo " "; \
	  echo "Running $$test..."; \
	  ./$(SCRATCH_DIR)/$$test || exit 1; \
	done

# Clean and recreate scratch dir
clean:
	rm -rf $(SCRATCH_DIR)
	mkdir -p $(SCRATCH_DIR)