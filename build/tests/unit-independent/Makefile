## Makefile
## Compiles tests for unit-independent and runs
## Updated: 2025-07-21

FC := mpifort
SOURCE_DIR := ../../source
VPATH := $(SOURCE_DIR) $(SOURCE_DIR)/elmfire

TEST := test_half_superbee
FFLAGS := -O0 -g -Wall -cpp -ffree-line-length-none

# List of source files required by this test
SOURCE_FILES := \
  elmfire_vars.f90 \
  elmfire_namelists.f90 \
  elmfire_subs.f90 \
  elmfire_spotting.f90 \
  elmfire_spotting_superseded.f90 \
  elmfire_io.f90 \
  elmfire_spread_rate.f90 \
  elmfire_vars.f90 \
  elmfire_level_set.f90

# Convert .f90 files to .o files
SOURCE_OBJ := $(SOURCE_FILES:.f90=.o)
TEST_OBJ := $(TEST).o

all: $(TEST)

$(TEST): $(TEST_OBJ) $(SOURCE_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

run: all
	@echo "Running test..."
	@./$(TEST)

clean:
	rm -f *.o *.mod $(TEST)