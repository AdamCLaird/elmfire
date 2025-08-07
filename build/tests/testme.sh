#!/bin/bash
## testme.sh
## Run file for executing automated testing suite
## Updated: 07-24-2025

set -e

COMMAND=$1
TARGET=$2

BIN="bin"
TEST_DIRS=( \
    "00-unit-independent/elmfire_init" \
    "00-unit-independent/elmfire_level_set" \
    "00-unit-independent/elmfire_spotting" \
    "00-unit-independent/elmfire_spread_rate" \
    "00-unit-independent/elmfire_subs" \
    "01-unit-dependent/elmfire_level_set" \
    "01-unit-dependent/elmfire_spread_rate" \
    "02-component" \
    "03-integration" \
    "04-regression" \
    "05-performance" \
)

# Function for running individual test files
run_test() {
    local TEST_NAME="test_$1"
    local TEST_BIN="${BIN}/${TEST_NAME}"
    local FOUND_SRC=""

    for dir in "${TEST_DIRS[@]}"; do
        if [[ -f "${dir}/${TEST_NAME}.f90" ]]; then
            FOUND_SRC="${dir}/${TEST_NAME}.f90"
            break
        fi
    done

    if [[ -z "$FOUND_SRC" ]]; then
        echo "Error: Test source for '$TEST_NAME' not found."
        exit 1
    fi

    make "$TEST_BIN"
    echo "Running $TEST_NAME..."
    "./$TEST_BIN"
}

# Function for running all tests in given directory
run_group() {
    local group=$1
    local TESTS=$(find "$group" -name 'test_*.f90' | sort)

    if [[ -z "$TESTS" ]]; then
        echo "No tests found in group: $group"
        exit 1
    fi

    echo "Building object files..."
    make bin  # ensures bin dir exists
    make build_objects

    for f in $TESTS; do
        local TEST_NAME=$(basename "$f" .f90)
        make "bin/$TEST_NAME"
        echo "Running $TEST_NAME..."
        ./bin/$TEST_NAME || exit 1
    done

    echo "Cleaning up object files..."
    rm -f bin/*.o bin/*.mod
}

# Process user commands
if [[ "$COMMAND" == "run" ]]; then
    echo "Building test binaries..."

    if [[ "$TARGET" == "all" || -z "$TARGET" ]]; then

        echo "Running all tests..."
        make run

    elif [[ "$TARGET" =~ ^[0-9]{2}-.* ]]; then
        run_group "$TARGET"

    else
        run_test "$TARGET"
    fi

elif [[ "$COMMAND" == "clean" ]]; then
    echo "Cleaning test environment..."
    make clean

else
    echo "Usage:"
    echo "  ./testme.sh run all                    # Run all tests"
    echo "  ./testme.sh run 00-unit-independent    # Run group of tests"
    echo "  ./testme.sh run calc_cfl               # Run individual test"
    echo "  ./testme.sh clean                      # Clean build artifacts"
    exit 1
fi