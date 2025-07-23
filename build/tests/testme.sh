#!/bin/bash
## testme.sh
## Run file for executing automated testing suite
## Updated: 07-21-2025

set -e

COMMAND=$1
TARGET=$2
SCRATCH="scratch"
TEST_DIRS=("unit-independent" "unit-dependent" "component" "integration" "regression")

if [[ "$COMMAND" == "run" ]]; then
    echo "Building test binaries..."

    if [[ "$TARGET" == "all" || -z "$TARGET" ]]; then
        make
        echo "Running all tests..."
        make run
    else
        TEST_NAME="test_${TARGET}"
        TEST_BIN="${SCRATCH}/${TEST_NAME}"

        # Find the source file in any known directory
        FOUND_SRC=""
        for dir in "${TEST_DIRS[@]}"; do
            if [[ -f "${dir}/${TEST_NAME}.f90" ]]; then
                FOUND_SRC="${dir}/${TEST_NAME}.f90"
                break
            fi
        done

        # Error message if test name not found
        if [[ -z "$FOUND_SRC" ]]; then
            echo "Error: Test source for '$TEST_NAME' not found in any test directory."
            exit 1
        fi

        make "$TEST_BIN"

        if [[ -x "$TEST_BIN" ]]; then
            echo "Running $TEST_NAME..."
            "./$TEST_BIN"
        else
            echo "Error: Test binary '$TEST_BIN' not found or not executable."
            exit 1
        fi
    fi

elif [[ "$COMMAND" == "clean" ]]; then
    echo "Cleaning test environment..."
    make clean

else
    echo "Usage:"
    echo "  ./testme.sh run all           # Run all tests"
    echo "  ./testme.sh run half_superbee # Run specific test"
    echo "  ./testme.sh clean             # Clean build artifacts"
    exit 1
fi