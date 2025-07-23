#!/bin/bash
## testme.sh
## Run file for executing automated testing suite
## Updated: 07-21-2025

set -e

COMMAND=$1
TARGET=$2
SCRATCH="scratch"

if [[ "$COMMAND" == "run" ]]; then
    echo "Building test binaries..."
    make

    if [[ "$TARGET" == "all" || -z "$TARGET" ]]; then
        echo "Running all tests..."
        make run
    else
        TEST_BIN="test_${TARGET}"
        if [[ -x "$SCRATCH/$TEST_BIN" ]]; then
            echo "Running $TEST_BIN..."
            "./$SCRATCH/$TEST_BIN"
        else
            echo "Error: Test binary '$TEST_BIN' not found. Did it compile?"
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