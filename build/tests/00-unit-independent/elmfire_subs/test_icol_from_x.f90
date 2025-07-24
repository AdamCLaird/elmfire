!> test_icol_from_x.f90
!! Unit test for function ICOL_FROM_X in elmfire_subs.f90
!! Copy and modify for new subroutines/functions
!! Updated: [DATE]

! *****************************************************************************
PROGRAM TEST_ICOL_FROM_X
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL :: XLL, CELL, X
INTEGER :: EXPECTED

PRINT *, 'TESTING ICOL_FROM_X...'

! Test on basic inputs
XLL = 100.0
CELL = 20.0

CALL CHECK(120.0, 2)
CALL CHECK(100.1, 1)
CALL CHECK(199.9, 5)

IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(X, EXPECTED)
! =============================================================================
REAL, INTENT(IN) :: X               ! Inputs
INTEGER, INTENT(IN) :: EXPECTED
INTEGER :: RESULT                   ! Locals

RESULT = ICOL_FROM_X(X, XLL, CELL)
IF (RESULT /= EXPECTED) THEN
    PRINT *, 'FAIL: X=', X, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_ICOL_FROM_X
! *****************************************************************************