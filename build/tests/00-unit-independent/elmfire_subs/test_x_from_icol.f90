!> test_x_from_icol.f90
!! Unit test for function X_FROM_ICOL in elmfire_subs.f90
!! Updated: 07-24-2025

! *****************************************************************************
PROGRAM TEST_X_FROM_ICOL
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL :: XLL, CELL, X
INTEGER :: ICOL
REAL :: EXPECTED

PRINT *, 'TESTING X_FROM_ICOL...'

XLL = 100.0
CELL = 20.0

! Standard Checks ------------------------------------------------------------
CALL CHECK(1, 110.0)        ! center of cell 1
CALL CHECK(2, 130.0)        ! center of cell 2
CALL CHECK(3, 150.0)        ! center of cell 3
CALL CHECK(4, 170.0)        ! center of cell 4
CALL CHECK(5, 190.0)        ! center of cell 5

! Edge Cases -----------------------------------------------------------------
CALL CHECK(0, 90.0)         ! center of cell below lower bound
CALL CHECK(6, 210.0)        ! center of cell above upper bound

IF (NFAIL == 0) THEN
PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(ICOL, EXPECTED)
! =============================================================================
INTEGER, INTENT(IN) :: ICOL
REAL, INTENT(IN) :: EXPECTED
REAL :: RESULT

RESULT = X_FROM_ICOL(ICOL, XLL, CELL)
IF (RESULT /= EXPECTED) THEN
    PRINT *, 'FAIL: X=', X, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_X_FROM_ICOL
! *****************************************************************************