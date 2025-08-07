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

PRINT *, 'TESTING X_FROM_ICOL...'

XLL = 100.0
CELL = 20.0

! Nominal Cases ---------------------------------------------------------------
CALL CHECK(1, 110.0, 'Nominal Case 1')      ! center of cell 1
CALL CHECK(2, 130.0, 'Nominal Case 2')      ! center of cell 2
CALL CHECK(3, 150.0, 'Nominal Case 3')      ! center of cell 3
CALL CHECK(4, 170.0, 'Nominal Case 4')      ! center of cell 4
CALL CHECK(5, 190.0, 'Nominal Case 5')      ! center of cell 5

! Edge Cases ------------------------------------------------------------------
CALL CHECK(0, 90.0, 'Below Lower Bound')    ! center of cell below lower bound
CALL CHECK(6, 210.0, 'Above Upper Bound')   ! center of cell above upper bound

! Check outputs and print results
IF (NFAIL == 0) THEN
PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(ICOL, EXPECTED, LABEL)
! =============================================================================
INTEGER, INTENT(IN) :: ICOL
REAL, INTENT(IN) :: EXPECTED
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: RESULT

RESULT = X_FROM_ICOL(ICOL, XLL, CELL)
IF (RESULT /= EXPECTED) THEN
    PRINT *, 'FAIL: LABEL', ' X=', X, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_X_FROM_ICOL
! *****************************************************************************