!> test_irow_from_y.f90
!! Unit test for function IROW_FROM_Y in elmfire_subs.f90
!! Updated: 07-24-2025

! *****************************************************************************
PROGRAM TEST_IROW_FROM_Y
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL :: YLL, CELL, Y
INTEGER :: EXPECTED

PRINT *, 'TESTING IROW_FROM_Y...'

! Test grid setup
YLL  = 100.0
CELL = 20.0

! Standard Checks --------------------------------------------------------------
CALL CHECK(110.0, 1)        ! center of cell 1
CALL CHECK(125.0, 2)        ! inside cell 2
CALL CHECK(143.0, 3)        ! inside cell 3
CALL CHECK(176.3, 4)        ! inside cell 4
CALL CHECK(183.0, 5)        ! inside cell 5

! Edge Cases -------------------------------------------------------------------
CALL CHECK(100.0, 0)        ! exactly at lower bound
CALL CHECK(100.01, 1)       ! just above lower bound
CALL CHECK(200.0, 5)        ! exactly at upper bound
CALL CHECK(199.9, 5)        ! just below upper bound

CALL CHECK(120.0, 1)        ! exactly at boundary between cell 1 and 2
CALL CHECK(160.0, 3)        ! exactly at boundary between cell 3 and 4

CALL CHECK(99.9, 0)         ! below domain
CALL CHECK(200.1, 6)        ! above domain

IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(Y, EXPECTED)
! =============================================================================
REAL, INTENT(IN) :: Y
INTEGER, INTENT(IN) :: EXPECTED
INTEGER :: RESULT

RESULT = IROW_FROM_Y(Y, YLL, CELL)
IF (RESULT /= EXPECTED) THEN
    PRINT *, 'FAIL: Y=', Y, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_IROW_FROM_Y
! *****************************************************************************