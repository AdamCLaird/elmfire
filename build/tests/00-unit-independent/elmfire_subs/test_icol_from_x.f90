!> test_icol_from_x.f90
!! Unit test for function ICOL_FROM_X in elmfire_subs.f90
!! Updated: 07-24-2025

! *****************************************************************************
PROGRAM TEST_ICOL_FROM_X
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL :: XLL, CELL, X

PRINT *, 'TESTING ICOL_FROM_X...'

! Basic inputs
XLL = 100.0
CELL = 20.0

! Standard Checks --------------------------------------------------------------
CALL CHECK(110.0, 1, 'Nominal Case 1')      ! center of cell 1
CALL CHECK(125.0, 2, 'Nominal Case 2')      ! inside cell 2
CALL CHECK(143.0, 3, 'Nominal Case 3')      ! inside cell 3
CALL CHECK(176.3, 4, 'Nominal Case 4')      ! inside cell 4
CALL CHECK(183.0, 5, 'Nominal Case 5')      ! inside cell 5

! Edge Cases -------------------------------------------------------------------
CALL CHECK(100.0, 0, 'Lower Bound')         ! exactly at lower bound
CALL CHECK(100.01, 1, 'Above Lower Bound')  ! just above lower bound
CALL CHECK(200.0, 5, 'Upper Bound')         ! exactly at upper bound
CALL CHECK(199.9, 5, 'Below Upper Bound')   ! just below upper bound

CALL CHECK(120.0, 1, 'Boundary Case 1')     ! exactly at boundary between cell 1 and 2
CALL CHECK(160.0, 3, 'Boundary Case 2')     ! exactly at boundary between cell 3 and 4

CALL CHECK(99.9, 0, 'Below Domain')         ! below domain
CALL CHECK(200.1, 6,'Above Domain')         ! above domain

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(X, EXPECTED, LABEL)
! =============================================================================
REAL, INTENT(IN) :: X               ! Inputs
INTEGER, INTENT(IN) :: EXPECTED
CHARACTER(*), INTENT(IN) :: LABEL
INTEGER :: RESULT                   ! Locals

RESULT = ICOL_FROM_X(X, XLL, CELL)
IF (RESULT /= EXPECTED) THEN
    PRINT *, 'FAIL: ', LABEL, ' X=', X, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_ICOL_FROM_X
! *****************************************************************************