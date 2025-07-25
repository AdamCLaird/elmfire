!> test_erfinv.f90
!! Unit test for subroutine ERFINV in elmfire_subs.f90
!! Updated: 07-25-2025

! *****************************************************************************
PROGRAM TEST_ERFINV
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
REAL, PARAMETER :: TOL = 1.2E-2 ! Allow for ~1.3% relative error
REAL :: X, Y, EXPECTED
INTEGER :: NFAIL = 0

PRINT *, 'TESTING ERFINV...'

! Canonical points
CALL CHECK(  0.0,      0.0       )
CALL CHECK(  0.1,      0.08885599)
CALL CHECK( -0.1,     -0.08885599)
CALL CHECK(  0.3,      0.27246271)
CALL CHECK( -0.3,     -0.27246271)
CALL CHECK(  0.5,      0.47693628)
CALL CHECK( -0.5,     -0.47693628)
CALL CHECK(  0.7,      0.73238303)
CALL CHECK( -0.7,     -0.73238303)

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
REAL, INTENT(IN) :: X, EXPECTED     ! Inputs
REAL :: ACTUAL                      ! Locals

ACTUAL = ERFINV(X)
IF (ABS(ACTUAL - EXPECTED) > TOL) THEN
    PRINT *, 'FAIL: X=',X,' -> ERFINV=',ACTUAL,' (expected=',EXPECTED,')'
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_ERFINV
! *****************************************************************************