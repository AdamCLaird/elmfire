!> test_erfinv.f90
!! Unit test for subroutine ERFINV in elmfire_subs.f90
!! Updated: 07-25-2025

! *****************************************************************************
PROGRAM TEST_ERFINV
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
REAL, PARAMETER :: EPSILON = 1.2E-2 ! Allow for ~1.3% relative error
REAL :: X, Y, EXPECTED
INTEGER :: NFAIL = 0

PRINT *, 'TESTING ERFINV...'

! Nominal Cases |x| < 0.8 -----------------------------------------------------
CALL CHECK(  0.0,  0.0,        "Nominal Case 1")
CALL CHECK(  0.1,  0.08885599, "Nominal Case 2")
CALL CHECK( -0.1, -0.08885599, "Nominal Case 3")
CALL CHECK(  0.3,  0.27246271, "Nominal Case 4")
CALL CHECK( -0.3, -0.27246271, "Nominal Case 5")
CALL CHECK(  0.5,  0.47693628, "Nominal Case 6")
CALL CHECK( -0.5, -0.47693628, "Nominal Case 7")
CALL CHECK(  0.7,  0.73238303, "Nominal Case 8")
CALL CHECK( -0.7, -0.73238303, "Nominal Case 9")

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
REAL, INTENT(IN) :: X, EXPECTED     ! Inputs
CHARACTER(*), INTENT (IN) :: LABEL
REAL :: RESULT                      ! Locals

RESULT = ERFINV(X)
IF (ABS(RESULT - EXPECTED) > EPSILON) THEN
   PRINT *, 'FAIL: ',LABEL,' INPUT=', X,' EXPECTED=', EXPECTED, ' GOT=', RESULT
   NFAIL = NFAIL + 1
ELSE
   PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_ERFINV
! *****************************************************************************