!> test_bilinear_interpolate.f90
!! Unit test for subroutine BILINEAR_INTERPOLATE in elmfire_subs.f90
!! Updated: 07-25-2025

! *****************************************************************************
PROGRAM TEST_BILINEAR_INTERPOLATE
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL :: X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22, EXPECTED

PRINT *, 'TESTING BILINEAR_INTERPOLATE...'

! Uniform field test -----------------------------------------------------------
CALL CHECK(1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 5.0, 5.0, 5.0, 5.0, 5.0, "Uniform")

! Varying corners --------------------------------------------------------------
CALL CHECK(1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 2.0, 4.0, 2.0, "Corners")

! Interpolation only in x ------------------------------------------------------
CALL CHECK(1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 2.0, 0.0, 4.0, 1.0, "X Interp")

! Interpolation only in y ------------------------------------------------------
CALL CHECK(0.0, 1.0, 0.0, 0.0, 0.0, 2.0, 1.0, 1.0, 3.0, 3.0, 2.0, "Y Interp")

! Diagonal center average ------------------------------------------------------
CALL CHECK(1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 1.0, 3.0, 3.0, 5.0, 3.0, "Diagonal")

! Check outputs and print results
IF (NFAIL == 0) THEN
   PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
   PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
   STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22, EXPECTED, LABEL)
! =============================================================================
REAL, INTENT(IN) :: X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22, EXPECTED  ! Inputs
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: RESULT                                                          ! Locals
REAL, PARAMETER :: EPSILON = 1.0E-6

RESULT = BILINEAR_INTERPOLATE(X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22)
IF (ABS(RESULT - EXPECTED) > EPSILON) THEN
   PRINT *, 'FAIL:',LABEL,'X=',X,'Y=',Y,' EXPECTED=', EXPECTED, ' GOT=', RESULT
   NFAIL = NFAIL + 1
ELSE
   PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_BILINEAR_INTERPOLATE
! *****************************************************************************