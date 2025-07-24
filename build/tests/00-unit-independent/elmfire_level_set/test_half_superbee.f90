!> test_half_superbee.f90
!! Unit test for function HALF_SUPERBEE(R) in elmfire_level_set.f90
!! Updated: 07-21-2025

! *****************************************************************************
PROGRAM TEST_HALF_SUPERBEE
! *****************************************************************************

USE ELMFIRE_LEVEL_SET

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0

PRINT *, 'TESTING HALF_SUPERBEE...'

! CHECK(INPUT, TRUE OUTPUT)
CALL CHECK(-1.0E6, 0.0)
CALL CHECK(-1.0E-12, 0.0)
CALL CHECK( 1.0E-12, 1.0E-12)
CALL CHECK( 1.0E-6, 1.0E-6)
CALL CHECK( 1.0E6, 1.0)
CALL CHECK( 1.0E38, 1.0)
CALL CHECK(-1.0, 0.0)
CALL CHECK( 0.0, 0.0)
CALL CHECK( 0.25, 0.25)
CALL CHECK( 0.5, 0.5)
CALL CHECK( 0.75, 0.5)
CALL CHECK( 1.0, 0.5)
CALL CHECK( 1.5, 0.75)
CALL CHECK( 2.0, 1.0)

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(R, EXPECTED)
! =============================================================================
REAL, INTENT(IN) :: R, EXPECTED   ! Inputs
REAL :: RESULT                    ! Locals
REAL, PARAMETER :: EPSILON = 1.0E-6

RESULT = HALF_SUPERBEE(R)         ! Use function & check values
IF (ABS(RESULT - EXPECTED) > EPSILON) THEN
  PRINT *, 'FAIL: R=', R, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
  NFAIL = NFAIL + 1
END IF 
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM
! *****************************************************************************