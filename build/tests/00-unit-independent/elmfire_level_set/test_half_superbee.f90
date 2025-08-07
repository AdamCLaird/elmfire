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

! Nominal Cases ---------------------------------------------------------------
CALL CHECK(-1.0, 0.0,   'Nominal Case 1')
CALL CHECK( 0.0, 0.0,   'Nominal Case 2')
CALL CHECK( 0.25, 0.25, 'Nominal Case 3')
CALL CHECK( 0.5, 0.5,   'Nominal Case 4')
CALL CHECK( 0.75, 0.5,  'Nominal Case 5')
CALL CHECK( 1.0, 0.5,   'Nominal Case 6')
CALL CHECK( 1.5, 0.75,  'Nominal Case 7')
CALL CHECK( 2.0, 1.0,   'Nominal Case 8')

! Edge Cases --------------------------------------------------------------
CALL CHECK(-1.0E6,   0.0,     'Edge Case 1')
CALL CHECK(-1.0E-12, 0.0,     'Edge Case 2')
CALL CHECK( 1.0E-12, 1.0E-12, 'Edge Case 3')
CALL CHECK( 1.0E-6,  1.0E-6,  'Edge Case 4')
CALL CHECK( 1.0E6,   1.0,     'Edge Case 5')
CALL CHECK( 1.0E38,  1.0,     'Edge Case 6')

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(R, EXPECTED, LABEL)
! =============================================================================
REAL, INTENT(IN) :: R, EXPECTED   ! Inputs
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: RESULT                    ! Locals
REAL, PARAMETER :: EPSILON = 1.0E-6

RESULT = HALF_SUPERBEE(R)         ! Use function & check values
IF (ABS(RESULT - EXPECTED) > EPSILON) THEN
  PRINT *, 'FAIL: ', LABEL, ' R=', R, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
  NFAIL = NFAIL + 1
ELSE
  PRINT *, 'PASS: ', LABEL
END IF 
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM
! *****************************************************************************