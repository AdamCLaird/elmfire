!> test_icol_fine_to_coarse.f90
!! Unit test for subroutine ICOL_FINE_TO_COARSE(...) in elmfire_subs.f90
!! Updated: 08-01-2025

! *****************************************************************************
PROGRAM TEST_ICOL_FINE_TO_COARSE
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

INTEGER :: NFAIL = 0

! Initialize required globals
FBFM%NCOLS = 10
WS%NCOLS   = 5
ALLOCATE(ICOL_ANALYSIS_F2C(10))
ICOL_ANALYSIS_F2C = [1,2,3,4,5,5,5,5,5,5]

PRINT *, 'TESTING ICOL_FINE_TO_COARSE...'

! Nominal case
CALL CHECK(3, 3, 'Nominal (in bounds)')

! Edge case: lower bound
CALL CHECK(0, 1, 'Lower bound (clamped to 1)')

! Edge case: upper bound
CALL CHECK(15, 5, 'Upper bound (clamped to 5)')

! Check results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(INPUT, EXPECTED, LABEL)
! =============================================================================
INTEGER, INTENT(IN) :: INPUT, EXPECTED
CHARACTER(*), INTENT(IN) :: LABEL
INTEGER :: RESULT

RESULT = ICOL_FINE_TO_COARSE(INPUT)
IF (RESULT /= EXPECTED) THEN
    PRINT *, 'FAIL: ', LABEL
    PRINT *, '  INPUT=', INPUT, ' RESULT=', RESULT, ' EXPECTED=', EXPECTED
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_ICOL_FINE_TO_COARSE
! *****************************************************************************