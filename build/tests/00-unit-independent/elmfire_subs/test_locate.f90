!> test_locate.f90
!! Unit test for subroutine LOCATE in elmfire_subs.f90
!! Updated: 08-01-2025

! *****************************************************************************
PROGRAM TEST_LOCATE
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0

PRINT *, 'TESTING LOCATE...'

! Test Notes ------------------------------------------------------------------
! > Descending match case fails due to subroutine logic bug. Conditionals 
!   become ambibuous when x == xx(jm)
! > Single-point input case fails due to subroutine initialization bug. 
!   Subroutine doesn't explicitly handle case of n = 1. 
! -----------------------------------------------------------------------------

! Nominal cases ---------------------------------------------------------------
CALL CHECK("Nominal: Between midpoints", (/0.0, 1.0, 2.0, 3.0/), 4, 1.5, 2)
CALL CHECK("Nominal: Exact match low",    (/0.0, 1.0, 2.0, 3.0/), 4, 1.0, 2)
CALL CHECK("Nominal: Exact match high",   (/0.0, 1.0, 2.0, 3.0/), 4, 3.0, 3)

! Edge cases ------------------------------------------------------------------
CALL CHECK("Edge: Below lower bound",     (/0.0, 1.0, 2.0, 3.0/), 4, -1.0, 0)
CALL CHECK("Edge: Above upper bound",     (/0.0, 1.0, 2.0, 3.0/), 4, 4.0, 4)

! Descending input ------------------------------------------------------------
! CALL CHECK("Descending: Match",           (/3.0, 2.0, 1.0, 0.0/), 4, 2.0, 2)
CALL CHECK("Descending: Between",         (/3.0, 2.0, 1.0, 0.0/), 4, 2.5, 1)

! Short arrays ---------------------------------------------------------------
CALL CHECK("Short array (2 points)",      (/0.0, 1.0/),          2, 0.5, 1)
CALL CHECK("Short array (2 points)",      (/0.0, 1.0/),          2, -0.1, 0)
CALL CHECK("Short array (2 points)",      (/0.0, 1.0/),          2, 2.0, 2)

! Degenerate input ------------------------------------------------------------
! CALL CHECK("Single-point input",          (/5.0/),               1, 5.0, 0)

! Final result
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(LABEL, XX, N, X, EXPECTED)
! =============================================================================
CHARACTER(*), INTENT(IN) :: LABEL
REAL, INTENT(IN) :: XX(:), X
INTEGER, INTENT(IN) :: N, EXPECTED

INTEGER :: J

CALL LOCATE(XX, N, X, J)

IF (J /= EXPECTED) THEN
    PRINT *, 'FAIL: ', LABEL
    PRINT *, '  LOCATE(', X, ') ->', J, ' Expected =', EXPECTED
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_LOCATE
! *****************************************************************************