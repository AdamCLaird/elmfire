!> test_ellipse_ucb.f90
!! Unit test for subroutine CALC_NORMAL_VECTORS(...) in elmfire_spread_rate.f90
!! Updated: 07-22-2025

! *****************************************************************************
PROGRAM TEST_ELLIPSE_UCB
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SPREAD_RATE

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: EPS = 1.0E-4

PRINT *, 'TESTING ELLIPSE_UCB...'

! Allocate dummy global fields
ALLOCATE(BLDG_AREA%R4(10,10,1))
ALLOCATE(BLDG_SEPARATION_DIST%R4(10,10,1))

! Case 1: Moderate wind, IFBFM = 91 -------------------------------------------
ALLOCATE(C)
 C%IX = 5
 C%IY = 5
 C%IFBFM = 91
 C%WS20_NOW = 10.0      ! ~4.47 m/s
 C%WIND_PROP = 1.0
BLDG_AREA%R4(5,5,1) = 10.0
BLDG_SEPARATION_DIST%R4(5,5,1) = 5.0
CALL CHECK(C, 1, 'Moderate wind, IFBFM=91')

! Case 2: Low wind (<10 m/s), IFBFM ≠ 91 --------------------------------------
 C%IFBFM = 99
 C%WS20_NOW = 20.0     ! ~8.94 m/s
 C%WIND_PROP = 0.5
CALL CHECK(C, 3, 'Low wind, IFBFM≠91')

! Case 3: High wind (> 17.3 m/s) ----------------------------------------------
 C%WS20_NOW = 45.0     ! ~20.1 m/s
 C%WIND_PROP = 0.9
CALL CHECK(C, 3, 'High wind')

DEALLOCATE(C)

! Final output
IF (NFAIL == 0) THEN
  PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
  PRINT *, 'FAIL:', NFAIL, ' TEST(S) FAILED.'
  STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(C, EXPECTED_FOREST, LABEL)
! =============================================================================
TYPE(NODE), POINTER :: C
INTEGER, INTENT(IN) :: EXPECTED_FOREST
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: A, B, MAJOR, ECC, MINOR, EST
REAL, PARAMETER :: TOL = 1.0E-4

CALL ELLIPSE_UCB(C)

IF (C%ELLIPSE_PARAMETERS%FOREST_FACTOR /= EXPECTED_FOREST) THEN
  PRINT *, 'FAIL: ', LABEL, 'Incorrect forest factor: ', C%ELLIPSE_PARAMETERS%FOREST_FACTOR
  NFAIL = NFAIL + 1
ELSE
  PRINT *, 'PASS: ', LABEL, '...FOREST_FACTOR'
END IF

! Reasonable bounds check
MAJOR = C%ELLIPSE_PARAMETERS%ELLIPSE_MAJOR
ECC   = C%ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY
MINOR = C%ELLIPSE_PARAMETERS%ELLIPSE_MINOR

IF (MAJOR <= 0.0 .OR. MINOR < 0.0) THEN
  PRINT *, 'FAIL: ', LABEL, 'Invalid ellipse dimensions: MAJOR=', MAJOR, ' MINOR=', MINOR
  NFAIL = NFAIL + 1
ELSE
  PRINT *, 'PASS: ', LABEL, '...Valid ellipse dimensions'
END IF

! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_ELLIPSE_UCB
! *****************************************************************************