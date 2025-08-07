!> test_sunrise_sunset_calcs.f90
!! Unit test for subroutine SUNRISE_SUNSET_CALCS in elmfire_io.f90
!! Updated: 08-01-2025

! *****************************************************************************
PROGRAM TEST_SUNRISE_SUNSET_CALCS
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_IO

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0

PRINT *, 'TESTING SUNRISE_SUNSET_CALCS...'

! Nominal Case: Summer solstice, equator --------------------------------------
CALL CHECK(0.0, 0.0, 2025, 172, 6.04, 18.16, "Equator, June Solstice")

! Edge Case: Arctic circle, winter solstice -----------------------------------
CALL CHECK(0.0, 66.5, 2025, 355, 10.07, 14.22, "Arctic Circle, Dec Solstice")

! Offset Case: San Francisco, spring equinox ----------------------------------
CALL CHECK(-122.419, 37.7749, 2025, 80, 15.42, 25.04, "SF, March Equinox")

IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(LON, LAT, YEAR, HOY, EXPECTED_SUNRISE, EXPECTED_SUNSET, LABEL)
! =============================================================================
REAL, INTENT(IN) :: LON, LAT, EXPECTED_SUNRISE, EXPECTED_SUNSET
INTEGER, INTENT(IN) :: YEAR, HOY
CHARACTER(*), INTENT(IN) :: LABEL
REAL, PARAMETER :: EPS = 1.0E-2

CALL SUNRISE_SUNSET_CALCS(LON, LAT, YEAR, HOY)

IF (ABS(SUNRISE_HOUR - EXPECTED_SUNRISE) > EPS .OR. ABS(SUNSET_HOUR - EXPECTED_SUNSET) > EPS) THEN
    PRINT *, 'FAIL:', LABEL
    PRINT *, '  SUNRISE=', SUNRISE_HOUR, ' EXPECTED=', EXPECTED_SUNRISE
    PRINT *, '  SUNSET =', SUNSET_HOUR, ' EXPECTED=', EXPECTED_SUNSET
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_SUNRISE_SUNSET_CALCS
! *****************************************************************************