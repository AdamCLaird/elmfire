!> test_get_bilinear_interpolate_coeffs.f90
!! Unit test for GET_BILINEAR_INTERPOLATE_COEFFS in elmfire_subs.f90
!! Updated: 07-25-2025

! *****************************************************************************
PROGRAM TEST_GET_BILINEAR_INTERPOLATE_COEFFS
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
REAL :: X1, Y1, X2, Y2, CX, CY
INTEGER :: I1, J1, I2, J2
INTEGER :: NFAIL = 0, CASE

PRINT *, 'TESTING GET_BILINEAR_INTERPOLATE_COEFFS...'

! Case 1: Aligned fuel/weather raster ------------------------------------------
CASE = 1
WS%XLLCORNER = 0.0
WS%YLLCORNER = 0.0
WS%CELLSIZE  = 1.0
WS%NCOLS     = 100
WS%NROWS     = 100

ADJ%XLLCORNER = 0.0
ADJ%YLLCORNER = 0.0
ADJ%CELLSIZE  = 1.0

CALL GET_BILINEAR_INTERPOLATE_COEFFS(50, 60, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)
CALL CHECK(I1, 50, 'I1 (Case 1)', NFAIL)
CALL CHECK(J1, 60, 'J1 (Case 1)', NFAIL)
CALL CHECK(I2, 51, 'I2 (Case 1)', NFAIL)
CALL CHECK(J2, 61, 'J2 (Case 1)', NFAIL)

! Case 2: Offset origin --------------------------------------------------------
CASE = 2
WS%XLLCORNER = 100.0
WS%YLLCORNER = 200.0
WS%CELLSIZE  = 2.0
WS%NCOLS     = 100
WS%NROWS     = 100

ADJ%XLLCORNER = 100.0
ADJ%YLLCORNER = 200.0
ADJ%CELLSIZE  = 2.0

CALL GET_BILINEAR_INTERPOLATE_COEFFS(25, 10, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)
CALL CHECK(I1, 25, 'I1 (Case 2)', NFAIL)
CALL CHECK(J1, 10, 'J1 (Case 2)', NFAIL)
CALL CHECK(I2, 26, 'I2 (Case 2)', NFAIL)
CALL CHECK(J2, 11, 'J2 (Case 2)', NFAIL)

! Case 3: Fuel finer than weather ----------------------------------------------
WS%XLLCORNER = 0.0
WS%YLLCORNER = 0.0
WS%CELLSIZE  = 10.0
WS%NCOLS     = 10
WS%NROWS     = 10

ADJ%XLLCORNER = 0.0
ADJ%YLLCORNER = 0.0
ADJ%CELLSIZE  = 1.0

CALL GET_BILINEAR_INTERPOLATE_COEFFS(35, 25, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)
CALL CHECK(I1, 4, 'I1 (Case 3)', NFAIL)
CALL CHECK(J1, 3, 'J1 (Case 3)', NFAIL)

! Case 4: Clipping near top-right corner ---------------------------------------
WS%XLLCORNER = 0.0
WS%YLLCORNER = 0.0
WS%CELLSIZE  = 1.0
WS%NCOLS     = 10
WS%NROWS     = 10

ADJ%XLLCORNER = 0.0
ADJ%YLLCORNER = 0.0
ADJ%CELLSIZE  = 1.0

CALL GET_BILINEAR_INTERPOLATE_COEFFS(10, 10, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)
CALL CHECK(I1, 10, 'I1 (Case 4)', NFAIL)
CALL CHECK(I2, 10, 'I2 (Case 4)', NFAIL)
CALL CHECK(J1, 10, 'J1 (Case 4)', NFAIL)
CALL CHECK(J2, 10, 'J2 (Case 4)', NFAIL)

! Check outputs and print results
IF (NFAIL == 0) THEN
   PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
   PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
   STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(ACTUAL, EXPECTED, LABEL, NFAIL)
! =============================================================================
INTEGER, INTENT(IN) :: ACTUAL, EXPECTED ! Inputs
CHARACTER(*), INTENT(IN) :: LABEL
INTEGER, INTENT(INOUT) :: NFAIL         ! In/Out

IF (ACTUAL /= EXPECTED) THEN
    PRINT *, 'FAIL: ', LABEL, '=', ACTUAL, ' (expected ', EXPECTED, ')'
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_GET_BILINEAR_INTERPOLATE_COEFFS
! *****************************************************************************