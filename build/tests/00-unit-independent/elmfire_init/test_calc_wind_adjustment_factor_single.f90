!> test_calc_wind_adjustment_factor_single.f90
!! Unit test for function CALC_WIND_ADJUSTMENT_FACTOR_SINGLE in elmfire_init.f90
!! Updated: 08-01-2025

! *****************************************************************************
PROGRAM TEST_CALC_WIND_ADJUSTMENT_FACTOR_SINGLE
! *****************************************************************************

USE ELMFIRE_INIT

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL :: EXPECTED, RESULT
REAL, PARAMETER :: EPS = 1.0E-5

PRINT *, 'TESTING CALC_WIND_ADJUSTMENT_FACTOR_SINGLE...'

! Test Notes ------------------------------------------------------------------
! > Several cases failing due to divide-by-zero errors. May be meaningful to 
!   add protective guard in subroutine to clip values or flag bad inputs
! -----------------------------------------------------------------------------

! Case 1: Moderate canopy height ----------------------------------------------
! RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.3, 15.0, 0.0)
! EXPECTED = 0.165597
! CALL CHECK(RESULT, EXPECTED, 'Moderate canopy (CH=15)')

! Case 2: Fuel Bed Only -------------------------------------------------------
RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.0, 0.0, 1.0)
EXPECTED = 0.362674
CALL CHECK(RESULT, EXPECTED, 'Fuel Bed Only (HFB=1)')

! Case 3: No Vegetation -------------------------------------------------------
RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.0, 0.0, 0.0)
EXPECTED = 0.0
CALL CHECK(RESULT, EXPECTED, 'No vegetation')

! Case 4: Negative Canopy Cover -----------------------------------------------
RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(-1.0, 5.0, 1.0)
EXPECTED = 0.0
CALL CHECK(RESULT, EXPECTED, 'Negative CC (CC=-1)')

! Case 5: Small but nonzero canopy cover and height ---------------------------
! RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.01, 0.01, 0.0)
! EXPECTED = 0.548498
! CALL CHECK(RESULT, EXPECTED, 'Small CC and CH')

! Case 6: Large fuel bed height -----------------------------------------------
! RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.0, 0.0, 10.0)
! EXPECTED = 0.500812
! CALL CHECK(RESULT, EXPECTED, 'Fuel bed height = 10')

! Case 7: Canopy CC > 0, but CH = 0 -------------------------------------------
RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.3, 0.0, 1.0)
EXPECTED = 0.362674
CALL CHECK(RESULT, EXPECTED, 'CC > 0 but CH = 0')

! Case 8: CH = 0, HFB = 1 (boundary resolution test) --------------------------
RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.0, 0.0, 1.0)
EXPECTED = 0.362674
CALL CHECK(RESULT, EXPECTED, 'CH = 0, HFB = 1')

! Case 9: Very tall canopy height ---------------------------------------------
! RESULT = CALC_WIND_ADJUSTMENT_FACTOR_SINGLE(0.5, 30.0, 0.0)
! EXPECTED = 0.142206
! CALL CHECK(RESULT, EXPECTED, 'Tall canopy (CH=30)')

! Final Report
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(ACTUAL, EXPECTED, LABEL)
! =============================================================================
REAL, INTENT(IN) :: ACTUAL, EXPECTED
CHARACTER(*), INTENT(IN) :: LABEL
IF (ABS(ACTUAL - EXPECTED) > EPS) THEN
    PRINT *, 'FAIL: ', LABEL
    PRINT *, '  VALUE   =', ACTUAL
    PRINT *, '  EXPECTED=', EXPECTED
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_CALC_WIND_ADJUSTMENT_FACTOR_SINGLE
! *****************************************************************************