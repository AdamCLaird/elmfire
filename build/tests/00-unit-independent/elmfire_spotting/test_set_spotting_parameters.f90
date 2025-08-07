!> test_set_spotting_parameters.f90
!> Unit test for subroutine SET_SPOTTING_PARAMETERS in elmfire_spotting.f90
!> Updated: 08-01-2025

! *****************************************************************************
PROGRAM TEST_SET_SPOTTING_PARAMETERS
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SPOTTING

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0, I
REAL, ALLOCATABLE :: R1(:)
REAL, PARAMETER :: EPS = 1.0E-4
CHARACTER(LEN=40) :: LABEL

PRINT *, 'TESTING SET_SPOTTING_PARAMETERS...'

! Set number of input coefficients
NUM_PARAMETERS_RASTERS = 0  ! Assume R1 starts from index 1
ALLOCATE(R1(8))
ALLOCATE(COEFFS_UNSCALED(8))

! Set dummy bounds for constants
MEAN_SPOTTING_DIST_MIN                    = 10.0
MEAN_SPOTTING_DIST_MAX                    = 30.0
NORMALIZED_SPOTTING_DIST_VARIANCE_MIN     = 1.0
NORMALIZED_SPOTTING_DIST_VARIANCE_MAX     = 5.0
SPOT_WS_EXP_LO                            = 0.5
SPOT_WS_EXP_HI                            = 1.5
SPOT_FLIN_EXP_LO                          = 0.1
SPOT_FLIN_EXP_HI                          = 0.9
NEMBERS_MAX_LO                            = 20.0
NEMBERS_MAX_HI                            = 40.0
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN  = 10.0
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX  = 60.0
CROWN_FIRE_SPOTTING_PERCENT_MIN           = 30.0
CROWN_FIRE_SPOTTING_PERCENT_MAX           = 90.0
PIGN_MIN                                  = 0.01
PIGN_MAX                                  = 0.99

SURFACE_FIRE_SPOTTING_PERCENT_MULT(1) = 1.0

! Case 1: Midpoint test (R1(:) = 0.5) -----------------------------------------
R1 = 0.5
CALL SET_SPOTTING_PARAMETERS(R1)
CALL CHECK(MEAN_SPOTTING_DIST,                  20.0, 'Mid: MEAN_SPOTTING_DIST')
CALL CHECK(NORMALIZED_SPOTTING_DIST_VARIANCE,    3.0, 'Mid: NORM_VAR')
CALL CHECK(SPOT_WS_EXP,                          1.0, 'Mid: SPOT_WS_EXP')
CALL CHECK(SPOT_FLIN_EXP,                        0.5, 'Mid: SPOT_FLIN_EXP')
CALL CHECK(REAL(NEMBERS_MAX),                   30.0, 'Mid: NEMBERS_MAX')
CALL CHECK(GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT,35.0, 'Mid: GLOBAL_PERCENT')
CALL CHECK(CROWN_FIRE_SPOTTING_PERCENT,         60.0, 'Mid: CROWN_PERCENT')
CALL CHECK(PIGN,                                 0.5, 'Mid: PIGN')

! Case 2: Lower Bounds (R1 = 0.0) ---------------------------------------------
R1 = 0.0
CALL SET_SPOTTING_PARAMETERS(R1)
CALL CHECK(MEAN_SPOTTING_DIST,                  10.0, 'Low: MEAN_SPOTTING_DIST')
CALL CHECK(NORMALIZED_SPOTTING_DIST_VARIANCE,    1.0, 'Low: NORM_VAR')
CALL CHECK(SPOT_WS_EXP,                          0.5, 'Low: SPOT_WS_EXP')
CALL CHECK(SPOT_FLIN_EXP,                        0.1, 'Low: SPOT_FLIN_EXP')
CALL CHECK(REAL(NEMBERS_MAX),                   20.0, 'Low: NEMBERS_MAX')
CALL CHECK(GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT,10.0, 'Low: GLOBAL_PERCENT')
CALL CHECK(CROWN_FIRE_SPOTTING_PERCENT,         30.0, 'Low: CROWN_PERCENT')
CALL CHECK(PIGN,                                0.01, 'Low: PIGN')

! Case 3: Upper Bounds (R1 = 1.0) ---------------------------------------------
R1 = 1.0
CALL SET_SPOTTING_PARAMETERS(R1)
CALL CHECK(MEAN_SPOTTING_DIST,                  30.0, 'High: MEAN_SPOTTING_DIST')
CALL CHECK(NORMALIZED_SPOTTING_DIST_VARIANCE,    5.0, 'High: NORM_VAR')
CALL CHECK(SPOT_WS_EXP,                          1.5, 'High: SPOT_WS_EXP')
CALL CHECK(SPOT_FLIN_EXP,                        0.9, 'High: SPOT_FLIN_EXP')
CALL CHECK(REAL(NEMBERS_MAX),                   40.0, 'High: NEMBERS_MAX')
CALL CHECK(GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT,60.0, 'High: GLOBAL_PERCENT')
CALL CHECK(CROWN_FIRE_SPOTTING_PERCENT,         90.0, 'High: CROWN_PERCENT')
CALL CHECK(PIGN,                                0.99, 'High: PIGN')

! Final report
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL:', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(VALUE, EXPECTED, LABEL)
! =============================================================================
REAL, INTENT(IN) :: VALUE, EXPECTED
CHARACTER(*), INTENT(IN) :: LABEL

IF (ABS(VALUE - EXPECTED) > EPS) THEN
    PRINT *, 'FAIL:', LABEL
    PRINT *, '  VALUE   =', VALUE
    PRINT *, '  EXPECTED=', EXPECTED
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! ============================================================================= 
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_SET_SPOTTING_PARAMETERS
! *****************************************************************************