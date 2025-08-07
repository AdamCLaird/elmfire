!> test_wx_icol_from_analysis_ix.f90
!! Unit test for WX_ICOL_FROM_ANALYSIS_IX(...) in elmfire_subs.f90
!! Updated: 08-01-2025

! *****************************************************************************
PROGRAM TEST_WX_ICOL_FROM_ANALYSIS_IX
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

INTEGER :: NFAIL = 0
INTEGER, PARAMETER :: NCOLS_FINE = 10, NCOLS_COARSE = 5

PRINT *, 'TESTING WX_ICOL_FROM_ANALYSIS_IX...'

! Initialize FBFM and WS dimensions
FBFM%NCOLS = NCOLS_FINE
WS%NCOLS   = NCOLS_COARSE

! Allocate and fill mapping array
ALLOCATE(ICOL_ANALYSIS_F2C(NCOLS_FINE))
ICOL_ANALYSIS_F2C = [1,2,3,4,5,5,5,5,5,5]

! Nominal Case ----------------------------------------------------------------
CALL CHECK(3, 3, 'Nominal')

! Clipping Cases --------------------------------------------------------------
CALL CHECK(0, 1, 'Low Clip')
CALL CHECK(11, 5, 'High Clip')

! Edge Case -------------------------------------------------------------------
CALL CHECK(5, 5, 'Edge')

! Final status
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL:', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(IXFINE, EXPECTED, LABEL)
! =============================================================================
INTEGER, INTENT(IN) :: IXFINE, EXPECTED
CHARACTER(*), INTENT(IN) :: LABEL
INTEGER :: ACTUAL

ACTUAL = WX_ICOL_FROM_ANALYSIS_IX(IXFINE)
IF (ACTUAL /= EXPECTED) THEN
    PRINT *, 'FAIL:', LABEL, ' ->', ACTUAL, 'Expected=', EXPECTED
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_WX_ICOL_FROM_ANALYSIS_IX
! *****************************************************************************