!> test_wx_irow_from_analysis_iy.f90
!! Unit test for WX_IROW_FROM_ANALYSIS_IY(...) in elmfire_subs.f90
!! Updated: 08-01-2025

! *****************************************************************************
PROGRAM TEST_WX_IROW_FROM_ANALYSIS_IY
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

INTEGER :: NFAIL = 0
INTEGER, PARAMETER :: NROWS_FINE = 10, NROWS_COARSE = 5

PRINT *, 'TESTING WX_IROW_FROM_ANALYSIS_IY...'

! Initialize FBFM and WS dimensions
FBFM%NROWS = NROWS_FINE
WS%NROWS   = NROWS_COARSE

! Allocate and fill mapping array
ALLOCATE(IROW_ANALYSIS_F2C(NROWS_FINE))
IROW_ANALYSIS_F2C = [1,2,3,4,5,5,5,5,5,5]

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
    PRINT *, 'FAIL:', NFAIL, 'TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(IYFINE, EXPECTED, LABEL)
! =============================================================================
INTEGER, INTENT(IN) :: IYFINE, EXPECTED
CHARACTER(*), INTENT(IN) :: LABEL
INTEGER :: ACTUAL

ACTUAL = WX_IROW_FROM_ANALYSIS_IY(IYFINE)
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
END PROGRAM TEST_WX_IROW_FROM_ANALYSIS_IY
! *****************************************************************************