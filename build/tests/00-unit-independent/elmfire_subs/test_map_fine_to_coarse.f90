
! *****************************************************************************
PROGRAM TEST_MAP_FINE_TO_COARSE
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
TYPE(RASTER_TYPE) :: FINE, COARSE
INTEGER, ALLOCATABLE :: ICOL_COARSE(:), IROW_COARSE(:)
INTEGER :: NFAIL = 0

PRINT *, 'TESTING MAP_FINE_TO_COARSE...'

CALL ALLOCATE_EMPTY_RASTER(COARSE, 2, 2, 1, 0.0, 0.0, 20.0, -999.0, 'FLOAT')
CALL ALLOCATE_EMPTY_RASTER(FINE,   4, 4, 1, 0.0, 0.0, 10.0, -999.0, 'FLOAT')

ALLOCATE(ICOL_COARSE(FINE%NCOLS))
ALLOCATE(IROW_COARSE(FINE%NROWS))

CALL MAP_FINE_TO_COARSE(COARSE, FINE, ICOL_COARSE, IROW_COARSE)

CALL CHECK(ICOL_COARSE, [1,1,2,2], 'ICOL_COARSE', NFAIL)
CALL CHECK(IROW_COARSE, [1,1,2,2], 'IROW_COARSE', NFAIL)

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL:', NFAIL, 'TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(ACTUAL, EXPECTED, LABEL, NFAIL)
! =============================================================================
INTEGER, INTENT(IN) :: ACTUAL(:), EXPECTED(:)   ! Inputs
CHARACTER(*), INTENT(IN) :: LABEL
INTEGER, INTENT(INOUT) :: NFAIL
INTEGER :: I                                    ! Locals

DO I = 1, SIZE(ACTUAL)
    IF (ACTUAL(I) /= EXPECTED(I)) THEN
        PRINT *, 'FAIL: ', LABEL, '(', I, ') =', ACTUAL(I), ' (expected ', EXPECTED(I), ')'
        NFAIL = NFAIL + 1
    END IF
END DO
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_MAP_FINE_TO_COARSE
! *****************************************************************************