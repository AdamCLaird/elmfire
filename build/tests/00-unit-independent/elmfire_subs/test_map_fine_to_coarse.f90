!> test_map_fine_to_coarse.f90
!! Unit test for MAP_FINE_TO_COARSE in elmfire_subs.f90
!! Updated: 07-25-2025

! *****************************************************************************
PROGRAM TEST_MAP_FINE_TO_COARSE
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
TYPE(RASTER_TYPE) :: FINE, COARSE
INTEGER, ALLOCATABLE :: ICOL_COARSE(:), IROW_COARSE(:)
INTEGER :: NFAIL = 0, I

PRINT *, 'TESTING MAP_FINE_TO_COARSE...'


! Case 1: Aligned Grids --------------------------------------------------------
CALL ALLOCATE_EMPTY_RASTER(COARSE, 2, 2, 1, 0.0, 0.0, 20.0, -999.0, 'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(FINE,   4, 4, 1, 0.0, 0.0, 10.0, -999.0, 'FLOAT     ')

ALLOCATE(ICOL_COARSE(FINE%NCOLS), IROW_COARSE(FINE%NROWS))
CALL MAP_FINE_TO_COARSE(COARSE, FINE, ICOL_COARSE, IROW_COARSE)
CALL CHECK(ICOL_COARSE, [1,1,2,2], 'ICOL_COARSE (Case 1)', NFAIL)
CALL CHECK(IROW_COARSE, [1,1,2,2], 'IROW_COARSE (Case 1)', NFAIL)

! Case 2: Offset Grids ---------------------------------------------------------
CALL ALLOCATE_EMPTY_RASTER(COARSE, 2, 2, 1, 100.0, 200.0, 20.0, -999.0, 'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(FINE,   4, 4, 1, 100.0, 200.0, 10.0, -999.0, 'FLOAT     ')
CALL MAP_FINE_TO_COARSE(COARSE, FINE, ICOL_COARSE, IROW_COARSE)
CALL CHECK(ICOL_COARSE, [1,1,2,2], 'ICOL_COARSE (Case 2)', NFAIL)
CALL CHECK(IROW_COARSE, [1,1,2,2], 'IROW_COARSE (Case 2)', NFAIL)

! Case 3: Finer Resolution -----------------------------------------------------
CALL ALLOCATE_EMPTY_RASTER(COARSE, 3, 3, 1, 0.0, 0.0, 30.0, -999.0, 'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(FINE,   6, 6, 1, 0.0, 0.0, 10.0, -999.0, 'FLOAT     ')
CALL MAP_FINE_TO_COARSE(COARSE, FINE, ICOL_COARSE, IROW_COARSE)
CALL CHECK(ICOL_COARSE, [1,1,1,2,2,2], 'ICOL_COARSE (Case 3)', NFAIL)
CALL CHECK(IROW_COARSE, [1,1,1,2,2,2], 'IROW_COARSE (Case 3)', NFAIL)

! Case 4: Grid Overlap & Clipping ----------------------------------------------
CALL ALLOCATE_EMPTY_RASTER(COARSE, 3, 3, 1, 0.0, 0.0, 30.0, -999.0, 'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(FINE,   6, 6, 1, 0.0, 0.0, 35.0, -999.0, 'FLOAT     ')
CALL MAP_FINE_TO_COARSE(COARSE, FINE, ICOL_COARSE, IROW_COARSE)
CALL CHECK(ICOL_COARSE, [1,2,3,3,3,3], 'ICOL_COARSE (Case 4)', NFAIL)
CALL CHECK(IROW_COARSE, [1,2,3,3,3,3], 'IROW_COARSE (Case 4)', NFAIL)

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
        PRINT *, 'FAIL: ', TRIM(LABEL), '(', I, ') =', ACTUAL(I), ' (expected ', EXPECTED(I), ')'
        NFAIL = NFAIL + 1
    ELSE
        PRINT *, 'PASS: ', TRIM(LABEL)
    END IF
END DO
! =============================================================================
END SUBROUTINE CHECK
! ==============================================================================

! *****************************************************************************
END PROGRAM TEST_MAP_FINE_TO_COARSE
! *****************************************************************************