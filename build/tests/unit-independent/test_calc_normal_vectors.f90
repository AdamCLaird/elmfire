!> test_calc_normal_vectors.f90
!! Unit test for subroutine CALC_NORMAL_VECTORS(...) in elmfire_level_set.f90
!! Updated: 07-22-2025

! *****************************************************************************
PROGRAM TEST_CALC_NORMAL_VECTORS
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_LEVEL_SET

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL, PARAMETER :: HALFRCELL = 0.5
REAL, PARAMETER :: EPSILON = 1.0E-6
INTEGER, PARAMETER :: NX = 5, NY = 5
TYPE(NODE), POINTER :: C

PRINT *, 'TESTING CALC_NORMAL_VECTORS...'

! Allocate and initialize grid
ALLOCATE(PHIP(NX,NY))
PHIP(:,:) = 0.0

! Allocate and initialize tagged list
ALLOCATE(C)
 C%IX = 3
 C%IY = 3
 C%NEXT => NULL()
LIST_TAGGED%HEAD => C
LIST_TAGGED%NUM_NODES = 1

! Case 1: Gradient in x only --------------------------------------------------
PHIP(4,3) = 1.0
PHIP(2,3) = -1.0
CALL CHECK(1, HALFRCELL, 1.0, 0.0, "X Gradient")

! Case 2: Gradient in y only --------------------------------------------------
PHIP = 0.0
PHIP(3,4) = 2.0
PHIP(3,2) = -2.0
CALL CHECK(1, HALFRCELL, 0.0, 1.0, "Y Gradient")

! Case 3: Diagonal gradient (1,1) ---------------------------------------------
PHIP = 0.0
PHIP(4,3) = 1.0
PHIP(2,3) = -1.0
PHIP(3,4) = 1.0
PHIP(3,2) = -1.0
CALL CHECK(1, HALFRCELL, SQRT(0.5), SQRT(0.5), "XY Diagonal")

! Case 4: Clamp Gradient ------------------------------------------------------
PHIP = 0.0
PHIP(4,3) = 1.0E10
PHIP(2,3) = -1.0E10
PHIP(3,4) = 1.0E10
PHIP(3,2) = -1.0E10
CALL CHECK(1, HALFRCELL, SQRT(0.5), SQRT(0.5), "Clamped Gradient")

! Case 5: Zero gradient -------------------------------------------------------
PHIP = 0.0
CALL CHECK(1, HALFRCELL, 0.0, 0.0, "Zero Gradient")


! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(ISTEP, HALFRCELL, EXPECTED_X, EXPECTED_Y, LABEL)
! =============================================================================
INTEGER, INTENT(IN) :: ISTEP                    ! Inputs
REAL,    INTENT(IN) :: HALFRCELL, EXPECTED_X, EXPECTED_Y
CHARACTER(*), INTENT(IN) :: LABEL
REAL, PARAMETER :: EPS = 1E-5                   ! Locals
REAL :: NX, NY

CALL CALC_NORMAL_VECTORS(ISTEP, HALFRCELL)
NX = C%NORMVECTORX
NY = C%NORMVECTORY

IF (ABS(NX - EXPECTED_X) > EPS .OR. ABS(NY - EXPECTED_Y) > EPS) THEN
    PRINT *, 'FAIL: ', LABEL, ' NORMX=', NX, ' NORMY=', NY, ' Expected=', EXPECTED_X, EXPECTED_Y
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_CALC_NORMAL_VECTORS
! *****************************************************************************