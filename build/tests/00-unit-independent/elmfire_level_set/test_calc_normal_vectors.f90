!> test_calc_normal_vectors.f90
!! Unit test for subroutine CALC_NORMAL_VECTORS(...) in elmfire_level_set.f90
!! Updated: 07-28-2025

!! NOT WORKING

! *****************************************************************************
PROGRAM TEST_CALC_NORMAL_VECTORS
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_LEVEL_SET
USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
INTEGER :: IX, IY
REAL :: TOL, PHITOL

! Parameters
REAL, PARAMETER :: HALFRCELL = 1.0
REAL, PARAMETER :: EPSILON = 1.0E-5
INTEGER, PARAMETER :: NX = 5, NY = 5

! Allocatable Arrays
REAL, ALLOCATABLE :: PHI(:,:), PHIP(:,:), NORMVECTORX(:), NORMVECTORY(:), PHIP_OLD(:)

ALLOCATE(PHI(NX,NY), PHIP(NX,NY))
ALLOCATE(NORMVECTORX(100), NORMVECTORY(100), PHIP_OLD(100))

PRINT *, 'TESTING CALC_NORMAL_VECTORS...'

! Case 1: Gradient in x only --------------------------------------------------
PHIP = 1.0
PHI(4,3) = -1.0
CALL CHECK(PHIP, HALFRCELL, -1.0, 0.0, "X Gradient")

! Case 2: Gradient in y only --------------------------------------------------
PHIP = 1.0
PHIP(3,4) = -1.0
CALL CHECK(PHIP, HALFRCELL, 0.0, -1.0, "Y Gradient")

! Case 3: Diagonal gradient (1,1) ---------------------------------------------
PHIP = 1.0
PHIP(4,4) = -1.0
CALL CHECK(PHIP, HALFRCELL, -SQRT(0.5), -SQRT(0.5), "XY Diagonal")

! Case 4: Clamp Gradient ------------------------------------------------------
PHIP = 1.0
PHIP(4,3) = -1.0E10
PHIP(2,3) = +1.0E10
PHIP(3,4) = -1.0E10
PHIP(3,2) = +1.0E10
CALL CHECK(PHIP, HALFRCELL, -SQRT(0.5), -SQRT(0.5), "Clamped Gradient")

! Case 5: Zero gradient -------------------------------------------------------
PHIP = 1.0
CALL CHECK(PHIP, HALFRCELL, 0.0, 0.0, "Zero Gradient")


! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(PHIP_IN, RCELL, EXPECTED_X, EXPECTED_Y, LABEL)
! =============================================================================
REAL, INTENT(IN) :: PHIP_IN(:,:), RCELL, EXPECTED_X, EXPECTED_Y
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: DX, DY
TYPE(DLL) :: LIST_TAGGED
TYPE(NODE), POINTER :: C

! Copy input
PHIP(:,:) = PHIP_IN

! Tag a band around (3,3)
TOL = 1.0
PHITOL = 0.5
CALL TAG_BAND(NX, NY, 3, 3, TOL, LIST_TAGGED)

! Resize output arrays to match tag count
IF (ALLOCATED(NORMVECTORX)) DEALLOCATE(NORMVECTORX, NORMVECTORY, PHIP_OLD)
ALLOCATE(NORMVECTORX(LIST_TAGGED%NUM_NODES))
ALLOCATE(NORMVECTORY(LIST_TAGGED%NUM_NODES))
ALLOCATE(PHIP_OLD  (LIST_TAGGED%NUM_NODES))

! Compute normals
CALL CALC_NORMAL_VECTORS(1, RCELL)

! Check first tagged node
 C => LIST_TAGGED%HEAD
DX = NORMVECTORX(C%INDEX)
DY = NORMVECTORY(C%INDEX)

IF (ABS(DX - EXPECTED_X) > EPSILON .OR. ABS(DY - EXPECTED_Y) > EPSILON) THEN
    PRINT *, 'FAIL: ', LABEL
    PRINT *, '  NORMX =', DX, ' EXPECTED =', EXPECTED_X
    PRINT *, '  NORMY =', DY, ' EXPECTED =', EXPECTED_Y
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_CALC_NORMAL_VECTORS
! *****************************************************************************