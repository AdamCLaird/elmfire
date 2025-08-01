!> test_limit_gradients.f90
!! Unit test for subroutine LIMIT_GRADIENTS() in elmfire_level_set.f90
!! Updated: 07-23-2025

!! NOT WORKING

! *****************************************************************************
PROGRAM TEST_LIMIT_GRADIENTS
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_LEVEL_SET

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
REAL :: RCELL
REAL, PARAMETER :: EPS = 1.0E-4
INTEGER, PARAMETER :: NX = 5, NY = 5
TYPE(NODE), POINTER :: C
REAL, DIMENSION(NX,NY) :: PHI

PRINT *, 'TESTING LIMIT_GRADIENTS...'

! ! Case 1: Only X Gradient, +U -------------------------------------------------
! RCELL = 0.5
! PHI(:,:) = 1.0
! PHI(3,3) = -1.0  ! Center
! PHI(4,3) = 0.8   ! East
! PHI(2,3) = 0.8   ! West
! CALL CHECK(PHI, 3, 3, 1.0, 0.0, -0.8, 1.0, 'X Gradient, +U')

! ! Case 2: Only Y Gradient, +U -------------------------------------------------
! RCELL = 0.5
! PHI(:,:) = 1.0
! PHI(3,3) = -1.0  ! Center
! PHI(3,4) = 0.8   ! North
! PHI(3,2) = 0.8   ! South
! CALL CHECK(PHI, 3, 3, 0.0, 1.0, -1.0, -0.8, 'Y Gradient, +U')

! Case 3: Negative UX, UY -----------------------------------------------------
RCELL = 0.5
PHI(:,:) = 1.0
PHI(3,3) = -1.0  ! Center
PHI(3,4) = 0.8   ! North
PHI(3,2) = -0.8  ! South
PHI(4,3) = 0.8   ! East
PHI(2,3) = -0.8  ! West
CALL CHECK(PHI, 3, 3, -0.85, -0.85, 0.8, 0.8, 'Symmetric Gradient, -UX/-UY')

! ! Case 4: Zero Gradient -------------------------------------------------------
! RCELL = 0.5
! PHI(:,:) = 1.0
! PHI(3,3) = -1.0  ! Center
! PHI(3,4) = -1.0  ! North
! PHI(3,2) = -1.0  ! South
! PHI(4,3) = -1.0  ! East
! PHI(2,3) = -1.0  ! West
! CALL CHECK(PHI, 3, 3, 0.0, 0.0, 0.8, 0.8, 'Zero Gradient')

! ! Case 5: Gradient Clipping ---------------------------------------------------
! RCELL = 1.0E-3
! PHI(:,:) = 1.0
! PHI(3,3) = -1.0  ! Center
! PHI(3,4) = 1.0   ! North
! PHI(3,2) = -1.0  ! South
! PHI(4,3) = 1.0   ! East
! PHI(2,3) = -1.0  ! West
! CALL CHECK(PHI, 3, 3, 1.0, 1.0, 0.0, 0.0, 'Gradient Clipping')

! ! Case 6: Asymmetric Gradient X Only ------------------------------------------
! RCELL = 0.5
! PHI(:,:) = 1.0
! PHI(3,3) = -1.0  ! Center
! PHI(4,3) =  0.5  ! East
! PHI(2,3) = -1.0  ! West
! PHI(1,3) = -2.0  ! West-2
! CALL CHECK(PHI, 3, 3, 1.0, 0.0, 1.5, 0.0, 'Asymmetric Gradient X Only')

! ! Case 7: Asymmetric Gradient Y Only ------------------------------------------
! RCELL = 0.5
! PHI(:,:) = 1.0
! PHI(3,3) = -1.0   ! Center
! PHI(3,4) =  1.0   ! North
! PHI(3,2) = -0.5   ! South
! PHI(3,1) = -1.0   ! South-2
! CALL CHECK(PHI, 3, 3, 1.0, 0.0, 0.0, 1.5, 'Asymmetric Gradient Y Only')

! ! Case 8: Edge Clipping, West -------------------------------------------------
! RCELL = 0.5
! PHI(:,:) = 1.0
! PHI(2,3) = -1.0   ! Center
! PHI(3,3) =  1.0   ! East
! PHI(1,3) = -1.0   ! West
! CALL CHECK(PHI, 2, 3, 1.0, 0.0, 2.0, 0.0, 'Edge Clipping West')

! ! Case 9: Edge Clipping, North ------------------------------------------------
! RCELL = 0.5
! PHI(:,:) = 1.0
! PHI(3,4) = -1.0   ! Center 
! PHI(3,5) =  1.0   ! North
! PHI(3,3) = -0.5   ! South
! CALL CHECK(PHI, 3, 4, 0.0, 1.0, 0.0, 1.5, 'Edge Clipping North')


! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(PHI_IN, IX, IY, UX, UY, EXPECT_X, EXPECT_Y, LABEL)
! =============================================================================
REAL, INTENT(IN) :: PHI_IN(NX,NY), EXPECT_X, EXPECT_Y
CHARACTER(*), INTENT(IN) :: LABEL
INTEGER, INTENT(IN) :: IX, IY
REAL, INTENT(IN) :: UX, UY
TYPE(NODE), POINTER :: C
REAL :: GX, GY

! Create fresh node
ALLOCATE(C)
 C%IX = IX
 C%IY = IY
 C%UX = UX
 C%UY = UY
 C%NEXT => NULL()
LIST_TAGGED%HEAD => C
LIST_TAGGED%NUM_NODES = 1

CALL LIMIT_GRADIENTS(RCELL, PHI_IN)
GX = C%DPHIDX_LIMITED
GY = C%DPHIDY_LIMITED

IF (ABS(GX - EXPECT_X) > EPS .OR. ABS(GY - EXPECT_Y) > EPS) THEN
  PRINT *, 'FAIL:', LABEL
  PRINT *, 'DPHIDX_LIMITED=', GX, 'DPHIDY_LIMITED=', GY, &
           'Expected=', EXPECT_X, EXPECT_Y
  NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_LIMIT_GRADIENTS
! *****************************************************************************