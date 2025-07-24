!> test_rk2_integrate.f90
!! Unit test for subroutine RK2_INTEGRATE(DT,ISTEP) in elmfire_level_set.f90
!! Updated: 07-21-2025

! *****************************************************************************
PROGRAM TEST_RK2_INTEGRATE
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_LEVEL_SET

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
TYPE(NODE), POINTER :: C
INTEGER, PARAMETER :: IX=2, IY=2

PRINT *, 'TESTING RK2_INTEGRATE...'

! Set grid size
ALLOCATE(PHIP(3,3))
PHIP(:,:) = 0.0

! Set up a single-node list
ALLOCATE(C)
 C%IX = IX
 C%IY = IY
 C%NEXT => NULL()
LIST_TAGGED%HEAD => C
LIST_TAGGED%NUM_NODES = 1

! Case 1: ISTEP = 1 -----------------------------------------------------------
 C%PHIP_OLD = 10.0
 C%UX = 1.0
 C%UY = 1.0
 C%DPHIDX_LIMITED = 2.0
 C%DPHIDY_LIMITED = 1.0
CALL CHECK(1.0, 1, 7.0, "Step 1")

! Case 2: ISTEP = 2 -----------------------------------------------------------
PHIP(IX,IY) = 7.0
 C%DPHIDX_LIMITED = 1.0
 C%DPHIDY_LIMITED = 1.0
CALL CHECK(1.0, 2, 7.5, "Step 2")

! Case 3: Clamp Upper ---------------------------------------------------------
 C%PHIP_OLD = 200.0
 C%UX = 0.0
 C%UY = 0.0
CALL CHECK(1.0, 1, 100.0, "Clamp Upper")

! Case 4: Clamp Lower ---------------------------------------------------------
 C%PHIP_OLD = -200.0
CALL CHECK(1.0, 1, -100.0, "Clamp Lower")


! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(DT_IN, ISTEP, EXPECTED, LABEL)
! =============================================================================
REAL, INTENT(IN) :: DT_IN, EXPECTED ! Inputs
INTEGER, INTENT(IN) :: ISTEP
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: DT                          ! Locals
REAL, PARAMETER :: EPSILON = 1.0E-6

DT = DT_IN
CALL RK2_INTEGRATE(DT, ISTEP)
IF (ABS(PHIP(IX,IY) - EXPECTED) > EPSILON) THEN
    PRINT *, 'FAIL: ', LABEL, 'DT=', DT, ' EXPECTED=', EXPECTED
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_RK2_INTEGRATE
! *****************************************************************************