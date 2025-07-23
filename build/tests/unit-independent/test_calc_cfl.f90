!> test_calc_cfl.f90
!! Unit test for subroutine CALC_CFL(DT) in elmfire_level_set.f90
!! Updated: 07-21-2025

! *****************************************************************************
PROGRAM TEST_CALC_CFL
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_LEVEL_SET

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
TYPE(NODE), POINTER :: C1, C2, C3

PRINT *, 'TESTING CALC_CFL...'

! Set up parameters
ANALYSIS_CELLSIZE = 10.0
SIMULATION_DTMAX = 5.0
TARGET_CFL = 0.8

! Case 1: Normal speed, UMAX = 4 ----------------------------------------------
ALLOCATE(C1, C2)
C1%UX = 2.0; C1%UY = 3.0; C1%BURNED = .FALSE.
C2%UX = 4.0; C2%UY = 1.0; C2%BURNED = .FALSE.
C1%NEXT => C2
C2%NEXT => NULL()
LIST_TAGGED%HEAD => C1
LIST_TAGGED%NUM_NODES = 2
CALL CHECK(1.0, 2.0, "Normal speed")

! Case 2: All nodes burned ----------------------------------------------------
C1%BURNED = .TRUE.
C2%BURNED = .TRUE.
CALL CHECK(1.0, 5.0, "All nodes burned")

! Case 3: Zero velocities -----------------------------------------------------
C1%BURNED = .FALSE.; C2%BURNED = .FALSE.
C1%UX = 0.0; C1%UY = 0.0
C2%UX = 0.0; C2%UY = 0.0
CALL CHECK(1.0, 5.0, "Zero velocities")

! Case 4: UMAX so high CFL > 1, test clamping ---------------------------------
C1%UX = 100.0; C1%UY = 0.0
C2%UX = 0.0;  C2%UY = 0.0
CALL CHECK(1.0, 0.08, "Test clamping")

! Case 5: Add third node with lower speed -------------------------------------
ALLOCATE(C3)
C2%NEXT => C3
C3%UX = 0.5; C3%UY = 0.5; C3%BURNED = .FALSE.
C3%NEXT => NULL()
LIST_TAGGED%NUM_NODES = 3
CALL CHECK(1.0, 0.08, "Three nodes")

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(DT_IN, EXPECTED, LABEL)
! =============================================================================
REAL, INTENT(IN) :: DT_IN, EXPECTED ! Inputs
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: DT                          ! Locals
REAL, PARAMETER :: EPSILON = 1.0E-4

DT = DT_IN
CALL CALC_CFL(DT)                   ! Call subroutine & check values
IF (ABS(DT - EXPECTED) > EPSILON) THEN
    PRINT *, 'FAIL: ', LABEL, 'DT=', DT, ' EXPECTED=', EXPECTED
    NFAIL = NFAIL + 1
END IF

! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM
! *****************************************************************************