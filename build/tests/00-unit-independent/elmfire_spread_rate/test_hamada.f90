!> test_hamada.f90
!! Unit test for subroutine HAMADA(C) in elmfire_spread_rate.f90
!! Updated 07-24-2025

! *****************************************************************************
PROGRAM TEST_HAMADA
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SPREAD_RATE

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
TYPE(NODE), POINTER :: C

PRINT *, 'TESTING HAMADA...'

! Allocate dummy global fields
ALLOCATE(BLDG_AREA%R4(10,10,1))
ALLOCATE(BLDG_SEPARATION_DIST%R4(10,10,1))
ALLOCATE(BLDG_NONBURNABLE_FRAC%R4(10,10,1))

! Case 1: Moderate Wind, Default Values ---------------------------------------
ALLOCATE(C)
 C%IX = 5
 C%IY = 5
 C%WS20_NOW = 10.0
BLDG_AREA%R4(5,5,1) = 20.0
BLDG_SEPARATION_DIST%R4(5,5,1) = 10.0
BLDG_NONBURNABLE_FRAC%R4(5,5,1) = 0.25
CALL CHECK(C, 'Moderate Wind')

! Case 2: Low Wind, NB Structures ---------------------------------------------
 C%WS20_NOW = 3.0
BLDG_AREA%R4(5,5,1) = 15.0
BLDG_SEPARATION_DIST%R4(5,5,1) = 5.0
BLDG_NONBURNABLE_FRAC%R4(5,5,1) = 1.0
CALL CHECK(C, 'Low Wind, Nonburnable')

! Case 3: High Wind, All Burnable ---------------------------------------------
 C%WS20_NOW = 25.0
BLDG_AREA%R4(5,5,1) = 30.0
BLDG_SEPARATION_DIST%R4(5,5,1) = 12.0
BLDG_NONBURNABLE_FRAC%R4(5,5,1) = 0.0
CALL CHECK(C, 'High Wind, All Burnable')

! Case 4: No Wind, Mixed Structures -------------------------------------------
 C%WS20_NOW = 0.0
BLDG_AREA%R4(5,5,1) = 10.0
BLDG_SEPARATION_DIST%R4(5,5,1) = 15.0
BLDG_NONBURNABLE_FRAC%R4(5,5,1) = 0.5
CALL CHECK(C, 'No Wind, Mixed Structures')

! Case 5: Zero Building Area --------------------------------------------------
 C%WS20_NOW = 12.0
BLDG_AREA%R4(5,5,1) = 0.0
BLDG_SEPARATION_DIST%R4(5,5,1) = 10.0
BLDG_NONBURNABLE_FRAC%R4(5,5,1) = 0.5
CALL CHECK(C, 'Zero Building Area')

! Case 6: Zero Building Separation --------------------------------------------
 C%WS20_NOW = 12.0
BLDG_AREA%R4(5,5,1) = 25.0
BLDG_SEPARATION_DIST%R4(5,5,1) = 0.0
BLDG_NONBURNABLE_FRAC%R4(5,5,1) = 0.25
CALL CHECK(C, 'Zero Building Separation')

! Final Output
IF (NFAIL == 0) THEN
  PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
  PRINT *, 'FAIL:', NFAIL, ' TEST(S) FAILED.'
  STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(C, LABEL)
! =============================================================================
TYPE(NODE), POINTER :: C
CHARACTER(*), INTENT(IN) :: LABEL
REAL :: VD, VS, VU, LOW

CALL HAMADA(C)

VD  = C%VELOCITY_DMS
VU  = C%VBACK
LOW = C%LOW
VS  = (VD + VU)/(2*LOW)

IF (VD <= 0 .OR. VU <= 0 .OR. LOW <= 0) THEN
  PRINT *, 'FAIL: ', LABEL, 'VELOCITY_DMS=', VD, 'VBACK=', VU, 'LOW=', LOW
  NFAIL = NFAIL + 1
ELSE
  PRINT *, 'PASS: ', LABEL, '...Valid outputs'
END IF

IF (LOW > 10.0) THEN
    PRINT *, 'FAIL: ', LABEL, 'LOW exceeds bounds: LOW=', LOW
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', LABEL, '...LOW within bounds'
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_HAMADA
! *****************************************************************************