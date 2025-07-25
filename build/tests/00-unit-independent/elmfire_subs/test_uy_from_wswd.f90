!> test_uy_from_wswd.f90
!! Unit test for function UY_FROM_WSWD in elmfire_subs.f90
!! Updated: 07-25-2025

! *****************************************************************************
PROGRAM TEST_UY_FROM_WSWD
! *****************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
REAL :: WS_VAL, WD_VAL
INTEGER :: NFAIL = 0

PRINT *, 'TESTING UY_FROM_WSWD...'

! Cardinal Wind Directions -----------------------------------------------------
CALL CHECK(10.0, 0.0, -10.0)        ! North
CALL CHECK(10.0, 90.0, 0.0)         ! East
CALL CHECK(10.0, 180.0, 10.0)       ! South
CALL CHECK(10.0, 270.0, 0.0)        ! West

! Diagonal Wind Directions -----------------------------------------------------
CALL CHECK(SQRT(2.0), 45.0, -1.0)   ! NE
CALL CHECK(SQRT(2.0), 135.0, 1.0)   ! SE
CALL CHECK(SQRT(2.0), 225.0, 1.0)   ! SW
CALL CHECK(SQRT(2.0), 315.0, -1.0)  ! NW

! Zero Wind Speed --------------------------------------------------------------
CALL CHECK(0.0, 123.0, 0.0)

! Negative Wind Speed ----------------------------------------------------------
CALL CHECK(-5.0, 90.0, 0.0)

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(WS_VAL, WD_VAL, EXPECTED)
! =============================================================================
REAL, INTENT(IN) :: WS_VAL, WD_VAL, EXPECTED     ! Inputs
REAL :: RESULT                                   ! Locals
REAL, PARAMETER :: TOL = 1.0E-5
RESULT = UY_FROM_WSWD(WS_VAL, WD_VAL)

IF (ABS(RESULT - EXPECTED) > TOL) THEN
    PRINT *, 'FAIL: WS_VAL=', WS_VAL, ' WD_VAL=', WD_VAL, &
            ' EXPECTED=', EXPECTED, ' GOT=', RESULT
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_UY_FROM_WSWD
! *****************************************************************************