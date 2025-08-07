!> test_hour_of_year_to_timestamp.f90
!! Unit test for HOUR_OF_YEAR_TO_TIMESTAMP in elmfire_subs.f90
!! Updated: 07-24-2025

! ******************************************************************************
PROGRAM TEST_HOUR_OF_YEAR_TO_TIMESTAMP
! ******************************************************************************

USE ELMFIRE_SUBS

IMPLICIT NONE

! Locals
CHARACTER(16) :: RESULT, EXPECTED
INTEGER :: NFAIL = 0, YEAR, HOUR

PRINT *, 'TESTING HOUR_OF_YEAR_TO_TIMESTAMP...'

! Basic Cases ------------------------------------------------------------------
CALL CHECK(2025,    0,  '2025-01-01 00:00')
CALL CHECK(2025,   24,  '2025-01-02 00:00')
CALL CHECK(2025,  8759, '2025-12-31 23:00')  ! Non-leap year

! Leap Year --------------------------------------------------------------------
CALL CHECK(2024,  8783, '2024-12-31 23:00')  ! 2024 is leap year (366 days)

! Month Boundary Checks --------------------------------------------------------
CALL CHECK(2025,  744,  '2025-02-01 00:00')  ! Feb 1 (Jan = 744 hrs)
CALL CHECK(2025, 1415,  '2025-02-28 23:00')
CALL CHECK(2025, 2160,  '2025-03-31 00:00')
CALL CHECK(2025, 2880,  '2025-04-30 00:00')
CALL CHECK(2025, 3624,  '2025-05-31 00:00')

! Edge Check -------------------------------------------------------------------
CALL CHECK(2024, 1439,  '2024-02-29 23:00')  ! Last second of Feb in leap year

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! ==============================================================================
SUBROUTINE CHECK(YEAR, HOUR, EXPECTED)
! ==============================================================================
INTEGER, INTENT(IN) :: YEAR, HOUR       ! Inputs
CHARACTER(16), INTENT(IN) :: EXPECTED
CHARACTER(16) :: RESULT                 ! Locals

RESULT = HOUR_OF_YEAR_TO_TIMESTAMP(YEAR, HOUR)
IF (TRIM(RESULT) /= TRIM(EXPECTED)) THEN
    PRINT *, 'FAIL: YEAR=',YEAR,' HOUR=',HOUR,' EXPECTED=', TRIM(EXPECTED),' GOT=', TRIM(RESULT)
    NFAIL = NFAIL + 1
ELSE
    PRINT *, 'PASS: ', EXPECTED
END IF
! ==============================================================================
END SUBROUTINE CHECK
! ==============================================================================

! ******************************************************************************
END PROGRAM TEST_HOUR_OF_YEAR_TO_TIMESTAMP
! ******************************************************************************