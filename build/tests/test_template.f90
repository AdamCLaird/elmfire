!> test_template.f90
!! Template for unit tests in ELMFIRE
!! Copy and modify for new subroutines/functions
!! Updated: [DATE]

! *****************************************************************************
PROGRAM TEST_TEMPLATE
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_LEVEL_SET

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0

PRINT *, 'TESTING TEMPLATE...'

! Setup 
! Allocate memory, initialize relevant fields
! For example:
! REAL :: DT, EXPECTED
! TYPE(NODE), POINTER :: C
! ALLOCATE(C)
! C%FIELD = value
! ...

! Test Cases
! CALL CHECK(<input1>, <expected1>)
! CALL CHECK(<input2>, <expected2>)

! Check outputs and print results
IF (NFAIL == 0) THEN
    PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
    PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
    STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE CHECK(INPUT, EXPECTED)
! =============================================================================
REAL, INTENT(IN) :: INPUT, EXPECTED ! Inputs
REAL :: RESULT                      ! Locals
REAL, PARAMETER :: EPSILON = 1.0E-6

! RESULT = MYFUNC(INPUT)
! or
! CALL MYSUB(INPUT, ...)
IF (ABS(RESULT - EXPECTED) > EPSILON) THEN
    PRINT *, 'FAIL: INPUT=', INPUT, ' EXPECTED=', EXPECTED, ' GOT=', RESULT
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_TEMPLATE
! *****************************************************************************