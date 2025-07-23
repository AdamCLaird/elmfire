!> test_tag_band.f90
!! Unit test for subroutine TAG_BAND(NX, NY, ...) in elmfire_level_set.f90
!! Updated: 07-22-2025

! *****************************************************************************
PROGRAM TEST_TAG_BAND
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS
USE ELMFIRE_LEVEL_SET

IMPLICIT NONE

! Locals
INTEGER, PARAMETER :: NX = 10, NY = 10, IXCEN = 5, IYCEN = 5
REAL, PARAMETER :: T = 100.0
INTEGER :: NFAIL
LOGICAL :: MASK1(NX,NY), MASK2(NX,NY), MASK3(NX,NY) ! Append for new cases

PRINT *, 'TESTING TAG_BAND...'

! Allocate and initialize
NFAIL = 0

ALLOCATE(TAGGED(NX,NY))
ALLOCATE(EVERTAGGED(NX,NY))
ALLOCATE(ISNONBURNABLE(NX,NY))

TAGGED = .FALSE.
EVERTAGGED = .FALSE.
ISNONBURNABLE = .FALSE.
NUM_EVERTAGGED = 0

LIST_TAGGED = NEW_DLL()
NULLIFY(LIST_TAGGED%TAIL)

ALLOCATE(PHIP(NX,NY))
ALLOCATE(PHI(NX,NY))
ALLOCATE(IGNTIME(NX,NY))
ALLOCATE(SUPPRESSION_TIME(NX,NY))
ALLOCATE(TOA(NX,NY))
ALLOCATE(BURNED(NX,NY))

PHIP(:,:)              = -1.0
PHI(:,:)               = -1.0
IGNTIME(:,:)           = 0.0
SUPPRESSION_TIME(:,:)  = -1.0
TOA(:,:)               = 0.0
BURNED(:,:)            = 0

! Case 1: BANDTHICKNESS = 3, center of domain ---------------------------------
BANDTHICKNESS = 3
MASK1 = .FALSE.
MASK1(3:7, 3:7) = .TRUE.   ! 3:7 in both directions
CALL CHECK_TAG_BAND(NX, NY, IXCEN, IYCEN, T, 49, MASK1)

! Case 2: BANDTHICKNESS = 1, edge-proximal ------------------------------------
BANDTHICKNESS = 1
CALL RESET_FIELDS(NX, NY)
MASK2 = .FALSE.
MASK2(2:4,2:4) = .TRUE.
CALL CHECK_TAG_BAND(NX, NY, 3, 3, T, 9, MASK2)

! Case 3: BANDTHICKNESS = 2, near lower boundary ------------------------------
BANDTHICKNESS = 2
CALL RESET_FIELDS(NX, NY)
MASK3 = .FALSE.
MASK3(3:4,3:4) = .TRUE.  ! tag window clamps at index 3 due to MAX(3, ...)
CALL CHECK_TAG_BAND(NX, NY, 2, 2, T, 4, MASK3)

! Case 4: Already tagged cells should not duplicate ---------------------------
CALL CHECK_TAG_BAND(NX, NY, 2, 2, T, 0, MASK3)


! Final result
IF (NFAIL == 0) THEN
  PRINT *, 'PASS: ALL TESTS PASSED.'
ELSE
  PRINT *, 'FAIL: ', NFAIL, ' TEST(S) FAILED.'
  STOP 1
END IF

CONTAINS

! =============================================================================
SUBROUTINE RESET_FIELDS(NX, NY)
! =============================================================================
INTEGER, INTENT(IN) :: NX, NY               ! Inputs
TYPE(NODE), POINTER :: CURR, NEXT           ! Pointers

! Reset tagging arrays
TAGGED         = .FALSE.
EVERTAGGED     = .FALSE.
ISNONBURNABLE  = .FALSE.
NUM_EVERTAGGED = 0

PHIP(:,:)              = -1.0
PHI(:,:)               = -1.0
IGNTIME(:,:)           = 0.0
SUPPRESSION_TIME(:,:)  = -1.0
TOA(:,:)               = 0.0
BURNED(:,:)            = 0

! Deallocate tagged nodes
CURR => LIST_TAGGED%HEAD
DO WHILE (ASSOCIATED(CURR))
    NEXT => CURR%NEXT
    DEALLOCATE(CURR)
    CURR => NEXT
ENDDO

! Reinitialize the linked list
LIST_TAGGED = NEW_DLL()
NULLIFY(LIST_TAGGED%TAIL)
! =============================================================================
END SUBROUTINE RESET_FIELDS
! =============================================================================

! =============================================================================
SUBROUTINE CHECK_TAG_BAND(NX, NY, IXLOC, IYLOC, T, EXPECTED_COUNT, EXPECTED_TAGGED)
! =============================================================================
INTEGER, INTENT(IN) :: NX, NY, IXLOC, IYLOC, EXPECTED_COUNT ! Inputs
LOGICAL, INTENT(IN) :: EXPECTED_TAGGED(NX,NY)
REAL,    INTENT(IN) :: T
INTEGER :: IX, IY, COUNT                                    ! Locals

CALL TAG_BAND(NX, NY, IXLOC, IYLOC, T)

! Count number of tagged cells
COUNT = 0
DO IY = 1, NY
  DO IX = 1, NX
    IF (TAGGED(IX,IY)) COUNT = COUNT + 1
  END DO
END DO

! Check tagged cell count
IF (COUNT /= EXPECTED_COUNT) THEN
  PRINT *, 'FAIL: IX=', IXLOC, ' IY=', IYLOC, ' TAGGED=', COUNT, ' EXPECTED=', EXPECTED_COUNT
  NFAIL = NFAIL + 1
END IF

! Check tag mask matches
DO IY = 1, NY
  DO IX = 1, NX
    IF (TAGGED(IX,IY) .NEQV. EXPECTED_TAGGED(IX,IY)) THEN
      PRINT *, 'FAIL: Unexpected tag at (', IX, ',', IY, ') - TAGGED =', TAGGED(IX,IY), &
                ', EXPECTED =', EXPECTED_TAGGED(IX,IY)
      NFAIL = NFAIL + 1
    END IF
  END DO
END DO
! =============================================================================
END SUBROUTINE CHECK_TAG_BAND
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_TAG_BAND
! *****************************************************************************