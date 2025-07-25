!> test_surface_spread_rate.f90
!! Unit test for subroutine SURFACE_SPREAD_RATE(...) in elmfire_spread_rate.f90
!! Updated 07-24-2025

! *****************************************************************************
PROGRAM TEST_SURFACE_SPREAD_RATE
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SPREAD_RATE

IMPLICIT NONE

! Locals
INTEGER :: NFAIL = 0
TYPE(NODE), POINTER :: C
TYPE(DLL) :: L

PRINT *, 'TESTING SURFACE_SPREAD_RATE...'

! Allocate and initialize a test node
ALLOCATE(C)
 C%IX = 5
 C%IY = 5
 C%IFBFM = 1       ! Valid FBFM
 C%M1    = 0.3     ! Fuel load 1h
 C%M10   = 0.2     ! Fuel load 10h
 C%M100  = 0.1     ! Fuel load 100h
 C%MLH   = 0.05    ! Live herbaceous
 C%MLW   = 0.05    ! Live woody
 C%TANSLP2 = 0.2
 C%ADJ = 1.0
 C%WSMF = 5.0

! Link into dummy list
 C%NEXT => NULL()
L%HEAD => C
L%NUM_NODES = 1

! Required control flags
ENABLE_EXTENDED_ATTACK = .FALSE.
PERTURB_ADJ = 0.0
DIURNAL_ADJUSTMENT_FACTOR = 1.0

! Minimal FUEL_MODEL_TABLE_2D initialization
ALLOCATE(FUEL_MODEL_TABLE_2D(100,121))
FUEL_MODEL_TABLE_2D(:,:) = FUEL_MODEL_TABLE_TYPE( &
    GP_WND_EMD_ES_HOC = 100.0, GP_WNL_EML_ES_HOC = 50.0, &
    MEX_DEAD = 0.25, MEX_LIVE = 0.8, R_MPRIMEDENOME14SUM_MEX_DEAD = 0.01, &
    TR = 1.0, XI = 0.1, RHOB = 30.0, B_COEFF = 0.5, &
    PHIWTERM = 0.3, PHISTERM = 0.2, &
    F = (/0.3, 0.2, 0.1, 0.0, 0.2, 0.2/), &
    FMEX = (/0.25, 0.25, 0.25, 0.25/), &
    FEPS = (/0.01, 0.01, 0.01, 0.01, 0.02, 0.02/), &
    WPRIMENUMER = (/0.1, 0.1, 0.1, 0.1/), &
    F_DEAD = 0.6, F_LIVE = 0.4 )

! Call subroutine under test
CALL SURFACE_SPREAD_RATE(L, DUMMY_NODE = NULL())

! Check results
CALL CHECK(C, 'Basic working case')

! Output results
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
REAL :: IR, V, FL

IR = C%IR
V  = C%VELOCITY_DMS_SURFACE
FL = C%FLIN_DMS_SURFACE

IF (IR <= 0.0 .OR. V <= 0.0 .OR. FL < 0.0) THEN
    PRINT *, 'FAIL:', LABEL
    PRINT *, ' IR=', IR, ' VEL=', V, ' FLIN=', FL
    NFAIL = NFAIL + 1
END IF
! =============================================================================
END SUBROUTINE CHECK
! =============================================================================

! *****************************************************************************
END PROGRAM TEST_SURFACE_SPREAD_RATE
! *****************************************************************************