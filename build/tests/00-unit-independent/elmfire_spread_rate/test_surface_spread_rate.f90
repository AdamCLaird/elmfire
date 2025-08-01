!> test_surface_spread_rate.f90
!! Unit test for subroutine SURFACE_SPREAD_RATE(...) in elmfire_spread_rate.f90
!! Updated 07-24-2025

!! NOT WORKING

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
TYPE(NODE), POINTER :: DUMMY => NULL()

PRINT *, 'TESTING SURFACE_SPREAD_RATE...'

! Allocate and initialize a test node
ALLOCATE(C)
 C%IX = 30
 C%IY = 30
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
FUEL_MODEL_TABLE_2D(1,30) = FUEL_MODEL_TABLE_TYPE( &
SHORTNAME = 'TEST', DYNAMIC = .FALSE., &
W0  = 0.0, WN = 0.0, SIG = 0.0, DELTA = 0.1, &
MEX_DEAD = 0.25, MEX_LIVE = 0.8, HOC = 8000.0, &
RHOB = 30.0, RHOP = 45.0, ST = 0.055, SE = 0.01, ETAS = 1.0, &
BETA = 0.01, BETAOP = 0.02, XI = 0.1, &
A_COEFF = 1.0, B_COEFF = 0.5, C_COEFF = 0.0, E_COEFF = 0.0, &
GAMMAPRIME = 1.0, GAMMAPRIMEPEAK = 1.0, &
A_DEAD = 0.0, A_LIVE = 0.0, A_OVERALL = 0.0, &
F_DEAD = 0.6, F_LIVE = 0.4, &
W0_DEAD = 0.2, W0_LIVE = 0.1, &
WN_DEAD = 0.15, WN_LIVE = 0.05, &
SIG_DEAD = 1200.0, SIG_LIVE = 1500.0, SIG_OVERALL = 1300.0, &
TR = 1.0, &
A = 0.0, F = (/0.3,0.2,0.1,0.0,0.2,0.2/), &
FMEX = 0.25, FW0 = 0.0, FSIG = 0.0, &
EPS = 0.01, FEPS = (/0.01,0.01,0.01,0.01,0.02,0.02/), &
WPRIMENUMER = 0.1, WPRIMEDENOM = 0.1, MPRIMEDENOM = 0.25, &
GP_WND_EMD_ES_HOC = 100.0, GP_WNL_EML_ES_HOC = 50.0, &
PHISTERM = 0.3, PHIWTERM = 0.2, &
WPRIMEDENOM56SUM = 0.2, WPRIMENUMER14SUM = 0.4, MPRIMEDENOM14SUM = 0.5, &
R_MPRIMEDENOME14SUM_MEX_DEAD = 0.01, &
UNSHELTERED_WAF = 1.0, &
B_COEFF_INVERSE = 2.0, WSMFEFF_COEFF = 1.0 )


! Call subroutine under test
CALL SURFACE_SPREAD_RATE(L, DUMMY)

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