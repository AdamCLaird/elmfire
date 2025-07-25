MODULE ELMFIRE_NAMELISTS

USE ELMFIRE_VARS

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE READ_MISC
! *****************************************************************************

INTEGER :: IOS

NAMELIST /MISCELLANEOUS/ BUILDING_FUEL_MODEL_FILE, FUEL_MODEL_FILE, MISCELLANEOUS_INPUTS_DIRECTORY, PATH_TO_GDAL, SCRATCH

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &MISCELLANEOUS namelist group'

!Set default values:
BUILDING_FUEL_MODEL_FILE       = 'building_fuel_models.csv'
FUEL_MODEL_FILE                = 'null'
MISCELLANEOUS_INPUTS_DIRECTORY = 'null'
PATH_TO_GDAL                   = '/usr/bin'
SCRATCH                        = 'null'

READ(LUINPUT,NML=MISCELLANEOUS,IOSTAT=IOS)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error: Problem with namelist group &MISCELLANEOUS.'
   STOP
ENDIF

PATH_TO_GDAL = TRIM(PATH_TO_GDAL) // PATH_SEPARATOR

! if dirs are still null don't add a path separator
IF (MISCELLANEOUS_INPUTS_DIRECTORY .NE. 'null') THEN
   MISCELLANEOUS_INPUTS_DIRECTORY = TRIM(MISCELLANEOUS_INPUTS_DIRECTORY) // PATH_SEPARATOR
ENDIF
IF (SCRATCH .NE. 'null') THEN
   SCRATCH = TRIM(SCRATCH) // PATH_SEPARATOR
ENDIF

! *****************************************************************************
END SUBROUTINE READ_MISC
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SMOKE
! *****************************************************************************

INTEGER :: IOS

NAMELIST /SMOKE/ DT_SMOKE_OUTPUTS, ENABLE_SMOKE_OUTPUTS, PM2P5_RELEASE_MIN_FOR_OUTPUT, SMOKE_HOC, &
                 SMOKE_OUTPUTS_DUMP_PERCENT, SMOKE_YIELD, USE_SMOKE_YIELD_BY_FUEL, SMOKE_YIELD_BY_FUEL

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SMOKE namelist group'

!Set default values:
DT_SMOKE_OUTPUTS               = 3600.0
ENABLE_SMOKE_OUTPUTS           = .FALSE.
PM2P5_RELEASE_MIN_FOR_OUTPUT   = 0.
SMOKE_HOC                      = 12.0
SMOKE_OUTPUTS_DUMP_PERCENT     = 100.0
SMOKE_YIELD                    = 0.01
SMOKE_YIELD_BY_FUEL(:)         = 0.01
USE_SMOKE_YIELD_BY_FUEL        = .FALSE.

READ(LUINPUT,NML=SMOKE,IOSTAT=IOS)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error: Problem with namelist group &SMOKE.'
   STOP
ENDIF

IF (.NOT. USE_SMOKE_YIELD_BY_FUEL) SMOKE_YIELD_BY_FUEL(:) = SMOKE_YIELD

! *****************************************************************************
END SUBROUTINE READ_SMOKE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_INPUTS
! *****************************************************************************

INTEGER :: I, IOS
INTEGER(8) :: I8DUMMY
REAL :: RDUMMY
CHARACTER(400) :: FN

NAMELIST /INPUTS/ &
ADJ_FILENAME, ASP_FILENAME, BLDG_AREA_FILENAME, BLDG_FOOTPRINT_FRAC_FILENAME, BLDG_FUEL_MODEL_FILENAME, &
BLDG_NONBURNABLE_FRAC_FILENAME, BLDG_SEPARATION_DIST_FILENAME, &
CBD_FILENAME, CBD_TIMES_100, CBH_FILENAME, CBH_TIMES_10, CC_FILENAME, CC_IN_PERCENT, &
CH_FILENAME, CH_TIMES_10, DEM_FILENAME, DT_METEOROLOGY, FBFM_FILENAME, FMC_FILENAME, FOLIAR_MOISTURE_CONTENT, &
FUELS_AND_TOPOGRAPHY_DIRECTORY, GRID_DECLINATION, &
IGNITIONS_CSV_FILENAME, IGNITION_MASK_FILENAME, LAND_VALUE_FILENAME, LH_MOISTURE_CONTENT, LW_MOISTURE_CONTENT, &
DEAD_MC_IN_PERCENT, LIVE_MC_IN_PERCENT, PHI_FILENAME, POPULATION_DENSITY_FILENAME, REAL_ESTATE_VALUE_FILENAME, &
SLP_FILENAME, ERC_FILENAME, FMC_FILENAME, M100_FILENAME, M10_FILENAME, M1_FILENAME, MLH_FILENAME, MLW_FILENAME, &
PYROMES_FILENAME, USE_BSQ_XML_HEADER, ROTATE_ASP, ROTATE_WD, WD_FILENAME, WS_FILENAME, USE_CONSTANT_FMC, &
USE_CONSTANT_LH, USE_CONSTANT_LW, USE_EXISTING_BSQS, USE_LAND_VALUE, USE_POPULATION_DENSITY, USE_REAL_ESTATE_VALUE, &
USE_TILED_IO, WEATHER_DIRECTORY, WS_AT_10M, VRT_INSTEAD_OF_TIF, SDI_FILENAME, TIMED_LOCATIONS_CSV, ONLY_READ_NEEDED_WX_BANDS

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &INPUTS namelist group'

!Set default values:
ADJ_FILENAME                   = ' '
ASP_FILENAME                   = ' '
BLDG_AREA_FILENAME             = ' '
BLDG_FOOTPRINT_FRAC_FILENAME   = ' '
BLDG_FUEL_MODEL_FILENAME       = ' '
BLDG_NONBURNABLE_FRAC_FILENAME = ' '
BLDG_SEPARATION_DIST_FILENAME  = ' '
CBD_FILENAME                   = ' '
CBD_TIMES_100                  = .TRUE.
CBH_FILENAME                   = ' '
CBH_TIMES_10                   = .TRUE.
CC_FILENAME                    = ' '
CC_IN_PERCENT                  = .TRUE.
CH_FILENAME                    = ' '
CH_TIMES_10                    = .TRUE.
DEM_FILENAME                   = ' '
DT_METEOROLOGY                 = -9999.9
FBFM_FILENAME                  = ' '
FMC_FILENAME                   = ' '
FOLIAR_MOISTURE_CONTENT        = 90.0
FUELS_AND_TOPOGRAPHY_DIRECTORY = ' '
GRID_DECLINATION               = 0.0
IGNITION_MASK_FILENAME         = ' '
LAND_VALUE_FILENAME            = ' '
LH_MOISTURE_CONTENT            = 60.0
LW_MOISTURE_CONTENT            = 60.0 
DEAD_MC_IN_PERCENT             = .TRUE.
LIVE_MC_IN_PERCENT             = .TRUE.
PHI_FILENAME                   = ' '
POPULATION_DENSITY_FILENAME    = ' '
REAL_ESTATE_VALUE_FILENAME     = ' '
SDI_FILENAME                   = ' '
SLP_FILENAME                   = ' '
ERC_FILENAME                   = ' '
FMC_FILENAME                   = ' '
IGNITIONS_CSV_FILENAME         = ' '
M100_FILENAME                  = ' '
M10_FILENAME                   = ' '
M1_FILENAME                    = ' '
MLH_FILENAME                   = ' '
MLW_FILENAME                   = ' '
PYROMES_FILENAME               = ' '
ROTATE_ASP                     = .FALSE.
ROTATE_WD                      = .FALSE.
TIMED_LOCATIONS_CSV            = 'null'
WD_FILENAME                    = ' '
WS_FILENAME                    = ' '
USE_BSQ_XML_HEADER             = .TRUE.
USE_CONSTANT_FMC               = .TRUE.
USE_CONSTANT_LH                = .TRUE.
USE_CONSTANT_LW                = .TRUE.
USE_EXISTING_BSQS              = .FALSE.
USE_LAND_VALUE                 = .FALSE.
USE_POPULATION_DENSITY         = .FALSE.
USE_REAL_ESTATE_VALUE          = .FALSE.
USE_TILED_IO                   = .FALSE.
VRT_INSTEAD_OF_TIF             = .FALSE.
WEATHER_DIRECTORY              = ' '
WS_AT_10M                      = .FALSE.
ONLY_READ_NEEDED_WX_BANDS      = .FALSE.

READ(LUINPUT,NML=INPUTS,IOSTAT=IOS)
IF (IOS > 0) THEN
    WRITE(*,*) 'Error: Problem with namelist group &INPUTS.'
    STOP
ENDIF

FUELS_AND_TOPOGRAPHY_DIRECTORY = TRIM(FUELS_AND_TOPOGRAPHY_DIRECTORY) // PATH_SEPARATOR
WEATHER_DIRECTORY              = TRIM(WEATHER_DIRECTORY             ) // PATH_SEPARATOR

PROCESS_TIMED_LOCATIONS = .FALSE.
IF (TRIM(TIMED_LOCATIONS_CSV) .EQ. 'null' ) RETURN

FN = TRIM(TIMED_LOCATIONS_CSV)
OPEN(LUAUXINPUT,FILE=TRIM(FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)

IF (IOS .NE. 0) THEN 
   WRITE(*,*) 'Could not open TIMED_LOCATIONS_CSV'
   RETURN
ENDIF

READ (LUAUXINPUT,*,IOSTAT=IOS)
IF (IOS .NE. 0) THEN 
   WRITE(*,*) 'Bad header in TIMED_LOCATIONS_CSV'
   RETURN
ENDIF

NUM_TIMED_LOCATIONS = 0
DO WHILE (IOS .EQ. 0)
   READ (LUAUXINPUT,*,IOSTAT=IOS) I8DUMMY, RDUMMY, RDUMMY
   IF (IOS .EQ. 0) NUM_TIMED_LOCATIONS = NUM_TIMED_LOCATIONS + 1
ENDDO
CLOSE(LUAUXINPUT)

IF (NUM_TIMED_LOCATIONS .EQ. 0) RETURN

PROCESS_TIMED_LOCATIONS = .TRUE.

ALLOCATE(TIMED_LOCATIONS_TRACKER(1:NUM_TIMED_LOCATIONS))

OPEN(LUAUXINPUT,FILE=TRIM(FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
READ (LUAUXINPUT,*,IOSTAT=IOS)
DO I = 1, NUM_TIMED_LOCATIONS
    READ (LUAUXINPUT,*,IOSTAT=IOS) TIMED_LOCATIONS_TRACKER(I)%ID, TIMED_LOCATIONS_TRACKER(I)%X, TIMED_LOCATIONS_TRACKER(I)%Y
ENDDO
CLOSE(LUAUXINPUT)

! *****************************************************************************
END SUBROUTINE READ_INPUTS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_OUTPUTS
! *****************************************************************************

INTEGER :: I, IOS
REAL, ALLOCATABLE, DIMENSION (:) :: TABA ! Time at burned acres

NAMELIST /OUTPUTS/ &
ACCUMULATE_EMBER_FLUX, BINARY_OUTPUTS_DUMP_FRACTION, CALCULATE_TIMES_BURNED, CALCULATE_FLAME_LENGTH_STATS, &
CONVERT_TO_GEOTIFF, DTDUMP, DUMP_AFFECTED_LAND_VALUE, DUMP_AFFECTED_POPULATION, DUMP_AFFECTED_REAL_ESTATE_VALUE, &
DUMP_BINARY_OUTPUTS, DUMP_CROWN_FIRE, DUMP_EMBER_FLUX, DUMP_CROWN_FIRE_AREA, &
DUMP_FIRE_SIZE_STATS, DUMP_FIRE_VOLUME, DUMP_FLAME_LENGTH, DUMP_FLIN, DUMP_HOURLY_RASTERS, &
DUMP_HPUA, DUMP_PHI, &
DUMP_REACTION_INTENSITY, DUMP_REAL_ESTATE_VALUE, DUMP_SPOTTING_IGNITION_TIME, &
DUMP_SPREAD_RATE, DUMP_SURFACE_FIRE, DUMP_SURFACE_FIRE_AREA, DUMP_TAGGED, DUMP_TIME_OF_ARRIVAL, DUMP_TIMINGS, &
DUMP_TRANSIENT_ACREAGE, DUMP_VELOCITY, DUMP_WD20, DUMP_WS20, EMBER_COUNT_BIN_LO, EMBER_COUNT_BIN_HI, &
FULL_BINARY_OUTPUTS, NUM_EMBER_COUNT_BINS, NUM_VIRTUAL_STATIONS, &
FLAME_LENGTH_BIN_LO, FLAME_LENGTH_BIN_HI, MINIMUM_AREA_FOR_BINARY_OUTPUTS, &
NUM_FLAME_LENGTH_BINS, OUTPUTS_DIRECTORY, USE_EMBER_COUNT_BINS, USE_FLAME_LENGTH_BINS, &
DUMP_SPOTTING_OUTPUTS, RUN_ID, DUMP_TOTAL_DFC_RECEIVED, DUMP_TOTAL_RAD_RECEIVED, &
DUMP_HRR_TRANSIENT, TIME_AT_BURNED_ACRES, USE_FOUR_DIGITS_IN_IWX_BAND, VIRTUAL_STATION_X, VIRTUAL_STATION_Y

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &OUTPUTS namelist group'

ALLOCATE(TABA(1:1000), TIME_AT_BURNED_ACRES(1:1000))

ACCUMULATE_EMBER_FLUX             = .FALSE.
BINARY_OUTPUTS_DUMP_FRACTION      = 1E0
CALCULATE_FLAME_LENGTH_STATS      = .FALSE.
CALCULATE_TIMES_BURNED            = .FALSE.
CONVERT_TO_GEOTIFF                = .TRUE. 
DTDUMP                            = 3600.0
DUMP_AFFECTED_LAND_VALUE          = .FALSE.
DUMP_AFFECTED_POPULATION          = .FALSE.
DUMP_AFFECTED_REAL_ESTATE_VALUE   = .FALSE.
DUMP_BINARY_OUTPUTS               = .FALSE.
DUMP_CROWN_FIRE                   = .FALSE.
DUMP_CROWN_FIRE_AREA              = .FALSE.
DUMP_EMBER_FLUX                   = .FALSE.
DUMP_SPOTTING_OUTPUTS             = .FALSE.
DUMP_FIRE_SIZE_STATS              = .TRUE. 
DUMP_FIRE_VOLUME                  = .FALSE.
DUMP_FLAME_LENGTH                 = .FALSE. 
DUMP_FLIN                         = .FALSE.
DUMP_HOURLY_RASTERS               = .FALSE.
DUMP_HPUA                         = .FALSE.
DUMP_HRR_TRANSIENT                = .FALSE.  
DUMP_PHI                          = .FALSE. 
DUMP_REACTION_INTENSITY           = .FALSE. 
DUMP_SPOTTING_IGNITION_TIME       = .FALSE. 
DUMP_SPREAD_RATE                  = .FALSE. 
DUMP_SURFACE_FIRE                 = .FALSE. 
DUMP_SURFACE_FIRE_AREA            = .FALSE.
DUMP_TAGGED                       = .FALSE. 
DUMP_TIME_OF_ARRIVAL              = .FALSE. 
DUMP_TIMINGS                      = .FALSE.
DUMP_TOTAL_DFC_RECEIVED           = .FALSE.
DUMP_TOTAL_RAD_RECEIVED           = .FALSE. 
DUMP_TRANSIENT_ACREAGE            = .FALSE. 
DUMP_VELOCITY                     = .FALSE. 
DUMP_WD20                         = .FALSE. 
DUMP_WS20                         = .FALSE. 
EMBER_COUNT_BIN_HI(:)             = 0
EMBER_COUNT_BIN_LO(:)             = 0
FLAME_LENGTH_BIN_HI(:)            = 0.
FLAME_LENGTH_BIN_LO(:)            = 0.
FULL_BINARY_OUTPUTS               = .TRUE.
MINIMUM_AREA_FOR_BINARY_OUTPUTS   = 0.
NUM_EMBER_COUNT_BINS              = 0
NUM_FLAME_LENGTH_BINS             = 0
NUM_VIRTUAL_STATIONS              = 0
OUTPUTS_DIRECTORY                 = ' '
RUN_ID                            = ''
TIME_AT_BURNED_ACRES(:)           = -9E9
USE_EMBER_COUNT_BINS              = .FALSE.
USE_FLAME_LENGTH_BINS             = .FALSE.
USE_FOUR_DIGITS_IN_IWX_BAND       = .FALSE.
VIRTUAL_STATION_X(:)              = 0.0
VIRTUAL_STATION_Y(:)              = 0.0

READ(LUINPUT,NML=OUTPUTS,IOSTAT=IOS)
IF (IOS > 0) THEN
    WRITE(*,*) 'Error: Problem with namelist group &OUTPUTS.'
    STOP
ENDIF

OUTPUTS_DIRECTORY = TRIM(OUTPUTS_DIRECTORY) // PATH_SEPARATOR

NUM_TIME_AT_BURNED_ACRES = 0
DO I = 1, 1000
   IF (TIME_AT_BURNED_ACRES(I) .LE. 0.) CYCLE
   NUM_TIME_AT_BURNED_ACRES = NUM_TIME_AT_BURNED_ACRES + 1
   TABA(NUM_TIME_AT_BURNED_ACRES) = TIME_AT_BURNED_ACRES(I)    
ENDDO

DEALLOCATE (TIME_AT_BURNED_ACRES)
ALLOCATE(TIME_AT_BURNED_ACRES(1:NUM_TIME_AT_BURNED_ACRES))
TIME_AT_BURNED_ACRES=TABA(1:NUM_TIME_AT_BURNED_ACRES)
DEALLOCATE(TABA)

! *****************************************************************************
END SUBROUTINE READ_OUTPUTS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_COMPUTATIONAL_DOMAIN
! *****************************************************************************

INTEGER :: IOS

NAMELIST /COMPUTATIONAL_DOMAIN/ A_SRS, COMPUTATIONAL_DOMAIN_CELLSIZE, COMPUTATIONAL_DOMAIN_XLLCORNER, &
         COMPUTATIONAL_DOMAIN_YLLCORNER

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &COMPUTATIONAL_DOMAIN namelist group'

A_SRS                             = 'EPSG:32610'
COMPUTATIONAL_DOMAIN_CELLSIZE     = 30.0
COMPUTATIONAL_DOMAIN_XLLCORNER    = 0.0
COMPUTATIONAL_DOMAIN_YLLCORNER    = 0.0

READ(LUINPUT,NML=COMPUTATIONAL_DOMAIN,IOSTAT=IOS)
IF (IOS > 0) THEN
    WRITE(*,*) 'Error: Problem with namelist group &COMPUTATIONAL_DOMAIN.'
    STOP
ENDIF

ANALYSIS_CELLSIZE  = COMPUTATIONAL_DOMAIN_CELLSIZE
ANALYSIS_XLLCORNER = COMPUTATIONAL_DOMAIN_XLLCORNER
ANALYSIS_YLLCORNER = COMPUTATIONAL_DOMAIN_YLLCORNER

! *****************************************************************************
END SUBROUTINE READ_COMPUTATIONAL_DOMAIN
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_TIME_CONTROL
! *****************************************************************************

INTEGER :: IOS

NAMELIST /TIME_CONTROL/ &
BAND_ONE_HOUR_OF_YEAR, BURN_PERIOD_CENTER_FRAC, BURN_PERIOD_LENGTH, CURRENT_YEAR, DT_INTERPOLATE_M1, &
DT_INTERPOLATE_M10, DT_INTERPOLATE_M100, DT_INTERPOLATE_MLH, DT_INTERPOLATE_MLW, DT_INTERPOLATE_FMC, &
DT_INTERPOLATE_WIND, FORECAST_START_HOUR, HOUR_OF_YEAR, LATITUDE, LONGITUDE, &
OVERNIGHT_ADJUSTMENT_FACTOR, RANDOMIZE_SIMULATION_TSTOP, SIMULATION_DT, SIMULATION_DTMAX, &
SIMULATION_TSTART, SIMULATION_TSTOP, SUNRISE_HOUR, SUNSET_HOUR, TARGET_CFL, &
USE_DIURNAL_ADJUSTMENT_FACTOR

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &TIME_CONTROL namelist group'

BAND_ONE_HOUR_OF_YEAR         = 0
BURN_PERIOD_CENTER_FRAC       = 0.667
BURN_PERIOD_LENGTH            = 10.0
CURRENT_YEAR                  = 1970
DT_INTERPOLATE_M1             = 300.0
DT_INTERPOLATE_M10            = 3000.0
DT_INTERPOLATE_M100           = 30000.0
DT_INTERPOLATE_MLH            = 9E8
DT_INTERPOLATE_MLW            = 9E8
DT_INTERPOLATE_FMC            = 9E8
DT_INTERPOLATE_WIND           = 300.0
FORECAST_START_HOUR           = 20.0
HOUR_OF_YEAR                  = 4368
LATITUDE                      = -40.
LONGITUDE                     = -120.
OVERNIGHT_ADJUSTMENT_FACTOR   = 0.1
RANDOMIZE_SIMULATION_TSTOP    = .FALSE. 
SIMULATION_DT                 = 5.0 
SIMULATION_DTMAX              = 600.0
SIMULATION_TSTART             = 0.0
SIMULATION_TSTOP              = 3600.0
SUNRISE_HOUR                  = 13.0 ! UTC, over-ridden by call to SUNRISE_SUNSET_CALCS
SUNSET_HOUR                   = 27.0 ! UTC, over-ridden by call to SUNRISE_SUNSET_CALCS
TARGET_CFL                    = 0.4
USE_DIURNAL_ADJUSTMENT_FACTOR = .FALSE.

READ(LUINPUT,NML=TIME_CONTROL,IOSTAT=IOS)
IF (IOS > 0) THEN
    WRITE(*,*) 'Error: Problem with namelist group &TIME_CONTROL.'
    STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_TIME_CONTROL
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_MONTE_CARLO
! *****************************************************************************

INTEGER :: IOS, IVARN

NAMELIST /MONTE_CARLO/ ADD_TO_IGNITION_MASK, ALLOW_MULTIPLE_IGNITIONS_AT_A_PIXEL, CSV_FIXED_IGNITION_LOCATIONS, &
EDGEBUFFER, ERC_IS_PLIGNRATE, &
IGNITION_MASK_SCALE_FACTOR, METEOROLOGY_BAND_START, METEOROLOGY_BAND_STOP, METEOROLOGY_BAND_SKIP_INTERVAL, &
NUM_ENSEMBLE_MEMBERS, NUM_METEOROLOGY_TIMES, NUM_RASTERS_TO_PERTURB, PDF_LOWER_LIMIT, PDF_TYPE, PDF_UPPER_LIMIT, &
PERCENT_OF_PIXELS_TO_IGNITE, RANDOM_IGNITIONS, RANDOM_IGNITIONS_TYPE, RASTER_TO_PERTURB, SEED, SPATIAL_PERTURBATION, &
TEMPORAL_PERTURBATION, USE_ERC, USE_IGNITION_MASK, WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX, &
WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN, WIND_SPEED_FLUCTUATION_INTENSITY_MAX, WIND_SPEED_FLUCTUATION_INTENSITY_MIN

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &MONTE_CARLO namelist group'

!Set default values:
ADD_TO_IGNITION_MASK                     = -9E9
ALLOW_MULTIPLE_IGNITIONS_AT_A_PIXEL      = .FALSE.
CSV_FIXED_IGNITION_LOCATIONS             = .FALSE.
EDGEBUFFER                               = 3000.
ERC_IS_PLIGNRATE                         = .FALSE.
IGNITION_MASK_SCALE_FACTOR               = 1.0
METEOROLOGY_BAND_START                   = -1
METEOROLOGY_BAND_STOP                    = -1
METEOROLOGY_BAND_SKIP_INTERVAL           = -1
NUM_ENSEMBLE_MEMBERS                     = 1
NUM_RASTERS_TO_PERTURB                   = 0
NUM_METEOROLOGY_TIMES                    = 1
PDF_LOWER_LIMIT(:)                       = 0.
PDF_TYPE(:)                              = 'null'
PDF_UPPER_LIMIT(:)                       = 0.
PERCENT_OF_PIXELS_TO_IGNITE              = 5.0
RANDOM_IGNITIONS                         = .FALSE.
RANDOM_IGNITIONS_TYPE                    = 1
RASTER_TO_PERTURB(:)                     = 'null'
SEED                                     = 2024
SPATIAL_PERTURBATION(:)                  = 'null'
TEMPORAL_PERTURBATION(:)                 = 'null'
USE_ERC                                  = .FALSE.
USE_IGNITION_MASK                        = .FALSE.
WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX = -1.0 
WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN = -1.0
WIND_SPEED_FLUCTUATION_INTENSITY_MAX     = -1.0
WIND_SPEED_FLUCTUATION_INTENSITY_MIN     = -1.0 

! Not part of namelist group but set here:
PERTURB_WIND_DIRECTION_FLUCTUATION_INTENSITY = .FALSE.
PERTURB_WIND_SPEED_FLUCTUATION_INTENSITY     = .FALSE. 

READ(LUINPUT,NML=MONTE_CARLO,IOSTAT=IOS)
IF (IOS > 0) THEN
    WRITE(*,*) 'Error: Problem with namelist group &MONTE_CARLO.'
    STOP
ENDIF

NUM_PARAMETERS_RASTERS    = 0
NUM_PARAMETERS_MISC       = 0
NUM_MONTE_CARLO_VARIABLES = 0

IF (RANDOM_IGNITIONS) THEN
   IF (USE_IGNITION_MASK) THEN
      IF ( TRIM(IGNITIONS_CSV_FILENAME) .EQ. '' .AND. TRIM(IGNITION_MASK_FILENAME) .EQ. '' ) THEN
         WRITE(*,200) 'When RANDOM_IGNITIONS = .TRUE. ELMFIRE requires a Float32 ignition'
         WRITE(*,200) 'mask specified via the keyword IGNITION_MASK_FILENAME on the &INPUTS'
         WRITE(*,200) 'namelist group OR a sequence of igntitions contained in a .csv file'
         WRITE(*,200) 'specified via the keyworkd IGNITIONS_CSV_FILENAME on the &INPUTS'
         WRITE(*,200) 'namelist group. Please set IGNITION_MASK_FILENAME or IGNITIONS_CSV_FILENAME'
         WRITE(*,200) 'and rerun.'
         STOP
      ENDIF
   ELSE
      IF (TRIM(IGNITIONS_CSV_FILENAME) .EQ. '') THEN
         WRITE(*,200) 'When RANDOM_IGNITIONS = .TRUE., setting USE_IGNITION_MASK = .FALSE. is now deprecated.'
         WRITE(*,200) 'unless IGNITIONS_CSV_FILENAME is specified.'
         WRITE(*,200) 'ELMFIRE assumes when RANDOM_IGNITIONS = .TRUE. that a Float32 ignition mask is provided'
         WRITE(*,200) 'via the keyword IGNITION_MASK_FILENAME on the &INPUTS namelist group. The keyword '
         WRITE(*,200) 'USE_IGNITION_MASK is scheduled for removal from the &MONTE_CARLO namelist group. '
         WRITE(*,200) 'Until that time, please set USE_IGNITION_MASK = .TRUE. and set IGNITION_MASK_FILENAME.'
         STOP
      ENDIF
   ENDIF
ENDIF

DO IVARN = 1, NUM_RASTERS_TO_PERTURB
   IF (SPATIAL_PERTURBATION(IVARN) .NE. 'GLOBAL' .AND. SPATIAL_PERTURBATION(IVARN) .NE. 'PIXEL') THEN
      WRITE(*,200) 'Error, SPATIAL_PERTURBATION must be GLOBAL or PIXEL. Variation: ', IVARN
      STOP
   ENDIF
   IF (TEMPORAL_PERTURBATION(IVARN) .NE. 'STATIC' .AND. TEMPORAL_PERTURBATION(IVARN) .NE. 'DYNAMIC') THEN
      WRITE(*,200) 'Error, TEMPORAL_PERTURBATION must be STATIC or DYNAMIC. Variation: ', IVARN
      STOP
   ENDIF
   IF (PDF_TYPE(IVARN) .NE. 'UNIFORM' ) THEN
      WRITE(*,200) 'Error, PDF_TYPE must be UNIFORM. Variation: ', IVARN
      STOP
   ENDIF
      
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'ADJ'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'CBD'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'CBH'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'CC'   ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'CH'   ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'FBFM' ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'FMC'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'M1'   ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'M10'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'M100' ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'MLH'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'MLW'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'WAF'  ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'WD'   ) THEN
   IF (RASTER_TO_PERTURB(IVARN) .NE. 'WS'   ) THEN
      WRITE(*,200) 'Error on variation ', IVARN, ' RASTER_TO_PERTURB must be one of: ' 
      WRITE(*,200) 'ADJ, CBD, CBH, CC, CH, FBFM, FMC, M1, M10, M100, MLH, MLW, WAF, WD, WS'
      STOP
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
   ENDIF
      
   IF (TRIM(SPATIAL_PERTURBATION(IVARN)) .NE. 'PIXEL') THEN
      IF (TRIM(TEMPORAL_PERTURBATION(IVARN)) .EQ. 'STATIC') THEN
         NUM_PARAMETERS_RASTERS = NUM_PARAMETERS_RASTERS + 1
      ELSE
         NUM_PARAMETERS_RASTERS = NUM_PARAMETERS_RASTERS + NUM_METEOROLOGY_TIMES
      ENDIF      
   ENDIF
      
ENDDO

IF (WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN .GT. 0. .AND. WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX .GT. 0.) THEN
   PERTURB_WIND_DIRECTION_FLUCTUATION_INTENSITY = .TRUE.
   NUM_PARAMETERS_MISC = NUM_PARAMETERS_MISC + 1
ENDIF

IF (WIND_SPEED_FLUCTUATION_INTENSITY_MIN .GT. 0. .AND. WIND_SPEED_FLUCTUATION_INTENSITY_MAX .GT. 0.) THEN
   PERTURB_WIND_SPEED_FLUCTUATION_INTENSITY = .TRUE.
   NUM_PARAMETERS_MISC = NUM_PARAMETERS_MISC + 1
ENDIF

NUM_MONTE_CARLO_VARIABLES = NUM_PARAMETERS_RASTERS + NUM_PARAMETERS_MISC + NUM_PARAMETERS_SPOTTING
ALLOCATE(COEFFS         (1:NUM_MONTE_CARLO_VARIABLES))
ALLOCATE(COEFFS_UNSCALED(1:NUM_MONTE_CARLO_VARIABLES))

! Figure out which bands to read and loop over
IF (METEOROLOGY_BAND_START.GT.0 .AND. METEOROLOGY_BAND_STOP.GT.0 .AND. METEOROLOGY_BAND_SKIP_INTERVAL.GT.0) THEN
   IWX_BAND_START = METEOROLOGY_BAND_START
   IWX_BAND_STOP  = METEOROLOGY_BAND_STOP
   IWX_BAND_SKIP  = METEOROLOGY_BAND_SKIP_INTERVAL
ELSE
   IWX_BAND_START = 1
   IWX_BAND_STOP  = 1
   IWX_BAND_SKIP  = 1
ENDIF

200 FORMAT(A, I9)

! *****************************************************************************
END SUBROUTINE READ_MONTE_CARLO
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SIMULATOR
! *****************************************************************************

INTEGER :: IOS

NAMELIST /SIMULATOR/ &
ALLOW_NONBURNABLE_PIXEL_IGNITION, BANDTHICKNESS, CRITICAL_CANOPY_COVER, &
CROWN_FIRE_ADJ, CROWN_FIRE_MODEL, CROWN_FIRE_SPREAD_RATE_LIMIT, CROWN_RATIO, DEBUG_LEVEL, DT_WIND_FLUCTUATIONS, &
ESTIMATE_URBAN_LOSSES, MAX_LOW, MAX_RUNTIME, MODE, MULTIPLE_HOSTS, NUM_IGNITIONS, NUM_NODES_OMP_THRESHOLD, &
PHIS_ADJ, PHIW_ADJ, PLIGNRATE_MIN, RANDOMIZE_RANDOM_SEED,SURFACE_ACCELERATION_TIME_CONSTANT, T_IGN, &
UNTAG_CELLS_TIMESTEP_INTERVAL, UNTAG_TYPE_2, UNTAG_TYPE_3, USE_PYROMES, &
WIND_DIRECTION_FLUCTUATION_INTENSITY, WIND_FLUCTUATIONS, WIND_SPEED_FLUCTUATION_INTENSITY, X_IGN, Y_IGN, &
WSMFEFF_LOW_MULT, WX_BILINEAR_INTERPOLATION

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SIMULATOR namelist group'

ALLOW_NONBURNABLE_PIXEL_IGNITION     = .TRUE.
BANDTHICKNESS                        = 2
CRITICAL_CANOPY_COVER                = 0.39
CROWN_FIRE_ADJ                       = 1.0
CROWN_FIRE_MODEL                     = 1
CROWN_FIRE_SPREAD_RATE_LIMIT         = 250.0
CROWN_RATIO                          = 1.0
DEBUG_LEVEL                          = 0
DT_WIND_FLUCTUATIONS                 = 15.0
ESTIMATE_URBAN_LOSSES                = .FALSE.
MAX_LOW                              = 8.0
MAX_RUNTIME                          = 999999.
MODE                                 = 1 !1 = level set propagation; 2 = fire potential; 3 = both
MULTIPLE_HOSTS                       = .FALSE.
NUM_IGNITIONS                        = 0
NUM_NODES_OMP_THRESHOLD              = 999999999
PHIS_ADJ                             = 1E0
PHIW_ADJ                             = 1E0
PLIGNRATE_MIN                        = 0E0
RANDOMIZE_RANDOM_SEED                = .FALSE. 
SURFACE_ACCELERATION_TIME_CONSTANT   = 1.0
T_IGN(:)                             = 0.0
UNTAG_CELLS_TIMESTEP_INTERVAL        = 10
UNTAG_TYPE_2                         = .FALSE.
UNTAG_TYPE_3                         = .FALSE.
WIND_DIRECTION_FLUCTUATION_INTENSITY = 0.0
WIND_FLUCTUATIONS                    = .FALSE.
WIND_SPEED_FLUCTUATION_INTENSITY     = 0.0
X_IGN(:)                             = 0.0
Y_IGN(:)                             = 0.0
USE_PYROMES                          = .FALSE.
WSMFEFF_LOW_MULT                     = 5.07955E-3
WX_BILINEAR_INTERPOLATION            = .FALSE.

READ(LUINPUT,NML=SIMULATOR,IOSTAT=IOS)
IF (IOS > 0) THEN
    WRITE(*,*) 'Error: Problem with namelist group &SIMULATOR.'
    STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_SIMULATOR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_WUI
! *****************************************************************************

INTEGER :: IOS

NAMELIST /WUI/ BLDG_AREA_CONSTANT, BLDG_NONBURNABLE_FRAC_CONSTANT, BLDG_SEPARATION_DIST_CONSTANT, &
               BLDG_SPREAD_MODEL_TYPE, BLDG_FOOTPRINT_FRAC_CONSTANT, BLDG_FUEL_MODEL_CONSTANT, &
               USE_BLDG_SPREAD_MODEL, USE_CONSTANT_BLDG_SPREAD_MODEL_PARAMS, GLOBAL_HARDENING_FACTOR

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &WUI namelist group'

BLDG_AREA_CONSTANT                    = 20.0
BLDG_SEPARATION_DIST_CONSTANT         = 10.0
BLDG_NONBURNABLE_FRAC_CONSTANT        = 0.0
BLDG_FOOTPRINT_FRAC_CONSTANT          = 1.0
BLDG_FUEL_MODEL_CONSTANT              = 1
BLDG_SPREAD_MODEL_TYPE                = 1 ! 1 = Hamada, 2 = UCB / UMD
USE_BLDG_SPREAD_MODEL                 = .FALSE.
USE_CONSTANT_BLDG_SPREAD_MODEL_PARAMS = .TRUE.
GLOBAL_HARDENING_FACTOR              = 1.0

READ(LUINPUT,NML=WUI,IOSTAT=IOS)
IF (IOS > 0) THEN
    WRITE(*,*) 'Error: Problem with namelist group &WUI.'
    STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_WUI
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SPOTTING
! *****************************************************************************

INTEGER :: IOS

NAMELIST /SPOTTING/ CRITICAL_SPOTTING_FIRELINE_INTENSITY, CROWN_FIRE_SPOTTING_PERCENT, CROWN_FIRE_SPOTTING_PERCENT_MAX, &
CROWN_FIRE_SPOTTING_PERCENT_MIN, ENABLE_SPOTTING, ENABLE_SURFACE_FIRE_SPOTTING, &
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT, GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX, GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN, &
MAX_SPOTTING_DISTANCE, MEAN_SPOTTING_DIST, MEAN_SPOTTING_DIST_MAX, MEAN_SPOTTING_DIST_MIN, MIN_SPOTTING_DISTANCE, &
NEMBERS_MAX, NEMBERS_MAX_HI, NEMBERS_MAX_LO, NEMBERS_MIN, NEMBERS_MIN_HI, NEMBERS_MIN_LO, &
NORMALIZED_SPOTTING_DIST_VARIANCE, NORMALIZED_SPOTTING_DIST_VARIANCE_MAX, NORMALIZED_SPOTTING_DIST_VARIANCE_MIN, &
PIGN, PIGN_MAX, PIGN_MIN, SPOTTING_DISTRIBUTION_TYPE, &
SPOT_FLIN_EXP, SPOT_FLIN_EXP_HI, SPOT_FLIN_EXP_LO, SPOT_WS_EXP, SPOT_WS_EXP_HI, SPOT_WS_EXP_LO, STOCHASTIC_SPOTTING, &
SURFACE_FIRE_SPOTTING_PERCENT, SURFACE_FIRE_SPOTTING_PERCENT_MULT, TAU_EMBERGEN, USE_UMD_SPOTTING_MODEL, EMBER_GR, SOURCE_FUEL_IGN_MULT, &
P_EPS, USE_PHYSICAL_SPOTTING_DURATION, USE_PHYSICAL_EMBER_NUMBER, EMBER_SAMPLING_FACTOR, USE_EULERIAN_SPOTTING, &
USE_SUPERSEDED_SPOTTING, DT_DUMP_EMBER_FLUX, NO_SURFACE_FIRE, USE_HALF_CFL_DT_FOR_SPOTTING, BUILD_EMBER_FLUX_TABLE, USE_EMBER_IGNITION_MODEL, &
USE_SIMPLE_IGNITION_MODEL, LOCAL_IGNITION_TIME, CELL_IGNITION_DELAY, USE_CUSTOMIZED_PDF, MU_CROSSWIND, &
SIGMA_CROSSWIND, MU_DOWNWIND, SIGMA_DOWNWIND, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SPOTTING namelist group'

CRITICAL_SPOTTING_FIRELINE_INTENSITY(:)   = 0.
CROWN_FIRE_SPOTTING_PERCENT               = 100.
CROWN_FIRE_SPOTTING_PERCENT_MAX           = 100.
CROWN_FIRE_SPOTTING_PERCENT_MIN           = 100.
EMBER_GR                                  = 1E-3
EMBER_SAMPLING_FACTOR                     = 1.0
ENABLE_SPOTTING                           = .FALSE. 
ENABLE_SURFACE_FIRE_SPOTTING              = .FALSE.
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT      = 0.0
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX  = 0.0
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN  = 0.0
MAX_SPOTTING_DISTANCE                     = 0. ! Set value for UMD model to construct lookup table
MEAN_SPOTTING_DIST                        = 0. 
MEAN_SPOTTING_DIST_MAX                    = 0.
MEAN_SPOTTING_DIST_MIN                    = 0.
MIN_SPOTTING_DISTANCE                     = 0.
NEMBERS                                   = 25
NEMBERS_MAX                               = 1
NEMBERS_MAX_HI                            = 1
NEMBERS_MAX_LO                            = 1
NEMBERS_MIN                               = 30
NEMBERS_MIN_HI                            = 0
NEMBERS_MIN_LO                            = 0
NORMALIZED_SPOTTING_DIST_VARIANCE         = 0.
NORMALIZED_SPOTTING_DIST_VARIANCE_MAX     = 0.
NORMALIZED_SPOTTING_DIST_VARIANCE_MIN     = 0.
PIGN                                      = 1.0 ! PERCENT
PIGN_MAX                                  = 1.0
PIGN_MIN                                  = 1.0
LOCAL_IGNITION_TIME                       = 30.0 ! seconds, for simple ignition model only
CELL_IGNITION_DELAY                       = 100.0 ! seconds, Delay of setting phi = -1 when attacked by firebrands and ignited, for simple ignition model only
SOURCE_FUEL_IGN_MULT(:)                   = 1.0
SPOTTING_DISTRIBUTION_TYPE                = 'LOGNORMAL'
SPOT_FLIN_EXP                             = 0.5
SPOT_FLIN_EXP_HI                          = 0.5
SPOT_FLIN_EXP_LO                          = 0.5
SPOT_WS_EXP                               = 0.9
SPOT_WS_EXP_HI                            = 0.9
SPOT_WS_EXP_LO                            = 0.9
STOCHASTIC_SPOTTING                       = .FALSE. !This is not currently used
SURFACE_FIRE_SPOTTING_PERCENT(:)          = 100.
SURFACE_FIRE_SPOTTING_PERCENT_MULT(:)     = 1.0
TAU_EMBERGEN                              = 6.0
USE_UMD_SPOTTING_MODEL                    = .FALSE.
P_EPS                                     = 0.01 
USE_EULERIAN_SPOTTING                     = .FALSE. 
USE_PHYSICAL_SPOTTING_DURATION            = .FALSE.
USE_PHYSICAL_EMBER_NUMBER                 = .FALSE.
USE_CUSTOMIZED_PDF                        = .FALSE.
USE_SUPERSEDED_SPOTTING                   = .TRUE.
DT_DUMP_EMBER_FLUX                        = 30 ! Time interval to save the ember flux
NO_SURFACE_FIRE                           = .FALSE.
USE_HALF_CFL_DT_FOR_SPOTTING              = .FALSE.
BUILD_EMBER_FLUX_TABLE                    = .FALSE.
USE_EMBER_IGNITION_MODEL                  = .FALSE.
USE_SIMPLE_IGNITION_MODEL                 = .TRUE.
MU_CROSSWIND                              = 0.0
SIGMA_CROSSWIND                           = 0.0
MU_DOWNWIND                               = 0.0
SIGMA_DOWNWIND                            = 0.0
EMBER_GR_PER_MW_BLDG                      = 10.0
EMBER_GR_PER_MW_VEGE                      = 33.3

READ(LUINPUT,NML=SPOTTING,IOSTAT=IOS)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error: Problem with namelist group &SPOTTING.'
   STOP
ENDIF

IF (ENABLE_SPOTTING) THEN
   NUM_PARAMETERS_SPOTTING = 8
ELSE
   NUM_PARAMETERS_SPOTTING = 0
ENDIF

IF (USE_UMD_SPOTTING_MODEL) ALLOCATE (SPOTTING_STATS(1:EMBER_TRACKER_SIZE))

! *****************************************************************************
END SUBROUTINE READ_SPOTTING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SUPPRESSION
! *****************************************************************************

INTEGER :: IOS

NAMELIST /SUPPRESSION/ AREA_NO_CONTAINMENT_CHANGE, B_SDI, DT_EXTENDED_ATTACK, &
                       ENABLE_EXTENDED_ATTACK, ENABLE_INITIAL_ATTACK, &
                       INITIAL_ATTACK_TIME, MAX_CONTAINMENT_PER_DAY, SDI_FACTOR, USE_SDI, USE_SDI_LOG_FUNCTION

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SUPPRESSION namelist group'

!Set default values:
B_SDI                       = 1.0
DT_EXTENDED_ATTACK          = 3600.
ENABLE_EXTENDED_ATTACK      = .FALSE.
ENABLE_INITIAL_ATTACK       = .FALSE.
AREA_NO_CONTAINMENT_CHANGE  = 10000.0
INITIAL_ATTACK_TIME         = 1800.0
MAX_CONTAINMENT_PER_DAY     = 100.0
SDI_FACTOR                  = 1.0
USE_SDI                     = .FALSE.
USE_SDI_LOG_FUNCTION        = .FALSE.

READ(LUINPUT,NML=SUPPRESSION,IOSTAT=IOS)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error: Problem with namelist group &SUPPRESSION.'
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_SUPPRESSION
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_CALIBRATION
! *****************************************************************************

INTEGER :: IOS

NAMELIST /CALIBRATION/ ADJUSTMENT_FACTORS_BY_PYROME, ADJUSTMENT_FACTORS_FILENAME, &
CALIBRATION_CONSTANTS_BY_PYROME, CALIBRATION_CONSTANTS_FILENAME, DURATION_MAX_DAYS, &
DURATION_PDF_BY_PYROME, DURATION_PDF_FILENAME

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &CALIBRATION namelist group'

ADJUSTMENT_FACTORS_BY_PYROME    = .FALSE.
ADJUSTMENT_FACTORS_FILENAME     = ''
CALIBRATION_CONSTANTS_BY_PYROME = .FALSE.
CALIBRATION_CONSTANTS_FILENAME  = ''
DURATION_MAX_DAYS               = 28
DURATION_PDF_BY_PYROME          = .FALSE.
DURATION_PDF_FILENAME           = ''

! Open input file and read &CALIBRATION namelist groups
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=CALIBRATION,IOSTAT=IOS)
IF (IOS > 0) THEN 
   WRITE(*,*) 'Error: Problem with namelist group &CALIBRATION.'
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_CALIBRATION
! *****************************************************************************

END MODULE