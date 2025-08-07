# Automated Testing Suite

A primary line of defense against program bugs and invalid model outputs is the
use of automated testing [1]. This is broken into several types [2]. The types 
of interest are expanded below. 

1. *Unit Tests* -- verify that individual subroutines or functions produce 
consistent outputs in isolation.
2. *Component Tests* -- verify that groups of related subroutines or functions 
produce consistent outputs together.
3. *Integration Tests* -- verify that the combination of all components in a 
simulation workflow operates consistently and robustly.
4. *Regression Tests* -- ensure that updates, refactors, or optimizations do 
not inadvertently alter model behavior.
5. *Performance Testing* -- evaluate whether the program executes within 
acceptable time, memory, and resource usage constraints under different 
workloads.

Further description of these types can be found in the reference [2]. The 
purpose of functional testing is to confirm the program or its subsections 
produce consistent and appropriate outputs for fixed inputs. Some common types 
of black-box tests used are given below. 

1. *Nominal Cases* -- Ensure correct behavior under typical inputs
2. *Edge/Boundary and Near-Boundary Cases* -- Catch off-by-one errors
3. *Offset and Resolution Mismatch Cases* -- Test raster utility functions
4. *Clipping Cases* -- Verify spatial robustness


# Testing Details
Here we provide details of the current tests in the suite. 

## Unit-Independent Testing
These functions/subroutines are independent of other functions/subroutines and 
can be ran in isolation. This is the lowest level to test. 

**elmfire_init**
- `test_calc_wind_adjustment_factor_single`

**elmfire_level_set**
- `test_calc_cfl.f90`: Tests CALC_CFL(DT) subroutine that calculates time step 
based on the Courant–Friedrichs–Lewy condition.
- `test_calc_normal_vectors.f90`: Tests CALC_NORMAL_VECTORS(...) subroutine that
 calculates outward normal directions of the fireline.
- `test_half_superbee.f90`: Tests HALF_SUPERBEE(R) flux limiter function.
- `test_rk2_integrate.f90`: Tests RK2_INTEGRATE(DT, ISTEP) subroutine that 
performs 2nd order Runge-Kutta time integration.

**elmfire_spotting**
- `test_set_spotting_parameters.f90`

**elmfire_spread_rate**
- `test_ellipse_ucb.f90`: Tests ELLIPSE_UCB(...) subroutine that calculates the 
ellipse geometry for the urban spread model.
- `test_hamada.f90`: Tests HAMADA(C) subroutine that calculates urban spread 
rate. 
- ~~`test_surface_spread_rate.f90`: ~~

**elmfire_subs**
- `test_bilinear_interpolate.f90`
- `test_erfinv.f90`
- `test_get_bilinear_interpolate_coeffs.f90`
- `test_hour_of_year_to_timestamp.f90`
- `test_icol_fine_to_coarse.f90`
- `test_icol_from_x.f90`
- `test_irow_from_y.f90`
- `test_locate.f90`
- `test_map_fine_to_coarse.f90`
- `test_sunrise_sunset_calcs.f90`
- `test_ux_from_wswd.f90`
- `test_uy_from_wswd.f90`
- `test_wx_icol_from_analysis_ix.f90`
- `test_wx_irow_from_analysis_iy.f90`
- `test_x_from_icol.f90`
- `test_y_from_irow.f90`

## Unit-Dependent Testing
These functions/subroutines have dependencies on other functions/subroutines,
but are still small enough to be ran as a unit.

**elmfire_level_set**
- ~~`test_tag_band.f90`: Tests TAG_BAND(...) subroutine that tags cells for ~~
~~narrow banded solver~~
- ~~`test_limit_gradients.f90`: Tests LIMIT_GRADIENTS(...) subroutine that ~~
~~approximates spatial gradients~~

**elmfire_spread_rate**

## Component Testing
At this level, we start encorporating testing procedures that follow published 
works. Here, we test individual physics models like wildifire spread, spotting, 
etc. Work in progress...


## Integration Testing
At this level, we test the functionality of the entire model to problems of 
interest. This includes running forecasts, hindcasts, fire potential, and risk 
analyses. Work in progress...


## Regression Testing
Work in progress...

## Performance Testing
Work in progress...

# References
[1] G. Wilson et al., “Best Practices for Scientific Computing,” PLoS Biol., 
vol. 12, no. 1, p. e1001745, Jan. 2014, doi: 10.1371/journal.pbio.1001745.

[2] U. Priya, “Types Of Automation Testing: Definition, Benefits And Best 
Practices,” Lambda Test. [Online]. 
Available: https://www.lambdatest.com/blog/types-of-automation-testing/


Questions? Blame [Adam Laird](mailto:adam.laird@berkeley.edu)