# Automated Testing Suite

A primary line of defense against program bugs and invalid model outputs is the use of automated testing [1]. This is broken into several types [2]. The types of interest are expanded below. 

1. *Unit Tests* -- verify that individual subroutines or functions produce consistent outputs in isolation.
2. *Component Tests* -- verify that groups of related subroutines or functions produce consistent outputs together.
3. *Integration Tests* -- verify that the combination of all components in a simulation workflow operates consistently and robustly.
4. *Regression Tests* -- ensure that updates, refactors, or optimizations do not inadvertently alter model behavior.
5. *Performance Testing* -- evaluate whether the program executes within acceptable time, memory, and resource usage constraints under different workloads.

Further description of these types can be found in the reference [2]. The purpose of functional testing is to confirm the program or its subsections produce consistent and appropriate outputs for fixed inputs. 


# Testing Details

Here we provide details of the current tests in the suite. 

## Unit-Dependent Testing

- `test_half_superbee.f90`: Tests HALF_SUPERBEE(R) flux limiter function



# References
[1] G. Wilson et al., “Best Practices for Scientific Computing,” PLoS Biol., vol. 12, no. 1, p. e1001745, Jan. 2014, doi: 10.1371/journal.pbio.1001745.

[2] U. Priya, “Types Of Automation Testing: Definition, Benefits And Best Practices,” Lambda Test. [Online]. Available: https://www.lambdatest.com/blog/types-of-automation-testing/

Created by [Adam Laird](mailto:adam.laird@berkeley.edu)