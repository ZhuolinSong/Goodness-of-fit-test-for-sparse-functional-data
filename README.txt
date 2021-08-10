This zip contains all necessary files to perform the bootstrap, direct, and multivariate tests for testing a quadratic polynomial covariance function against a general nonparametric alternative.

In the ‘example.R’ file, we include an example of how to generate data using the ‘gen.data.R’ function, which formed the basis of the simulation study. Another example uses the (dense) DTI dataset from the refund package. The bootstrap and direct tests can handle any properly formatted data, see below. The multivariate test can only be applied to dense (balanced) data where all subjects are observed at the same time points.

Implementing the tests can be done by calling the relevant testing functions (‘bootstrap.test.R’, ‘direct.test.R’, ‘multivariate.test.R’) and inputting the dataset (“data”) and vector of all possible time points (“times”; scaled between -1 and 1). Data should not include any missing values, and should be formatted as a data.frame with 3 variables:
.value: Functional predictor, does not need to be demeaned
.index: Observed time point, between -1 to 1
.id: Subject ids, must be in sequential order beginning from 1, with no missing subject indices

Time points do not have to be equally-spaced, but must include all possible values.

The bootstrap test can be slow and we recommend first running the test with 10 bootstrap resamples (nbs=10) to gauge run time before scaling-up for the full test (nbs=1000 recommended). Increasing resamples will scale faster than linear because the slowest step is the tensor-product spline fit, which only need be done once per dataset.

The bootstrap and direct tests are designed to return relevant errors if there are problems fitting the null or alternative models. This may arise for simulated data when the estimated error variance is small or the covariance is not positive semidefinite. If so, please change the starting seed and try again.

For any questions, please contact the corresponding author: stchen3@ncsu.edu.
