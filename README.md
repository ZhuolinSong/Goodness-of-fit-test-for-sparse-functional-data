# Goodness-of-fit test of covariance for sparse functional data
- Tackle the inflation size problem in the goodness-of-fit test for sparse functional data. Simulations show that our method has empirical type I errors close to the nominal levels, with an order of magnitude improvement to the original test.
- Our implementation incorporates FACE: fast covariance estimator, which ensures accurate estimation of the error variance and adapts to the covariance matrix structure to reduce computing time.
- Prove the symmetric coefficients matrix structure of FACE is theoretically sound, thus reduce the number of coefficients by half. Show optimality of the coefficients matrix truncation, which guarantees the estimated covariance matrix to be positive semi-definite.
- Modified from Stephanie Chen's original implementation
