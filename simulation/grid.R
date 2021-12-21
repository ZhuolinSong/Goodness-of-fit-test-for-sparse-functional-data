f_devnull <- function(r, deviation = "trigonometric", n = 5e5) {
    grid <- seq(-1, 1, length.out = n)
    var_z <- switch(deviation,
        "quadratic" = r^2 * grid^4,
        "trigonometric" = r^2 * (sin(2 * pi * grid)^2 + sin(4 * pi * grid)^2)
    )
    mean(var_z / (1 + grid^2 + var_z))
}

# dev_grid <- seq(0, 1 - 1e-2, length.out = 5)
# r_grid_quad <- sapply(dev_grid, function(t) {
#                 uniroot(function(x) f_devnull(x, "quadratic") - t,
#                 c(0, 1e5))$root})
# r_grid_trig <- sapply(dev_grid, function(t) {
#                 uniroot(function(x) f_devnull(x) - t,
#                 c(0, 1e5))$root})
#
# dev_grid <- seq(0, 0.6, length.out = 5)
# r_grid_quad_500 <- sapply(dev_grid, function(t) {
#                 uniroot(function(x) f_devnull(x, "quadratic") - t,
#                 c(0, 1e5))$root})
# r_grid_trig_500 <- sapply(dev_grid, function(t) {
#                 uniroot(function(x) f_devnull(x) - t,
#                 c(0, 1e5))$root})
#
# save(r_grid_quad, r_grid_trig, r_grid_quad_500, r_grid_trig_500 file = "grid.RData")
# 
# r_grid_quad <- c(0.000000, 2.082020, 5.232181, 19.514433, 12338.236991)
# # r_grid_quad <- c(0, 1.113279, 1.870488, 2.821660, 4.221890, 6.595471,
# #     11.401126, 24.291923, 86.899079, 12338.434381)
# r_grid_trig <- c(0.0000000, 0.6756494, 1.2510933, 2.4905759, 52.8500282)

# r_grid_trig <- C(0, 0.4032925, 0.6220464, 0.8429342, 1.0987569, 1.4288137,
# 1.9113791, 2.7638348, 4.9790601, 52.8087983)