f_devnull <- function(r, deviation = "trigonometric", n = 5e5) {
    grid <- seq(-1, 1, length.out = n)
    var_z <- switch(deviation,
        "quadratic" = r^2 * grid^4,
        "trigonometric" = r^2 * (sin(2 * pi * grid)^2 + sin(4 * pi * grid)^2)
    )
    mean(var_z / (1 + grid^2 + var_z))
}

dev_grid <- seq(0, 1 - 1e-2, length.out = 5)
r_grid_quad <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x, "quadratic") - t,
                c(0, 1e5))$root})
r_grid_trig <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x) - t,
                c(0, 1e5))$root})

dev_grid <- seq(0, 0.6, length.out = 5)
r_grid_quad_500 <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x, "quadratic") - t,
                c(0, 1e5))$root})
r_grid_trig_500 <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x) - t,
                c(0, 1e5))$root})

dev_grid <- seq(0.12375, 0.86625, length.out = 4)
an_r_grid_quad <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x, "quadratic") - t,
                c(0, 1e5))$root})
an_r_grid_trig <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x) - t,
                c(0, 1e5))$root})

dev_grid <- seq(0.075, 0.525, length.out = 4)
an_r_grid_quad_500 <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x, "quadratic") - t,
                c(0, 1e5))$root})
an_r_grid_trig_500 <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x) - t,
                c(0, 1e5))$root})

save(r_grid_quad, r_grid_trig, r_grid_quad_500, r_grid_trig_500, an_r_grid_quad,
an_r_grid_trig, an_r_grid_quad_500, an_r_grid_trig_500, grid_quad, grid_trig, grid_quad_500, grid_trig_500, file = "grid.RData")
# 
# r_grid_quad <- c(0.000000, 2.082020, 5.232181, 19.514433, 12338.236991)
# r_grid_trig <- c(0.0000000, 0.6756494, 1.2510933, 2.4905759, 52.8500282)
# r_grid_quad_500 <- c(0.000000, 1.379251, 2.530851, 4.385599, 8.314654)
# r_grid_trig_500 <- c(0.0000000, 0.4853339, 0.7805143, 1.1249110, 1.6216148)

# an_r_grid_quad <- c(1.204569, 3.275307, 9.130286, 70.157208)        
# an_r_grid_trig <- c(0.4321446, 0.9330529, 1.7040942, 4.5141220)
# an_r_grid_quad_500 <- c(0.8754257, 1.9079677, 3.3201874, 5.9214775)
# an_r_grid_trig_500 <- c(0.3247052, 0.6317589, 0.9415426, 1.3442830)

grid_quad <- sort(c(r_grid_quad, an_r_grid_quad))
grid_trig <- sort(c(r_grid_trig, an_r_grid_trig))
grid_quad_500 <- sort(c(r_grid_quad_500, an_r_grid_quad_500))
grid_trig_500 <- sort(c(r_grid_trig_500, an_r_grid_trig_500))

save(r_grid_quad, r_grid_trig, r_grid_quad_500, r_grid_trig_500, an_r_grid_quad,
an_r_grid_trig, an_r_grid_quad_500, an_r_grid_trig_500, grid_quad, grid_trig, grid_quad_500, grid_trig_500, file = "grid.RData")
