install.packages("nloptr")
library(nloptr)

# Define the Ackley function
ofunc <- function(x) {
  term1 <- -20*exp(-0.2*sqrt(0.5*(x[1]^2+x[2]^2)))
  term2 <- -exp(0.5*(cos(2*pi*x[1])+cos(2*pi*x[2])))
  term3 <- exp(1) + 20
  return(term1 + term2 + term3)
}

# Set search space and constraints
lower_bounds <- c(-1.5, -1.5)
upper_bounds <- c(1.5, 1.5)

# Grid search for initial points
num_points <- 10 # Adjust the number of points as desired
grid <- expand.grid(seq(lower_bounds[1], upper_bounds[1], length.out = num_points),
                    seq(lower_bounds[2], upper_bounds[2], length.out = num_points))

results <- list()

# Run optimization for each starting point
for (i in 1:nrow(grid)) {
  x_beg <- grid[i, ]
  result <- optim(par = x_beg, fn = ofunc,
                  method = "L-BFGS-B",
                  lower = lower_bounds, upper = upper_bounds,
                  control = list(maxit = 10000))
  results[[i]] <- result
}

# Find the best solution
best_result <- do.call(what = "rbind", args = lapply(results, function(x) x$par))
best_value <- which.min(sapply(results, function(x) x$value))
global_min <- results[[best_value]]

# Print global minimum
print(global_min)


# Load required packages
install.packages("rgl")
library(rgl)

# Create data for plotting
x1_range <- seq(lower_bounds[1], upper_bounds[1], length.out = 200)
x2_range <- seq(lower_bounds[2], upper_bounds[2], length.out = 200)
plot_grid <- expand.grid(x1 = x1_range, x2 = x2_range)
plot_grid$z <- apply(plot_grid, 1, function(row) ofunc(c(row[1], row[2])))

# Reshape data into a matrix form
z_matrix <- matrix(plot_grid$z, nrow = length(x1_range), ncol = length(x2_range))

# Plot the 3D surface
persp3d(x1_range, x2_range, z_matrix, col = "lightblue", xlab = "X1", ylab = "X2", zlab = "Ackley Function Value")

# Add the global minimum point
points3d(global_min$par[1], global_min$par[2], global_min$value, col = "red", size = 5)

install.packages("ggplot2")
library(ggplot2)

# Create data for plotting
x1_range <- seq(lower_bounds[1], upper_bounds[1], length.out = 200)
x2_range <- seq(lower_bounds[2], upper_bounds[2], length.out = 200)
plot_grid <- expand.grid(x1 = x1_range, x2 = x2_range)
plot_grid$z <- apply(plot_grid, 1, function(row) ofunc(c(row[1], row[2])))

# Plot contour lines
ggplot(data = plot_grid, aes(x = x1, y = x2, z = z)) +
  geom_contour(aes(colour = ..level..)) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  geom_point(aes(x = global_min$par[1], y = global_min$par[2]), colour = "red", size = 3) +
  theme_minimal()