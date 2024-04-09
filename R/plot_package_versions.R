# plot.package_versions --------------------------------------------------------

#' @export
plot.package_versions <- function(
  versions, r_range = c(1, 10), rmax = 1.1 * r_range[2L], dphi = NULL, 
  ticklen = 1
)
{
  #r_range=c(1,10);rmax = 1.1 * r_range[2L]
  old_par <- graphics::par(mar = c(2, 2, 1, 1))
  on.exit(graphics::par(old_par))

  versions$r <- map_to_range(
    x = as.integer(kwb.utils::selectColumns(versions, "date")), 
    y_range = r_range
  )

  X <- split(versions, kwb.utils::selectColumns(versions, "package"))
  
  phis <- seq_rad_len(length(X))

  lim <- c(-rmax, rmax)
  graphics::plot(0, 0, xlim = lim, ylim = lim, asp = 1, axes = FALSE)

  # Draw circles around the centre
  draw_circles(r = r_range, n_corners = length(X))

  dphi <- kwb.utils::defaultIfNULL(dphi, seq_rad_len(3 * length(X))[2L])
  
  for (i in seq_along(X)) {
    x <- X[[i]]
    #points(polar_to_xy(phi = phis[i], r = x$r), cex = 0.5)
    xy <- polar_to_xy(phi = phis[i], r = x$r)
    
    dx <- ticklen * sin(phis[i])
    dy <- ticklen * cos(phis[i])
    
    xy_0 <- cbind(xy[, 1L] - dx, xy[, 2L] + dy)
    xy_1 <- cbind(xy[, 1L] + dx, xy[, 2L] - dy)
    
    graphics::segments(
      x0 = xy_0[, 1L], 
      x1 = xy_1[, 1L], 
      y0 = xy_0[, 2L], 
      y1 = xy_1[, 2L]
    )

    xy <- polar_to_xy(phi = phis[i], r = c(min(x$r), r_range[2L]))
    
    graphics::segments(
      x0 = xy[1L, 1L], 
      y0 = xy[1L, 2L], 
      x1 = xy[2L, 1L], 
      y1 = xy[2L, 2L]
    )
    
    xy <- polar_to_xy(phi = phis[i], r = 1.04 * r_range[2L])
    
    graphics::text(
      xy[, 1L], 
      xy[, 2L], 
      label = x$package[1L], 
      srt = phis[i] / pi * 180, 
      cex = 0.8, 
      adj = 0
    )
  }
}

# map_to_range -----------------------------------------------------------------
map_to_range <- function(x, y_range, x_range = range(x))
{
  y_range[1L] + (x - x_range[1L]) / diff(x_range) * diff(y_range)
}

# seq_rad_len ------------------------------------------------------------------
seq_rad_len <- function(n)
{
  kwb.package::gradToRad(kwb.package::equidistantAngles(n))
}

# draw_circles -----------------------------------------------------------------
draw_circles <- function(r, n_corners = 36L, col = "lightgrey")
{
  phis <- seq_rad_len(n_corners)
  
  for (ri in r) {
    xy <- do.call(rbind, lapply(c(phis, phis[1L]), polar_to_xy, r = ri))
    graphics::lines(xy, col = col)
  }
}

# polar_to_xy ------------------------------------------------------------------
polar_to_xy <- function(phi, r)
{
  data <- c(cos(phi) * r, sin(phi) * r)
  
  matrix(data, ncol = 2L, dimnames = list(NULL, c("x", "y")))
}
