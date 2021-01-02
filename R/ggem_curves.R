#' Create curved generative plots.
#'
#' @section Point generation:
#' Positions for each point are generated via the following process.
#' First, an evenly-spaced grid of points is created via the call:
#' `seq(pts_min, pts_max, by) %>% expand_grid(x = ., y = .)`. x values are
#' then adjusted via `xseed * x^xexp - xfun(y^yexp)`,
#' where `xseed` is a random value set by
#' `runif(1, random_min_x, random_max_x)`, `xexp` is a random integer set by
#' `sample(seq(exponent_min_x, exponent_max_x, 1), 1)`, and `yexp` a random
#' integer set by the same call using the `exponent_min_y` and `exponent_max_y`
#' values. y values are then set via a similar call of
#' `yseed * y^yexp - yfun(x^xexp)`. Points are then plotted via
#' [ggplot2::geom_point] using the provided `alpha`, `size`, and `color` values.
#'
#' @param pts_min,pts_max,by Arguments determining the initial values for points
#' in the x and y directions (generated via `seq(pts_min, pts_max, by)`).
#' @param random_min,random_max,random_min_y,random_min_x,random_max_y,random_max_x
#' The minimum and maximum values that will be used to generate "seed"
#' multipliers. If the x- and y- variants are NULL, they will inherit from the
#' untagged variants.
#' @param exponent_min,exponent_max,exponent_min_y,exponent_min_x,exponent_max_y,exponent_max_x
#' The minimum and maximum values that will be used to generate exponential
#' multipliers. If the x- and y- variants are NULL, they will inherit from the
#' untagged variants.
#' @param alpha,size,color Arguments passed to [ggplot2::geom_point].
#' @param fun,xfun,yfun The function to be applied to generate point values.
#' If the x- and y- variants are NULL, they will inherit from the
#' untagged variant.
#' @param polar Logical: use `coord_polar`?
#' @param save_args A list of arguments to pass to `ggsave`. If NULL, plots are
#' not saved.
#' @param seed The random seed to use.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' gemm_curves(-1, 1, seed = 125, polar = FALSE)
#'
#' @export
gemm_curves <- function(pts_min = -3, pts_max = 3, by = 0.01, random_min = -1,
                        random_max = 1, random_min_y = NULL,
                        random_min_x = NULL, random_max_y = NULL,
                        random_max_x = NULL, exponent_min = 1, exponent_max = 4,
                        exponent_min_y = NULL, exponent_min_x = NULL,
                        exponent_max_y = NULL, exponent_max_x = NULL,
                        alpha = 0.1, size = 0,
                        color = sample(grDevices::colors(), 1),
                        fun = switch(sample(1:3, 1), sin, cos, tan),
                        xfun = NULL, yfun = NULL,
                        polar = sample(c(TRUE, FALSE), 1),
                        save_args = NULL, seed = NULL) {


  if (!is.null(seed)) {
    if(!exists(".Random.seed")) set.seed(NULL)
    store_seed <- .Random.seed
    set.seed(seed)
  }

  if (is.null(random_min_y)) random_min_y <- random_min
  if (is.null(random_min_x)) random_min_x <- random_min
  if (is.null(random_max_y)) random_max_y <- random_max
  if (is.null(random_max_x)) random_max_x <- random_max

  if (is.null(exponent_min_y)) exponent_min_y <- exponent_min
  if (is.null(exponent_min_x)) exponent_min_x <- exponent_min
  if (is.null(exponent_max_y)) exponent_max_y <- exponent_max
  if (is.null(exponent_max_x)) exponent_max_x <- exponent_max

  if (is.null(xfun)) xfun <- fun
  if (is.null(yfun)) yfun <- fun

  xseed <- stats::runif(1, random_min_x, random_max_x)
  xexp <- sample(seq(exponent_min_x, exponent_max_x, 1), 1)

  yseed <- stats::runif(1, random_min_y, random_max_y)
  yexp <- sample(seq(exponent_min_y, exponent_max_y, 1), 1)

  pt_grid <- seq(from = pts_min, to = pts_max, by = by)
  pt_grid <- expand.grid(x_i = pt_grid, y_i = pt_grid)
  pt_grid$x <- xseed * pt_grid$x_i^xexp - xfun(pt_grid$y_i^yexp)
  pt_grid$y <- yseed * pt_grid$y_i^yexp - yfun(pt_grid$x_i^xexp)

  p <- ggplot2::ggplot() +
    ggplot2::aes(x = pt_grid$x, y = pt_grid$y) +
    ggplot2::geom_point(alpha = alpha, size = size, color = color) +
    ggplot2::theme_void()

  if (polar) p <- p + ggplot2::coord_polar()
  if (!is.null(save_args)) do.call(ggplot2::ggsave,
                                   c(list(plot = p), save_args))

  if (!is.null(seed)) set.seed(store_seed)

  p

}
