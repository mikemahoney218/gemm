#' Create random walks in ggplot
#'
#' @param n Number of steps to take in the walk
#' @inheritParams gemm_curves
#'
#' @export
gemm_random_walk <- function(n = sample(seq(1000, 10000, 1000), 1), random_min = -1,
                             step_opts = NULL,
                             random_max = 1, random_min_y = NULL,
                             random_min_x = NULL, random_max_y = NULL,
                             random_max_x = NULL, exponent_min = 1, exponent_max = 4,
                             exponent_min_y = NULL, exponent_min_x = NULL,
                             exponent_max_y = NULL, exponent_max_x = NULL,
                             color = sample(grDevices::colors(), 1),
                             fun = switch(sample(1:2, 1), sin, cos),
                             xfun = NULL, yfun = NULL,
                             polar = sample(c(TRUE, FALSE), 1),
                             save_args = NULL, seed = NULL) {

  if (is.null(step_opts)) {
    xsteps <- c(-1, 1, 0, 0)
    ysteps <- c(0, 0, -1, 1)
  } else {
    xsteps <- step_opts[[1]]
    ysteps <- step_opts[[2]]
  }


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

  xseed <- stats::runif(1, random_min_x, random_max_x)
  xexp <- sample(seq(exponent_min_x, exponent_max_x, 1), 1)

  yseed <- stats::runif(1, random_min_y, random_max_y)
  yexp <- sample(seq(exponent_min_y, exponent_max_y, 1), 1)

  random_walk <- function(n, xsteps, ysteps) {
    dir <- sample(1:length(xsteps), n - 1, replace = TRUE)
    data.frame(
      x = c(0, cumsum(xsteps[dir])),
      y = c(0, cumsum(ysteps[dir]))
    )
  }

  walk <- random_walk(n, xsteps, ysteps)
  walk$key <- (xseed * walk$x^xexp - fun(walk$y^yexp))

  p <- ggplot2::ggplot(walk, ggplot2::aes(x, y, color = key)) +
    ggplot2::geom_path() +
    ggplot2::scale_color_gradient(low = "black", high = color) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  if (polar) p <- p + ggplot2::coord_polar()
  if (!is.null(save_args)) do.call(ggplot2::ggsave,
                                   c(list(plot = p), save_args))

  if (!is.null(seed)) set.seed(store_seed)

  p

}
