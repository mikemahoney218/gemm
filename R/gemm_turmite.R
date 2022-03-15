.cursor <- R6::R6Class(
  "cursor",
  list(
    x = 0,
    y = 0,
    direction = 0,
    left = function() {
      self$direction <- self$direction + 270
      invisible(self)
    },
    right = function() {
      self$direction <- self$direction + 90
      invisible(self)
    },
    none = function() {
      self$direction <- self$direction
      invisible(self)
    },
    turn = function() {
      self$direction <- self$direction + 180
      invisible(self)
    },
    advance = function() {
      switch(
        sample(1:4, 1),
        self$left(),
        self$right(),
        self$none(),
        self$turn()
      )
      if (self$direction >= 360) self$direction <- self$direction %% 360
      if (self$direction == 0) {
        self$y <- self$y + 1
      } else if (self$direction == 90) {
        self$x <- self$x + 1
      } else if (self$direction == 180) {
        self$y <- self$y - 1
      } else {
        self$x <- self$x - 1
      }
      invisible(self)
    },
    execute = function(n_steps = 1000) {
      output <- setNames(
        data.frame(matrix(nrow = n_steps, ncol = 2)),
        c("x", "y")
      )
      for (i in seq_len(n_steps)) {
        self$advance()
        output[i, ]$x <- self$x
        output[i, ]$y <- self$y
      }
      output
    }
  )
)

#' Create turmites in ggplot2
#'
#' [Turmites](https://en.wikipedia.org/wiki/Turmite) are a type of Turing
#' machine, randomly walking across a 2D grid. This function simulates a
#' two-state relative turmite and graphs the result in ggplot2.
#'
#' @param n_steps The number of steps to walk the turmite through.
#' @param color The color to graph "success" values in (the more rare color).
#' @inheritParams gemm_curves
#'
#' @export
gemm_turmite <- function(n_steps = 1000,
                         color = sample(grDevices::colors(), 1),
                         save_args = NULL,
                         seed = NULL) {
  if (!is.null(seed)) {
    if(!exists(".Random.seed")) set.seed(NULL)
    store_seed <- .Random.seed
    set.seed(seed)
  }

  cursor <- .cursor$new()

  turmite <- dplyr::mutate(
    dplyr::count(
      cursor$execute(n_steps),
      x,
      y,
      name = "z"),
    z = z %% 2)

  p <- ggplot2::ggplot(turmite, ggplot2::aes(x, y, fill = factor(z))) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = c("white", color)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = "white"))

  if (!is.null(save_args)) do.call(ggplot2::ggsave,
                                   c(list(plot = p), save_args))

  if (!is.null(seed)) set.seed(store_seed)

  p
}
