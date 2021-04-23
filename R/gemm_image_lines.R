#' Convert an image to a series of different-width lines.
#'
#' This function is adopted directly from  Georgios Karamanis' aRtist project
#' at '<https://github.com/gkaramanis/aRtist/blob/main/portraits/portraits%20line/line-portraits.R>'
#'
#' @param photo The path to an image to visualize, read via [magick::image_read]
#' @param filepath The path to save the output image to
#' @param line_color The color to draw the lines of the image
#' @param background_color The flat color to use as a background
#'
#' @return filepath
#'
#' @export
gemm_image_lines <- function(photo,
                             filepath = tempfile(fileext = ".png"),
                             line_color = "black",
                             background_color = sample(colors(), 1)) {

  # Read in image and convert to grayscale
  img <- magick::image_read(photo)
  img <- magick::image_contrast(img, 4)
  img <- magick::image_convert(img, colorspace = "gray")

  # Get dimensions
  img_w <- magick::image_info(img)$width
  img_h <- magick::image_info(img)$height


  # Resize the longest dimension to 80 pixels
  if (img_w >= img_h) {
    img <- magick::image_resize(img, "80")
  } else {
    img <- magick::image_resize(img, ("x80"))
  }
  # Create array and number rows and columns
  img_array <- drop(as.integer(img[[1]]))
  rownames(img_array) <- 1:nrow(img_array)
  colnames(img_array) <- 1:ncol(img_array)
  # Create data frame from array and rename columns
  img_df <- as.data.frame.table(img_array)
  names(img_df) <- c("y", "x", "b")
  img_df <- dplyr::mutate(img_df, dplyr::across(dplyr::everything(), as.numeric), bf = 1 - b / 255)
  img_df <- dplyr::rowwise(img_df)
  img_df <- dplyr::mutate(img_df, t = list(x + seq(0, 1, by = 0.05)))
  img_df <- tidyr::unnest(img_df, t)

  ggplot2::ggplot(img_df) +
    ggplot2::geom_path(
      ggplot2::aes(x = t, y = y + bf * sin(4 * pi * t) / 2, group = y),
      color = line_color
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = background_color, color = NA)
    ) +
    ggplot2::ggsave(filepath)

  return(filepath)

}

#' Generate line images using Unsplash photos.
#'
#' @param unsplash_token Your API access key for Unsplash
#' @inheritParams gemm_image_lines
#'
#' @return A list containing the filepath of the output image
#' (as `image_output`) and the image credit (as `image_credit`)
#'
#' @export
gemm_unsplash_image_lines <- function(unsplash_token,
                                      filepath = tempfile(fileext = ".png"),
                                      line_color = "black",
                                      background_color = sample(colors(), 1)) {
  photo <- httr::GET(
    url = "https://api.unsplash.com/photos/random",
    config = list(
      httr::add_headers("Accept-Version" = "v1")
    ),
    query = list(
      client_id = unsplash_token
    )
  )

  if (!httr::http_error(photo)) {
    photo <- httr::content(photo)
  }
  else stop("Error downloading image: ", httr::http_status(photo))

  return(
    list(
      image_output = gemm_image_lines(photo$urls$full,
                                 filepath = filepath,
                                 line_color = line_color,
                                 background_color = background_color),
      image_credit = paste("Image by", photo$user$name, "on Unsplash:", photo$user$links$html)
    )
  )
}
