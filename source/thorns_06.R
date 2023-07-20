receive_thorn <- function(seed) {

  library(Rcpp)
  library(ggplot2)
  library(ggforce)
  library(voronoise)
  library(dplyr)

  sys_id <- "06"
  sys_name <- "thorns"
  sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

  # seed
  cat(seed, "\n")
  set.seed(seed)

  # fixed / default
  scheme <- seed
  layers <- 5
  iter <- 50000
  mesh <- .03
  f <- 1.5
  col_trans <- rank
  flip <- TRUE
  ex <- 0
  rd <- 0

  # filename
  prefix <- paste0(sys_name, "_", sys_id, "_")
  fname <- paste0(prefix, seed, ".png")
  fpath <- here::here("image", paste0("sys_", sys_id), fname)


  # palette specification ---------------------------------------------------

  sample_shades <- function() {
    pal <- sample(colorir::colores$palette_name, 1)
    shades <- colorir::colores$colour[colorir::colores$palette_name == pal]
    return(shades)
  }
  pal <- sample_shades()

  sample_black <- function() {
    colorir::colores %>%
      filter(colour_name == "Black") %>%
      pull(colour) %>%
      sample(1)
  }

  blend_shades <- function(x, y, p = .5) {
    x <- col2rgb(x)
    y <- col2rgb(y)
    z <- round(p*x + (1-p)*y)
    z <- rgb(red = z[1, ]/255, green = z[2, ]/255, blue = z[3, ]/255)
    return(z)
  }

  # generate the data -------------------------------------------------------

  cat("generating...\n")

  # create data frame
  df <- thorns(iter, layers)
  df <- as.data.frame(df)
  names(df) <- c("x","y","c")

  # filter and transform
  df <- df[-(1:100),]
  filter_x <- c(-f, f)
  filter_y <- c(-f, f)
  noise <- data.frame(
    x = runif(1000, -f, f),
    y = runif(1000, -f, f),
    c = sample(df$c, 1000, TRUE)
  )
  df <- rbind(df, noise)
  if(!is.null(filter_x)) {
    x_ok <- df$x > filter_x[1] & df$x < filter_x[2]
    y_ok <- df$y > filter_y[1] & df$y < filter_y[2]
    df <- df[x_ok & y_ok, ]
  }
  if(!is.null(col_trans)){
    df$c <- col_trans(df$c)
  }
  if(flip) {
    df$y <- -df$y
  }

  # scale the co-ordinates to the image size
  px <- 5000
  xrng <- max(df[,1]) - min(df[,1])
  yrng <- max(df[,2]) - min(df[,2])
  rng <- max(c(xrng, yrng))

  # create a vector of colours
  ncol <- length(pal)
  col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * (ncol - 1)) + 1L
  df$col <- pal[col_idx]



  # generate the image ------------------------------------------------------

  cat("rendering...\n")

  sift <- function(grain = .025) {
    function(data) {
      data <- data %>%
        group_by(group) %>%
        mutate(tilesize = (max(x) - min(x)) * (max(y) - min(y))) %>%
        ungroup()
      data$tilealpha <- .05
      data$tilealpha[data$tilesize < grain^2] <- 1
      return(data)
    }
  }


  bg <- sample(pal, 1)

  p <- ggplot(
    data = df,
    mapping = aes(
      x = x,
      y = y,
      group = 1,
      fill = col,
      alpha = after_stat(tilealpha)
    )
  ) +
    geom_voronoise(
      perturb = sift(grain = mesh),
      max.radius = NULL,
      radius = rd,
      expand = ex
    ) +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void() +
    theme(panel.background = element_rect(fill = bg, colour = bg)) +
    coord_cartesian(
      xlim = filter_x * .9,
      ylim = filter_y * .9
    )

  ggsave(
    file = fpath,
    plot = p,
    width = 5000 / 300,
    height = 5000 / 300,
    dpi = 300
  )

}


# run it ------------------------------------------------------------------

for(seed in 1201:1300) {
  cat("seed:", seed, "\n")
  receive_thorn(seed)
}


