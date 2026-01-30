## -----------------------------------------------------------------------------
#| label: setup
#| include: false
r_libs <- Sys.getenv("R_LIBS")
if (nzchar(r_libs)) {
  libs <- strsplit(r_libs, .Platform$path.sep, fixed = TRUE)[[1]]
  libs <- libs[nzchar(libs)]
  .libPaths(c(libs, .libPaths()))
}
library(dplyr)
library(plotbb)


## ----plotbb-aes---------------------------------------------------------------
library(plotbb)

p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl)))
p + bb_grid(col = "grey50", lty = "dashed") + bb_point(pch = 19)


## ----plotbb-layer-------------------------------------------------------------
#| fig-height: 4
p2 <- p + bb_point() + bb_lm(bb_aes(group = cyl), lwd = 2)
p3 <- p2 + bb_lm(col = "red", lwd = 3, lty = "dotted")
p4 <- p + bb_text(bb_aes(label = cyl), cex = 2)
aplot::plot_list(p2, p3, p4, ncol = 3, tag_levels = "A")


## ----plotbb-line-seg-errorbar-------------------------------------------------
d2 <- group_by(mtcars, cyl) %>%
  summarize(
    x = cyl[1],
    y = mean(disp),
    sd = sd(disp)
  )

g1 <- bbplot(d2, bb_aes(x, y)) +
  bb_line(col = "grey50") +
  bb_point(pch = 19, cex = 1.5) +
  bb_errorbar(bb_aes(ymin = y - sd, ymax = y + sd), width = 0.8)

seg <- data.frame(x = 15, y = 150, xend = 30, yend = 450)
g2 <- bbplot(seg, bb_aes(x, y)) +
  bb_point(pch = 19) +
  bb_segment(bb_aes(xend = xend, yend = yend), lwd = 3, col = "steelblue")

aplot::plot_list(g1, g2, ncol = 2, tag_levels = "A")


## ----plotbb-bar---------------------------------------------------------------
mtcars$cyl <- factor(mtcars$cyl, levels = c(8, 4, 6))
bbplot(mtcars, bb_aes(cyl, col = cyl)) +
  bb_bar(stat = "count") +
  bb_legend(position = "topleft")


## ----plotbb-bar-identity------------------------------------------------------
dbar <- group_by(mtcars, cyl) %>%
  summarize(mpg = mean(mpg), .groups = "drop")
bbplot(dbar, bb_aes(cyl, mpg, col = cyl)) +
  bb_bar() +
  bb_legend(position = "topleft")


## ----plotbb-boxplot-----------------------------------------------------------
bbplot(mtcars, bb_aes(factor(cyl), mpg)) +
  bb_boxplot(fill = "grey90")


## ----plotbb-hist--------------------------------------------------------------
bbplot(mtcars, bb_aes(mpg)) +
  bb_hist(col = "grey80", border = "white")


## ----plotbb-signif------------------------------------------------------------
bbplot(mtcars, bb_aes(factor(cyl), mpg)) +
  bb_boxplot(fill = "grey90") +
  bb_signif(comparisons = list(c("4", "6"), c("4", "8")))


## ----plotbb-heatmap-----------------------------------------------------------
#| fig-height: 12
df <- data.frame(
  x = rep(1:10, 12),
  y = rep(1:12, each = 10),
  values = rnorm(120, mean = 10, sd = 5),
  type = sample(LETTERS[1:5], 120, replace = TRUE),
  stringsAsFactors = FALSE
)

h1 <- bbplot(df, bb_aes(x, y, col = values)) + bb_tile() +
  bb_title("heatmap for continuous numerical values")
h2 <- bbplot(df, bb_aes(x, y, col = values)) + bb_tile() + bb_scale_col_palette("YlOrRd") +
  bb_title("applying a color palette")
h3 <- bbplot(df, bb_aes(x, y, col = type)) + bb_tile() +
  bb_title("heatmap for discrete categorical values")
h4 <- bbplot(df, bb_aes(x, y, col = values)) + bb_tile() + bb_text(col = "black") +
  bb_title("heatmap with text labels") + bb_theme_expand()
aplot::plot_list(h1, h2, h3, h4, ncol = 2, tag_levels = "A")


## ----plotbb-labs--------------------------------------------------------------
p2 + bb_labs(
  title = "hello",
  sub = "just for demo",
  xlab = "this is xlab",
  ylab = "this is ylab"
) +
  bb_title("hello world")


## ----plotbb-theme-------------------------------------------------------------
#| fig-height: 6
g <- p2 +
  bb_theme(
    col.main = "red",
    cex.main = 2,
    mar = c(4, 4, 3, 1)
  ) +
  bb_title("applying graphics::par")
g2 <- p2 + bb_title("theme has no side effect")
aplot::plot_list(g, g2, ncol = 2, tag_levels = "A")


## ----plotbb-theme-expand------------------------------------------------------
#| fig-height: 6
p4 <- p3 + bb_theme_expand()
aplot::plot_list(p4, p3, ncol = 2, tag_levels = "A")


## ----plotbb-theme-grey--------------------------------------------------------
p + bb_grid(col = "grey50", lty = "dashed") +
  bb_point(pch = 19) +
  bb_theme_expand() +
  bb_theme_grey()


## ----plotbb-theme-blue--------------------------------------------------------
p + bb_point(pch = 19, cex = 2) +
  bb_theme_expand() +
  bb_theme_deepblue()


## ----plotbb-theme-bw-minimal--------------------------------------------------
#| fig-height: 6
g_bw <- p2 + bb_theme_expand() + bb_theme_bw() + bb_title("bb_theme_bw")
g_min <- p2 + bb_theme_expand() + bb_theme_minimal() + bb_title("bb_theme_minimal")
aplot::plot_list(g_bw, g_min, ncol = 2, tag_levels = "A")


## ----plotbb-scale-manual------------------------------------------------------
pal <- c(`4` = "#1b9e77", `6` = "#d95f02", `8` = "#7570b3")
bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) +
  bb_point(pch = 19) +
  bb_scale_col_manual(pal) +
  bb_legend()


## ----plotbb-scale-gradient----------------------------------------------------
bbplot(mtcars, bb_aes(mpg, disp, col = hp)) +
  bb_point(pch = 19) +
  bb_scale_col_gradient(low = "grey80", high = "firebrick") +
  bb_legend()


## ----plotbb-scale-aes---------------------------------------------------------
#| fig-width: 16
p5 <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl), pch = factor(gear), cex = hp)) +
  bb_point() +
  bb_scale_cex_continuous(range = c(0.8, 2)) +
  bb_legend(aesthetic = "col", position = "topleft") +
  bb_legend(aesthetic = "pch", position = "bottomleft")

p6 <- bbplot(mtcars, bb_aes(mpg, disp, lty = factor(gear))) +
  bb_lm(lwd = 2) +
  bb_scale_lty_manual(values = c(`3` = "solid", `4` = "dashed", `5` = "dotted")) +
  bb_legend(aesthetic = "lty", position = "topleft")

aplot::plot_list(p5, p6, ncol = 2, tag_levels = "A")


## ----plotbb-legend------------------------------------------------------------
p + bb_point(pch = 19) + bb_legend()


## ----plotbb-facet-------------------------------------------------------------
#| fig-height: 4
p + bb_point(pch = 19) + bb_facet_wrap(~cyl, ncol = 3)


## ----plotbb-facet-free--------------------------------------------------------
#| fig-height: 4
p + bb_point(pch = 19) + bb_facet_wrap(~cyl, ncol = 3, scales = "free_y")


## -----------------------------------------------------------------------------
f <- function() {
  plot(mtcars$mpg, mtcars$disp)
  abline(lm(disp ~ mpg, data = mtcars), col = "red")
}


## -----------------------------------------------------------------------------
library(dplyr)
d <- group_by(mtcars, cyl) %>%
  summarize(xm = mean(mpg), ym = mean(disp))

pp <- as.bbplot(f) +
  bb_theme_expand() +
  bb_theme_grey() +
  bb_lm(bb_aes(mpg, disp, group = cyl, col = factor(cyl)), data = mtcars, lwd = 2, lty = "dashed") +
  bb_point(bb_aes(xm, ym, col = factor(cyl)), data = d, pch = 19, cex = 2) +
  bb_title("hello plotbb") +
  bb_grid(col = "grey30", lty = "dashed")


## ----plotbb-base--------------------------------------------------------------
pp + (~points(30, 400, pch = 19, col = "red", cex = 3)) +
  ~text(30, 420, label = "have fun :)", col = "blue", cex = 1.2)


## ----plotbb-ape---------------------------------------------------------------
#| fig-height: 8.5
require(ape)
set.seed(2020 - 09 - 10)
x = rtree(10)

p = as.bbplot(
  function()
    plot(
      x,
      cex = 2,
      edge.width = 2,
      edge.color = "white",
      tip.color = "purple"
    )
)

p + bb_theme_expand() + bb_theme_deepblue() +
  (~nodelabels(node = 14, pie = matrix(rep(1, 4), 1), cex = 2)) +
  (~nodelabels(node = 15, pie = matrix(rep(1, 3), 1), cex = 2)) +
  (~nodelabels(node = 18, pie = matrix(rep(1, 5), 1), cex = 2)) +
  bb_labs(title = "phylogenetic tree plotted by base graphics") +
  bb_theme(col.main = "firebrick", cex.main = 2)

