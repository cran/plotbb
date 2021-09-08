## ----style, echo=FALSE, results="asis", message=FALSE-------------------------
knitr::opts_chunk$set(tidy = FALSE,
		   message = FALSE)

library(dplyr)
library(plotbb) 

## ----include=F----------------------------------------------------------------
library(dplyr)
library(plotbb) 

## ----plotbb-aes---------------------------------------------------------------
library(plotbb)

p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl)))
p + bb_grid(col='grey50', lty='dashed') + bb_point(pch=19) 

## ----plotbb-layer, fig.width=12-----------------------------------------------
p2 <- p + bb_point() + bb_lm(bb_aes(group=cyl), lwd=2)
p3 <- p2 + bb_lm(col="red", lwd=3, lty='dotted')
p4 <- p + bb_text(bb_aes(label=cyl), cex=2)
## oldpar <- par()$mfrow
## par(mfrow=c(1,3))
## p2; p3; p4
## par(mfrow=oldpar)
aplot::plot_list(p2, p3, p4, ncol=3, tag_levels = 'A')

## ----plotbb-heatmap, fig.width=10, fig.height=10------------------------------
df <- data.frame(x = rep(1:10, 12),
                 y = rep(1:12, each = 10),
                 values = rnorm(120, mean = 10, sd = 5),
                 type = sample(LETTERS[1:5], 120, replace=TRUE),
                 stringsAsFactors = FALSE)

h1 <- bbplot(df, bb_aes(x,y, col=values)) + bb_tile() + 
  bb_title("heatmap for continuous numerical values") 
h2 <- bbplot(df, bb_aes(x,y, col=values)) + bb_tile() + bb_scale_col_palette("YlOrRd") +
  bb_title("applying a color palette") 
h3 <- bbplot(df, bb_aes(x,y, col=type)) + bb_tile() +
  bb_title("heatmap for discrete categorical values") 
h4 <- bbplot(df, bb_aes(x,y, col=values)) + bb_tile() + bb_text(col='black') +
  bb_title("heatmap with text labels") + bb_theme_expand()
aplot::plot_list(h1, h2, h3, h4, ncol=2, tag_levels = 'A')

## ----plotbb-labs--------------------------------------------------------------
p2 + bb_labs(title = "hello", sub = "just for demo",
              xlab="this is xlab", ylab = "this is ylab") +
  bb_title("hello world") # last one rules

## ----plotbb-theme, fig.width=10-----------------------------------------------
g <- p2 +
     bb_theme(col.main="red", cex.main=2,
             mar = c(4, 4, 3, 1)) +
     bb_title("applying graphics::par")
g2 <- p2 + bb_title("theme has no side effect")
aplot::plot_list(g, g2, ncol=2, tag_levels = 'A')

## ----plotbb-theme-expand, fig.width=10----------------------------------------

p4 <- p3 + bb_theme_expand()
aplot::plot_list(p4, p3, ncol=2, tag_levels = 'A')

## ----plotbb-theme-grey--------------------------------------------------------
p + bb_grid(col='grey50', lty='dashed') +
  bb_point(pch=19) +
  bb_theme_expand() +
  bb_theme_grey() 

## ----plotbb-theme-blue--------------------------------------------------------
p + bb_point(pch=19, cex=2) +
  bb_theme_expand() +
  bb_theme_deepblue() 

## -----------------------------------------------------------------------------
f <- function() { 
  plot(mtcars$mpg, mtcars$disp)
  abline(lm(disp ~ mpg, data=mtcars), col='red')
}

## -----------------------------------------------------------------------------
library(dplyr) 
d <- group_by(mtcars, cyl) %>%
  summarize(xm=mean(mpg), ym=mean(disp))

pp <- as.bbplot(f) +
   bb_theme_expand() +
   bb_theme_grey() +
   bb_lm(bb_aes(mpg, disp, group=cyl, col=factor(cyl)), data=mtcars, lwd=2, lty='dashed') +
   bb_point(bb_aes(xm, ym, col=factor(cyl)), data=d, pch=19, cex=2) +
   bb_title("hello plotbb") +
   bb_grid(col='grey30', lty='dashed') ## grid lines were plotted as background by default

## ----plotbb-base--------------------------------------------------------------
pp + (~points(30, 400, pch=19, col="red", cex=3)) + 
   ~text(30, 420, label="hae fun :)", col="blue", cex=1.2)

## ----plotbb-ape, fig.width=8, figh.height=7-----------------------------------
require(ape)
set.seed(2020-09-10)
x = rtree(10)

p = as.bbplot(
  function() 
    plot(x, cex=2, edge.width=2, 
        edge.color="white", tip.color='purple')
  )

p + bb_theme_expand() + bb_theme_deepblue() +
  (~nodelabels(node = 14, pie = matrix(rep(1, 4), 1), cex = 2)) +
  (~nodelabels(node = 15, pie = matrix(rep(1, 3), 1), cex = 2)) +
  (~nodelabels(node = 18, pie = matrix(rep(1, 5), 1), cex = 2)) +
  bb_labs(title = 'phylogenetic tree plotted by base graphics') +
  bb_theme(col.main = "firebrick", cex.main=2)

