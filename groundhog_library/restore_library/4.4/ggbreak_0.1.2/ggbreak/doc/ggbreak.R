## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE,
                      message = TRUE)

library(ggplot2)
library(ggbreak)
library(patchwork)

CRANpkg <- function (pkg) {
    cran <- "https://CRAN.R-project.org/package"
    fmt <- "[%s](%s=%s)"
    sprintf(fmt, pkg, cran, pkg)
}


## ----fig.keep="last"----------------------------------------------------------
library(ggplot2)
library(ggbreak) 
library(patchwork)

set.seed(2019-01-19)
d <- data.frame(x = 1:20,
   y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
)
 
p1 <- ggplot(d, aes(y, x)) + geom_col(orientation="y")
d2 <- data.frame(x = c(2, 18), y = c(7, 26), label = c("hello", "world"))
p2 <- p1 + scale_x_break(c(7, 17)) + 
  geom_text(aes(y, x, label=label), data=d2, hjust=1, colour = 'firebrick')  + 
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p2

## -----------------------------------------------------------------------------
p2 + scale_x_break(c(18, 21))

## -----------------------------------------------------------------------------
p1 + scale_x_break(c(7, 17), scales = 1.5) + scale_x_break(c(18, 21), scales=2)

## ----fig.keep='last'----------------------------------------------------------
g <- ggplot(d, aes(x, y)) + geom_col()
g2 <- g + scale_y_break(c(7, 17), scales = 1.5) + 
  scale_y_break(c(18, 21), scale=2) + scale_y_reverse()
g + g2

## ----fig.keep='last', fig.width=10, fig.height=5------------------------------
p2 <- p1 + scale_x_break(c(7, 17)) 
p3 <- p1 + scale_x_break(c(7, 17)) + scale_x_log10()
p2 + p3

## ----message=FALSE------------------------------------------------------------
g + coord_flip() + scale_y_break(c(7, 18))

## ----message=FALSE------------------------------------------------------------
set.seed(2019-01-19)
d <- data.frame(
  x = 1:20,
  y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
  group = c(rep("A", 10), rep("B", 10)),
  face=c(rep("C", 5), rep("D", 5), rep("E", 5), rep("F", 5))
)

p <- ggplot(d, aes(x=x, y=y)) +
     geom_col(orientation="x") +
     scale_y_reverse() +
     facet_wrap(group~.,
                scales="free_y",
                strip.position="right",
                nrow=2
                ) +
     coord_flip()
pg <- p +
  scale_y_break(c(7, 17), scales="free") +
  scale_y_break(c(19, 21), scales="free")
print(pg)

## ----message=FALSE------------------------------------------------------------
pg <- pg + aes(fill=group) + theme(legend.position = "bottom")
print(pg)

## ----message=FALSE------------------------------------------------------------
pg + labs(title="test title", subtitle="test subtitle", tag="A tag", caption="A caption") +
     theme_bw() +
     theme(
           legend.position = "bottom",
           strip.placement = "outside",
           axis.title.x=element_text(size=10),
           plot.title = element_text(size = 22),
           plot.subtitle = element_text(size = 16),
           plot.tag = element_text(size = 10),
           plot.title.position = "plot",
           plot.tag.position = "topright",
           plot.caption = element_text(face="bold.italic"),

     )

## ----message=FALSE, fig.width=10, fig.height=6--------------------------------
require(ggplot2)
library(ggbreak)
set.seed(2019-01-19)
d <- data.frame(
  x = 1:20,
  y =  c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
  group = c(rep("A", 10), rep("B", 10))
)

p <- ggplot(d, aes(x=x, y=y)) +
     scale_y_reverse() +
     scale_x_reverse() +
     geom_col(aes(fill=group)) +
     scale_fill_manual(values=c("#00AED7", "#009E73")) +
     facet_wrap(
         group~.,
         scales="free_y",
         strip.position="right",
         nrow=2
     ) +
     coord_flip()                                                                                                                                                                                                  

p +
     scale_y_break(c(7, 10), scales=0.5, ticklabels=c(10, 11.5, 13)) +
     scale_y_break(c(13, 17), scales=0.5, ticklabels=c(17, 18, 19)) +
     scale_y_break(c(19,21), scales=1, ticklabels=c(21, 22, 23))

## ----fig.width=10, fig.height=5-----------------------------------------------
p <- ggplot(mpg, aes(displ, hwy)) +
     geom_point() +
     scale_y_continuous(
       "mpg (US)",
       sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
     ) +
      theme(
        axis.title.y.left = element_text(color="deepskyblue"),
        axis.title.y.right = element_text(color = "orange")
      )
p1 <- p + scale_y_break(breaks = c(20, 30))
p2 <- p + scale_x_break(breaks = c(3, 4))
p1 + p2

## ----message=FALSE, fig.width=8, fig.height=5, fig.keep="last"----------------
library(patchwork)

set.seed(2019-01-19)
d <- data.frame(
               x = 1:20,
               y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
)

p <- ggplot(d, aes(x, y)) + geom_col()
x <- p+scale_y_break(c(7, 17 ))

x + p

## ----fig.width=6, fig.height=8------------------------------------------------
p <- ggplot(economics, aes(x=date, y = unemploy, colour = uempmed)) +
  geom_line()

p + scale_wrap(n=4)

## ----fig.width=7, fig.height=6------------------------------------------------
ggplot(mpg, aes(class, hwy)) + 
  geom_boxplot() +
      scale_wrap(n = 2)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggbreak)
set.seed(2019-01-19)
d <- data.frame(
     x = 1:20,
     y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
 )
p <- ggplot(d, aes(x, y)) + geom_col()
p + scale_y_cut(breaks=c(7, 18), which=c(1, 3), scales=c(3, 0.5))

## -----------------------------------------------------------------------------
p + scale_y_cut(breaks=c(7, 18), which=c(1, 3), scales=c(3, 0.5), space=.5)

## ----legend-subview, fig.keep="last"------------------------------------------
## original plot
p1 <- ggplot(mpg, aes(displ, hwy, color=factor(cyl))) + geom_point()

## ggbreak plot without legend
p2 <- p1 + scale_x_break(c(3, 4)) +
    theme(legend.position="none") 

## extract legend from original plot
leg = cowplot::get_legend(p1)

## redraw the figure
p3 <- ggplotify::as.ggplot(print(p2))

## place the legend 
p3 + ggimage::geom_subview(x=.9, y=.8, subview=leg)

