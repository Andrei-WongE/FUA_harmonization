## ----echo=FALSE---------------------------------------------------------------
required <- c("survey", "huxtable", "broom", "lme4", "quantreg")
if (!all(sapply(required, requireNamespace, quietly = TRUE)))
  knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.width = 6, 
                      fig.height = 4, dpi = 125, render = knitr::normal_print)
library(jtools)

## -----------------------------------------------------------------------------
library(jtools) # Load jtools
data(movies) # Telling R we want to use this data
fit <- lm(metascore ~ imdb_rating + log(us_gross) + genre5, data = movies)
summ(fit)

## ----render = 'knit_print'----------------------------------------------------
summ(fit)

## -----------------------------------------------------------------------------
summ(fit, robust = "HC1")

## -----------------------------------------------------------------------------
summ(fit, scale = TRUE)

## -----------------------------------------------------------------------------
summ(fit, scale = TRUE, n.sd = 2)

## -----------------------------------------------------------------------------
summ(fit, center = TRUE)

## -----------------------------------------------------------------------------
summ(fit, confint = TRUE, digits = 3)

## -----------------------------------------------------------------------------
summ(fit, confint = TRUE, ci.width = .5)

## -----------------------------------------------------------------------------
summ(fit, confint = TRUE, pvals = FALSE)

## -----------------------------------------------------------------------------
fitg <- glm(metascore/100 ~ imdb_rating + log(us_gross) + genre5, data = movies,
            family = quasibinomial())

summ(fitg)

## -----------------------------------------------------------------------------
summ(fitg, exp = TRUE)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

summ(fm1)

## -----------------------------------------------------------------------------
effect_plot(fitg, pred = imdb_rating, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)

## -----------------------------------------------------------------------------
plot_summs(fit)

## -----------------------------------------------------------------------------
plot_summs(fit, robust = TRUE)

## -----------------------------------------------------------------------------
plot_summs(fit, inner_ci_level = .9)

## -----------------------------------------------------------------------------
plot_summs(fit, plot.distributions = TRUE, inner_ci_level = .9)

## -----------------------------------------------------------------------------
fit2 <- lm(metascore ~ imdb_rating + log(us_gross) + log(budget) + genre5,
           data = movies)
plot_summs(fit, fit2)

## -----------------------------------------------------------------------------
plot_summs(fit, fit2, plot.distributions = TRUE)

## -----------------------------------------------------------------------------
plot_summs(fit, fit, fit, robust = list(FALSE, "HC0", "HC5"),
           model.names = c("OLS", "HC0", "HC5"))

## ----eval = FALSE-------------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE)

## ----echo = FALSE, results = 'asis'-------------------------------------------
huxtable::print_html(export_summs(fit, fit2, scale = TRUE))

## ----eval = FALSE-------------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE,
#               error_format = "[{conf.low}, {conf.high}]")

## ----echo = FALSE, results = 'asis'-------------------------------------------
huxtable::print_html(export_summs(fit, fit2, scale = TRUE,
                     error_format = "[{conf.low}, {conf.high}]"))

## ----eval = FALSE-------------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE, to.file = "docx", file.name = "test.docx")

## -----------------------------------------------------------------------------
summ(fit, model.info = FALSE, model.fit = FALSE)

## -----------------------------------------------------------------------------
summ(fit, model.info = FALSE, digits = 5)

## -----------------------------------------------------------------------------
summ(fit, model.info = FALSE, digits = 1)

## -----------------------------------------------------------------------------
options("jtools-digits" = 2)
summ(fit, model.info = FALSE)

## ----echo = F-----------------------------------------------------------------
options("jtools-digits" = NULL)

## -----------------------------------------------------------------------------
j <- summ(fit, digits = 3)

j$coeftable

## ----eval = F-----------------------------------------------------------------
#  set_summ_defaults(digits = 2, pvals = FALSE, robust = "HC3")

## -----------------------------------------------------------------------------
summ(fit, vifs = TRUE)

