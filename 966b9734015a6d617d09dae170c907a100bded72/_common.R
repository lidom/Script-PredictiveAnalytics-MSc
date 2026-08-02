## Shared knitr setup, sourced by a setup chunk at the top of every chapter.
##
## The site ships a light (flatly) and a dark (darkly) theme, but plots are
## raster images with a baked-in background, so an opaque white canvas shows up
## as a glaring white box in dark mode. Two fixes, applied globally:
##   1. the device background is transparent, so the page shows through, and
##   2. the plot chrome (axes, box, labels) is drawn in a mid grey that stays
##      legible on both a white and a dark page.
## Data colours are left untouched -- only the chrome is themed.
##
## Everything lives inside local(): chapter 1 teaches the workspace and runs
## both ls() and rm(list = ls()), so a helper variable in the global
## environment would be shown to students and then deleted, breaking the hook
## for every chunk after it.

local({

  fg <- "#808080"

  knitr::opts_chunk$set(dev.args = list(bg = "transparent"))

  ## par() is device-level and is reset for every chunk, so it has to be set
  ## from a `before` hook rather than once at the top. `fg` is resolved in this
  ## local() environment, not the global one.
  knitr::knit_hooks$set(plot_theme = function(before, options, envir) {
    if (before) {
      par(bg       = NA,
          fg       = fg,
          col.axis = fg,
          col.lab  = fg,
          col.main = fg,
          col.sub  = fg)
    }
  })
  knitr::opts_chunk$set(plot_theme = TRUE)

  ## ggplot2 ignores par(), so its default theme needs the same treatment.
  ## Note: a chunk that calls theme_bw() or theme_classic() explicitly overrides
  ## this and needs its own + theme(...) to stay dark-mode safe.
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::theme_set(
      ggplot2::theme_get() +
        ggplot2::theme(
          plot.background   = ggplot2::element_rect(fill = NA, colour = NA),
          panel.background  = ggplot2::element_rect(fill = NA, colour = NA),
          legend.background = ggplot2::element_rect(fill = NA, colour = NA),
          legend.key        = ggplot2::element_rect(fill = NA, colour = NA),
          text              = ggplot2::element_text(colour = fg),
          axis.text         = ggplot2::element_text(colour = fg),
          axis.line         = ggplot2::element_line(colour = fg),
          axis.ticks        = ggplot2::element_line(colour = fg)
        )
    )
  }

})
