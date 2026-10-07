## From within R

library("quarto")
quarto::quarto_render(
    input         = "index.qmd", 
    cache_refresh = TRUE, 
    output_format = "html") # defaults to html

## From terminal
quarto render --cache-refresh
