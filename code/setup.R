
# load packages 
requires <- c(#"bookdown",
              "tidyverse",
              "scales",
              "magrittr",
              "broom", #FIXME is this still needed
              "here",
              "msm", #FIXME is this still needded
              "kableExtra",
              "modelsummary",
              #"dotwhisker",
              "mediation",
              #"lme4", #FIXME should now have been replaced with fixest everywhere
              #"lmerTest",
              "fixest",
              "flextable",
              "magick",
              "equatiomatic", 
              "latex2exp")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)

library(scales)
library(magrittr)
library(broom)
#library(dotwhisker)
library(here)
library(knitr)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
library(mediation)
library(fixest)
library(modelsummary)
library(tidyverse)


fig.path <- here("docs", "figs")

## Sets defaults for R chunks
knitr::opts_chunk$set(echo = FALSE, # echo = TRUE means that code will show
                      cache = TRUE,
                      #cache = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      fig.show="hold",
                      fig.pos= "htbp",
                      fig.path = "figs/",
                      fig.align ='center',
                      fig.cap = '   ',
                      fig.retina = 3,
                      fig.height = 3,
                      fig.width = 7,
                      out.width = "100%",
                      out.extra = "")

# defaults for plots
library(ggplot2); theme_set(theme_bw());
options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_color_discrete <- function(...){
  scale_color_viridis_d(..., direction = -1, 
                        begin = 0, end = .6, option = "plasma")}
scale_fill_discrete <- function(...){
  scale_fill_viridis_d(..., direction = -1, 
                       begin = 0, end = .6, option = "plasma")}

scale_color_continuous <- function(...){
  scale_color_viridis_c(..., direction = -1, 
                        option = "plasma")}
scale_fill_continuous <- function(...){
  scale_fill_viridis_c(..., direction = -1, 
                       option = "plasma")}

# Table formatting
library(kableExtra)
kablebox <- . %>% 
  slice_head(n = 100) %>%
  knitr::kable() %>% 
  kable_styling() %>% 
  scroll_box(height = "400px")

library(flextable)

set_flextable_defaults(
  font.family = "Arial", font.size = 9, 
  border.color = "gray")

if( knitr:::is_html_output() | knitr::is_latex_output() ){
  output = 'kableExtra'
} else{
  output = "flextable"
} 

# a function to format kables for modelsummary output
kable_model <- function(x, file = "unnamed"){
  if( knitr:::is_html_output() | knitr::is_latex_output() ){
    x %>% row_spec(row = 1, bold = T, hline_after = TRUE) %>% 
      kable_paper() %>% 
      kable_styling(font_size = 9, full_width = TRUE, latex_options = c("repeat_header"))
  } else{
    x %>% # column widths
      bold(part = "header") %>% 
      autofit()
    }
}


# a function to format kables for different output formats 
kable2 <- function(x, file = "unnamed"){
  if( knitr:::is_html_output() | knitr::is_latex_output() ){
      x %>% row_spec(row = 1, bold = T, hline_after = TRUE) %>% 
      kable_paper() %>% 
      kable_styling(font_size = 9, full_width = TRUE, latex_options = c("repeat_header"))
  } else{
    x %>%
      kableExtra::as_image(x, width = 6.5, file = paste0("figs/", file, ".png"))
    }
}

# A function to trim and format tables for different outputs 
kable3 <- function(x, 
                   caption = "", 
                   height = '400px',
                   full_width = F,
                   align = 'l',
                   font_size = 11,
                   latex_options = "repeat_header"){
  if(knitr:::is_html_output()) {
    x %>% 
      ungroup() %>% 
      slice_head(n = 100) %>%
      mutate(across(where(is.numeric), pretty_num)  ) %>%
      knitr::kable(caption = caption) %>% 
      kable_paper() %>% 
      kable_styling() %>% 
      scroll_box(height = height)
  } else{
    if(knitr::is_latex_output() ){
      x %>% 
        ungroup() %>% 
        slice_head(n = 20) %>%
        mutate(across(where(is.numeric), pretty_num)  ) %>%
        knitr::kable(format = "latex",# "pipe", # "latex"?
                     caption = caption,
                     booktabs = T,
                     align = align,
                     linesep = "\\addlinespace") %>%
        kable_styling(font_size = font_size, 
                      full_width = full_width,
                      latex_options = latex_options)
    } else{x %>% 
        ungroup() %>% 
        slice_head(n = 20) %>%
        flextable() %>% 
        set_caption(caption) %>% 
        autofit() %>% 
        fit_to_width(max_width = 6)
      #knitr::kable(caption = caption) %>% 
      #kableExtra::as_image(file = paste0("figs", caption, ".png"))
    }
  }
}

options(scipen=999)

# smarter number functions
smart_number <- function(n, ...) {
  # if non-int below ten, return as is
  if ( (n != as.numeric(n)) ) {
    return(n)
  } else 
    # if non-int above ten, return number()
    if (abs(n) >= 10 | as.numeric(n) != as.integer(n) ) {
      return( prettyNum(n, big.mark = ",") )#scales::number(n, big.mark = ",", ...))
    } else 
      # if int below 10, print english
      if (abs(n) < 10 & as.numeric(n) == as.integer(n) ) {
        return(english::english(n, ...))
      } else 
        stop("??")
}



# inline formatting 
knit_hooks$set(inline = function(x) {
  if (is.na(as.numeric(x))) {
    return(x)
    } else 
      x <- as.numeric(x)
      # omit years 
    if (x > 2021 | x < 1980)  {
      return(smart_number(x))
      } else 
        return(x) #prettyNum(x, big.mark = ",") # 
})

# number formatting 
pretty_num <- . %>% prettyNum(big.mark = ",")

options(stringsAsFactors = FALSE)

options(knitr.graphics.auto_pdf = TRUE)


#functions for case insensitive string manipulation
str_rm_all <- function(string, pattern) {
  str_remove_all(string, regex(pattern, ignore_case = TRUE))
}

str_rpl <- function(string, pattern, replacement) {
  str_replace(string, regex(pattern, ignore_case = TRUE), replacement)
}

str_rm <- function(string, pattern) {
  str_remove(string, regex(pattern, ignore_case = TRUE))
}

str_dct <- function(string, pattern) {
  str_detect(string, regex(pattern, ignore_case = TRUE))
}

str_ext <- function(string, pattern) {
  str_extract(string, regex(pattern, ignore_case = TRUE))
}

str_spl <- function(string, pattern) {
  str_split(string, regex(pattern, ignore_case = TRUE))
}




# consistant phrases for causal models
draft <-
  'Agency publishes draft policy'
strategy <-
  'Interest group chooses lobbying strategy'
technical <-
  'Interest group creates or provides technical information'
policy <-
  'Information persuades agency officials to  revise policy'

mass <-
  'Interest group creates or provides  political information'

mass2 <-
  'Interest group  creates or provides  political information (e.g., through a public pressure campaign)'

oversight <-
  'Information persuades political principals to contact agency'

