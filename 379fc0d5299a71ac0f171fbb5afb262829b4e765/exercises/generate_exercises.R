## Exercise sheets for the script, generated with the R package 'exams'.
##
## Run this file from the 'exercises' directory, e.g.,
##   setwd("exercises"); source("generate_exercises.R")
##
## For each chapter, this produces two PDFs in 'pdf/':
##   ChN_Exercises.pdf                 (problems only)
##   ChN_Exercises_with_Solutions.pdf  (problems and solutions)
## Quarto copies 'exercises/pdf/' into 'docs/' (see 'resources' in _quarto.yml),
## so the PDFs are published together with the script.
##
## The numbers in the problems are drawn in an R chunk at the top of each .Rnw
## file. The seed per chapter makes the published sheets reproducible; a
## different seed gives a new variant (e.g., for exams).

library("exams")

## Make R's Sweave.sty visible to LaTeX (RStudio does this automatically, Rscript does not)
Sys.setenv(TEXINPUTS = paste0(R.home("share"), "/texmf/tex/latex:", Sys.getenv("TEXINPUTS")))

make_sheet <- function(files, chapter, title, seed) {
  set.seed(seed)
  name <- paste0("Ch", chapter, c("_Exercises", "_Exercises_with_Solutions"))
  exams2pdf(files,
            n        = 1,
            name     = name,
            dir      = "pdf",
            edir     = "problems",
            template = c("templates/exercises.tex", "templates/solution.tex"),
            header   = list(Date = "", ID = paste0("Chapter ", chapter, ": ", title)))
  ## exams2pdf() appends the replication number: ChN_Exercises1.pdf -> ChN_Exercises.pdf
  file.rename(file.path("pdf", paste0(name, "1.pdf")),
              file.path("pdf", paste0(name, ".pdf")))
  invisible(NULL)
}


## Chapter 2: Probability ######################################################
exercises_ch2 <- c("Ch2_01_MC_CoinTosses.Rnw",
                   "Ch2_02_MC_DisjointEvents.Rnw",
                   "Ch2_03_MC_DieIndependence.Rnw",
                   "Ch2_04_MC_MedicalTest.Rnw",
                   "Ch2_05_MC_Cards.Rnw",
                   "Ch2_07_CarSales_pmf_cdf.Rnw",
                   "Ch2_09_ExponentialWaitingTime.Rnw",
                   "Ch2_11_JointPMF_LIE.Rnw",
                   "Ch2_12_TwoDimPDF_Independence.Rnw",
                   "Ch2_14_MC_RegressionFunction.Rnw",
                   "Ch2_15_BivariateNormal_CondMean_LIE.Rnw",
                   "Ch2_16_MeanIndependence.Rnw")

## Not on the sheet (largely redundant with the problems above); pool for exam questions:
##   Ch2_06_MC_DiscreteCDF.Rnw                (cf. Ch2_07)
##   Ch2_08_Bernoulli.Rnw                     (cf. Ch2_07)
##   Ch2_10_MC_JointPMF_Conditional.Rnw       (cf. Ch2_11)
##   Ch2_13_BivariateNormal_Independence.Rnw  (cf. Ch2_12, Ch2_16)

make_sheet(exercises_ch2, chapter = 2, title = "Probability", seed = 2026)
