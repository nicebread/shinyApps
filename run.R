# run locally
library(shiny)
#library(shinyIncubator)
library(shinythemes)
library(shinyjs)

runApp("PPV")

runApp("p-hacker/p-hacker")

runApp("../p-checker")

runApp("Pick_a_prior")

runApp("BF_robustness")
runApp("BF_r_match")
runApp("polySurface")
runApp("BF_sensitivity_plot")
runApp("SBF1")
runApp("Skewed_density")
runApp("lmfit")	
runApp("seq_binom")
runApp("seq_binom2")
runApp("feel_BF2")
runApp("R-Index")

# run from GitHub
runGitHub("shinyapps", user="nicebread", subdir="BF_robustness")
runGitHub("shinyapps", user="nicebread", subdir="polySurface")