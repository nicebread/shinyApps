# run locally
library(shiny)
runApp("BF_robustness")
runApp("polySurface")
runApp("BF_sensitivity_plot")

# run from GitHub
runGitHub("shinyapps", user="nicebread", subdir="BF_robustness")
runGitHub("shinyapps", user="nicebread", subdir="polySurface")