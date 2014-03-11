# run locally
library(shiny)
runApp("BF_robustness")
runApp("polySurface")

# run from GitHub
runGitHub("shinyapps", user="nicebread", subdir="BF_robustness")
runGitHub("shinyapps", user="nicebread", subdir="polySurface")