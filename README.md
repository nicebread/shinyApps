# Nicebread's ShinyApps #

*A collection of statistical widgets for exploration and teaching*

This is a collection of interactive tools, built with shiny. 


---

*You can run the apps in several ways:*

## Run in a web browser - no local R installation needed! ##

Some of the apps are also hosted on ShinyApps.io, a server provided by the RStudio team.
These apps can run in the browser on any computer, without the need of a local R installation! This might be helpful for teaching purposes.

Currently, the following apps can be run from the server:

* BF_robustness: [https://nicebread.shinyapps.io/BF_sensitivity/](https://nicebread.shinyapps.io/BF_sensitivity/)
* Polynomial surface Explorer [https://nicebread.shinyapps.io/polySurface/](https://nicebread.shinyapps.io/polySurface/)

## Run locally without installation ##

You can run the apps locally by downloading them temporarily from Github (i.e., you only need an internet connection for the initial download, then the app runs locally. However, when closing R the download is lost and not available locally any more).
You will need the most recent R installation, the most recent shiny package, and (depending on the specific shiny app) several other packages. When you install these packages, all my shinyapps should run on your system:

    install.packages(c("shiny", "BayesFactor", "plyr", "RSA"), dependencies = TRUE)

When these preconditions are met, you can run an app with a single line of code, after loading the shiny package. The specific app is specified in the `subdir` parameter. Examples:

    library(shiny)
    runGitHub("shinyapps", user="nicebread", subdir="BF_robustness")

    library(shiny)
    runGitHub("shinyapps", user="nicebread", subdir="polySurface")


## Run locally with installation ##

Of course you can also download or clone the complete repository and save it on your local machine. Then you can run the apps with the function `runApp()`. See `run.r` in the top directory for function calls.


----
All of my apps are **free software**, released under the GPL v2 license, "which guarantees end users (individuals, organizations, companies) the freedoms to use, study, share (copy), and modify the software" (see [Wikipedia](http://en.wikipedia.org/wiki/GNU_General_Public_License)).