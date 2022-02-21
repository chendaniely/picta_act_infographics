# Project is Archived

Please see: https://github.com/chendaniely/bitsi


# PICTA ACT Infographics

## Setup

1. Open the `00-install/01-install_rpkgs.R` script
2. Select all the code and run it with CTRL+ENTER (or click the source button in RStudio)
3. Repeat the process for the `00-install/02-install_tinytex.R` script

## Run

Make sure the 2 installation/setup scripts have been run.

On Windows, if you have R version 4.0.2 installed in the standard Program Files location, you should also be able to double click the `runApp.bat` file.
You can edit this file in Rstudio (or any other text editor) if your version of R changes or is different.

Otherwise you can follow these steps:

1. go into rstudio and go to the top right corner
2. click the Project dropdown
3. go to Open Project
4. Navigate to the picta_act_infographics/pictaACTinfographics folder
5. You should see a pictaACTinfographics.Rproj file
6. Select that project file and click Open

7. The files panel in Rstudio should open to that folder
8. Navigate to the "shiny" folder
9. Open either the global.R server.R or ui.R (they all should do the same thing)
10. On the top you should see a Run App button with a drop-down
11. Click the arrow, and make sure the "Run External" is selected (this will open the app in a browser instead of within rstudio)
12. then click Run App and the app should launch.

Once you open and launch the project file in step 6, you can also run the shiny runApp command in the "Console" (make sure you don't type it in the "Terminal"):

`shiny::runApp('shiny', launch.browser = TRUE)`

13. A browser should open up
