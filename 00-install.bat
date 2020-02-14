C:\"Program Files"\R\R-3.6.2\bin\Rscript.exe -e "install.packages(c('remotes', 'ggplot2', 'png', 'extrafont', 'knitr', 'rmarkdown', 'shiny', 'shinydashboard', 'tinytex', 'here', 'RCurl', 'devtools'), repo = 'https://cloud.r-project.org/')"
C:\"Program Files"\R\R-3.6.2\bin\Rscript.exe -e "remotes::remotes::install_github('wilkelab/ggtext')"
C:\"Program Files"\R\R-3.6.2\bin\Rscript.exe -e "tinytex::install_tinytex()"
C:\"Program Files"\R\R-3.6.2\bin\Rscript.exe -e "tinytex:::install_yihui_pkgs()"
@PAUSE
