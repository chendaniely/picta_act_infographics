"Rscript.exe - Shortcut.lnk" -e "install.packages(c('ggplot2', 'png', 'extrafont', 'knitr', 'rmarkdown', 'shiny', 'shinydashboard', 'tinytex', 'here', 'RCurl', 'devtools', 'remotes'), repo = 'https://cloud.r-project.org/')"
"Rscript.exe - Shortcut.lnk" -e "remotes::remotes::install_github('wilkelab/ggtext')"
"Rscript.exe - Shortcut.lnk" -e "tinytex::install_tinytex()"
"Rscript.exe - Shortcut.lnk" -e "tinytex:::install_yihui_pkgs()"
@PAUSE
