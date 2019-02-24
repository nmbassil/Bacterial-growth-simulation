# Bacterial growth simulation

## Introduction and Purpose

This is a Shiny app script that simulates the growth of 6 extremophilic bacterial species under different environmental conditions on solid media.
I have used this app in science outreach activities and demonstrations to show that bacteria thrive under conditions considered extreme to humans.

The simulated experiment starts with 1 colony each, from 6 different bacterial species (*Escherichia coli*, *Bacillus pseudofirmus*, *Psychrobacter luti*, *Leptospirillum ferrooxidans*, *Thermus aquaticus*, *Clostridium paradoxum*). The user selects the pH and temperature (from the slide bars) of the "experiment" and presses the play button, and watches a timelapse of the spread of the colonies on the solid media. The increase in the number of colonies for each bacterial species is dependant on their ability to grow under the conditions of the "experiment".

The bottom picture shows the environments that these bacteria can live/thrive in.

## Try out without installation

You can try out the app without any installation by following the link:

http://nmbassil.shinyapps.io/bacterial-growth-simulation

## Requirements and instructions

Alternativly, if you want to use the app more regularly, you would require the following:

R, https://www.r-project.org

shiny package in R, https://shiny.rstudio.com

ggplot2 package in R, https://ggplot2.tidyverse.org

fGarch package in R, https://cran.r-project.org/web/packages/fGarch/index.html

### Notes

In R type the following to install all dependencies and run the app:
```
install.packages("shiny")

library(shiny)

runGitHub("Bacterial-growth-simulation", "nmbassil")
```
