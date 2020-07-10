# pacman handles the installation and loading of packages

if (!require('pacman')) install.packages('pacman')

pacman::p_load('deSolve',
               'ggplot2',
               'tictoc',
               'scales',
               'here',
               'dplyr',
               'viridis',
               'shiny',
               'shinythemes',
               'gridExtra'
)