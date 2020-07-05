if (!require('pacman')) install.packages('pacman')
# pacman handles the installation and loading of packages
pacman::p_load('deSolve',
               'ggplot2',
               'tictoc',
               'scales',
               'here',
               'dplyr'
)