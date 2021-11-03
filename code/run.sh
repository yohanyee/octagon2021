#!/bin/bash

# Parse data
# Wrangle cognitive data manually
# Read in Hexodata into one file
/usr/bin/time -v Rscript code/read_all_hexodata.R

# Things to test
# -PLS/Sparse PLS
# -Sleep
# -HR variability

# Also, plot basic activity cadence signal over time

# Bayesian networks

# 

# Why does PCA give overlapping points?

# Effect of season? Of weekend/weekday?

# Variables to add
Sleep: night before window

Sleep_total_cadence
Sleep_max_hours
Sleep_total_cycles
Sleep_HR_mean
Sleep_HR_variability

PW: "prior window"

PW_HR_mean
PW_HW_variability
PW_BR_mean
PW_BR_variability
PW_Cadence_total

Categorical variables:
Summer (T/F based on whether a school year or not - May to August)
Weekend


ICA of HR and BR signals -> does smoothed version decompose into exercise vs stress components? - check if exercise based on when there is exercise

filter(hexo, ...)