#!/usr/bin/env sh

quarto render ./INV_quality_control_nb.qmd

quarto render ./INV_quality_control_nb.qmd --to ipynb

