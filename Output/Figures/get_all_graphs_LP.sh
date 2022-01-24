#!/bin/bash

# Pequeno comando bash para distribuir as imagens em pastas referentes às variáveis pertinentes

mkdir -p Figures/Infl_ext_endo
mkdir -p Figures/Infl_ext_exo_comm
mkdir -p Figures/Infl_ext_exo_petro

cp -t Figures/Infl_ext_endo MENSAL_*exo[notrend]*/*
cp -t Figures/Infl_ext_exo MENSAL_*exo[comm]*/*
cp -t Figures/Infl_ext_exo MENSAL_*exo[petro]*/*

