# Bayesian-analysis-models

OpenBUGS code together with R functions to implement Bayesian inferences for basket trials, with possibility of borrowing information across patient subgroups.

Files contained in this repository can be used to reproduce the numerical results reported in the paper entitled
# H. Zheng and J.M.S. Wason. Borrowing of information across patient subgroups in a basket trial based on distributional discrepancy.

The file "HDist.txt" is the model specification for the proposed methodology to be implemented through OpenBUGS, while the files "StandardHM.txt", "EXNEX.txt" and "Stratified.txt" are those for alternative Bayesian models that may be used for analysing basket trial data. In particular, "StandardHM.txt" is the standard hierarchical model which assumes the subgroup-specific parameters to be fully exchangeable, "EXNEX.txt" is the robust extension proposed by Neuenschwander et al. (2016), and "Stratified.txt" is the approach of no borrowing. 

Numerical results reported in Section 4 of the paper as well as those in the Supplementary Materials can be reproduced by calling these models in R with the R2OpenBUGS package. Example R functions have been provided in the file "Implementation.R".
