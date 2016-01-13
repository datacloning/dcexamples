#' ---
#' title: "seeds: random effects logistic regression (BUGS Examples Vol. 1)"
#' subtitle: "dcmle examples"
#' ---
#'
#' This example is taken from Table 3 of Crowder (1978), and concerns the 
#' proportion of seeds that germinated on each of 21 plates arranged 
#' according to a 2 by 2 factorial layout by seed and type of root extract. 
#' The data are shown below, where r i and n i are the number of germinated 
#' and the total number of seeds on the $i$th plate, $i = 1,...,N$. 
#' These data are also analysed by, for example, Breslow: and Clayton (1993).
#'
library(dcmle)
load.module("glm")
seeds <- makeDcFit(
    multiply = "N",
    data = list(
        "N" = 21,
        "r" = 
            c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 
            3, 22, 15, 32, 3),
        "n" = 
            c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 
            4, 12, 41, 30, 51, 7),
        "x1" = 
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        "x2" = 
            c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)),
    inits = list(
        "tau" = 1, 
        "alpha0" = 0, 
        "alpha1" = 0, 
        "alpha2" = 0, 
        "alpha12" = 0),
    model = function() {
        alpha0  ~ dnorm(0.0,1.0E-6);     # intercept
        alpha1  ~ dnorm(0.0,1.0E-6);     # seed coeff
        alpha2  ~ dnorm(0.0,1.0E-6);     # extract coeff
        alpha12 ~ dnorm(0.0,1.0E-6);     # intercept
        tau     ~ dgamma(1.0E-3,1.0E-3); # 1/sigma^2
        sigma  <- 1.0/sqrt(tau);
        for (i in 1:N) {
           b[i]         ~ dnorm(0.0,tau);
           logit(p[i]) <- alpha0 + alpha1*x1[i] + alpha2*x2[i] +
                          alpha12*x1[i]*x2[i] + b[i];
           r[i]         ~ dbin(p[i],n[i]);
        }
    },
    params = c("alpha0", "alpha1", "alpha2", "alpha12", "sigma"))
#dcmle(seeds, n.clones=1:2, n.iter=1000)
