###########
## CLIMAR FRAMEWORK


# x as list of x's
# betas as list of betas
calculate_hazard <- function(x,
                             betas,
                             beta0=0) {
    hazard <- beta0 + x*betas
    return(hazard)
}

# x as list of x's
# betas as list of betas
calculate_exposure <- function(x,
                               betas,
                               beta0=0) {
    exposure <- beta0 + x*betas
    return(exposure)
}

# x as list of x's
# betas as list of betas
calculate_vulnerability <- function(x, 
                                    betas,
                                    beta0=0) {
    vulnerability <- beta0 + x*betas
    return(vulnerability)
}


# x as list of x's
# betas as list of betas
calculate_risk <- function(hazard,
                           exposure,
                           vulnerability) {
    risk <- hazard * exposure * vulnerability
    return(risk)
}


# x as list of x's
# betas as list of betas
draw_impact <- function(risk) {
    impact <- qbinom(risk)
    return(impact)
}








