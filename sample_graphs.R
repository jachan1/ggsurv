source_https <- function(url, ...) {
    # load package
    require(RCurl)
    
    # parse and evaluate each .R script
    tmp <- sapply(c(url, ...), function(u) {
        eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
    })
    message(sprintf("%s loaded into global environment", paste(substr(names(tmp), max(grep("/", strsplit(names(tmp), "")[[1]]))+1, nchar(names(tmp))), collapse=", ")))
}

# Example
source_https("https://raw.githubusercontent.com/jachan1/ggsurv/master/ggsurvFxn.R")

## build survival object
sv1 <- with(ovarian, Surv(futime, fustat))

## fit intercept only survival
sf1 <- survfit(sv1 ~ 1, ovarian)
## basic km curve
ggsurv(sf1)
## cumulative probability instead of survival
ggsurv(sf1, cumProb=T)
## add in survival counts
ggsurv(sf1, cumProb=T, addCounts=T)

## fit model with two strata
sf2 <- survfit(sv1 ~ resid.ds, ovarian)
## basic km curve
ggsurv(sf2, CI=F)
## add in confidence intervals
ggsurv(sf2, CI=T)
## cumulative probability instead of survival
ggsurv(sf2, cumProb=T)
## add in survival counts
ggsurv(sf2, cumProb=T, addCounts=T)

