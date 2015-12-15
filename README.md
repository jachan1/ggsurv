## Synopsis

ggplot based function to draw Kaplan-Meiers curves. Based on http://www.inside-r.org/packages/cran/GGally/docs/ggsurv with different confidence bands and plottable SAS-esque survival counts.

## Code Example

sample_graphs.R gives some working examples.

## Motivation

ggplot does not have a function that takes a survfit object and returns a useful plot. 

## Installation

sample_graphs.R provides instructions to source the function from github, or just download the file

## parameters
CI - logical turning on and off confidence intervals        
plot.cens - logical turning on and off the plotting of censor times              
cens.col - controls color of cens if plotted             
cens.shape - controls shape of cens if plotted          
surv.col - allows specification of curve color         
cumProb - if true plots cumulative probability instead of survival    
addCounts - adds in survival counts below graph        
    
