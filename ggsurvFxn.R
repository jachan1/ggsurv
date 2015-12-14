ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'red', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = '', main = '', cumProb = F, yTicks=5, addNRisk=F, dataLabels="", addCounts=F, bw=F){
    
    require(ggplot2)
    require(dplyr)
    strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
    stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
    stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
    
    ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                         cens.col = 'red', lty.est = 1, lty.ci = 2,
                         cens.shape = 3, back.white = F, xlab = 'Time',
                         ylab = '', main = '', cumProb=F, 
                         yAxisScale=scale_y_continuous(), addNRisk=F, dataLabels="", addCounts=F){
        if(ylab == ""){
            ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
        }
        if (cumProb) {
            survCol <- 1 - c(1, s$surv)
            lowCol <- 1 - c(1, s$lower)
            highCol <- 1 - c(1, s$upper)
        } else {
            survCol <- c(1, s$surv)
            lowCol <- c(1, s$lower)
            highCol <- c(1, s$upper)
        }
        dat <- data.frame(time = c(0, s$time),
                          surv = survCol,
                          up = highCol,
                          low = lowCol,
                          groupFull = "All",
                          cens = c(0, s$n.censor),
                          atRisk = c(s$n.risk[1], s$n.risk))
        dat$timeMax <- c(dat$time[-1], dat$time[length(dat$time)])
        dat.cens <- subset(dat, cens != 0)
        
        col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
        
        count_cuts <- dat %>% mutate(time_grp = cut(time, seq(min(time), max(time), length.out = 6))) %>% 
            filter(!is.na(time_grp)) %>% arrange(time) %>%
            group_by(time_grp) %>% summarise(time=max(time))
        
        dat$count_lab <- dat$atRisk
        dat$count_lab[!dat$time %in% c(min(dat$time), count_cuts$time)] <- NA
        dat<-bind_rows(dat, data_frame(time=min(dat$time), count_lab_tick="N"))
        
        pl <- ggplot(dat, aes(x = time, y = surv)) +
            xlab(xlab) + ylab(ylab) + ggtitle(main) +
            yAxisScale +
            geom_step(col = col, lty = lty.est) +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(), panel.border=element_rect(fill=NA))
        
        if(addCounts) {
            pl <- pl + geom_text(aes(label=count_lab, y=-0.08), size=3.5) +
                geom_hline(y=min(dat$time), color="#CCCCCC", linetype="dotted") +
                geom_text(aes(label=count_lab_tick, y=-0.08), size=3.5, hjust=4)
        }
        
        if(addNRisk) print("addNRisk functionality removed. Use addCounts instead.")
        if(FALSE) { ## disabling addNRisk since it doesn't really work
            xTicks <- ggplot_build(pl)$panel$ranges[[1]]$x.major_source
            
            getRowsFxn <- function(x, xTicks){
                x[findInterval(xTicks, x$time), c("time", "atRisk")] %>% mutate(time = xTicks)
            }
            
            nRiskTab <- dat %>% group_by(groupFull) %>% do(getRowsFxn(., xTicks))
            colnames(nRiskTab) <- c("strata", "time", "n.risk")
            rownames(nRiskTab) <- NULL
            stopifnot(length(dataLabels) == 1)
            dataLabs <- ifelse(dataLabels=="", "All", dataLabels)
            data.table <- ggplot(nRiskTab,aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
                geom_text(data=nRiskTab, aes(x=time, y=strata, label=n.risk), size=3.5) + theme_bw() +
                scale_y_discrete(breaks = "All",
                                 labels = dataLabs) +
                scale_x_continuous("Numbers at risk", limits = c(0, max(dat$time))) +
                theme(axis.title.x = element_text(size = 10, vjust = 1),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.border = element_blank(),axis.text.x = element_blank(),
                      axis.ticks = element_blank(),axis.text.y = element_text(face = "bold",hjust = 0))
            data.table <- data.table + 
                theme(legend.position = "none") + xlab(NULL) + ylab(NULL)
            print(tmp)
            m <- 5 
            data.table <- data.table +
                theme(plot.margin = unit(c(-1.5, 1, 0.1, 3.5 - 0.28 * m), "lines")) # ADJUST POSITION OF TABLE FOR AT RISK
            blank.pic <- ggplot(dat, aes(time, surv)) +
                geom_blank() + theme_bw() +
                theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
                      axis.title.x = element_blank(),axis.title.y = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid.major = element_blank(),panel.border = element_blank())
        }
        
        
        pl <- if(CI == T | CI == 'def') {
            pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up),
                           fill=col, alpha = 0.1)
        } else (pl)
        
        pl <- if(plot.cens == T & length(dat.cens) > 0){
            pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                            col = cens.col)
        } else if (plot.cens == T & length(dat.cens) == 0){
            stop ('There are no censored observations')
        } else(pl)
        
        if(FALSE) { ## addNRisk functionality removed
            require(gridExtra)
            arrangeGrob(pl, blank.pic, data.table, clip = FALSE, nrow = 3,
                        ncol = 1, heights = unit(c(2, .1, .25), c("null", "null", "null")))
        } else {
            pl
        }
    }
    
    ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                         cens.col = 'red', lty.est = 1, lty.ci = 2,
                         cens.shape = 3, back.white = F, xlab = 'Time',
                         ylab = '', main = '', cumProb=F, 
                         yAxisScale=scale_y_continuous(), addNRisk=F, dataLabels="", addCounts=F) {
        if(ylab == ""){
            ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
        }
        
        n <- s$strata
        
        groups <- factor(unlist(strsplit(names
                                         (s$strata), '='))[seq(2, 2*strata, by = 2)])
        gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
        gr.df <- vector('list', strata)
        ind <- vector('list', strata)
        n.ind <- c(0,n); n.ind <- cumsum(n.ind)
        for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
        
        for(i in 1:strata){
            if (cumProb) {
                survCol <- 1 - c(1, s$surv[ ind[[i]] ])
                lowCol <- 1 - c(1, s$lower[ ind[[i]] ])
                highCol <- 1 - c(1, s$upper[ ind[[i]] ])
            } else {
                survCol <- c(1, s$surv[ ind[[i]] ])
                lowCol <- c(1, s$lower[ ind[[i]] ])
                highCol <- c(1, s$upper[ ind[[i]] ])
            }
            tmp <-data_frame(
                time = c(0, s$time[ ind[[i]] ]),
                surv = survCol,
                up = highCol,
                low = lowCol,
                cens = c(0, s$n.censor[ ind[[i]] ]),
                group = rep(groups[i], n[i] + 1),
                groupFull = rep(names(s$strata)[i], n[i] + 1),
                atRisk = c(s$n.risk[1], s$n.risk[ ind[[i]] ]))
            tmp$timeMax <- c(tmp$time[-1], tmp$time[length(tmp$time)])
            gr.df[[i]] <- tmp
        }
        
        makecnts <- function(x){
            x$count_lab[nrow(x)] <- x$atRisk[nrow(x)]
            x$count_lab_x[nrow(x)] <- as.numeric(as.character(x$time_max[nrow(x)]))
            x$count_lab_y <- -0.08 - 0.065*(as.numeric(x$group) - 1)
            x
        }
        
        makezeros <- function(x) {
            x$lablab <- NA
            x$lablaby <- NA
            x$lablab[1] <- paste0(as.numeric(x$group[1]), ":")
            x$lablaby[1] <- -0.08 - 0.065*(as.numeric(x$group[1]) - 1)
            
            x$count_lab[1] <- x$atRisk[1]
            x$count_lab_x[1] <- 0
            x
        }
        dat <- bind_rows(gr.df) %>% mutate(group=factor(group)) %>%
            mutate(time_grp = cut(time, seq(min(time), max(time), length.out = 6), include.lowest = T), 
                   time_max = factor(as.numeric(time_grp), levels=1:length(levels(time_grp)), labels=seq(min(time), max(time), length.out = 6)[-1])) %>% 
            group_by(group, time_grp) %>% arrange(time) %>% do(makecnts(.)) %>% 
            group_by(group) %>% do(makezeros(.)) %>% ungroup()
        
        if(addCounts) dat$group <- factor(sprintf("%g: %s", as.numeric(dat$group), dat$group))
    
        pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
            xlab(xlab) + ylab(ylab) + ggtitle(main) +
            yAxisScale +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(), panel.border=element_rect(fill=NA))
        
        if(bw){
            pl <- pl + geom_step(aes(lty = group))
        } else {
            pl <- pl + geom_step(aes(col = group))
        }
         
        pl <- pl + theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(), panel.border=element_rect(fill=NA))
        
        if(addCounts) {
            pl <- pl + geom_text(aes(x= count_lab_x, label=count_lab, y=count_lab_y), size=3.5) +
                geom_hline(y=0, color="#CCCCCC", linetype="dotted") +
                geom_text(aes(x=min(dat$time)-range(dat$time)[2]*0.03, label=lablab, y=lablaby), size=3.5)
        }
        if(addNRisk) print("addNRisk functionality removed. Use addCounts instead.")
        if(FALSE) { ## addNRisk functionality removed
            xTicks <- ggplot_build(pl)$panel$ranges[[1]]$x.major_source
            
            getRowsFxn <- function(x, xTicks){
                x[findInterval(xTicks, x$time), c("time", "atRisk")] %>% mutate(time = xTicks)
            }
            
            nRiskTab <- dat %>% group_by(groupFull) %>% do(getRowsFxn(., xTicks))
            colnames(nRiskTab) <- c("strata", "time", "n.risk")
            rownames(nRiskTab) <- NULL
            dataLabs <- if (dataLabels[1]==""){
                names(s$strata)
            } else {
                rev(dataLabels)
            }
            data.table <- ggplot(dat,aes(x = time, y = strata, label = format(atRisk, nsmall = 0))) +
                geom_text(data=nRiskTab, aes(x=time, y=strata, label=n.risk), size=3.5) + theme_bw() +
                scale_y_discrete(breaks = names(s$strata),
                                 labels = dataLabs) +
                scale_x_continuous("Numbers at risk", limits = c(0, max(dat$time))) + 
                theme(axis.title.x = element_text(size = 10, vjust = 1),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.border = element_blank(),axis.text.x = element_blank(),
                      axis.ticks = element_blank(),axis.text.y = element_text(face = "bold",hjust = 1))
            
            data.table <- data.table +
                theme(legend.position = "none") + xlab(NULL) + ylab(NULL)
            m <- max(nchar(dataLabs))
            data.table <- data.table +
                theme(plot.margin = unit(c(-1.5, 1, 0.1, 2.5 - 0.28 * m), "lines")) # ADJUST POSITION OF TABLE FOR AT RISK
            blank.pic <- ggplot(dat, aes(time, surv)) +
                geom_blank() + theme_bw() +
                theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
                      axis.title.x = element_blank(),axis.title.y = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid.major = element_blank(),panel.border = element_blank())
        }
        
        
        col <- if(length(surv.col) == 1){
            scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
        } else{
            scale_colour_manual(name = gr.name, values = surv.col)
        }
        
        pl <- if(surv.col[1] != 'gg.def'){
            pl + col
        } else {pl + scale_colour_discrete(name = gr.name)}
        
        line <- if(length(lty.est) == 1){
            scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
        } else {scale_linetype_manual(name = gr.name, values = lty.est)}
        
        pl <- pl + line
        
        pl <- if(CI == T) {
            if(length(surv.col) > 1 && length(lty.est) > 1){
                stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
            }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
                pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up, fill = group),
                               alpha = 0.1) +
                    scale_fill_manual(name = gr.name, values = rep(surv.col, strata))
            } else{
                pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up, fill = group),
                               alpha = 0.1) +
                    scale_fill_manual(name = gr.name, values = rep(surv.col, strata))
            }
        } else {pl}
        
        
        pl <- if(plot.cens == T & any(dat$cens > 0)){
            pl + geom_point(data = dat %>% filter(cens > 0), aes(y = surv), shape = cens.shape,
                            col = cens.col)
        } else if (plot.cens == T & all(dat$cens == 0)){
            stop ('There are no censored observations')
        } else(pl)
        
        lPos <- if(cumProb) {
            c(1,0)
        } else {
            c(1,1)
        }
        pl <- pl + theme(legend.justification=lPos, legend.position=lPos)
        
        if(FALSE) { ## addNRisk functionality removed
            require(gridExtra)
            arrangeGrob(pl, blank.pic, data.table, clip = FALSE, nrow = 3,
                        ncol = 1, heights = unit(c(2, .1, .25), c("null", "null", "null")))
        } else {
            pl
        }
    }
    
    
    
    pl <- if(strata == 1) {
        nticks <- seq(0,1, length.out=yTicks)
        nlabs <- paste0(100*nticks, "%")
        yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.075, -0.025),1.1), breaks=nticks, labels = nlabs)
        ggsurv.s(s, CI , plot.cens, surv.col ,
                 cens.col, lty.est, lty.ci,
                 cens.shape, back.white, xlab,
                 ylab, main, cumProb, yAxisScale, addNRisk, dataLabels, addCounts)
    } else {
        nticks <- seq(0,1, length.out=yTicks)
        nlabs <- paste0(100*nticks, "%")
        yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.1 - 0.065*(strata-1), -0.025),1.1), breaks=nticks, labels = nlabs)
        ggsurv.m(s, CI, plot.cens, surv.col ,
                 cens.col, lty.est, lty.ci,
                 cens.shape, back.white, xlab,
                 ylab, main, cumProb, yAxisScale, addNRisk, dataLabels, addCounts)}
    pl
}