ggsurv <- function(s, ...){
  UseMethod("ggsurv")
}

ggsurv.survfit <- function(s, CI = T, plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'red', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, xlab = 'Time',
                   ylab = '', main = '', cumProb = F, yTicks=5, 
                   addNRisk=F, dataLabels="", addCounts=F, bw=F,
                   legend_title, legend_pos){
    
    require(ggplot2)
    require(dplyr)
    ## confirm validity of parameters
    strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
    stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
    stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
    if(class(s) != "survfit") stop("First parameter needs to be a survfit object")
    ## need a separate construction for single strata and multi-strata
    ggsurv_s <- function(s, yAxisScale){
        ## given default informative y axis label
        if(ylab == ""){
            ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
        }
        ## set up survival data in ggplot form
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
        
        ## if col not specified use black 
        col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
        
        ## extra rows need to be created in order for survival numbers to be added
        ncount_labs <- 6
        time_seq <- with(dat, seq(min(time), max(time), length.out = ncount_labs))
        
        count_cuts <- dat %>% mutate(time_grp = cut(time, time_seq)) %>% 
            group_by(time_grp) %>% summarise(time=max(time)) %>% right_join(data_frame(time_grp=levels(.$time_grp)), by="time_grp")
        
        dat$count_lab <- dat$atRisk
        dat$count_lab[!dat$time %in% c(min(dat$time), count_cuts$time)] <- NA
        dat <- dat %>% 
            left_join(data_frame(time = c(min(dat$time), count_cuts$time), 
                                 lab_time = time_seq), by="time")
       
        ## initial ggplot object created
        pl <- ggplot(dat, aes(x = time, y = surv)) +
            xlab(xlab) + ylab(ylab) + ggtitle(main) +
            yAxisScale +
            geom_step(col = col, lty = lty.est) +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border=element_rect(fill=NA),
                  panel.background = element_blank())
        
        ## add counts below graph
        if(addCounts) {
            pl <- pl + geom_text(aes(x=lab_time, label=count_lab, y=-0.08), size=3.5) +
                geom_hline(yintercept=min(dat$time), color="#BBBBBB", linetype="dashed")
        }
        
        if(addNRisk) message("addNRisk functionality removed. Use addCounts instead.")
        
        ## add rectangles for error
        pl <- if(CI) {
            pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up),
                           fill=col, alpha = 0.1)
        } else (pl)
        
        ## plot censor data as crosses
        pl <- if(plot.cens == T & length(dat.cens) > 0){
            pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                            col = cens.col)
        } else if (plot.cens == T & length(dat.cens) == 0){
            stop ('There are no censored observations')
        } else(pl)
        pl
        
    }
    
    ggsurv_m <- function(s, yAxisScale, legend_title, legend_pos) {
        ## given default informative y axis label
      
        if(ylab == ""){
            ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
        }
        
        ## set up survival data in ggplot form
        
        groups <- factor(unlist(strsplit(names
                                         (s$strata), '='))[seq(2, 2*strata, by = 2)])
#         groups_labs <- sapply(levels(groups), expression)
#         names(groups_labs) <- levels(groups)

        if(missing(legend_title)){
          gr.name <- unlist(strsplit(names(s$strata), '='))[1]
        }else{
          gr.name = legend_title
        }
        
        gr.df <- vector('list', strata)
        ind <- vector('list', strata)
        n_ind <- c(0,s$strata); n_ind <- cumsum(n_ind)
        
        ## need to build data separately for strata
        for(i in 1:strata){
          ind[[i]] <- (n_ind[i]+1):n_ind[i+1]
          
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
            group = rep(groups[i], s$strata[i] + 1),
            groupFull = rep(names(s$strata)[i], s$strata[i] + 1),
            atRisk = c(s$n.risk[1], s$n.risk[ ind[[i]] ]))
          tmp$timeMax <- c(tmp$time[-1], tmp$time[length(tmp$time)])
          gr.df[[i]] <- tmp
        }
        ## helper functions
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
        ## time_grps helpful for adding counts
        time_grps <- with(dat, seq(min(time), max(time), length.out = 6))
        ## dat needs to have addcounts info added before ggplot first called
        if(addCounts) {
            dat$group <- factor(sprintf("%g: %s", as.numeric(dat$group), dat$group))
            tmp <- as.data.frame(with(dat %>% filter(time > 0), table(group, time_grp))) %>% filter(Freq > 0) 
            allrows <- NULL
            levels(tmp[['time_grp']]) <- time_grps[-1]
            for(i in 1:nrow(tmp)){
                time_cut <- as.numeric(as.character(tmp[['time_grp']][i]))
                allrows <- bind_rows(allrows,
                                     dat %>% filter(group == as.character(tmp[['group']][i])) %>% filter(time == max(ifelse(time <= time_cut, time, -Inf))) %>% 
                                         mutate(time = time_cut, count_lab = atRisk, count_lab_x = time_cut))
            }
            dat <- bind_rows(allrows, dat) %>% arrange(group, time)
        }
        
        ## initial plot created
        pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
            xlab(xlab) + ylab(ylab) + ggtitle(main) +
            yAxisScale +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border=element_rect(fill=NA),
                  panel.background = element_blank())
        
        if(bw){
            pl <- pl + geom_step(aes(lty = group))
        } else {
            pl <- pl + geom_step(aes(col = group))
        }
        
        ## counts added
        if(addCounts) {
            pl <- pl + geom_text(aes(x= count_lab_x, label=count_lab, y=count_lab_y), size=3) +
                geom_hline(yintercept=0, color="#CCCCCC", linetype="dotted") +
                geom_text(aes(x=min(time)-range(time)[2]*0.03, label=lablab, y=lablaby), size=3)
        }
        
        ## colors defined
        pl <- if(surv.col[1] != 'gg.def'){
            pl + col
        } else {
          pl + scale_colour_discrete(name = gr.name)
        }
        
        pl <- if(length(lty.est) == 1){
            pl + scale_linetype_manual(name = gr.name,
                                       values = rep(lty.est, strata))
        } else {pl + scale_linetype_manual(name = gr.name, 
                                           values = lty.est)}
        
        ## add in confidence intervals
        if(CI){
            col <- if(length(surv.col) == 1){
                scale_fill_discrete(name = gr.name)
            } else{
                scale_fill_manual(name = gr.name, 
                                  values = surv.col)
            }
            pl <- pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up, fill = group),
                           alpha = 0.1) + col
        }
        
        ## plot censor points
        if(plot.cens == T & any(dat$cens > 0)){
            pl <- pl + geom_point(data = dat %>% filter(cens > 0), aes(y = surv), shape = cens.shape,
                                  col = cens.col)
        } else if(plot.cens == T & all(dat$cens == 0)) {
            stop ('There are no censored observations')
        }
        lPos <- if(missing(legend_pos)){
          if(cumProb) {
            c(0,1)
          } else {
            c(1,1)
          }
        } else {
          legend_pos
        }
        
        pl + theme(legend.justification=lPos, legend.position=lPos)
    }
    
    ## -0.8 used as an addCount spacer throughout
    ## call either single or multi strata function
    if(strata == 1) {
        nticks <- if(addCounts){
            c(-0.08, seq(0,1, length.out=yTicks))
        } else seq(0,1, length.out=yTicks)
        nlabs <- if(addCounts) {
            c("N", paste0(100*(nticks[-1]), "%"))
        } else paste0(100*nticks, "%")
        yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.1, -0.025),1.1), breaks=nticks, labels = nlabs)
        ggsurv_s(s, yAxisScale)
    } else {
        nticks <- seq(0,1, length.out=yTicks)
        nlabs <- paste0(100*nticks, "%")
        yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.125 - 0.065*(strata-1), -0.025),1.1), breaks=nticks, labels = nlabs)
        ggsurv_m(s, yAxisScale, legend_title, legend_pos)
    }
}

ggsurv.survfit.cox <- function(s, CI = T, plot.cens = T, surv.col = 'gg.def',
                           cens.col = 'red', lty.est = 1, lty.ci = 2,
                           cens.shape = 3, xlab = 'Time',
                           ylab = '', main = '', cumProb = F, yTicks=5, 
                           addNRisk=F, dataLabels="", addCounts=F, bw=F,
                           legend_title, legend_pos, strata_names){
  
  require(ggplot2)
  require(dplyr)
  ## confirm validity of parameters
  strata <- ifelse(is.null(ncol(s$surv)) ==T, 1, ncol(s$surv))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  if(!missing(strata_names)) {
    stopifnot(length(strata_names) == strata)
  } else {
    strata_names <- names(s$strata)
  }
  # if(class(s) != "survfit") stop("First parameter needs to be a survfit object")
  ## need a separate construction for single strata and multi-strata
  ggsurv_s <- function(s, yAxisScale){
    ## given default informative y axis label
    if(ylab == ""){
      ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
    }
    ## set up survival data in ggplot form
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
    
    ## if col not specified use black 
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    ## extra rows need to be created in order for survival numbers to be added
    ncount_labs <- 6
    time_seq <- with(dat, seq(min(time), max(time), length.out = ncount_labs))
    
    count_cuts <- dat %>% mutate(time_grp = cut(time, time_seq)) %>% 
      group_by(time_grp) %>% summarise(time=max(time)) %>% right_join(data_frame(time_grp=levels(.$time_grp)), by="time_grp")
    
    dat$count_lab <- dat$atRisk
    dat$count_lab[!dat$time %in% c(min(dat$time), count_cuts$time)] <- NA
    dat <- dat %>% 
      left_join(data_frame(time = c(min(dat$time), count_cuts$time), 
                           lab_time = time_seq), by="time")
    
    ## initial ggplot object created
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      yAxisScale +
      geom_step(col = col, lty = lty.est) +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            panel.border=element_rect(fill=NA))
    
    ## add counts below graph
    if(addCounts) {
      pl <- pl + geom_text(aes(x=lab_time, label=count_lab, y=-0.08), size=3.5) +
        geom_hline(yintercept=min(dat$time), color="#BBBBBB", linetype="dashed")
    }
    
    if(addNRisk) message("addNRisk functionality removed. Use addCounts instead.")
    
    ## add rectangles for error
    pl <- if(CI) {
      pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up),
                     fill=col, alpha = 0.1)
    } else (pl)
    
    ## plot censor data as crosses
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    pl
    
  }
  
  ggsurv_m <- function(s, yAxisScale, legend_title, legend_pos) {
    ## given default informative y axis label
    if(ylab == ""){
      ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
    }
    
    ## set up survival data in ggplot form
    
    #         groups_labs <- sapply(levels(groups), expression)
    #         names(groups_labs) <- levels(groups)
    
    gr.name <- if(missing(legend_title)) {
      ""
    } else {
      gr.name = legend_title
    }
    
    gr.df <- vector('list', strata)
    
    ## need to build data separately for strata
    for(i in 1:strata){
      
      if (cumProb) {
        survCol <- 1 - c(1, s$surv[,i])
        lowCol <- 1 - c(1, s$lower[,i])
        highCol <- 1 - c(1, s$upper[,i])
      } else {
        survCol <- c(1, s$surv[,i])
        lowCol <- c(1, s$lower[,i])
        highCol <- c(1, s$upper[,i])
      }
      tmp <- data_frame(time=c(0, s$time), surv=survCol, 
                        up=highCol, low=lowCol,
                        cens=c(0, s$n.censor), group=strata_names[i],
                        atRisk=c(s$n.risk[1], s$n.risk))
      tmp$timeMax <- c(tmp$time[-1], tmp$time[length(tmp$time)])
      gr.df[[i]] <- tmp
    }
    
    ## helper functions
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
    ## time_grps helpful for adding counts
    time_grps <- with(dat, seq(min(time), max(time), length.out = 6))
    ## dat needs to have addcounts info added before ggplot first called
    if(addCounts) {
      dat$group <- factor(sprintf("%g: %s", as.numeric(dat$group), dat$group))
      tmp <- as.data.frame(with(dat %>% filter(time > 0), table(group, time_grp))) %>% filter(Freq == 0) 
      allrows <- NULL
      levels(tmp[['time_grp']]) <- time_grps[-1]
      for(i in 1:nrow(tmp)){
        time_cut <- as.numeric(as.character(tmp[['time_grp']][i]))
        allrows <- bind_rows(allrows,
                             dat %>% filter(group == as.character(tmp[['group']][i])) %>% filter(time == max(ifelse(time <= time_cut, time, -Inf))) %>% 
                               mutate(time = time_cut, count_lab = atRisk, count_lab_x = time_cut))
      }
      dat <- bind_rows(allrows, dat) %>% arrange(group, time)
    }
    
    ## initial plot created
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      yAxisScale +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border=element_rect(fill=NA),
            panel.background = element_blank())
    
    if(bw){
      pl <- pl + geom_step(aes(lty = group))
    } else {
      pl <- pl + geom_step(aes(col = group))
    }
    
    ## counts added
    if(addCounts) {
      pl <- pl + geom_text(aes(x= count_lab_x, label=count_lab, y=count_lab_y), size=3) +
        geom_hline(yintercept=0, color="#CCCCCC", linetype="dotted") +
        geom_text(aes(x=min(dat$time)-range(dat$time)[2]*0.03, label=lablab, y=lablaby), size=3)
    }
    
    ## colors defined
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {
      pl + scale_colour_discrete(name = gr.name)
    }
    
    pl <- if(length(lty.est) == 1){
      pl + scale_linetype_manual(name = gr.name,
                                 values = rep(lty.est, strata))
    } else {pl + scale_linetype_manual(name = gr.name, 
                                       values = lty.est)}
    
    ## add in confidence intervals
    if(CI){
      col <- if(length(surv.col) == 1){
        scale_fill_discrete(name = gr.name)
      } else{
        scale_fill_manual(name = gr.name, 
                          values = surv.col)
      }
      pl <- pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up, fill = group),
                           alpha = 0.1) + col
    }
    
    ## plot censor points
    if(plot.cens == T & any(dat$cens > 0)){
      pl <- pl + geom_point(data = dat %>% filter(cens > 0), aes(y = surv), shape = cens.shape,
                            col = cens.col)
    } else if(plot.cens == T & all(dat$cens == 0)) {
      stop ('There are no censored observations')
    }
    lPos <- if(missing(legend_pos)){
      if(cumProb) {
        c(0,1)
      } else {
        c(1,1)
      }
    } else {
      legend_pos
    }
    
    pl + theme(legend.justification=lPos, legend.position=lPos)
  }
  
  ## -0.8 used as an addCount spacer throughout
  ## call either single or multi strata function
  if(strata == 1) {
    nticks <- if(addCounts){
      c(-0.08, seq(0,1, length.out=yTicks))
    } else seq(0,1, length.out=yTicks)
    nlabs <- if(addCounts) {
      c("N", paste0(100*(nticks[-1]), "%"))
    } else paste0(100*nticks, "%")
    yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.1, -0.025),1.1), breaks=nticks, labels = nlabs)
    ggsurv_s(s, yAxisScale)
  } else {
    nticks <- seq(0,1, length.out=yTicks)
    nlabs <- paste0(100*nticks, "%")
    yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.125 - 0.065*(strata-1), -0.025),1.1), breaks=nticks, labels = nlabs)
    ggsurv_m(s, yAxisScale, legend_title, legend_pos)
  }
}