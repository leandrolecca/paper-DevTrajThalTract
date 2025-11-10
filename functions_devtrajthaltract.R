# plt.models <- function(ATLAS,
#                        x,
#                        y,
#                        model.fem.left,
#                        model.fem.right,
#                        model.male.left,
#                        model.male.right,
#                        xlab,
#                        ylab,
#                        line.width,
#                        title,
#                        legend = TRUE,
#                        hide.x.axis = FALSE,
#                        toWT = FALSE,
#                        tag,
#                        tag.size,
#                        font.size) {
#   
#   if (missing(xlab)){
#     xlab="Age"
#   }
#   
#   if (missing(x)){
#     x="AGE"
#   }
#   
#   if (missing(line.width)){
#     line.width = 0.75
#   }
#   
#   if(toWT) {
#     ATLAS$yfrac_WT <- (ATLAS[,y]/ ATLAS$Whole_thalamus)*100
#     ylab = bquote(frac(Vol[.(y)], Vol['Thalamus']) ~ '[%]')
#     y = "yfrac_WT"
#   }
#   
#   if (missing(font.size)) {
#     font.size <- 24
#   }
#   
#   plt.vol <- ggplot(ATLAS,aes_string(x=x,
#                                      y=y,
#                                      colour="hemgen",
#                                      shape="hemgen",
#                                      linetype="hemgen")) + 
#     geom_point(alpha=0.5,key_glyph = large_points) + 
#     # females-left
#     stat_smooth(data=subset(ATLAS,Hemisphere == "Left" & Gender=="Female"),
#                 method = "lm", formula = model.fem.left, 
#                 alpha=0, size = line.width) +
#     # females-right
#     stat_smooth(data=subset(ATLAS,Hemisphere == "Right" & Gender=="Female"),
#                 method = "lm", formula = model.fem.right, 
#                 alpha=0, size = line.width) +
#     # males-left
#     stat_smooth(data=subset(ATLAS,Hemisphere == "Left" & Gender=="Male"),
#                 method = "lm", formula = model.male.left, 
#                 alpha=0, size = line.width) +
#     # males-right
#     stat_smooth(data=subset(ATLAS,Hemisphere == "Right" & Gender=="Male"),
#                 method = "lm", formula = model.male.right, 
#                 alpha=0, size = line.width) +
#     theme_bw() + labs(x=xlab, y=ylab) +
#     scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
#     scale_linetype_manual("",values=c(1,2,1,2)) +
#     scale_shape_manual("",values=c(16,17,16,17)) +
#     guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
#            shape=guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
#     theme(plot.title = element_text(hjust = 0.5, size = font.size),
#           text = element_text(size=font.size, family = "Arial"),
#           strip.text.x = element_text(size = font.size),
#           axis.text = element_text(size = font.size - 4),
#           # Hide panel borders and remove grid lines
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           # Change axis line
#           axis.line = element_line(colour = "black"),
#           # facet_wrap without boxes
#           strip.background = element_blank(),
#           legend.key.size = unit(2.5,"line"),
#           legend.key.width = unit(5,"line"),
#           aspect.ratio=1)
#   
#   if (!missing(title)){
#     plt.vol <- plt.vol + ggtitle(title)
#   }
#   
#   if (!legend) {
#     plt.vol <- plt.vol + theme(legend.position = "none")
#   }
#   
#   if (hide.x.axis) {
#     plt.vol <- plt.vol + theme(axis.ticks.x = element_blank(),
#                                axis.text.x = element_text(size=20, 
#                                                           colour = "white"),
#                                axis.title.x = element_text(colour = "white"))
#   }
#   
#   if (!missing(tag)) {
#     plt.vol <- plt.vol + labs(tag = tag) 
#     if(!missing(tag.size)) {
#       plt.vol <- plt.vol + theme(plot.tag = element_text(size = tag.size))
#     } else {
#       plt.vol <- plt.vol + theme(plot.tag = element_text(size = 30))
#     }
#   }
#   
#   return(plt.vol)
# }

install.env <- function(install.packages, libspath = "~/llecca/soft/R/4.3/lib/R/lib-devtract"){
  if (!dir.exists(libspath)){
    dir.create(libspath, recursive = TRUE)
  }
  if (install.packages){
    list.packages <- c(
      "dplyr",
      "tzdb",
      "readr",
      "tidyr",
      "languageserver",
      "jsonlite",
      "devtools",
      "ggh4x",
      "reshape2",
      "car",
      "plyr",
      "Rmisc",
      "pak",
      "ggpmisc",
      "rlang",
      "reshape",
      "ggpubr",
      "tidyverse",
      "ggstatsplot",
      "cowplot",
      "rsq",
      "RColorBrewer",
      "pracma",
      "patchwork",
      "ggsci",
      "effsize",
      "svglite",
      "Rcpp",
      "ggplot2",
      "data.table",
      "XML",
      "scales",
      "viridis",
      "matrixStats",
      "rjson",
      "ez",
      "httpgd",
      "lme4",
      "knitr",
      "rstatix",
      "ggnewscale",
      "wesanderson",
      "see",
      "afex",
      "ggprism",
      "lme4",
      "officer",
      "lmerTest",
      "flextable",
      "httpgd",
      "nlme"
    )
    
    list.packages <- list.packages[!list.packages %in% installed.packages(lib.loc = libspath)[,1]]
    print(list.packages)
    
    install.packages(
      list.packages, 
      lib = libspath,
      dependencies = TRUE
    )
    if (!"ggpmisc" %in% installed.packages(lib.loc = libspath)[,1]){
      # library(pak, lib.loc = libspath)
      # pak::pak("MASS")
      # pak::pak("Matrix")
      packageurl <- "https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.6-5.tar.gz"
      install.packages(packageurl, repos = NULL, type = "source",  lib = libspath)
      packageurl <- "https://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-60.tar.gz"
      install.packages(packageurl, repos = NULL, type = "source",  lib = libspath)
      
      install.packages("ggpmisc", lib = libspath)
    }
    
  } else {
    # Path to user .Rprofile
    rprofile_path <- path.expand("~/.Rprofile")
    
    # Content to write
    rprofile_content <- sprintf(
      '# Automatically set library path\n.libPaths("%s")\nmessage("Active library paths: ", paste(.libPaths(), collapse = ", "))\n',
      libspath
    )
    
    # Write (overwrite) .Rprofile
    writeLines(rprofile_content, rprofile_path)
    
    library(dplyr, lib.loc = libspath)
    library(tzdb, lib.loc = libspath)
    library(readr, lib.loc = libspath)
    library(ggplot2, lib.loc = libspath)
    library(tidyr, lib.loc = libspath)
    library(ggh4x, lib.loc = libspath)
    library(reshape2, lib.loc = libspath)
    library(car, lib.loc = libspath)
    library(plyr, lib.loc = libspath)
    library(Rmisc, lib.loc = libspath)
    library(Matrix, lib.loc = libspath)
    library(ggpmisc, lib.loc = libspath)
    library(reshape, lib.loc = libspath)
    library(ggpubr, lib.loc = libspath)
    library(tidyverse, lib.loc = libspath)
    # library(ggstatsplot, lib.loc = libspath)
    library(cowplot, lib.loc = libspath)
    library(rsq, lib.loc = libspath)
    library(RColorBrewer, lib.loc = libspath)
    library(pracma, lib.loc = libspath)
    library(patchwork, lib.loc = libspath)
    library(ggsci, lib.loc = libspath)
    library(effsize, lib.loc = libspath)
    library(svglite, lib.loc = libspath)
    library(data.table, lib.loc = libspath)
    library(XML, lib.loc = libspath)
    library(scales, lib.loc = libspath)
    library(viridis, lib.loc = libspath)
    library(matrixStats, lib.loc = libspath)
    library(rjson, lib.loc = libspath)
    library(ez, lib.loc = libspath)
    # library(httpgd, lib.loc = libspath)
    library(lme4, lib.loc = libspath)
    library(rstatix, lib.loc = libspath)
    library(ggnewscale, lib.loc = libspath)
    library(wesanderson, lib.loc = libspath)
    library(see, lib.loc = libspath)
    library(afex, lib.loc = libspath)
    library(ggprism, lib.loc = libspath)
    library(lme4, lib.loc = libspath)
    library(officer, lib.loc = libspath)
    library(lmerTest, lib.loc = libspath)
    library(flextable, lib.loc = libspath)
    library(nlme, lib.loc = libspath)
  }
}

label.outliers <- function(dt, sdmultiplier, tolerance, y = "val"){
  dt <- as.data.frame(dt)
  mean <- rowMeans(sapply(dt[,y], unlist))
  sd <- rowSds(sapply(dt[,y], unlist))
  
  for (idx in 1:nrow(dt)){
    
    values <- unlist(dt[idx, y])
    
    above <- sum(values > (mean + sd*sdmultiplier))
    below <- sum(values < (mean - sd*sdmultiplier))
    
    all <- above + below
    
    if (all > round(length(values)*(tolerance/100))){
      dt[idx,'outlier'] <- TRUE
    } else{
      dt[idx,'outlier'] <- FALSE 
    } 
  } 
  return(as.data.frame(dt))
}

get.profile.mean.sd <- function(dt, y = 'val'){
  dt <- as.data.frame(dt)
  tmpt <- as.data.frame(matrix(NA, nrow = 1, ncol = 3))
  names(tmpt) <- c('mean','sd','xlen')
  tmpt$mean <- vector(mode = "list",length=nrow(tmpt))
  tmpt$mean[[1]] <- rowMeans(sapply(dt[,y], unlist))
  tmpt$sd <- vector(mode = "list",length=nrow(tmpt))
  tmpt$sd[[1]] <- rowSds(sapply(dt[,y], unlist))
  tmpt$xlen <- vector(mode = "list",length=nrow(tmpt))
  tmpt$xlen[[1]] <- c(1:length(tmpt$mean[[1]]))
  return(tmpt)
}

plt.profiles <- function(dtclean,
                         TCKS,
                         metric,
                         ses,
                         ylabel,
                         tracts,
                         facet.formula = "Gender ~ fulltractlabel",
                         sdmultiplier = 3,
                         dt.mean.sd = NULL,
                         suffix = NULL,
                         line.width = 0.25){
  
  cols <- c("sub", "ses", "meas", "tract")
  A <- subset(dtclean,ses == ses & meas == metric)
  A <- A[A$fulltractlabel %in% TCKS, ]
  
  A <- as.data.frame(unnest(A, cols = c('val','xlen')))
  
  plt <- ggplot(A, aes(x=xlen,
                       y=val,
                       group=interaction(sub,AGE),
                       color=AGE)) + 
    geom_line(size = line.width,
              alpha = 0.5)
  
  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
    dt.mean.sd <- dt.mean.sd[dt.mean.sd$fulltractlabel %in% TCKS, ]
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(x = xlen,
                      # group = interaction(Gender,Hemisphere,fulltractlabel),
                      ymax = mean + sdmultiplier*sd,
                      ymin = mean - sdmultiplier*sd),
                  colour = 'blue',
                  fill = NA,
                  size = 1,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'red',
                size = 1.25,
                alpha = 1)
  }
  
  plt <- plt + facet_wrap(facet.formula, scales = "free_x")
  # plt <- plt + facet_wrap(~fulltractlabel, scales = "free_x")
  
  plt <- plt + theme_bw() + labs(x='Disk', y = ylabel) +
    scale_color_viridis(option="plasma") +
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 26),
          strip.text.x = element_text(size = 26),
          axis.text = element_text(size = 26 - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  # pdf(NULL)
  # plt
  # x <- dev.off()
  
  if (!is.null(suffix)){
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd_",
                      suffix,
                      ".png")
    
  }else {
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd",
                      ".png")
  }  
  
  
  
  if (grepl("FO",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height=12,# 4 each plot 
           width=20,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else if (grepl("MD",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height=24,# 4 each plot 
           width=32,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else{
    ggsave(file=file.path(figpath,figname),
           height=24,# 4 each plot 
           width=32,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } 
  # return(plt)
  return(A)
}


plt.profiles.V2 <- function(dtclean,
                         TCKS,
                         metric,
                         ses,
                         ylabel,
                         tracts,
                         sdmultiplier = 3,
                         dt.mean.sd = NULL,
                         dt.mean.sd2 = NULL,
                         suffix = NULL,
                         facet.formula = "fulltractlabel ~ Gender + Hemisphere",
                         text.size = 36,
                         line.width = 0.25,
                         ncol = 2,
                         linetype.legend.title = "Age group  ",
                         color.legend.title = "Age",
                         plt.sections = FALSE){
  
  cols <- c("sub", "ses", "meas", "tract")
  A <- subset(dtclean,ses == ses & meas == metric)
  A <- A[A$fulltractlabel %in% TCKS, ]
  
  A <- as.data.frame(unnest(A, cols = c('val','xlen')))
  
  plt <- ggplot(A, aes(x=xlen,
                       y=val,
                       group=interaction(sub,AGE),
                       color=AGE)) + 
    geom_line(size = line.width,
              alpha = 0.5)
  
  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
    dt.mean.sd <- dt.mean.sd[dt.mean.sd$fulltractlabel %in% TCKS, ]
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(x = xlen,
                      # group = interaction(Gender,Hemisphere,fulltractlabel),
                      ymax = mean + sdmultiplier*sd,
                      ymin = mean - sdmultiplier*sd),
                  colour = 'blue',
                  fill = NA,
                  size = 1,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'red',
                size = 1.25,
                alpha = 1)
  }
  
  if (!is.null(dt.mean.sd2)) {
    dt.mean.sd2 <- subset(dt.mean.sd2, meas == metric)
    dt.mean.sd2 <- dt.mean.sd2[dt.mean.sd2$fulltractlabel %in% TCKS, ]
    dt.mean.sd2 <- as.data.frame(unnest(dt.mean.sd2, cols = c('mean','sd','xlen')))
    
    plt <- plt +
      geom_line(data=dt.mean.sd2,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen,
                    linetype = agegroup),
                colour = 'black',
                size = 1.25,
                alpha = 1)
  }
  
  # plt <- plt + facet_wrap(Gender+Hemisphere~fulltractlabel, scales = "free_x")
  # plt <- plt + facet_wrap(~fulltractlabel, scales = "free_x")
  if (plt.sections){
    dat.vline <- expand.grid(group=unique(A[,"tractlabel"]),Hemisphere=unique(A[,"Hemisphere"]),xval=c(12,38,64,90))
    plt <- plt + geom_vline(aes(xintercept=xval), color = "black",linetype = 3, dat.vline)
  }
  
  plt <- plt + facet_grid2(facet.formula, # as.formula(paste(facet.y,"~ Gender + Hemisphere")),#fulltractlabel ~ Gender + Hemisphere,
    axes = "all",
    # independent = "y",
    # remove_labels = "x",
    remove_labels = "all",
    scales = "fixed"
  )
  first.break <- (text.size-10)*22/26 #22/26
  first.break.character <- as.character(first.break)
  last.break <- 100-(text.size-10)*14/26 #14/26  
  last.break.character <- as.character(last.break)
  plt <- plt + theme_bw() + labs(x='Section', y = ylabel) +
    scale_color_viridis(option="plasma") +
    # scale_x_continuous(limits = c(1, 100)) +
    # coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    # scale_x_continuous(breaks = c(1, seq(25, 100, by = 25)), lim = c(1,100), expand = FALSE)  +
    # scale_x_continuous(breaks = c(1,seq(25, 100, by = 25))) +
    
    # 26 --> 22  | (text.size-10)*22/26
    # 26 --> 14  | (text.size-10)*14/26
    # scale_x_continuous(breaks = c(22,86),
    #                    labels=c("22" = "Thalamus", "86" = "Cortex")) +
    scale_x_continuous(breaks = c(first.break,last.break),
                       labels = c(first.break.character = "Thalamus", 
                                  last.break.character = "Cortex")) +
    # scale_x_continuous(breaks = c(first.break, last.break),
    #                    labels = c(as.character(first.break) = "Thalamus", 
    #                               as.character(last.break) = "Cortex")) +
    scale_y_continuous(breaks = c(0,seq(0.2, 1, by = 0.2))) +
    expand_limits(y = c(0.1, 0.9)) +
    coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    labs(colour = color.legend.title) +
    guides(colour = guide_colorbar(barwidth = 2, barheight = 16, 
                                   ticks.colour = "black",
                                   ticks.linewidth = 2))+
    # override.aes = list(size = text.size-10))) +
    guides(linetype = guide_legend(keywidth = unit(5, 'line'),
                                   title = linetype.legend.title)) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = text.size),
          strip.text.x = element_text(size = text.size),
          strip.text.y = element_text(size = text.size, face = "bold"),
          axis.text.y = element_text(size = text.size - 4),
          axis.text.x = element_text(size = text.size - 10),
          axis.ticks.x = element_blank(),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(-2.5, "lines"),
          panel.spacing.y = unit(-1.5, "lines"),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  if (!is.null(suffix)){
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd_",
                      suffix,
                      ".png")
    
  }else {
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd",
                      ".png")
  }  
  
  
  
  if (grepl("FO",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A$fulltractlabel))*3,# 4 each plot 
           width = ncol*5+4,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else if (grepl("MD",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A$fulltractlabel))*3,# 3 each plot 
           width = ncol*5+4,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  }
  return(plt)
}

plt.profiles.V4 <- function(dtclean,
                         TCKS,
                         metric,
                         ses,
                         ylabel,
                         tracts,
                         sdmultiplier = 3,
                         dt.mean.sd = NULL,
                         dt.mean.sd2 = NULL,
                         suffix = NULL,
                         facet.formula = "fulltractlabel ~ Gender + Hemisphere",
                         text.size = 36,
                         line.width = 0.25,
                         ncol = 2,
                         group.legend.title = "Age group  ",
                         color.legend.title = "Age",
                         plt.sections = FALSE){
  
  cols <- c("sub", "ses", "meas", "tract")
  A <- subset(dtclean,ses == ses & meas == metric)
  A <- A[A$fulltractlabel %in% TCKS, ]
  
  A <- as.data.frame(unnest(A, cols = c('val','xlen')))
  
  plt <- ggplot(A, aes(
    x = xlen,
    y = val,
    group = interaction(sub, AGE),
    color = AGE
  )) +
    geom_line(
      size = line.width,
      alpha = 0.5
    ) +
    scale_color_viridis(option = "plasma") +
    # guides(colour = guide_colorbar(
    #   barwidth = 2, barheight = 16,
    #   ticks.colour = "black",
    #   ticks.linewidth = 2
    # ))

  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
    dt.mean.sd <- dt.mean.sd[dt.mean.sd$fulltractlabel %in% TCKS, ]
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(x = xlen,
                      # group = interaction(Gender,Hemisphere,fulltractlabel),
                      ymax = mean + sdmultiplier*sd,
                      ymin = mean - sdmultiplier*sd),
                  colour = 'blue',
                  fill = NA,
                  size = 1,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'red',
                size = 1.25,
                alpha = 1)
  }
  
  if (!is.null(dt.mean.sd2)) {
    dt.mean.sd2 <- subset(dt.mean.sd2, meas == metric)
    dt.mean.sd2 <- dt.mean.sd2[dt.mean.sd2$fulltractlabel %in% TCKS, ]
    dt.mean.sd2 <- as.data.frame(unnest(dt.mean.sd2, cols = c('mean','sd','xlen')))
    
    # plt <- plt +
    #   geom_line(data=dt.mean.sd2,
    #             inherit.aes = FALSE,
    #             aes(y = mean,
    #                 x = xlen,
    #                 linetype = agegroup),
    #             colour = 'black',
    #             size = 1.25,
    #             alpha = 1)
    plt <- plt + new_scale_colour() +
      geom_line(data=dt.mean.sd2,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen,
                    colour = agegroup),
                size = 1.25,
                alpha = 1) +
                guides(colour = guide_legend(keywidth = unit(5, 'line'),
                                   title = group.legend.title))

  }
  
  # plt <- plt + facet_wrap(Gender+Hemisphere~fulltractlabel, scales = "free_x")
  # plt <- plt + facet_wrap(~fulltractlabel, scales = "free_x")
  if (plt.sections){
    dat.vline <- expand.grid(group=unique(A[,"tractlabel"]),Hemisphere=unique(A[,"Hemisphere"]),xval=c(12,38,64,90))
    plt <- plt + geom_vline(aes(xintercept=xval), color = "black",linetype = 3, dat.vline)
  }
  
  plt <- plt + facet_grid2(facet.formula, # as.formula(paste(facet.y,"~ Gender + Hemisphere")),#fulltractlabel ~ Gender + Hemisphere,
    axes = "all",
    # independent = "y",
    # remove_labels = "x",
    remove_labels = "all",
    scales = "fixed"
  )
  first.break <- (text.size-10)*22/26 #22/26
  first.break.character <- as.character(first.break)
  last.break <- 100-(text.size-10)*14/26 #14/26  
  last.break.character <- as.character(last.break)
  plt <- plt + theme_bw() + labs(x='Section', y = ylabel) +
    # scale_x_continuous(limits = c(1, 100)) +
    # coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    # scale_x_continuous(breaks = c(1, seq(25, 100, by = 25)), lim = c(1,100), expand = FALSE)  +
    # scale_x_continuous(breaks = c(1,seq(25, 100, by = 25))) +
    
    # 26 --> 22  | (text.size-10)*22/26
    # 26 --> 14  | (text.size-10)*14/26
    # scale_x_continuous(breaks = c(22,86),
    #                    labels=c("22" = "Thalamus", "86" = "Cortex")) +
    scale_x_continuous(breaks = c(first.break,last.break),
                       labels = c(first.break.character = "Thalamus", 
                                  last.break.character = "Cortex")) +
    # scale_x_continuous(breaks = c(first.break, last.break),
    #                    labels = c(as.character(first.break) = "Thalamus", 
    #                               as.character(last.break) = "Cortex")) +
    scale_y_continuous(breaks = c(0,seq(0.2, 1, by = 0.2))) +
    expand_limits(y = c(0.1, 0.9)) +
    coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    labs(colour = color.legend.title) +
    
    # override.aes = list(size = text.size-10))) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = text.size),
          strip.text.x = element_text(size = text.size),
          strip.text.y = element_text(size = text.size, face = "bold"),
          axis.text.y = element_text(size = text.size - 4),
          axis.text.x = element_text(size = text.size - 10),
          axis.ticks.x = element_blank(),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(-2.5, "lines"),
          panel.spacing.y = unit(-1.5, "lines"),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  if (!is.null(suffix)){
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd_",
                      suffix,
                      ".png")
    
  }else {
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd",
                      ".png")
  }  
  
  
  
  if (grepl("FO",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A$fulltractlabel))*3,# 4 each plot 
           width = ncol*5+4,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else if (grepl("MD",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A$fulltractlabel))*3,# 3 each plot 
           width = ncol*5+4,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  }
  return(plt)
}

plt.profiles.V5 <- function(dtclean,
                            group,
                            groups,
                            metric,
                            ses,
                            ylabel,
                            y = "val",
                            tracts,
                            sdmultiplier = 3,
                            dt.mean.sd = NULL,
                            dt.mean.sd2 = NULL,
                            suffix = NULL,
                            facet.formula = "fulltractlabel ~ Gender + Hemisphere",
                            text.size = 36,
                            line.width = 0.25,
                            ncol = 2,
                            group.legend.title = "Age group  ",
                            color.legend.title = "Age",
                            plt.sections = FALSE){
    
  cols <- c("sub", "ses", "meas", "tract")
  A <- subset(dtclean,ses == ses & meas == metric)
  A <- as.data.frame(A)
  A <- A[A[,group] %in% groups,]
  
  A <- as.data.frame(unnest(A, cols = c(y,'xlen')))
  plt <- ggplot(A, aes(
    x = xlen,
    y = .data[[y]],
    group = interaction(sub, AGE),
    color = AGE
  )) +
    geom_line(
      size = line.width,
      alpha = 0.5
    ) +
    scale_color_viridis(option = "plasma") +
    guides(colour = guide_colorbar(
      barwidth = 2, barheight = 16,
      ticks.colour = "black",
      ticks.linewidth = 2
    ))

  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
    dt.mean.sd <- as.data.frame(dt.mean.sd)
    dt.mean.sd <- dt.mean.sd[dt.mean.sd[,group] %in% groups, ]
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(x = xlen,
                      # group = interaction(Gender,Hemisphere,fulltractlabel),
                      ymax = mean + sdmultiplier*sd,
                      ymin = mean - sdmultiplier*sd),
                  colour = 'blue',
                  fill = NA,
                  size = 1,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'red',
                size = 1.25,
                alpha = 1)
  }
  
  if (!is.null(dt.mean.sd2)) {
    dt.mean.sd2 <- subset(dt.mean.sd2, meas == metric)
    # dt.mean.sd2 <- dt.mean.sd2[dt.mean.sd2$fulltractlabel %in% TCKS, ]
    dt.mean.sd2 <- as.data.frame(dt.mean.sd2)
    dt.mean.sd2 <- dt.mean.sd2[dt.mean.sd2[,group] %in% groups, ] 
    dt.mean.sd2 <- as.data.frame(unnest(dt.mean.sd2, cols = c('mean','sd','xlen')))
    # plt <- plt +
    #   geom_line(data=dt.mean.sd2,
    #             inherit.aes = FALSE,
    #             aes(y = mean,
    #                 x = xlen,
    #                 linetype = agegroup),
    #             colour = 'black',
    #             size = 1.25,
    #             alpha = 1)
    mypal <- rev(wes_palette("Rushmore1", n = 5))[2:3]
    mypal <- c(wes_palette("Darjeeling1", n = 1),mypal)
    # mypal <- c(wes_palette("FantasticFox1", n = 3)[3],mypal)
    # mypal <- c(rev(wes_palette("Darjeeling1", n = 5))[1],mypal)
    plt <- plt +# new_scale_colour() +
      geom_point(data=dt.mean.sd2,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen,
                    fill = agegroup),
                    shape = 22,
                    colour = "black",
                    stroke = NA,
                size = 2,
                alpha = 1) + 
                scale_fill_manual(values = mypal) +
               # scale_fill_manual(values = rev(wes_palette("Rushmore1", n = 5))) +
                guides(fill = guide_legend(title = group.legend.title, 
                override.aes = list(size = 10)))# +
                # guides(colour = guide_legend(keywidth = unit(5, 'line'),
                #                    title = group.legend.title))

  }
  
  # plt <- plt + facet_wrap(Gender+Hemisphere~fulltractlabel, scales = "free_x")
  # plt <- plt + facet_wrap(~fulltractlabel, scales = "free_x")
  if (plt.sections){
    dat.vline <- expand.grid(group=unique(A[,"tractlabel"]),Hemisphere=unique(A[,"Hemisphere"]),xval=c(12,38,64,90))
    plt <- plt + geom_vline(aes(xintercept=xval), color = "black",linetype = 3, dat.vline)
  }
  
  plt <- plt + facet_grid2(facet.formula, # as.formula(paste(facet.y,"~ Gender + Hemisphere")),#fulltractlabel ~ Gender + Hemisphere,
    axes = "all",
    # independent = "y",
    # remove_labels = "x",
    remove_labels = "all",
    scales = "fixed"
  )
  first.break <- (text.size-10)*22/26 #22/26
  first.break.character <- as.character(first.break)
  last.break <- 100-(text.size-10)*14/26 #14/26  
  last.break.character <- as.character(last.break)
  plt <- plt + theme_bw() + labs(x='Section', y = ylabel) +
    # scale_x_continuous(limits = c(1, 100)) +
    # coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    # scale_x_continuous(breaks = c(1, seq(25, 100, by = 25)), lim = c(1,100), expand = FALSE)  +
    # scale_x_continuous(breaks = c(1,seq(25, 100, by = 25))) +
    
    # 26 --> 22  | (text.size-10)*22/26
    # 26 --> 14  | (text.size-10)*14/26
    # scale_x_continuous(breaks = c(22,86),
    #                    labels=c("22" = "Thalamus", "86" = "Cortex")) +
    scale_x_continuous(breaks = c(first.break,last.break),
                       labels = c(first.break.character = "Thalamus", 
                                  last.break.character = "Cortex")) +
    # scale_x_continuous(breaks = c(first.break, last.break),
    #                    labels = c(as.character(first.break) = "Thalamus", 
    #                               as.character(last.break) = "Cortex")) +

    scale_y_continuous(breaks = c(0,seq(0.2, 1, by = 0.2))) +
    expand_limits(y = c(0.1, 0.9)) +
    
    coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    labs(colour = color.legend.title) +
    
    # override.aes = list(size = text.size-10))) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = text.size),
          strip.text.x = element_text(size = text.size),
          strip.text.y = element_text(size = text.size, face = "bold"),
          axis.text.y = element_text(size = text.size - 4),
          axis.text.x = element_text(size = text.size - 10),
          axis.ticks.x = element_blank(),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(-2.5, "lines"),
          panel.spacing.y = unit(-1.5, "lines"),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  if (!is.null(suffix)){
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd_",
                      suffix,
                      ".png")
    
  }else {
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd",
                      ".png")
  }  
  
  
  
  if (grepl("FO",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A[,group]))*3,# 4 each plot 
           width = ncol*5+4,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else if (grepl("MD",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A[,group]))*3,# 3 each plot 
           width = ncol*5+4,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  }
  return(plt)
}

plt.profiles.V6 <- function(
    dtclean,
    group,
    groups,
    metric,
    ses,
    ylabel,
    y = "val",
    tracts,
    sdmultiplier = 3,
    dt.mean.sd = NULL,
    dt.mean.sd2 = NULL,
    suffix = NULL,
    facet.formula = "fulltractlabel ~ Gender + Hemisphere",
    text.size = 36,
    line.width = 0.25,
    ncol = 2,
    group.legend.title = "Age group  ",
    color.legend.title = "Age",
    plt.sections = FALSE
    ){
    
  cols <- c("sub", "ses", "meas", "tract")
  A <- subset(dtclean,ses == ses & meas == metric)
  A <- as.data.frame(A)
  A <- A[A[,group] %in% groups,]
  
  A <- as.data.frame(unnest(A, cols = c(y,'xlen')))
  plt <- ggplot(A, aes(
    x = xlen,
    y = .data[[y]],
    group = interaction(sub, AGE),
    color = AGE
  )) +
    geom_line(
      size = line.width,
      alpha = 0.5
    ) +
    scale_color_viridis(option = "plasma",
    guide = guide_colorbar(
      barwidth = 2, barheight = 16,
      ticks.colour = "black",
      ticks.linewidth = 2,
      title = color.legend.title
    ))# +
    # guides(colour = guide_colorbar(
    #   barwidth = 2, barheight = 16,
    #   ticks.colour = "black",
    #   ticks.linewidth = 2
    # ))

  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
    dt.mean.sd <- as.data.frame(dt.mean.sd)
    dt.mean.sd <- dt.mean.sd[dt.mean.sd[,group] %in% groups, ]
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(x = xlen,
                      # group = interaction(Gender,Hemisphere,fulltractlabel),
                      ymax = mean + sdmultiplier*sd,
                      ymin = mean - sdmultiplier*sd),
                  colour = 'blue',
                  fill = NA,
                  size = 1,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'red',
                size = 1.25,
                alpha = 1)
  }
  
  if (!is.null(dt.mean.sd2)) {
    dt.mean.sd2 <- subset(dt.mean.sd2, meas == metric)
    # dt.mean.sd2 <- dt.mean.sd2[dt.mean.sd2$fulltractlabel %in% TCKS, ]
    dt.mean.sd2 <- as.data.frame(dt.mean.sd2)
    dt.mean.sd2 <- dt.mean.sd2[dt.mean.sd2[,group] %in% groups, ] 
    dt.mean.sd2 <- as.data.frame(unnest(dt.mean.sd2, cols = c('mean','sd','xlen')))
    # plt <- plt +
    #   geom_line(data=dt.mean.sd2,
    #             inherit.aes = FALSE,
    #             aes(y = mean,
    #                 x = xlen,
    #                 linetype = agegroup),
    #             colour = 'black',
    #             size = 1.25,
    #             alpha = 1)
    mypal <- rev(wes_palette("Rushmore1", n = 5))[2:5] #3
    mypal <- c(wes_palette("Darjeeling1", n = 1),mypal)
    # mypal <- c(wes_palette("FantasticFox1", n = 3)[3],mypal)
    # mypal <- c(rev(wes_palette("Darjeeling1", n = 5))[1],mypal)
    plt <- plt + new_scale_colour() +
      geom_line(data=dt.mean.sd2,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen,
                    colour = agegroup),
                size = 2,
                alpha = 1) + 
                scale_color_manual(values = mypal,
                guide = guide_legend(keywidth = unit(5, 'line'),
                       title = group.legend.title))
                # scale_color_discrete(guide = guide_legend(keywidth = unit(5, 'line'),
                #        title = group.legend.title))
               # scale_fill_manual(values = rev(wes_palette("Rushmore1", n = 5))) +
                # guides(colour = guide_legend(keywidth = unit(5, 'line'),
                #        title = group.legend.title))

  }
  
  # plt <- plt + facet_wrap(Gender+Hemisphere~fulltractlabel, scales = "free_x")
  # plt <- plt + facet_wrap(~fulltractlabel, scales = "free_x")
  if (plt.sections){
    dat.vline <- expand.grid(group=unique(A[,group]),Hemisphere=unique(A[,"Hemisphere"]),xval=c(12,38,64,90))
    plt <- plt + geom_vline(aes(xintercept=xval), color = "black",linetype = 3, dat.vline)
  }
  
  plt <- plt + facet_grid2(facet.formula, # as.formula(paste(facet.y,"~ Gender + Hemisphere")),#fulltractlabel ~ Gender + Hemisphere,
    axes = "all",
    # independent = "y",
    # remove_labels = "x",
    remove_labels = "all",
    scales = "fixed"
  )
  
  first.break <- (text.size-10)*22/26 #22/26
  first.break.character <- as.character(first.break)
  last.break <- 100-(text.size-10)*14/26 #14/26  
  last.break.character <- as.character(last.break)
  
  plt <- plt + theme_bw() + labs(x='Section', y = ylabel) +
    # scale_x_continuous(limits = c(1, 100)) +
    # coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    # scale_x_continuous(breaks = c(1, seq(25, 100, by = 25)), lim = c(1,100), expand = FALSE)  +
    # scale_x_continuous(breaks = c(1,seq(25, 100, by = 25))) +
    
    # 26 --> 22  | (text.size-10)*22/26
    # 26 --> 14  | (text.size-10)*14/26
    # scale_x_continuous(breaks = c(22,86),
    #                    labels=c("22" = "Thalamus", "86" = "Cortex")) +
    scale_x_continuous(breaks = c(first.break,last.break),
                       labels = c(first.break.character = "Thalamus", 
                                  last.break.character = "Cortex")) +
    # scale_x_continuous(breaks = c(first.break, last.break),
    #                    labels = c(as.character(first.break) = "Thalamus", 
    #                               as.character(last.break) = "Cortex")) +
    scale_y_continuous(breaks = c(0,seq(0.2, 1, by = 0.2))) +
    expand_limits(y = c(0.1, 0.9)) +
    coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    labs(colour = color.legend.title) +
    # override.aes = list(size = text.size-10))) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = text.size),
          strip.text.x = element_text(size = text.size),
          strip.text.y = element_text(size = text.size, face = "bold"),
          axis.text.y = element_text(size = (text.size - 4)),
          axis.text.x = element_text(size = (text.size - 10)),
          # axis.ticks.x = element_blank(),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(-2.5, "lines"),
          panel.spacing.y = unit(-1.5, "lines"),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  if (!is.null(suffix)){
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd_",
                      suffix,
                      ".png")
    
  }else {
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd",
                      ".png")
  }  
  ggsave(file=file.path(figpath,figname),
         height = length(unique(A[,group]))*3,# 4 each plot 
         width = ncol*5+4,# 5 each plot + 4 legend
         bg = "white",
         units = "in",
         dpi = 200,
         device = "png",
         plt)
  
  # if (grepl("FO",tracts,fixed=true)) {
  #   ggsave(file=file.path(figpath,figname),
  #          height = length(unique(A[,group]))*3,# 4 each plot 
  #          width = ncol*5+4,# 5 each plot + 4 legend
  #          bg = "white",
  #          units = "in",
  #          dpi = 200,
  #          device = "png",
  #          plt)
  # } else if (grepl("MD",tracts,fixed=true)) {
  #   ggsave(file=file.path(figpath,figname),
  #          height = length(unique(A[,group]))*3,# 3 each plot 
  #          width = ncol*5+4,# 5 each plot + 4 legend
  #          bg = "white",
  #          units = "in",
  #          dpi = 200,
  #          device = "png",
  #          plt)
  # } else{
  #   # ggsave(file=file.path(figpath,figname),
  #   #        height=24,# 4 each plot 
  #   #        width=32,# 4 each plot + 4 legend
  #   #        bg = "white",
  #   #        units = "in",
  #   #        dpi = 200,
  #   #        device = "png",
  #   #        plt)
  #   ggsave(file=file.path(figpath,figname),
  #          height = length(unique(A[,group]))*3,# 3 each plot 
  #          width = ncol*5+4,# 5 each plot + 4 legend
  #          bg = "white",
  #          units = "in",
  #          dpi = 200,
  #          device = "png",
  #          plt)
  # } 
  return(plt)
}

plt.profiles.V3 <- function(dtclean,
                            group,
                            groups,
                            metric,
                            ses,
                            y = "val",
                            ylabel,
                            tracts,
                            sdmultiplier = 3,
                            dt.mean.sd = NULL,
                            dt.mean.sd2 = NULL,
                            suffix = NULL,
                            facet.formula = "fulltractlabel ~ Gender + Hemisphere",
                            text.size = 36,
                            line.width = 0.25,
                            linetype.legend.title = "Age group  ",
                            color.legend.title = "Age",
                            ncol = 2,
                            plt.sections = FALSE){
  
  A <- subset(dtclean, ses == ses & meas == metric)
  A <- A[A[,group] %in% groups,]
  # A %>%  mutate(group = factor(group, levels = groups))
  A <- as.data.frame(unnest(A, cols = c(y,'xlen')))
  
  plt <- ggplot(A, aes(x=xlen,
                              y=avgvector,
                              group=interaction(sub,AGE),
                              color=AGE)) + 
    geom_line(size = line.width,
              alpha = 0.5)
  
  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
    dt.mean.sd <- dt.mean.sd[dt.mean.sd[,group] %in% groups,]
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(x = xlen,
                      # group = interaction(Gender,Hemisphere,fulltractlabel),
                      ymax = mean + sdmultiplier*sd,
                      ymin = mean - sdmultiplier*sd),
                  colour = 'blue',
                  fill = NA,
                  size = 1,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'red',
                size = 1.25,
                alpha = 1)
  }
  
  if (!is.null(dt.mean.sd2)) {
    dt.mean.sd2 <- subset(dt.mean.sd2, meas == metric)
    dt.mean.sd2 <- dt.mean.sd2[dt.mean.sd2[,group] %in% groups,]
    dt.mean.sd2 <- as.data.frame(unnest(dt.mean.sd2, cols = c('mean','sd','xlen')))
    
    plt <- plt +
      geom_line(data=dt.mean.sd2,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen,
                    linetype = agegroup),
                colour = 'black',
                size = 1.25,
                alpha = 1)
  }
  
  # plt <- plt + facet_wrap(Gender+Hemisphere~fulltractlabel, scales = "free_x")
  # plt <- plt + facet_wrap(~fulltractlabel, scales = "free_x")
  
  if (plt.sections){
    dat.vline <- expand.grid(group=unique(A[,group]),Hemisphere=unique(A[,"Hemisphere"]),xval=c(12,38,64,90))
    plt <- plt + geom_vline(aes(xintercept=xval), color = "black",linetype = 3, dat.vline)
  }
  
  
  plt <- plt + facet_grid2(facet.formula,#fulltractlabel ~ Gender + Hemisphere,
                           axes = "all", 
                           # independent = "y",
                           # remove_labels = "x",
                           remove_labels = "all",
                           scales = "fixed")
  
  # (text.size-10)
  first.break <- (text.size-10)*22/26 #22/26
  first.break.character <- as.character(first.break)
  last.break <- 100-(text.size-10)*14/26 #14/26  
  last.break.character <- as.character(last.break)
  plt <- plt + theme_bw() + labs(x='Section', y = ylabel) +
    scale_color_viridis(option="plasma") +
    # scale_x_continuous(limits = c(1, 100)) +
    # coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    # scale_x_continuous(breaks = c(1, seq(25, 100, by = 25)), lim = c(1,100), expand = FALSE)  +
    # scale_x_continuous(breaks = c(1,seq(25, 100, by = 25))) +
    
    # 26 --> 22  | (text.size-10)*22/26
    # 26 --> 14  | (text.size-10)*14/26
    # scale_x_continuous(breaks = c(22,86),
    #                    labels=c("22" = "Thalamus", "86" = "Cortex")) +
    scale_x_continuous(breaks = c(first.break,last.break),
                       labels = c(first.break.character = "Thalamus", 
                                  last.break.character = "Cortex")) +
    # scale_x_continuous(breaks = c(first.break, last.break),
    #                    labels = c(as.character(first.break) = "Thalamus", 
    #                               as.character(last.break) = "Cortex")) +
    scale_y_continuous(breaks = c(0,seq(0.2, 1, by = 0.2))) +
    expand_limits(y = c(0.1, 0.9)) +
    coord_cartesian(expand = FALSE, xlim = c(1, NA)) +
    labs(colour = color.legend.title) +
    guides(colour = guide_colorbar(barwidth = 2, barheight = 16, 
                                   ticks.colour = "black",
                                   ticks.linewidth = 2))+
    # override.aes = list(size = text.size-10))) +
    guides(linetype = guide_legend(keywidth = unit(5, 'line'),
                                   title = linetype.legend.title)) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = text.size),
          strip.text.x = element_text(size = text.size),
          strip.text.y = element_text(size = text.size, face = "bold"),
          axis.text.y = element_text(size = text.size - 4),
          axis.text.x = element_text(size = text.size - 10),
          axis.ticks.x = element_blank(),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(-2.5, "lines"),
          panel.spacing.y = unit(-1.5, "lines"),
         # panel.spacing.x = unit(1, "lines"),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  if (!is.null(suffix)){
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      # as.character(sdmultiplier),
                      # "sd_",
                      suffix,
                      ".png")
    
  }else {
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      # "_",
                      # as.character(sdmultiplier),
                      # "sd",
                      ".png")
  }  
  
  
  
  if (grepl("FO",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A[,group]))*3,# 4 each plot 
           width = ncol*5+4,#24,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else if (grepl("MD",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height = length(unique(A[,group]))*3,# 3 each plot 
           width = ncol*5+4,#24,# 5 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  }
  return(plt)
}

plt.profiles.outliers <- function(dtclean,
                                  TCKS,
                                  metric,
                                  ses,
                                  ylabel,
                                  tracts,
                                  sdmultiplier = 3,
                                  dt.mean.sd = NULL,
                                  suffix = NULL,
                                  line.width = 0.25,
                                  xwidth = NULL,
                                  yheight = NULL){
  
  cols <- c("sub", "ses", "meas", "tract")
  A <- subset(dtclean,ses == ses & meas == metric)
  A <- A[A$fulltractlabel %in% TCKS, ]
  
  A <- as.data.frame(unnest(A, cols = c('val','xlen')))
  
  plt <- ggplot(A, aes(x = xlen,
                       y = val,
                       # group = interaction(sub,AGE),
                       color = sub)) + 
    geom_line(size = line.width,
              alpha = 0.5)
  
  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
    dt.mean.sd <- dt.mean.sd[dt.mean.sd$fulltractlabel %in% TCKS, ]
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(x = xlen,
                      # group = interaction(Gender,Hemisphere,fulltractlabel),
                      ymax = mean + sdmultiplier*sd,
                      ymin = mean - sdmultiplier*sd),
                  colour = 'blue',
                  fill = NA,
                  size = 1,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'red',
                size = 1.25,
                alpha = 1)
  }
  
  plt <- plt + facet_wrap(Gender~fulltractlabel, scales = "free_x")
  
  plt <- plt + theme_bw() + labs(x='Disk', y = ylabel) +
    # scale_color_viridis(option="plasma") +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 26),
          strip.text.x = element_text(size = 26),
          axis.text = element_text(size = 26 - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  pdf(NULL)
  plt
  x <- dev.off()
  
  if (is.null(xwidth)) {
    if (grepl("FO",tracts,fixed=true)) {
      xwidth <- 20
    } else if (grepl("MD",tracts,fixed=true)) {
      xwidth <- 32
    }
  }
  
  if (is.null(yheight)) {
    if (grepl("FO",tracts,fixed=true)) {
      yheight <- 12
    } else if (grepl("MD",tracts,fixed=true)) {
      yheight <- 24
    }
  }
  
  if (!is.null(suffix)){
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd_",
                      suffix,
                      ".png")
    
  }else {
    figname <- paste0("Fig_",
                      tracts,
                      "_",
                      metric,
                      "_",
                      as.character(sdmultiplier),
                      "sd",
                      ".png")
  }  
  
  
  
  if (grepl("FO",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height=yheight,# 4 each plot 
           width=xwidth,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else if (grepl("MD",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,figname),
           height=yheight,# 4 each plot 
           width=xwidth,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  }
  return(plt)
}

plt.profiles.orig <- function(dtclean,TCKS,metric,ses,ylabel,tracts,dt.mean.sd=NULL){
  cols <- c("sub", "ses", "meas", "tract")
  A <- subset(dtclean,ses == ses & meas == metric)
  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- subset(dt.mean.sd, meas == metric)
  }
  if (grepl("MD",tracts,fixed=true)) {
    A <- A[A$fulltractlabel %in% TCKS, ]
    dt.mean.sd <- dt.mean.sd[dt.mean.sd$fulltractlabel %in% TCKS, ]
  } else {
    A <- A[A$tract %in% TCKS, ]
  }
  A <- as.data.frame(unnest(A, cols = c('val','xlen')))
  
  plt <- ggplot(A, aes(x=xlen,
                       y=val,
                       group=interaction(sub,AGE),
                       color=AGE)) + 
    geom_line(size=0.25,alpha=0.5)
  if (!is.null(dt.mean.sd)){
    dt.mean.sd <- as.data.frame(unnest(dt.mean.sd, cols = c('mean','sd','xlen')))
    print(dt.mean.sd)
    plt <- plt +
      geom_ribbon(data=dt.mean.sd,
                  inherit.aes = FALSE,
                  aes(y = mean,
                      x = xlen,
                      ymax = mean + 3*sd,
                      ymin = mean - 3*sd),
                  colour = 'blue',
                  fill = 'red',
                  size = 1.2,
                  alpha = 0.25,
                  linetype = 2) +
      geom_line(data=dt.mean.sd,
                inherit.aes = FALSE,
                aes(y = mean,
                    x = xlen),
                colour = 'green',
                size = 1,
                alpha = 0.75)
  }
  
  if (tracts == "FO") {
    plt <- plt + facet_wrap(Gender~tract,scales = "free_x")
  } else if (grepl("MD",tracts,fixed=true)) {
    plt <- plt + facet_wrap(Gender~fulltractlabel, scales = "free_x")
  }
  
  plt <- plt + theme_bw() + labs(x='Disk', y = ylabel) +
    scale_color_viridis(option="plasma") +
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
    # scale_color_continuous(guide = guide_colourbar()) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 26),
          strip.text.x = element_text(size = 26),
          axis.text = element_text(size = 26 - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  pdf(NULL)
  plt
  x <- dev.off()
  
  if (tracts == "FO") {
    ggsave(file=file.path(figpath,paste0("Fig_",tracts,"_",metric,"test.png")),
           height=12,# 4 each plot 
           width=20,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  } else if (grepl("MD",tracts,fixed=true)) {
    ggsave(file=file.path(figpath,paste0("Fig_",tracts,"_",metric,"test.png")),
           height=24,# 4 each plot 
           width=32,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 200,
           device = "png",
           plt)
  }
  return(plt)
}


large_points <- function(data, params, size) {
  # Multiply by some number
  data$size <- data$size * 4
  draw_key_point(data = data, params = params, size = size)
}

plt.devtraj <- function(test,ses,metric,TCKS,y,line.width,model,figdir,figname) {
  A <- subset(test,ses == ses & meas == metric & tractlabel %in% TCKS)
  plt <- ggplot(A, aes_string(x="AGE",
                              y=y,
                              color="hemgen",
                              shape="hemgen",
                              linetype="hemgen")) + 
    geom_point(alpha=0.5,key_glyph = large_points) + 
    # females-left
    stat_smooth(data=subset(A,Hemisphere == "Left" & Gender=="Female"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    # females-right
    stat_smooth(data=subset(A,Hemisphere == "Right" & Gender=="Female"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    # males-left
    stat_smooth(data=subset(A,Hemisphere == "Left" & Gender=="Male"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    # males-right
    stat_smooth(data=subset(A,Hemisphere == "Right" & Gender=="Male"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
    scale_linetype_manual("",values=c(1,2,1,2)) +
    scale_shape_manual("",values=c(16,17,16,17)) +
    guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
           shape=guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    facet_wrap(.~tractlabel,scales = "free") + theme_bw() +
    labs(x='Age', y=metric) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 26),
          strip.text.x = element_text(size = 26),
          axis.text = element_text(size = 26 - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  # legend.position = "none")
  ggsave(file=file.path(figdir,figname),
         width=36,
         height=25,
         units = "in",
         dpi = 100,
         plt)
}

plt.devtraj2 <- function(test,TCKS,y,line.width,model,figname) {
  A <- subset(test, tractlabel %in% TCKS)
  plt <- ggplot(A, aes_string(x="AGE",
                              y=y,
                              color="hemgen",
                              shape="hemgen",
                              linetype="hemgen")) + 
    geom_point(alpha=0.5,key_glyph = large_points) + 
    # females-left
    stat_smooth(data=subset(A,Hemisphere == "Left" & Gender=="Female"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    # females-right
    stat_smooth(data=subset(A,Hemisphere == "Right" & Gender=="Female"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    # males-left
    stat_smooth(data=subset(A,Hemisphere == "Left" & Gender=="Male"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    # males-right
    stat_smooth(data=subset(A,Hemisphere == "Right" & Gender=="Male"),
                method = "lm", formula = model, 
                alpha=0, size = line.width) +
    scale_colour_manual("",values=c("#E41A1C","#377EB8")) +
    scale_linetype_manual("",values=c(1,2,1,2)) +
    scale_shape_manual("",values=c(16,17,16,17)) +
    guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
           shape=guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    facet_wrap(.~tractlabel,scales = "free") + theme_bw() +
    labs(x='Age', y=y) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 26),
          strip.text.x = element_text(size = 26),
          axis.text = element_text(size = 26 - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  # legend.position = "none")
  ggsave(file=file.path(getwd(),figname),
         width=36,
         height=25,
         units = "in",
         dpi = 100,
         plt)
}

readData <- function(basedir,project,rtpversion,analysis) {
  projPath <- file.path(basedir,
                        project,
                        "nifti/derivatives",
                        paste("rtp-pipeline",rtpversion,sep="_"),
                        paste("analysis",analysis,sep="-"))
  subSesList <- file.path(projPath, "subSesList.txt")
  ssl <- read.table(file = subSesList, 
                    header = TRUE, 
                    sep = ",", 
                    stringsAsFactors = TRUE,
                    colClasses = c('character',
                                   'character',
                                   'logical',
                                   'logical',
                                   'logical',
                                   'logical'))
  subs <- unique(ssl$sub)
  ssl <- ssl[ssl$dwi == TRUE, ]
  rows <- unique(ssl[, c("sub", "ses")])
  
  bigCSV <- file.path(projPath, "DATA", "RTP_*.csv")
  
  measures <- c("fa", "md", "rd", "ad", "volume")
  cols <- c("sub", "ses", "meas", "tract")
  SLF <- data.frame()
  
  tractparams <- read.csv(file.path(projPath,'tractparams.csv'))
  # numtracts <- 88  # This are the SLF ones
  
  # numtracts <- nrow(unique(tractparams['label']))
  numtracts <- 26
  print(numtracts)
  
  for (nr in 1:nrow(rows)) {
    sub <- as.character(rows$sub[nr])
    ses <- as.character(rows$ses[nr])
    
    tmpT <- data.frame()
    for (m in measures) {
      # Create the table
      howManyRows <- numtracts
      tmpt <- as.data.frame(matrix(NA, nrow = howManyRows, ncol = length(cols)))
      tmpt$val <- vector(mode = "list",length=nrow(tmpt))
      tmpt$xlen <- vector(mode = "list",length=nrow(tmpt))
      
      # Change col names
      colnames(tmpt) <- c(cols,"val","xlen")
      
      # Create the rownames
      tmpt$project <- rep(project, each = howManyRows)
      tmpt$sub <- rep(sub, each = howManyRows)
      tmpt$ses <- rep(ses, each = howManyRows)
      tmpt$meas <- rep(m, each = howManyRows)
      
      # Read the csv
      csvpath <- file.path(projPath, paste0("sub-", sub), paste0("ses-", ses), "output")
      tmp <- readr::read_csv(file.path(csvpath, paste0("RTP_", m, ".csv")))
      
      # tmpt$vecs[[1]] <- c(1,2,3,100,500,22)
      
      # Add tract names
      tmpt$tract <- as.character(names(tmp))
      
      # Add values
      tractvalues <- t(as.matrix(tmp))
      for (nt in 1:numtracts){
        tmpt$val[[nt]] <- tractvalues[nt,]
        tmpt$xlen[[nt]] <- as.double(1:length(tractvalues[nt,]))
      }
      
      #tmpt$val <- as.list(t(as.matrix(tmp)))
      
      # Concatenate
      tmpT <- bind_rows(tmpT, tmpt)
    }
    SLF <- bind_rows(SLF, tmpT)
  }
  return(SLF)
}

readData.V2 <- function(basedir,project,container,version,analysis) {
  projPath <- file.path(basedir,
                        project,
                        "nifti/derivatives",
                        paste(container,version,sep="_"),
                        paste("analysis",analysis,sep="-"))
  if (!dir.exists(projPath)) {
    stop(sprintf("Path %s doesn't exist", projPath))
  }
  subSesList <- file.path(projPath, "subSesList.txt")
  ssl <- read.table(file = subSesList, 
                    header = TRUE, 
                    sep = ",", 
                    stringsAsFactors = TRUE,
                    colClasses = c('character',
                                   'character',
                                   'logical',
                                   'logical',
                                   'logical',
                                   'logical'))
  subs <- unique(ssl$sub)
  ssl <- ssl[ssl$dwi == TRUE, ]
  rows <- unique(ssl[, c("sub", "ses")])
  
  bigCSV <- file.path(projPath, "DATA", "RTP_*.csv")
  
  measures <- c("fa", "md", "rd", "ad", "volume")
  cols <- c("sub", "ses", "meas", "tract")
  SLF <- data.frame()
  
  tractparams <- read.csv(file.path(projPath,'tractparams.csv'))
  # numtracts <- 88  # This are the SLF ones
  
  numtracts <- nrow(unique(tractparams['label']))
  # numtracts <- 26
  print(numtracts)
  
  for (nr in 1:nrow(rows)) {
    sub <- as.character(rows$sub[nr])
    ses <- as.character(rows$ses[nr])
    tmpT <- data.frame()
    for (m in measures) {
      # Create the table
      howManyRows <- numtracts
      tmpt <- as.data.frame(matrix(NA, nrow = howManyRows, ncol = length(cols)))
      tmpt$val <- vector(mode = "list",length=nrow(tmpt))
      tmpt$xlen <- vector(mode = "list",length=nrow(tmpt))
      
      # Change col names
      colnames(tmpt) <- c(cols,"val","xlen")
      
      # Create the rownames
      tmpt$project <- rep(project, each = howManyRows)
      tmpt$sub <- rep(sub, each = howManyRows)
      tmpt$ses <- rep(ses, each = howManyRows)
      tmpt$meas <- rep(m, each = howManyRows)
      
      # Read the csv
      csvpath <- file.path(projPath, paste0("sub-", sub), paste0("ses-", ses), "output")
      writeLines(sprintf("Reading sub-%s_ses-%s , measure:%s",sub,ses,m))
      tmp <- readr::read_csv(file.path(csvpath, paste0("RTP_", m, ".csv")))
      
      # tmpt$vecs[[1]] <- c(1,2,3,100,500,22)
      
      # Add tract names
      tmpt$tract <- as.character(names(tmp))
      
      # Add values
      tractvalues <- t(as.matrix(tmp))
      for (nt in 1:numtracts){
        tmpt$val[[nt]] <- tractvalues[nt,]
        tmpt$xlen[[nt]] <- as.double(1:length(tractvalues[nt,]))
      }
      
      #tmpt$val <- as.list(t(as.matrix(tmp)))
      
      # Concatenate
      tmpT <- bind_rows(tmpT, tmpt)
    }
    SLF <- bind_rows(SLF, tmpT)
  }
  return(SLF)
}

readMacroData <- function(basedir,project,analysis){
  projPath <- file.path(basedir,
                        project,
                        paste("analysis",analysis,sep="-"))
  subjectsdir <- list.dirs(projPath,full.names = TRUE,recursive = FALSE)
  tractparams <- read.csv(file.path(projPath,'tractparams.csv'))
  TCKS <- tractparams[,'label']
  
  myData <- c()
  
  for (sub in subjectsdir){
    for (tck in TCKS){
      sub <- sub('.*sub-', '', basename(sub))
      jsonfile <- file.path(projPath,
                            paste("sub",sub,sep="-"),
                            'ses-001',
                            'output',
                            paste(tck,"json",sep="."))
      
      if (file.exists(jsonfile)){
        aux <- as.data.frame(fromJSON(file=jsonfile))
        aux <- cbind(sub=sub,project=project,tract=tck,aux)
        myData <- rbind(myData,aux)
      } else{
        next 
      }  

    }
  }
  return(myData)
}

get.macro.data <- function(dir.path, projects, analyses) {
  dat <- c()
  for (an in analyses){
    for (pr in projects) {
      aux <- readMacroData(dir.path, pr, an)
      dat <- rbind(dat,aux)
    }
  }
  dat$volume <- dat$volume/1000 # pass from mm to cm 
  dat$tractlabel <- sub('.*_','',dat$tract)
  dat[grepl('Left', dat$tract, fixed = TRUE),"Hemisphere"] <- "Left"
  dat[grepl('Right', dat$tract, fixed = TRUE),"Hemisphere"] <- "Right"
  dat[dat$tractlabel == 'V1V2','tractlabel'] <- 'OR' 
  dat[dat$tractlabel == 'OR','fulltractlabel'] <- 'Optic Radiation'
  dat[dat$tractlabel == 'MR','fulltractlabel']   <- 'Motor Radiation'
  dat[dat$tractlabel == 'DT','fulltractlabel']   <- 'Dentatothalamic'
  dat[dat$tractlabel == 'AR','fulltractlabel']   <- 'Acoustic Radiation'
  dat[dat$tractlabel == 'SR','fulltractlabel']   <- 'Somatosensory Radiation'
  MD.tractparams <- read.csv(file.path(dir.path, 
                                       pr,
                                       paste("analysis","HO",sep="-"),
                                       "tractparams.csv"))
  for (roi2 in MD.tractparams$roi2){
    label <- as.character(MD.dictionary[sub("^[^_]*_","",roi2)])
    tract <- MD.tractparams[MD.tractparams$roi2 == roi2,'label'] 
    if (strsplit(roi2,split="_")[[1]][1] == 'L'){
      dat[dat$tract == tract,'Hemisphere'] <- 'Left' 
      dat[dat$tract == tract,'fulltractlabel'] <- label
    } else if (strsplit(roi2,split="_")[[1]][1] == 'R'){
      dat[dat$tract == tract,'Hemisphere'] <- 'Right' 
      dat[dat$tract == tract,'fulltractlabel'] <- label
    } 
  }
  return(dat)
}

# plt.models.gender <- function(ATLAS,
#                               y,
#                               x,
#                               model.female,
#                               model.male,
#                               xlab,
#                               ylab,
#                               line.width,
#                               title,
#                               facewrap=FALSE,
#                               legend=TRUE,
#                               hide.x.axis = FALSE,
#                               tag,
#                               tag.size) {
#   
#   if (missing(xlab)){
#     xlab="Age"
#   }
#   
#   if (missing(x)){
#     x="AGE"
#   }
#   
#   if (missing(line.width)){
#     line.width= 0.75
#   }
#   
#   plt.vol <- ggplot(ATLAS,aes_string(x=x,
#                                      y=y,
#                                      shape="Gender")) + 
#     geom_point(alpha=0.5,key_glyph = large_points) + 
#     # females
#     stat_smooth(data=subset(ATLAS,Gender=="Female"),
#                 aes_string(linetype="Gender"),method = "lm", 
#                 formula = model.female, alpha=0, size=line.width) +
#     # males
#     stat_smooth(data=subset(ATLAS,Gender=="Male"),
#                 aes_string(linetype="Gender"),method = "lm", 
#                 formula = model.male, alpha=0, size=line.width) +
#     scale_linetype_manual("",values=c(1,2)) +
#     scale_shape_manual("",values=c(16,17)) +
#     theme_bw() + labs(x=xlab, y=ylab) +
#     scale_color_brewer(palette="Set1") +  scale_fill_brewer(palette="Set1") +
#     guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA))),
#            shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
#     theme(plot.title = element_text(hjust = 0.5, 
#                                     size = 24, 
#                                     face="bold"),
#           text = element_text(size=24, family = "Arial"),
#           strip.text.x = element_text(size=24),
#           axis.text = element_text(size=20),
#           # Hide panel borders and remove grid lines
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           # Change axis line
#           axis.line = element_line(colour = "black"),
#           # facet_wrap without boxes
#           strip.background = element_blank(),
#           legend.key.size = unit(2.5,"line"),
#           legend.key.width = unit(5,"line"),
#           aspect.ratio=1)
#   
#   if (facewrap) {
#     plt.vol <- plt.vol + facet_wrap(~Gender)
#   }
#   
#   if (!missing(title)){
#     plt.vol <- plt.vol + ggtitle(title)
#   }
#   
#   if (!legend) {
#     plt.vol <- plt.vol + theme(legend.position = "none")
#   }
#   
#   if (hide.x.axis) {
#     plt.vol <- plt.vol + theme(axis.ticks.x = element_blank(),
#                                axis.text.x = element_text(size=20, 
#                                                           colour = "white"),
#                                axis.title.x = element_text(colour = "white"))
#   }
#   
#   if (!missing(tag)) {
#     plt.vol <- plt.vol + labs(tag = tag) 
#     if(!missing(tag.size)) {
#       plt.vol <- plt.vol + theme(plot.tag = element_text(size = tag.size))
#     } else {
#       plt.vol <- plt.vol + theme(plot.tag = element_text(size = 30))
#     }
#   }
#   
#   return(plt.vol)
# }


#%%%
#%% Dictionary
# Dorsal Prefrontal group
dPFC <- c(
  'Area_9_anterior' = '9a',
  'Area_9_Posterior' = '9p',
  'Area_8Ad' = '8Ad',
  'Area_9-46d' = '9-46d',
  'Area_anterior_9-46v' = 'a9-46v',
  'Area_posterior_9-46v' = 'p9-46v',
  'Area_46' = '46',
  'Area_8BM' = '8BM',
  'Area_8C' = '8C',
  'Area_8Av' = '8Av',
  'Inferior_6-8_Transitional_Area' = 'i6-8',
  'Superior_6-8_Transitional_Area' = 's6-8',
  'Superior_Frontal_Language_Area' = 'SFL',
  'Area_8B_Lateral' = '8BL'
)

dPFC.label <- c(
  '9a',
  '9p',
  '8Ad',
  '9-46d',
  'a9-46v',
  'p9-46v',
  '46',
  '8BM',
  '8C',
  '8Av',
  'i6-8',
  's6-8',
  'SFL',
  '8BL'
)

# Medial Prefrontal group
mPFC <- c(
  'Area_9_Middle' = '9m',  # medial BA9   
  'Area_10r' = '10r',      # rostral BA10  
  'Area_10v' = '10v',      # ventral BA10 
  'Area_s32' = 's32',      # subgenual BA32
  'Area_p32' = 'p32',      # pregenual BA32 
  'Area_25' = '25'
)     

mPFC.label <- c(
  '9m',
  '10r',
  '10v',
  's32',
  'p32',
  '25'
)

# Orbital and polar prefrontal
OrbPoPFC <- c(
  'Area_11l' = '11l',                # lateral BA11 
  'Area_13l' = '13l',                # lateral BA13 
  'Orbital_Frontal_Complex' = 'OFC', # orbitofrontal complex  
  'posterior_OFC_Complex' = 'pOFC',  # posterior orbitofrontal complex
  'Area_10d' = '10d',                # dorsal BA10  
  'Polar_10p' = '10pp',              # most polar portion of BA10  
  'Area_anterior_10p' = 'a10p',      # anterior polar portion of BA10  
  'Area_posterior_10p' = 'p10p'      # posterior polar portion of BA10
)     

OrbPoPFC.label <- c(
  '11l', 
  '13l',
  'OFC',
  'pOFC',
  '10d',
  '10pp',
  'a10p',
  'p10p'
)

# Inferolateral/opercular prefrontal cortex
IFC <- c(
  'Area_44' = '44',              # BA44  
  'Area_45' = '45',              # BA45
  'Area_47l' = '47l',            # BA47l
  'Area_anterior_47r' = 'a47r',  # anterior rostral BA47 
  'Area_posterior_47r' = 'p47r', # posterior rostral BA47 
  'Area__IFSa' = 'IFSa',         # anterior inferior frontal sulcus  
  'Area_IFSp' = 'IFSp',          # posterior inferior frontal sulcus 
  'Area_IFJa' = 'IFJa',          # anterior junction of the inferior frontal 
                                 # and the precentral sulcus  
  'Area_IFJp' = 'IFJp',          # posterior junction of the inferior frontal 
                                 # and the precentral sulcus  
  'Area_47m' = '47m',            # medial BA47 
  'Area_47s' = '47s'             # sulcal part of BA47
)            

IFC.label <- c(
  '44',
  '45',
  '47l',
  'a47r',
  'p47r',
  'IFSa',
  'IFSp',
  'IFJa',
  'IFJp',
  '47m',
  '47s'
)

MD.labels <- c(dPFC.label,mPFC.label,OrbPoPFC.label,IFC.label)
MD.dictionary <- c(dPFC,mPFC,OrbPoPFC,IFC)

FO.labels <- c(
  "Optic Radiation",
  "Acoustic Radiation",
  "Motor Radiation",
  "Somatosensory Radiation",
  "Dentatothalamic"
)

FO.tracts <- c("OR", "AR", "MR", "SR", "DT")

add.grouplabel <- function(df){
  df[df$fulltractlabel %in% FO.labels,"group"] <- "FO"
  df[df$fulltractlabel %in% dPFC.label,"group"] <- "dPFC"
  df[df$fulltractlabel %in% mPFC.label,"group"] <- "mPFC"
  df[df$fulltractlabel %in% IFC.label,"group"] <- "IFC"
  df[df$fulltractlabel %in% OrbPoPFC.label,"group"] <- "OrbPoPFC"
  df$group <- factor(df$group, levels = c("FO","dPFC","mPFC","IFC","OrbPoPFC"))
  return(df)
}

# roi2<-md.tractparams[md.tractparams$label == 'HAL8','roi2']
# as.character(MD_dictionary[sub("^[^_]*_","",roi2)])

# models.BIC.gender <- function(dataset,x,y,R_bootstrap){
#   models.data <- dataset %>%
#     group_by(Gender) %>%
#     group_modify(~models.boots(dataset=.x,
#                                x="AGE",
#                                y=y,
#                                R_bootstrap = 1000))
#   
#   BIC <- reshape2::melt(models.data,
#                         measure.vars=c("BIClinear",
#                                        "BICquadratic",
#                                        "BICcubic"),
#                         variable.name="model",
#                         value.name="y")
#   
#   BIC$model <- mapvalues(BIC$model,from=c("BIClinear",
#                                           "BICquadratic",
#                                           "BICcubic"),
#                          to = c("Linear","Quadratic","Cubic"))
#   
#   BIC <- summarySE(data=BIC,
#                    measurevar="y",
#                    groupvars=c("Gender","model"))
#   
#   BIC$vol <- y
#   return(BIC)
# }
# 
# models.boots <- function(dataset,x,y,R_bootstrap){
#   
#   if(!missing(dataset)){
#     dataset <- as.data.frame(dataset)
#     x <- dataset[,x]
#     y <- dataset[,y]
#     
#   }
#   
#   rsquared.linear <- array(0,dim=R_bootstrap)
#   rsquared.quadratic <- array(0,dim=R_bootstrap)
#   rsquared.cubic <- array(0,dim=R_bootstrap)
#   BIC.linear <- array(0,dim=R_bootstrap)
#   BIC.quadratic <- array(0,dim=R_bootstrap)
#   BIC.cubic <- array(0,dim=R_bootstrap)
#   id <- array(0,dim=R_bootstrap)
#   
#   for (k in 1:R_bootstrap){
#     idx <- sample(1:length(x), length(x), replace=TRUE)
#     
#     fit.linear <- lm(y[idx]~poly(x[idx],1))
#     rsquared.linear[k] <- summary(fit.linear)$adj.r.squared
#     BIC.linear[k] <- BIC(fit.linear) 
#     
#     fit.quadratic <- lm(y[idx]~poly(x[idx],2))
#     rsquared.quadratic[k] <- summary(fit.quadratic)$adj.r.squared
#     BIC.quadratic[k] <- BIC(fit.quadratic)
#     
#     fit.cubic <- lm(y[idx]~poly(x[idx],3))
#     rsquared.cubic[k] <- summary(fit.cubic)$adj.r.squared
#     BIC.cubic[k] <- BIC(fit.cubic)
#     
#     id[k] <- k
#   }
#   return(data.frame("id"=id,
#                     "rsquaredlinear"=rsquared.linear,
#                     "rsquaredquadratic"=rsquared.quadratic,
#                     "rsquaredcubic"=rsquared.cubic,
#                     "BIClinear"=BIC.linear,
#                     "BICquadratic"=BIC.quadratic,
#                     "BICcubic"=BIC.cubic))
# }
# 
# # ------------------------------------------------------------------------------
# ## Summarizes data.
# ## Gives count, mean, standard deviation, standard error of the mean, and 
# ## confidence interval (default 95%).
# ##   data: a data frame.
# ##   measurevar: the name of a column that contains the variable to be 
# ##               summariezed
# ##   groupvars: a vector containing names of columns that contain grouping 
# ##              variables
# ##   na.rm: a boolean that indicates whether to ignore NA's
# ##   conf.interval: the percent range of the confidence interval 
# ##                  (default is 95%)
# summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
#                       conf.interval=.95, .drop=TRUE) {
#   library(plyr)
#   
#   # New version of length which can handle NA's: if na.rm==T, don't count them
#   length2 <- function (x, na.rm=FALSE) {
#     if (na.rm) sum(!is.na(x))
#     else       length(x)
#   }
#   
#   # This does the summary. For each group's data frame, return a vector with
#   # N, mean, and sd
#   datac <- ddply(data, groupvars, .drop=.drop,
#                  .fun = function(xx, col) {
#                    c(N    = length2(xx[[col]], na.rm=na.rm),
#                      mean = mean   (xx[[col]], na.rm=na.rm),
#                      sd   = sd     (xx[[col]], na.rm=na.rm)
#                    )
#                  },
#                  measurevar
#   )
#   
#   # Rename the "mean" column    
#   datac <- plyr::rename(datac, c("mean" = measurevar))
#   
#   datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
#   
#   # Confidence interval multiplier for standard error
#   # Calculate t-statistic for confidence interval: 
#   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#   ciMult <- qt(conf.interval/2 + .5, datac$N-1)
#   datac$ci <- datac$se * ciMult
#   
#   return(datac)
# }
# 
# diff.BIC <- function(data,variable){
#   data <- as.data.frame(data)
#   vector <- data[,variable]
#   diffvalues <- -diff(vector)
#   
#   if(sum(diffvalues > 2) == length(diffvalues)){
#     n=length(diffvalues)+1
#   } else if (sum(diffvalues>2) == 0){
#     n=1
#   } else {
#     n=which(diffvalues>2)+1
#     #check it to the linear model
#     if((vector[1] - vector[n]) > 2) {
#       n=n
#     } else {
#       n=1
#     }
#   }
#   return(data.frame("model"=n))
# }

scaleFUN <- function(x) sprintf("%.1f",x)

plt.linear.regressions <- function(dt,
                                   x,
                                   y,
                                   xlab,
                                   ylab = NULL,
                                   line.width = 1.5,
                                   font.size = 24,
                                   leg.size = 5,
                                   facetgroup,
                                   facetgroup2 = NULL,
                                   ncol,
                                   ylabeller = NULL,
                                   roundx = FALSE,
                                   scales = NULL,
                                   independenty = FALSE,
                                   removelabs = NULL,
                                   hideyaxis = FALSE,
                                   linearegressions = TRUE,
                                   stats = TRUE,
                                   legend = TRUE){

  model.linear <- y ~ poly(x,1)
  
  plt <- ggplot(dt,aes_string(x=x,
                              y=y,
                              shape="hemgen",
                              colour = "hemgen",
                              linetype = "hemgen")) + 
    geom_point(alpha=0.5,key_glyph = large_points)
  
  
  if (is.null(scales)){
    scales = "free_y"
  } 
  
  if (is.null(removelabs)){
    removelabs = "x"
  } 
  
  if (!is.null(ylabeller)){
    plt <- plt +
    facet_wrap2(as.formula(paste("~",facetgroup)),
                ncol=7, axes = "all",
                remove_labels = "x",
                scales = "free_y",
                strip.position = "left",
                labeller = as_labeller(ylabeller, label_parsed)
    )
    ylab <- NULL
  } else if (!is.null(facetgroup2)){
    if (independenty){
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    independent = "y",
                    remove_labels = removelabs,
                    scales = scales)
      
    } else{
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    remove_labels = removelabs,
                    scales = scales)
    } 
    
    
  } else{
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)), 
                  ncol=ncol, 
                  axes = "all", 
                  remove_labels = "x",
                  scales = scales)
    
  } 
  
  if (linearegressions){
    plt <- plt + 
      # females-left
      stat_smooth(data=subset(dt,Hemisphere == "Left" & Gender=="Female"),
                  method = "lm", formula = model.linear, 
                  alpha=0, size = line.width) +
      # females-right
      stat_smooth(data=subset(dt,Hemisphere == "Right" & Gender=="Female"),
                  method = "lm", formula = model.linear, 
                  alpha=0, size = line.width) +
      # males-left
      stat_smooth(data=subset(dt,Hemisphere == "Left" & Gender=="Male"),
                  method = "lm", formula = model.linear, 
                  alpha=0, size = line.width) +
      # males-right
      stat_smooth(data=subset(dt,Hemisphere == "Right" & Gender=="Male"),
                  method = "lm", formula = model.linear, 
                  alpha=0, size = line.width)
  } 
  
  if(stats){
    annot.size <- font.size/6
    plt <- plt +
      # stat_cor(data=subset(dt, Gender == "Female"), 
      #          aes(label = paste("'R = '", ..r.., "'p[Female] = '", ..p.., sep = "~` `~")),
      #          label.x.npc = "left",
      #          size = font.size/5) +
      
      # stat_fit_glance(data=subset(dt, Gender == "Female" & Hemisphere == "Left"),
      #                 method = "cor.test",
      #                 label.x = "left",
      #                 label.y = 1,
      #                 method.args = list(formula = ~ x + y),
      #                 mapping = aes(label = sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"="~%.2g',
    #                               after_stat(estimate), after_stat(p.value))),
    #                 parse = TRUE,
    #                 size = font.size/8)    +
    # stat_fit_glance(data=subset(dt, Gender == "Female" & Hemisphere == "Right"),
    #                 method = "cor.test",
    #                 label.x = "left",
    #                 label.y = 0.95,
    #                 method.args = list(formula = ~ x + y),
    #                 mapping = aes(label = sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"="~%.2g',
    #                                               after_stat(estimate), after_stat(p.value))),
    #                 parse = TRUE,
    #                 size = font.size/8)    +
    # stat_fit_glance(data=subset(dt, Gender == "Male" & Hemisphere == "Left"),
    #                 method = "cor.test",
    #                 label.x = "left",
    #                 label.y = 0.9,
    #                 method.args = list(formula = ~ x + y),
    #                 mapping = aes(label = sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"="~%.2g',
    #                                               after_stat(estimate), after_stat(p.value))),
    #                 parse = TRUE,
    #                 size = font.size/8)    +   
    # stat_fit_glance(data=subset(dt, Gender == "Male" & Hemisphere == "Right"),
    #                 method = "cor.test",
    #                 label.x = "left",
    #                 label.y = 0.85,
    #                 method.args = list(formula = ~ x + y),
    #                 mapping = aes(label = sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"="~%.2g',
    #                                               after_stat(estimate), after_stat(p.value))),
    #                 parse = TRUE,
    #                 size = font.size/8)    +   
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    stat_fit_glance(data=subset(dt, Gender == "Female" & Hemisphere == "Left"),
                    method = "cor.test",
                    label.x = "left",
                    label.y = 1,
                    method.args = list(formula = ~ x + y),
                    aes(label=ifelse(..p.value..< 0.001, 
                                     sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.001**"',
                                             after_stat(estimate)),
                                     ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
                                            sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.05*"',
                                                    after_stat(estimate)), 
                                            sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"> 0.05"',
                                                    after_stat(estimate))))),
                    parse = TRUE,
                    size = annot.size)    +
      stat_fit_glance(data=subset(dt, Gender == "Female" & Hemisphere == "Right"),
                      method = "cor.test",
                      label.x = "left",
                      label.y = 0.95,
                      method.args = list(formula = ~ x + y),
                      aes(label=ifelse(..p.value..< 0.001, 
                                       sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.001**"',
                                               after_stat(estimate)),
                                       ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
                                              sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.05*"',
                                                      after_stat(estimate)), 
                                              sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"> 0.05"',
                                                      after_stat(estimate))))),
                      parse = TRUE,
                      size = annot.size)    +
      stat_fit_glance(data=subset(dt, Gender == "Male" & Hemisphere == "Left"),
                      method = "cor.test",
                      label.x = "left",
                      label.y = 0.9,
                      method.args = list(formula = ~ x + y),
                      aes(label=ifelse(..p.value..< 0.001, 
                                       sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.001**"',
                                               after_stat(estimate)),
                                       ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
                                              sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.05*"',
                                                      after_stat(estimate)), 
                                              sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"> 0.05"',
                                                      after_stat(estimate))))),
                      parse = TRUE,
                      size = annot.size)    +
      stat_fit_glance(data=subset(dt, Gender == "Male" & Hemisphere == "Right"),
                      method = "cor.test",
                      label.x = "left",
                      label.y = 0.85,
                      method.args = list(formula = ~ x + y),
                      aes(label=ifelse(..p.value..< 0.001, 
                                       sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.001**"',
                                               after_stat(estimate)),
                                       ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
                                              sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.05*"',
                                                      after_stat(estimate)), 
                                              sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"> 0.05"',
                                                      after_stat(estimate))))),
                      parse = TRUE,
                      size = annot.size)
      
      
      
      # stat_cor(data=subset(dt, Gender == "Female"),
      #          aes(label = paste("'R = '", ..r.., "'p = '", ..p.., sep = "~`,`~")),
      #          label.x.npc = "left",
      #          label.y.npc = "center",
      #          size = font.size/5) +
      #   label.x.npc = .65,
      # label.y.npc = 1.0,
      # vjust = 1)
  }
  
  plt <- plt + theme_bw() + labs(x=xlab, y=ylab) +
    scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
    scale_linetype_manual("",values=c(1,2,1,2)) +
    scale_shape_manual("",values=c(16,17,16,17))
  
  if (roundx){
    plt <- plt + scale_x_continuous(labels=scaleFUN)
  }  
    
  plt <- plt +
    guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
           shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(
      plot.title = element_text(hjust = 0.5, 
                                size = font.size, 
                                face="bold"),
      text = element_text(size=font.size, family = "Arial"),
      strip.text.x = element_text(size=font.size,
                                  face="bold"),
      strip.text.y = element_text(size=font.size),
      axis.text = element_text(size=font.size - 4),
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "black"),
      # facet_wrap without boxes
      strip.background = element_blank(),
      legend.key.size = unit(leg.size/2,"line"),
      legend.key.width = unit(leg.size,"line"),
      strip.placement = "outside",
      aspect.ratio = 1)
    
  if (!legend){
    plt <- plt +
      theme(legend.position = "none")
  }
  return(plt)  
} 

plt.linear.regressions2 <- function(dt,
                                   x,
                                   y,
                                   group,
                                   xlab,
                                   ylab = NULL,
                                   line.width = 1.5,
                                   font.size = 24,
                                   leg.size = 5,
                                   facetgroup,
                                   facetgroup2 = NULL,
                                   ncol,
                                   ylabeller = NULL,
                                   roundx = FALSE,
                                   scales = NULL,
                                   independenty = FALSE,
                                   removelabs = NULL,
                                   hideyaxis = FALSE,
                                   linearegressions = TRUE,
                                   legend = TRUE){
  
  model.linear <- y ~ poly(x,1)
  
  plt <- ggplot(dt, aes(
    x = .data[[x]],
    y = .data[[y]],
    shape = .data[[group]],
    colour = .data[[group]],
    linetype = .data[[group]]
  )) +
    geom_point(alpha = 0.7, key_glyph = large_points)
  
  
  if (is.null(scales)){
    scales = "free_y"
  } 
  
  if (is.null(removelabs)){
    removelabs = "x"
  } 
  
  if (!is.null(ylabeller)){
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)),
                  ncol=7, axes = "all",
                  remove_labels = "x",
                  scales = "free_y",
                  strip.position = "left",
                  labeller = as_labeller(ylabeller, label_parsed)
      )
    ylab <- NULL
  } else if (!is.null(facetgroup2)){
    if (independenty){
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    independent = "y",
                    remove_labels = removelabs,
                    scales = scales)
      
    } else{
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    remove_labels = removelabs,
                    scales = scales)
    } 
    
    
  } else{
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)), 
                  ncol=ncol, 
                  axes = "all", 
                  remove_labels = "x",
                  scales = scales)
    
  } 
  
  if (linearegressions){
    annot.size <- font.size/6
    plt <- plt + 
      stat_smooth(method = "lm", formula = model.linear, 
                 alpha = 0, size = line.width) + 
      stat_fit_glance(data=subset(dt, agegroup == "Young"),
                      method = "cor.test",
                      label.x = "left",
                      label.y = 1,
                      method.args = list(formula = ~ x + y),
                      aes(label=ifelse(..p.value..< 0.001,
                                       sprintf('italic(R)[Y]~"="~%.2f~~italic(p)[Y]~"< 0.001**"',
                                               after_stat(estimate)),
                                       ifelse(..p.value..>=0.001 & ..p.value..<0.05,
                                              sprintf('italic(R)[Y]~"="~%.2f~~italic(p)[Y]~"< 0.05*"',
                                                      after_stat(estimate)),
                                              sprintf('italic(R)[Y]~"="~%.2f~~italic(p)[Y]~"> 0.05"',
                                                      after_stat(estimate))))),
                      parse = TRUE,
                      size = annot.size) +
      stat_fit_glance(data=subset(dt, agegroup == "Adult"),
                      method = "cor.test",
                      label.x = "left",
                      label.y = 0.95,
                      method.args = list(formula = ~ x + y),
                      aes(label=ifelse(..p.value..< 0.001,
                                       sprintf('italic(R)[A]~"="~%.2f~~italic(p)[A]~"< 0.001**"',
                                               after_stat(estimate)),
                                       ifelse(..p.value..>=0.001 & ..p.value..<0.05,
                                              sprintf('italic(R)[A]~"="~%.2f~~italic(p)[A]~"< 0.05*"',
                                                      after_stat(estimate)),
                                              sprintf('italic(R)[A]~"="~%.2f~~italic(p)[A]~"> 0.05"',
                                                      after_stat(estimate))))),
                      parse = TRUE,
                      size = annot.size) +
      stat_fit_glance(data=subset(dt, agegroup == "Old"),
                      method = "cor.test",
                      label.x = "left",
                      label.y = 0.9,
                      method.args = list(formula = ~ x + y),
                      aes(label=ifelse(..p.value..< 0.001,
                                       sprintf('italic(R)[O]~"="~%.2f~~italic(p)[O]~"< 0.001**"',
                                               after_stat(estimate)),
                                       ifelse(..p.value..>=0.001 & ..p.value..<0.05,
                                              sprintf('italic(R)[O]~"="~%.2f~~italic(p)[O]~"< 0.05*"',
                                                      after_stat(estimate)),
                                              sprintf('italic(R)[O]~"="~%.2f~~italic(p)[O]~"> 0.05"',
                                                      after_stat(estimate))))),
                      parse = TRUE,
                      size = annot.size)
  } 
  
  
  # stat_fit_glance(data=subset(dt, Gender == "Female" & Hemisphere == "Left"),
  #                 method = "cor.test",
  #                 label.x = "left",
  #                 label.y = 1,
  #                 method.args = list(formula = ~ x + y),
  #                 aes(label=ifelse(..p.value..< 0.001, 
  #                                  sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.001**"',
  #                                          after_stat(estimate)),
  #                                  ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
  #                                         sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.05*"',
  #                                                 after_stat(estimate)), 
  #                                         sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"> 0.05"',
  #                                                 after_stat(estimate))))),
  #                 parse = TRUE,
  #                 size = annot.size)    +
  #   stat_fit_glance(data=subset(dt, Gender == "Female" & Hemisphere == "Right"),
  #                   method = "cor.test",
  #                   label.x = "left",
  #                   label.y = 0.95,
  #                   method.args = list(formula = ~ x + y),
  #                   aes(label=ifelse(..p.value..< 0.001, 
  #                                    sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.001**"',
  #                                            after_stat(estimate)),
  #                                    ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
  #                                           sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"< 0.05*"',
  #                                                   after_stat(estimate)), 
  #                                           sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"> 0.05"',
  #                                                   after_stat(estimate))))),
  #                   parse = TRUE,
  #                   size = annot.size)    +
  #   stat_fit_glance(data=subset(dt, Gender == "Male" & Hemisphere == "Left"),
  #                   method = "cor.test",
  #                   label.x = "left",
  #                   label.y = 0.9,
  #                   method.args = list(formula = ~ x + y),
  #                   aes(label=ifelse(..p.value..< 0.001, 
  #                                    sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.001**"',
  #                                            after_stat(estimate)),
  #                                    ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
  #                                           sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.05*"',
  #                                                   after_stat(estimate)), 
  #                                           sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"> 0.05"',
  #                                                   after_stat(estimate))))),
  #                   parse = TRUE,
  #                   size = annot.size)    +
  #   stat_fit_glance(data=subset(dt, Gender == "Male" & Hemisphere == "Right"),
  #                   method = "cor.test",
  #                   label.x = "left",
  #                   label.y = 0.85,
  #                   method.args = list(formula = ~ x + y),
  #                   aes(label=ifelse(..p.value..< 0.001, 
  #                                    sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.001**"',
  #                                            after_stat(estimate)),
  #                                    ifelse(..p.value..>=0.001 & ..p.value..<0.05, 
  #                                           sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"< 0.05*"',
  #                                                   after_stat(estimate)), 
  #                                           sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"> 0.05"',
  #                                                   after_stat(estimate))))),
  #                   parse = TRUE,
  #                   size = annot.size)    +
    
    
  
    # scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
    # scale_linetype_manual("",values=c(1,2,1,2)) +
    # scale_shape_manual("",values=c(16,17,16,17))
  
  if (roundx){
    plt <- plt + scale_x_continuous(labels=scaleFUN)
  }  
  
  plt <- plt + theme_bw() + labs(x=xlab, y=ylab) +
    # guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
    #        shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(
      plot.title = element_text(hjust = 0.5, 
                                size = font.size, 
                                face="bold"),
      text = element_text(size=font.size, family = "Arial"),
      strip.text.x = element_text(size=font.size,
                                  face="bold"),
      strip.text.y = element_text(size=font.size),
      axis.text = element_text(size=font.size - 4),
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "black"),
      # facet_wrap without boxes
      strip.background = element_blank(),
      legend.key.size = unit(leg.size/2,"line"),
      legend.key.width = unit(leg.size,"line"),
      strip.placement = "outside",
      aspect.ratio = 1)
  
  if (!legend){
    plt <- plt +
      theme(legend.position = "none")
  }
  return(plt)  
} 

plt.linear.regressions3 <- function(
  dt,
  x,
  y,
  group,
  group.label = NULL,
  xlab,
  ylab = NULL,
  line.width = 1.5,
  font.size = 24,
  leg.size = 5,
  facetgroup,
  facetgroup2 = NULL,
  ncol,
  ylabeller = NULL,
  roundx = FALSE,
  scales = NULL,
  independenty = FALSE,
  removelabs = NULL,
  hideyaxis = FALSE,
  legend = TRUE,
  linearegressions = FALSE,
  linearegressions.hem = FALSE,
  linearegressions.color = "#545454",
  linearegressions.hem.gen = FALSE,
  bold.title.colums = FALSE,
  bold.title.rows = FALSE,
  scale.x = TRUE
){
  
  model.linear <- y ~ poly(x,2)
  
  plt <- ggplot(
    dt,
    aes(
      x = .data[[x]],
      y = .data[[y]],
      colour = .data[[group]]
    )
  ) + 
  geom_point(alpha=0.7,key_glyph = large_points) +
  scale_color_viridis(
    option = "plasma",
    guide = guide_colorbar(
      barwidth = 2, 
      barheight = 16,
      ticks.colour = "black",
      ticks.linewidth = 2,
      title = group.label
    )
  )
  
  if (linearegressions){
    annot.size <- font.size/5
    plt <- plt +
      stat_smooth(
        method = "lm",
        alpha = 0,
        formula = model.linear,
        linewidth = line.width,
        color = linearegressions.color
      ) +
      # stat_fit_glance(method = "cor.test",
      #                 label.x = "left",
      #                 label.y = 1,
      #                 method.args = list(formula = ~ x + y),
      #                 aes(label=ifelse(..p.value..< 0.001,
      #                                  sprintf('italic(R)~"="~%.2f~~italic(p)~"< 0.001**"',
      #                                          after_stat(estimate)),
      #                                  ifelse(..p.value..>=0.001 & ..p.value..<0.05,
      #                                         sprintf('italic(R)~"="~%.2f~~italic(p)~"< 0.05*"',
      #                                                 after_stat(estimate)),
      #                                         sprintf('italic(R)~"="~%.2f~~italic(p)~"> 0.05"',
      #                                                 after_stat(estimate))))),
      #                   parse = TRUE,
      #                   size = annot.size)
      stat_fit_glance(method = "cor.test",
                      label.x = "left",
                      label.y = 1,
                      method.args = list(formula = ~ x + y),
                      aes(label=sprintf('italic(R)~"="~%.2f',
                                        after_stat(estimate))),
                      parse = TRUE,
                      size = annot.size) #+
      # stat_fit_glance(method = "cor.test",
      #                 label.x = "left",
      #                 label.y = 0.90,
      #                 method.args = list(formula = ~ x + y),
      #                 aes(label=ifelse(..p.value..>= 0.05,
      #                                  sprintf('italic(p)~"ns"',
      #                                          after_stat(estimate)),
      #                                  ifelse(..p.value..<= 0.05,
      #                                         sprintf('italic(p)~"*"',
      #                                                 after_stat(estimate)),
      #                                         ifelse(..p.value..<=0.01,
      #                                                sprintf('italic(p)~"**"',
      #                                                       after_stat(estimate)),
      #                                                sprintf('italic(p)~"***"',
      #                                                        after_stat(estimate)))))),
      # 
      # 
      #                                  # ifelse(..p.value..>=0.001 & ..p.value..<0.05,
      #                                  #        sprintf('italic(p)~"< 0.05*"',
      #                                  #                after_stat(estimate)),
      #                                  #        sprintf('italic(p)~"ns"',
      #                                  #                after_stat(estimate))))),
      #                 parse = TRUE,
      #                 size = annot.size)
  }
  
  if (linearegressions.hem){
    plt <- plt +
      stat_smooth(
        aes(linetype = .data[["Hemisphere"]]),
        method = "lm",
        formula = model.linear,
        alpha = 0,
        linewidth = line.width,
        color = linearegressions.color,
        alpha = 0.5
      )
  }
  
  if (linearegressions.hem.gen){
    plt <- plt +
    new_scale_colour()  +
    stat_smooth(
      inherit.aes = FALSE,
      data = dt,
      aes(
        x = .data[[x]],
        y = .data[[y]],
        linetype = .data[["Hemisphere"]],
        colour = .data[["Gender"]],
        group = interaction(.data[["Hemisphere"]], .data[["Gender"]])
      ),
      method = "lm", 
      formula = model.linear, 
      alpha = 0, 
      linewidth = line.width
    )
  }
  
  if (is.null(scales)){
    scales = "free_y"
  } 
  
  if (is.null(removelabs)){
    removelabs = "x"
  } 
  
  if (!is.null(ylabeller)){
    plt <- plt +
      facet_wrap2(
        as.formula(paste("~",facetgroup)),
        ncol=7, axes = "all",
        remove_labels = "x",
        scales = "free_y",
        strip.position = "left",
        labeller = as_labeller(ylabeller, label_parsed)
      )
    ylab <- NULL
  } else if (!is.null(facetgroup2)){
    if (independenty){
      plt <- plt +
        facet_grid2(
          as.formula(paste(facetgroup,"~",facetgroup2)),
          axes = "all", 
          independent = "y",
          remove_labels = removelabs,
          scales = scales
        )
      
    } else{
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    remove_labels = removelabs,
                    scales = scales)
    } 
    
  } else{
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)), 
                  ncol=ncol, 
                  axes = "all", 
                  remove_labels = "x",
                  scales = scales)
    
  } 
  
  plt <- plt + theme_bw() + labs(x=xlab, y=ylab)
  # scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
  # scale_linetype_manual("",values=c(1,2,1,2)) +
  # scale_shape_manual("",values=c(16,17,16,17))
  
  if (roundx){
    plt <- plt + scale_x_continuous(labels=scaleFUN)
  }  
  
  if(bold.title.colums){
    x.face.style = "bold"
  }else{
    x.face.style = NULL
  }
  
  if(bold.title.rows){
    y.face.style = "bold"
  }else{
    y.face.style = NULL
  }
  
  if(scale.x){
    plt <- plt + scale_x_continuous(breaks = seq(0.2, 1, 0.1), limits = c(NA, NA)) 
  }
  
  plt <- plt + 
    # guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
    #        shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(
      plot.title = element_text(
        hjust = 0.5, 
        size = font.size, 
        face = "bold"
      ),
      text = element_text(
        size = font.size, 
        family = "Arial"
      ),
      strip.text.x = element_text(
        size = font.size,
        face = x.face.style
      ),
      strip.text.y = element_text(
        size =  font.size,
        face = y.face.style
      ),
      # axis.text = element_text(size=font.size - 4),
      axis.text.y = element_text(size = font.size- 4),
      axis.text.x = element_text(size = font.size - 8),
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # horizontal space
      panel.spacing.x = unit(1.25,"cm"),
      # vertical space
      panel.spacing.y = unit(1,"cm"),
      # Change axis line
      axis.line = element_line(colour = "black"),
      # facet_wrap without boxes
      strip.background = element_blank(),
      legend.key.size = unit(leg.size/2,"line"),
      legend.key.width = unit(leg.size,"line"),
      strip.placement = "outside",
      aspect.ratio = 1
    )
  
  if (!legend){
    plt <- plt +
      theme(legend.position = "none")
  }
  return(plt)  
} 

plt.xy.regressions4 <- function(
    dt,
    x,
    y,
    group,
    group.label = NULL,
    xlab,
    ylab = NULL,
    line.width = 1.5,
    font.size = 24,
    leg.size = 5,
    facetgroup,
    facetgroup2 = NULL,
    ncol,
    ylabeller = NULL,
    roundx = FALSE,
    scales = NULL,
    independenty = FALSE,
    removelabs = NULL,
    hideyaxis = FALSE,
    legend = TRUE,
    regressions = FALSE,
    regressions.hem = FALSE,
    regressions.color = "#545454",
    regressions.hem.gen = FALSE,
    bold.title.colums = FALSE,
    bold.title.rows = FALSE,
    scale.x = TRUE,
    regression.degree = 1,
    plt.corr.stats = FALSE
){
  
  reg.model <- y ~ poly(x, regression.degree, raw = TRUE)
  
  plt <- ggplot(
    dt,
    aes(
      x = .data[[x]],
      y = .data[[y]],
      colour = .data[[group]]
    )
  ) + 
    geom_point(alpha=0.6,key_glyph = large_points) +
    scale_color_viridis(
      option = "plasma",
      guide = guide_colorbar(
        barwidth = 2, 
        barheight = 16,
        ticks.colour = "black",
        ticks.linewidth = 2,
        title = group.label
      )
    )
  
  if (regressions){
    annot.size <- font.size/5
    plt <- plt +
      stat_smooth(
        method = "lm",
        alpha = 0,
        formula = reg.model,
        linewidth = line.width,
        color = regressions.color
      )
    if (plt.corr.stats){
      annot.size <- font.size/5
      plt <- plt +
        stat_fit_glance(method = "cor.test",
                        label.x = "left",
                        label.y = 1,
                        method.args = list(formula = ~ x + y),
                        aes(label=ifelse(..p.value..< 0.001,
                                         sprintf('italic(R)~"="~%.2f~~italic(p)~"***"',#< 0.001
                                                 after_stat(estimate)),
                                         ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                sprintf('italic(R)~"="~%.2f~~italic(p)~"**"', #< 0.01
                                                        after_stat(estimate)),
                                                ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                       sprintf('italic(R)~"="~%.2f~~italic(p)~"*"', #< 0.05
                                                               after_stat(estimate)),
                                                       sprintf('italic(R)~"="~%.2f~~italic(p)~"ns"', #> 0.05
                                                               after_stat(estimate)))))),
                        parse = TRUE,
                        size = annot.size)
    }
      
  }
  
  if (regressions.hem){
    if (length(regressions.color>1)){
      plt <- plt +
        new_scale_colour() +
        stat_smooth(
          aes(colour = Hemisphere),
          method = "lm",
          formula = reg.model,
          linewidth = line.width,
          alpha = 0
        ) +
        scale_color_manual(values = regressions.color)
      
    } else{
      plt <- plt +
        stat_smooth(
          aes(linetype = .data[["Hemisphere"]]),
          method = "lm",
          formula = reg.model,
          alpha = 0,
          linewidth = line.width,
          color = regressions.color,
          alpha = 0.5
        )
    }
  }
  
  if ((regressions.hem.gen) & ("hemgen" %in% names(dt))){
    a <- levels(dt[,"hemgen"])
    if(is.null(regressions.color)){
      regressions.color <- c()
      for (i in a){ 
        if (grepl("Left",i,fixed=TRUE)){ 
          regressions.color <- c(regressions.color,"#E41A1C")
        } else { 
          regressions.color <- c(regressions.color,"#377EB8")
        }
      }
    }
    
    custom.linetype <- c()
    for (i in a){ 
      if (grepl("Female",i,fixed=TRUE)){ 
        custom.linetype <- c(custom.linetype,1)
      } else { 
        custom.linetype <- c(custom.linetype,2)
      }
    }
    
    plt <- plt +
      new_scale_colour()  +
      stat_smooth(
        inherit.aes = FALSE,
        data = dt,
        aes(
          x = .data[[x]],
          y = .data[[y]],
          colour = .data[["hemgen"]],
          alpha = 0.5,
          linetype = .data[["hemgen"]]#,
          # group = interaction(.data[["Hemisphere"]], .data[["Gender"]])
        ),
        method = "lm", 
        formula = reg.model, 
        se = FALSE,
        linewidth = line.width
      ) +
      scale_color_manual(
        "", 
        values = regressions.color
      ) +
      scale_linetype_manual(
        "", 
        values = custom.linetype
      ) +
      scale_alpha(guide = "none") +
      guides(
        color = guide_legend(
          label.theme = element_text(
            size = font.size - 10, 
            margin = margin(t=10,b=10)
          ), 
          keyheight = unit(2,"lines")
        ),
        linetype = guide_legend(
          label.theme = element_text(
            size = font.size - 10, 
            margin = margin(t=10,b=10)
          ), 
          keyheight = unit(2,"lines")
        )
      )
    
    if (plt.corr.stats){
      annot.size <- font.size/5.5
      
      a <- levels(dt[,"Hemisphere"])
      stats.color <- c()
      for (i in a){ 
        if (grepl("Left",i,fixed=TRUE)){ 
          stats.color <- c(stats.color,"#E41A1C")
        } else { 
          stats.color <- c(stats.color,"#377EB8")
        }
      }
      
      plt <- plt + new_scale_color() +
        stat_fit_glance(data = subset(dt, Gender == "Female" & Hemisphere == "Left"),
                        method = "cor.test",
                        label.x = "left",  
                        label.y = 1,
                        method.args = list(formula = ~ x + y),
                        aes(label=ifelse(..p.value..< 0.001,
                                         sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"***"',#< 0.001
                                                 after_stat(estimate)),
                                         ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"**"', #< 0.01
                                                        after_stat(estimate)),
                                         ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"*"', #< 0.05
                                                        after_stat(estimate)),
                                                sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"ns"', #> 0.05
                                                        after_stat(estimate))))),
                            colour = .data[["Hemisphere"]]),
                        parse = TRUE,
                        size = annot.size)    +
          stat_fit_glance(data = subset(dt, Gender == "Female" & Hemisphere == "Right"),
                          method = "cor.test",
                          label.x = "left",
                          label.y = 0.93,
                          method.args = list(formula = ~ x + y),
                          aes(label=ifelse(..p.value..< 0.001,
                                           sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"***"',#< 0.001
                                                   after_stat(estimate)),
                                           ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                  sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"**"', #< 0.01
                                                          after_stat(estimate)),
                                                  ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                         sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"*"', #< 0.05
                                                                 after_stat(estimate)),
                                                         sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"ns"', #> 0.05
                                                                 after_stat(estimate))))),
                              colour = .data[["Hemisphere"]]),
                          parse = TRUE,
                          size = annot.size)    +
          stat_fit_glance(data = subset(dt, Gender == "Male" & Hemisphere == "Left"),
                          method = "cor.test",
                          label.x = "left",
                          label.y = 0.86,
                          method.args = list(formula = ~ x + y),
                          aes(label=ifelse(..p.value..< 0.001,
                                           sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"***"',#< 0.001
                                                   after_stat(estimate)),
                                           ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                  sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"**"', #< 0.01
                                                          after_stat(estimate)),
                                                  ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                         sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"*"', #< 0.05
                                                                 after_stat(estimate)),
                                                         sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"ns"', #> 0.05
                                                                 after_stat(estimate))))),
                              colour = .data[["Hemisphere"]]),
                          parse = TRUE,
                          size = annot.size)    +
          stat_fit_glance(data = subset(dt, Gender == "Male" & Hemisphere == "Right"),
                          method = "cor.test",
                          label.x = "left",
                          label.y = 0.79,
                          method.args = list(formula = ~ x + y),
                          aes(label=ifelse(..p.value..< 0.001,
                                           sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"***"',#< 0.001
                                                   after_stat(estimate)),
                                           ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                  sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"**"', #< 0.01
                                                          after_stat(estimate)),
                                                  ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                         sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"*"', #< 0.05
                                                                 after_stat(estimate)),
                                                         sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"ns"', #> 0.05
                                                                 after_stat(estimate))))),
                              colour = .data[["Hemisphere"]]),
                          parse = TRUE,
                          size = annot.size) +
        scale_color_manual(
          "", 
          values = stats.color
        )
    }
  } else if (!("hemgen" %in% names(dt))){
    print("Missing hemgen column in the dataframe")
  }
  
  if (is.null(scales)){
    scales = "free_y"
  } 
  
  if (is.null(removelabs)){
    removelabs = "x"
  } 
  
  if (!is.null(ylabeller)){
    plt <- plt +
      facet_wrap2(
        as.formula(paste("~",facetgroup)),
        ncol=7, axes = "all",
        remove_labels = "x",
        scales = "free_y",
        strip.position = "left",
        labeller = as_labeller(ylabeller, label_parsed)
      )
    ylab <- NULL
  } else if (!is.null(facetgroup2)){
    if (independenty){
      plt <- plt +
        facet_grid2(
          as.formula(paste(facetgroup,"~",facetgroup2)),
          axes = "all", 
          independent = "y",
          remove_labels = removelabs,
          scales = scales
        )
      
    } else{
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    remove_labels = removelabs,
                    scales = scales)
    } 
    
  } else{
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)), 
                  ncol=ncol, 
                  axes = "all", 
                  remove_labels = "x",
                  scales = scales)
    
  } 
  
  plt <- plt + theme_bw() + labs(x=xlab, y=ylab)
  # scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
  # scale_linetype_manual("",values=c(1,2,1,2)) +
  # scale_shape_manual("",values=c(16,17,16,17))
  
  if (roundx){
    plt <- plt + scale_x_continuous(labels=scaleFUN)
  }  
  
  if(bold.title.colums){
    x.face.style = "bold"
  }else{
    x.face.style = NULL
  }
  
  if(bold.title.rows){
    y.face.style = "bold"
  }else{
    y.face.style = NULL
  }
  
  if(scale.x){
    plt <- plt + scale_x_continuous(breaks = seq(0.2, 1, 0.1), limits = c(NA, NA)) 
  }
  
  plt <- plt + 
    # guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
    #        shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(
      plot.title = element_text(
        hjust = 0.5, 
        size = font.size, 
        face = "bold"
      ),
      text = element_text(
        size = font.size, 
        family = "Arial"
      ),
      strip.text.x = element_text(
        size = font.size,
        face = x.face.style
      ),
      strip.text.y = element_text(
        size =  font.size,
        face = y.face.style
      ),
      # axis.text = element_text(size=font.size - 4),
      axis.text.y = element_text(size = font.size- 4),
      axis.text.x = element_text(size = font.size - 8),
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # horizontal space
      panel.spacing.x = unit(1.25,"cm"),
      # vertical space
      panel.spacing.y = unit(1,"cm"),
      # Change axis line
      axis.line = element_line(colour = "black"),
      # facet_wrap without boxes
      strip.background = element_blank(),
      legend.key.size = unit(leg.size/2,"line"),
      legend.key.width = unit(leg.size,"line"),
      # legend.spacing.y = unit(12,"cm"),
      strip.placement = "outside",
      aspect.ratio = 1
    )
  
  if (!legend){
    plt <- plt +
      theme(legend.position = "none")
  }
  return(plt)  
} 

plt.xy.regressions5 <- function(
    dt,
    x,
    y,
    group,
    group.label = NULL,
    xlab,
    ylab = NULL,
    line.width = 1.5,
    font.size = 24,
    leg.size = 5,
    facetgroup,
    facetgroup2 = NULL,
    ncol,
    ylabeller = NULL,
    roundx = FALSE,
    scales = NULL,
    independenty = FALSE,
    removelabs = NULL,
    hideyaxis = FALSE,
    legend = TRUE,
    regressions = FALSE,
    regressions.hem = FALSE,
    regressions.color = "#545454",
    regressions.hem.gen = FALSE,
    bold.title.colums = FALSE,
    bold.title.rows = FALSE,
    scale.x = TRUE,
    regression.degree = 1,
    dt.optimal.degree = NULL,
    plt.corr.stats = FALSE
){
  
  reg.model <- y ~ poly(x, regression.degree, raw = TRUE)
  
  plt <- ggplot(
    dt,
    aes(
      x = .data[[x]],
      y = .data[[y]],
      colour = .data[[group]]
    )
  ) + 
    geom_point(alpha=0.6,key_glyph = large_points) +
    scale_color_viridis(
      option = "plasma",
      guide = guide_colorbar(
        barwidth = 2, 
        barheight = 16,
        ticks.colour = "black",
        ticks.linewidth = 2,
        title = group.label
      )
    )
  
  if (regressions){
    annot.size <- font.size/5
    if (is.null(dt.optimal.degree)){
      plt <- plt +
        stat_smooth(
          method = "lm",
          alpha = 0,
          formula = reg.model,
          linewidth = line.width,
          color = regressions.color
        )
    } else {
      dt2 <- dt %>%
        left_join(dt.optimal.degree, by = c(facetgroup,facetgroup2))
      fits <- dt2 %>%
        group_by_at(c(facetgroup,facetgroup2,"model")) %>%
        group_modify(~ {
          deg <- unique(.y$model)
          f <- as.formula(paste(y, "~ poly(",x,",", deg, ",raw = TRUE)"))
          model <- lm(f, data = .x)
          # Predict Y at those X values
          .x$yhat <- predict(model, .x)
          .x
        })
      fits <- as.data.frame(fits)
      plt <- plt +
        geom_line(
          data = fits,
          aes(
            y = yhat,
            x = .data[[x]],
          ),
          linewidth = line.width,
          color = regressions.color
        )
    }
    
    if (plt.corr.stats){
      annot.size <- font.size/5
      plt <- plt +
        stat_fit_glance(method = "cor.test",
                        label.x = "left",
                        label.y = 1,
                        color = regressions.color,
                        method.args = list(formula = ~ x + y),
                        aes(label=ifelse(..p.value..< 0.001,
                                         sprintf('italic(R)~"="~%.2f~~italic(p)~"***"',#< 0.001
                                                 after_stat(estimate)),
                                         ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                sprintf('italic(R)~"="~%.2f~~italic(p)~"**"', #< 0.01
                                                        after_stat(estimate)),
                                                ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                       sprintf('italic(R)~"="~%.2f~~italic(p)~"*"', #< 0.05
                                                               after_stat(estimate)),
                                                       sprintf('italic(R)~"="~%.2f~~italic(p)~"ns"', #> 0.05
                                                               after_stat(estimate)))))),
                        parse = TRUE,
                        size = annot.size)
    }
    
  }
  
  if (regressions.hem){
    if (length(regressions.color>1)){
      plt <- plt +
        new_scale_colour() +
        stat_smooth(
          aes(colour = Hemisphere),
          method = "lm",
          formula = reg.model,
          linewidth = line.width,
          alpha = 0
        ) +
        scale_color_manual(values = regressions.color)
      
    } else{
      plt <- plt +
        stat_smooth(
          aes(linetype = .data[["Hemisphere"]]),
          method = "lm",
          formula = reg.model,
          alpha = 0,
          linewidth = line.width,
          color = regressions.color,
          alpha = 0.5
        )
    }
  }
  
  if ((regressions.hem.gen) & ("hemgen" %in% names(dt))){
    a <- levels(dt[,"hemgen"])
    if(is.null(regressions.color) || (length(regressions.color) < 4)){
      regressions.color <- c()
      for (i in a){ 
        if (grepl("Left",i,fixed=TRUE)){ 
          regressions.color <- c(regressions.color,"#E41A1C")
        } else { 
          regressions.color <- c(regressions.color,"#377EB8")
        }
      }
    }
    
    custom.linetype <- c()
    for (i in a){ 
      if (grepl("Female",i,fixed=TRUE)){ 
        custom.linetype <- c(custom.linetype,1)
      } else { 
        custom.linetype <- c(custom.linetype,2)
      }
    }
    # 
    if (is.null(dt.optimal.degree)){
      plt <- plt +
        new_scale_colour()  +
        stat_smooth(
          inherit.aes = FALSE,
          data = dt,
          aes(
            x = .data[[x]],
            y = .data[[y]],
            colour = .data[["hemgen"]],
            alpha = 0.5,
            linetype = .data[["hemgen"]]#,
            # group = interaction(.data[["Hemisphere"]], .data[["Gender"]])
          ),
          method = "lm", 
          formula = reg.model, 
          se = FALSE,
          linewidth = line.width
        )
      
    } else {
      dt2 <- dt %>%
        left_join(dt.optimal.degree, by = c(facetgroup,facetgroup2))
      fits <- dt2 %>%
        group_by_at(c(facetgroup,facetgroup2,"hemgen","model")) %>%
        group_modify(~ {
          deg <- unique(.y$model)
          f <- as.formula(paste(y, "~ poly(",x,",", deg, ",raw = TRUE)"))
          model <- lm(f, data = .x)
          # Predict Y at those X values
          .x$yhat <- predict(model, .x)
          .x
        })
      fits <- as.data.frame(fits)
      plt <- plt +
        new_scale_colour()  +
        geom_line(
          data = fits,
          aes(
            y = yhat,
            x = .data[[x]],
            colour = .data[["hemgen"]],
            linetype = .data[["hemgen"]]
          ),
          linewidth = line.width,
          alpha = 0.75
        )
    }
      
    plt <- plt +
      scale_color_manual(
        "", 
        values = regressions.color
      ) +
      scale_linetype_manual(
        "", 
        values = custom.linetype
      ) +
      scale_alpha(guide = "none") +
      guides(
        color = guide_legend(
          label.theme = element_text(
            size = font.size - 10, 
            margin = margin(t=10,b=10)
          ), 
          keyheight = unit(2,"lines")
        ),
        linetype = guide_legend(
          label.theme = element_text(
            size = font.size - 10, 
            margin = margin(t=10,b=10)
          ), 
          keyheight = unit(2,"lines")
        )
      )
    
    if (plt.corr.stats){
      annot.size <- font.size/5.5
      
      a <- levels(dt[,"Hemisphere"])
      stats.color <- c()
      for (i in a){ 
        if (grepl("Left",i,fixed=TRUE)){ 
          stats.color <- c(stats.color,"#E41A1C")
        } else { 
          stats.color <- c(stats.color,"#377EB8")
        }
      }
      
      plt <- plt + new_scale_color() +
        stat_fit_glance(data = subset(dt, Gender == "Female" & Hemisphere == "Left"),
                        method = "cor.test",
                        label.x = "left",  
                        label.y = 1,
                        method.args = list(formula = ~ x + y),
                        aes(label=ifelse(..p.value..< 0.001,
                                         sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"***"',#< 0.001
                                                 after_stat(estimate)),
                                         ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"**"', #< 0.01
                                                        after_stat(estimate)),
                                                ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                       sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"*"', #< 0.05
                                                               after_stat(estimate)),
                                                       sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"ns"', #> 0.05
                                                               after_stat(estimate))))),
                            colour = .data[["Hemisphere"]]),
                        parse = TRUE,
                        size = annot.size)    +
        stat_fit_glance(data = subset(dt, Gender == "Female" & Hemisphere == "Right"),
                        method = "cor.test",
                        label.x = "left",
                        label.y = 0.93,
                        method.args = list(formula = ~ x + y),
                        aes(label=ifelse(..p.value..< 0.001,
                                         sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"***"',#< 0.001
                                                 after_stat(estimate)),
                                         ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"**"', #< 0.01
                                                        after_stat(estimate)),
                                                ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                       sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"*"', #< 0.05
                                                               after_stat(estimate)),
                                                       sprintf('italic(R)[F]~"="~%.2f~~italic(p)[F]~"ns"', #> 0.05
                                                               after_stat(estimate))))),
                            colour = .data[["Hemisphere"]]),
                        parse = TRUE,
                        size = annot.size)    +
        stat_fit_glance(data = subset(dt, Gender == "Male" & Hemisphere == "Left"),
                        method = "cor.test",
                        label.x = "left",
                        label.y = 0.86,
                        method.args = list(formula = ~ x + y),
                        aes(label=ifelse(..p.value..< 0.001,
                                         sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"***"',#< 0.001
                                                 after_stat(estimate)),
                                         ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"**"', #< 0.01
                                                        after_stat(estimate)),
                                                ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                       sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"*"', #< 0.05
                                                               after_stat(estimate)),
                                                       sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"ns"', #> 0.05
                                                               after_stat(estimate))))),
                            colour = .data[["Hemisphere"]]),
                        parse = TRUE,
                        size = annot.size)    +
        stat_fit_glance(data = subset(dt, Gender == "Male" & Hemisphere == "Right"),
                        method = "cor.test",
                        label.x = "left",
                        label.y = 0.79,
                        method.args = list(formula = ~ x + y),
                        aes(label=ifelse(..p.value..< 0.001,
                                         sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"***"',#< 0.001
                                                 after_stat(estimate)),
                                         ifelse(..p.value..>=0.001 & ..p.value..<0.01,
                                                sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"**"', #< 0.01
                                                        after_stat(estimate)),
                                                ifelse(..p.value..>=0.01 & ..p.value..<0.05,
                                                       sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"*"', #< 0.05
                                                               after_stat(estimate)),
                                                       sprintf('italic(R)[M]~"="~%.2f~~italic(p)[M]~"ns"', #> 0.05
                                                               after_stat(estimate))))),
                            colour = .data[["Hemisphere"]]),
                        parse = TRUE,
                        size = annot.size) +
        scale_color_manual(
          "", 
          values = stats.color
        )
    }
  } else if (!("hemgen" %in% names(dt))){
    print("Missing hemgen column in the dataframe")
  }
  
  if (is.null(scales)){
    scales = "free_y"
  } 
  
  if (is.null(removelabs)){
    removelabs = "x"
  } 
  
  if (!is.null(ylabeller)){
    plt <- plt +
      facet_wrap2(
        as.formula(paste("~",facetgroup)),
        ncol=7, axes = "all",
        remove_labels = "x",
        scales = "free_y",
        strip.position = "left",
        labeller = as_labeller(ylabeller, label_parsed)
      )
    ylab <- NULL
  } else if (!is.null(facetgroup2)){
    if (independenty){
      plt <- plt +
        facet_grid2(
          as.formula(paste(facetgroup,"~",facetgroup2)),
          axes = "all", 
          independent = "y",
          remove_labels = removelabs,
          scales = scales
        )
      
    } else{
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    remove_labels = removelabs,
                    scales = scales)
    } 
    
  } else{
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)), 
                  ncol=ncol, 
                  axes = "all", 
                  remove_labels = "x",
                  scales = scales)
    
  } 
  
  plt <- plt + theme_bw() + labs(x=xlab, y=ylab)
  # scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
  # scale_linetype_manual("",values=c(1,2,1,2)) +
  # scale_shape_manual("",values=c(16,17,16,17))
  
  if (roundx){
    plt <- plt + scale_x_continuous(labels=scaleFUN)
  }  
  
  if(bold.title.colums){
    x.face.style = "bold"
  }else{
    x.face.style = NULL
  }
  
  if(bold.title.rows){
    y.face.style = "bold"
  }else{
    y.face.style = NULL
  }
  
  if(scale.x){
    plt <- plt + scale_x_continuous(breaks = seq(0.2, 1, 0.1), limits = c(NA, NA)) 
  }
  
  plt <- plt + 
    # guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
    #        shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(
      plot.title = element_text(
        hjust = 0.5, 
        size = font.size, 
        face = "bold"
      ),
      text = element_text(
        size = font.size, 
        family = "Arial"
      ),
      strip.text.x = element_text(
        size = font.size,
        face = x.face.style
      ),
      strip.text.y = element_text(
        size =  font.size,
        face = y.face.style
      ),
      # axis.text = element_text(size=font.size - 4),
      axis.text.y = element_text(size = font.size- 4),
      axis.text.x = element_text(size = font.size - 8),
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # horizontal space
      panel.spacing.x = unit(1.25,"cm"),
      # vertical space
      panel.spacing.y = unit(1,"cm"),
      # Change axis line
      axis.line = element_line(colour = "black"),
      # facet_wrap without boxes
      strip.background = element_blank(),
      legend.key.size = unit(leg.size/2,"line"),
      legend.key.width = unit(leg.size,"line"),
      # legend.spacing.y = unit(12,"cm"),
      strip.placement = "outside",
      aspect.ratio = 1
    )
  
  if (!legend){
    plt <- plt +
      theme(legend.position = "none")
  }
  return(plt)  
} 

models.BIC <- function(dataset,x,y,R_bootstrap){
  models.data <- dataset %>%
    group_by(Hemisphere,Gender) %>%
    group_modify(
      ~models.boots(
        dataset=.x,
        x=x,
        y=y,
        R_bootstrap = 1000
      )
    )
  
  BIC <- reshape2::melt(
    models.data,
    measure.vars=c(
      "BIClinear",
      "BICquadratic",
      "BICcubic"
    ),
    variable.name="model",
    value.name="y"
  )
  
  BIC$model <- mapvalues(
    BIC$model,
    from=c(
      "BIClinear",
      "BICquadratic",
      "BICcubic"
    ),
    to = c(
      "Linear",
      "Quadratic",
      "Cubic"
    )
  )
  
  BIC <- summarySE(
    data=BIC,
    measurevar="y",
    groupvars=c(
      "Hemisphere",
      "Gender",
      "model"
    )
  )
  
  BIC$vol <- y
  return(BIC)
}

models.BIC.V2 <- function(
    dataset,
    x,
    y,
    groupby = c("Hemisphere","Gender"),
    R_bootstrap = 1000){
  models.data <- dataset %>%
    group_by_at(groupby) %>%
    group_modify(
      ~models.boots(
        dataset=.x,
        x=x,
        y=y,
        R_bootstrap = R_bootstrap
      )
    )
  
  BIC <- reshape2::melt(
    models.data,
    measure.vars=c(
      "BIClinear",
      "BICquadratic",
      "BICcubic"
    ),
    variable.name="model",
    value.name="y"
  )
  
  BIC$model <- mapvalues(
    BIC$model,
    from=c(
      "BIClinear",
      "BICquadratic",
      "BICcubic"
    ),
    to = c(
      "Linear",
      "Quadratic",
      "Cubic"
    )
  )
  
  BIC <- summarySE(
    data=BIC,
    measurevar="y",
    groupvars=c(
      groupby,
      "model"
    )
  )
  
  BIC$x <- x
  BIC$vol <- y
  return(BIC)
}

plt.BIC <- function(
    dataset,
    volume,
    xlab,
    ylab,
    title,
    legend = FALSE,
    hide.x.axis.female = TRUE,
    hide.x.axis.male = TRUE
){
  dataset$model <- mapvalues(
    dataset$model,
    from=c(
      "Linear",
      "Quadratic",
      "Cubic"
    ),
    to = c("1","2","3")
  )
  
  dataset <- subset(dataset,vol == volume)
  
  optimal.BIC <- dataset %>%
    group_by(Hemisphere,Gender) %>%
    group_modify(~diff.BIC(data=.x,
                           variable="y"))
  optimal.BIC$model <- as.factor(optimal.BIC$model)
  
  female <- subset(dataset,Gender == "Female")
  male <- subset(dataset,Gender == "Male")
  
  optimal.female <- subset(optimal.BIC,Gender == "Female")
  optimal.male <- subset(optimal.BIC,Gender == "Male")
  
  
  plt.female <- ggplot(
    data=female,
    aes(
      x=model,y=y,
      colour=Hemisphere,
      fill=Hemisphere,
      group=Hemisphere
    )
    ) + 
    geom_vline(data=optimal.female,aes(xintercept = model),color="red")+
    geom_ribbon(aes(ymin=(y-ci),ymax=(y+ci)),colour=NA,alpha=0.25) +
    geom_line(linetype=1,alpha=1,size=1) +
    geom_point(shape=16,size=2.25,key_glyph = large_points) +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_color_brewer(palette="Set1") +  scale_fill_brewer(palette="Set1") +
    scale_y_continuous(breaks= pretty_breaks()) +
    facet_wrap(~Hemisphere,scales="free_y") +
    theme(plot.title = element_text(hjust = 0.5, 
                                    vjust=0.1, 
                                    size = 24, 
                                    face="bold"),
          text = element_text(size=24, family = "Arial"),
          strip.text.x = element_text(size=24),
          axis.text = element_text(size=20),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank(),
          legend.key.size = unit(2.5,"line"),
          aspect.ratio=1)
  
  plt.male <- ggplot(data=male,aes(x=model,y=y,
                                   colour=Hemisphere,
                                   fill=Hemisphere,
                                   group=Hemisphere)) + 
    geom_vline(data=optimal.male,aes(xintercept = model),color="red")+
    geom_ribbon(aes(ymin=(y-ci),ymax=(y+ci)),colour=NA,alpha=0.25) +
    geom_line(linetype=2,alpha=1,size=1) +
    geom_point(shape=17,size=2.25,key_glyph = large_points) +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_color_brewer(palette="Set1") +  scale_fill_brewer(palette="Set1") +
    scale_y_continuous(breaks= pretty_breaks()) +
    facet_wrap(~Hemisphere,scales="free_y") +
    theme(plot.title = element_text(hjust = 0.5, 
                                    vjust=0.1, 
                                    size = 24, 
                                    face="bold"),
          text = element_text(size=24, family = "Arial"),
          strip.text.x = element_text(size=24),
          axis.text = element_text(size=20),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank(),
          legend.key.size = unit(2.5,"line"),
          aspect.ratio=1)
  
  if(missing(title)) {
    plt.female <- plt.female + ggtitle(paste0(volume,' | Female'))
    plt.male <- plt.male + ggtitle(paste0(volume,' | Male'))
  } else {
    plt.female <- plt.female + ggtitle(paste0(title,' | Female'))
    plt.male <- plt.male + ggtitle(paste0(title,' | Male'))
  }
  
  if (!legend) {
    plt.female <- plt.female + theme(legend.position = "none")
    plt.male <- plt.male + theme(legend.position = "none")
  }
  
  if (hide.x.axis.female) {
    plt.female <- plt.female + 
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"))
  }
  
  if (hide.x.axis.male) {
    plt.male <- plt.male + 
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"))
  }
  
  return(list(female=plt.female,male=plt.male))
}

plt.BIC.V2 <- function(
    dataset,
    groupby = c("Hemisphere","Gender"),
    xlab="",
    ylab="",
    title="",
    bold.title.colums = FALSE,
    legend = FALSE,
    plt.size = 24
){
  dataset$model <- mapvalues(
    dataset$model,
    from=c(
      "Linear",
      "Quadratic",
      "Cubic"
    ),
    to = c("1","2","3")
  )
  
  # plot BIC tracts
  optimal.BIC <- dataset %>%
    group_by_at(groupby) %>%
    group_modify(~diff.BIC(data=.x,
                           variable="y"))
  optimal.BIC$model <- as.factor(optimal.BIC$model)
  
  if (length(groupby) == 1){
    facet.formula <- as.formula(
      paste(
        groupby[1],
        "~."
      )
    )
  } else {
    facet.formula <- as.formula(
      paste(
        groupby[1],
        "~",
        groupby[2]
      )
    )
  }
  
  if(bold.title.colums){
    x.face.style = "bold"
  }else{
    x.face.style = NULL
  }
  
  plt <- ggplot(
    data = dataset,
    aes(
      x = model,
      y = y, 
      group = 1
    )
  ) + 
    geom_vline(data=optimal.BIC, aes(xintercept = model), color="red")+
    geom_ribbon(aes(ymin=(y-ci),ymax=(y+ci)),colour=NA,alpha=0.25) +
    geom_line(linetype=1,alpha=1,size=1) +
    geom_point(shape=16,size=2.25,key_glyph = large_points) +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_color_brewer(palette="Set1") +  scale_fill_brewer(palette="Set1") +
    scale_y_continuous(breaks= pretty_breaks()) +
    facet_grid2(facet.formula,
                axes = "all", 
                independent = "y",
                remove_labels = "x",
                scales = "free_y") +
    theme(plot.title = element_text(hjust = 0.5, 
                                    vjust=0.1, 
                                    size = 30, 
                                    face="bold"),
          text = element_text(size=plt.size, family = "Arial"),
          strip.text.x = element_text(
            size=plt.size,
            face = x.face.style
          ),
          axis.text = element_text(
            size=(plt.size-4)
          ),
          panel.spacing.y = unit(1,"cm"),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank(),
          legend.key.size = unit(2.5,"line"),
          aspect.ratio=1) +
    ggtitle(title)
  

  return(list(plt = plt, optimal.BIC  = as.data.frame(optimal.BIC)))
}

make_title <- function(base_expr, label) {
  ggtitle(
    bquote(
      bold(.(base_expr) ~ .(label))
    )
  )
}

perform.ICV.correction <- function(
    dt,
    volume = "MD",
    adjustBy = "ICV",
    byHemisphere = TRUE,
    byGender = TRUE
){
  if(byHemisphere & !byGender){
    print("1")
    dt.L <- subset(dt, Hemisphere == "Left")
    dt.R <- subset(dt, Hemisphere == "Right")
    
    dt.R[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.R[,volume], 
      dt.R[,adjustBy]
    )
    dt.L[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.L[,volume], 
      dt.L[,adjustBy]
    )
    
    dt <- rbind(dt.L,dt.R)
    return(dt)
    
  } else if (byGender & !byHemisphere){
    print("2")
    dt.F <- subset(dt, Gender == "Female")
    dt.M <- subset(dt, Gender == "Male")
    
    dt.F[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.F[,volume], 
      dt.F[,adjustBy]
    )
    dt.M[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.M[,volume], 
      dt.M[,adjustBy]
    )
    
    dt <- rbind(dt.F,dt.M)
    return(dt)
    
  } else if (byHemisphere & byGender){
    print("3")
    dt.FL <- subset(dt, Gender == "Female" & Hemisphere == "Left")
    dt.FR <- subset(dt, Gender == "Female" & Hemisphere == "Right")
    dt.ML <- subset(dt, Gender == "Male" & Hemisphere == "Left")
    dt.MR <- subset(dt, Gender == "Male" & Hemisphere == "Right")
    print(unique(dt.MR$Gender))
    print(unique(dt.MR$Hemisphere))
    
    dt.FL[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.FL[,volume], 
      dt.FL[,adjustBy]
    )
    dt.FR[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.FR[,volume], 
      dt.FR[,adjustBy]
    )
    
    dt.ML[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.ML[,volume], 
      dt.ML[,adjustBy]
    )
    dt.MR[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt.MR[,volume], 
      dt.MR[,adjustBy]
    )
    
    dt <- rbind(dt.FL, dt.FR, dt.ML, dt.MR)
    return(dt)
    
  } else {
    print("4")
    dt[,paste0(volume,"adj",adjustBy)] <- var_correction(
      dt[,volume], 
      dt[,adjustBy]
    )
    return(dt)
  }
  
}

plt.agegroups <- function(dt,
                          metric,
                          x,
                          y,
                          xlab,
                          ylab,
                          group = NULL,
                          line.width = 1.5,
                          point.size = 1,
                          ncol,
                          ylabeller = NULL,
                          facetgroup,
                          facetgroup2 = NULL,
                          font.size = 24,
                          leg.size = 5,
                          scales = NULL,
                          independenty = FALSE,
                          removelabs = NULL,
                          legend = TRUE,
                          y.axis.scientific = FALSE,
                          title = NULL,
                          figname = NULL) {
  
  if(is.null(group)) {
    group <- "hemagegroup"
  }
  
  dt.agegroup.mean <- dt %>%
    group_by_at(c("part",
                  "agegroup",
                  facetgroup,
                  facetgroup2,
                  "Hemisphere",
                  "hemgen",
                  group,
                  "meas")) %>%
    summarise_at(vars(y), list(value = mean))
  
  plt <- ggplot(subset(dt.agegroup.mean, meas == metric),
                aes_string(x = x,
                           y = "value",
                           shape = group,
                           colour = group,
                           linetype = group,
                           group = group)) + 
    geom_line(size = line.width) +
    geom_point(alpha = 0.5, size = point.size, key_glyph = large_points)
  
  if (is.null(scales)){
    scales = "free_y"
  } 
  
  if (is.null(removelabs)){
    removelabs = "x"
  } 
  
  if (!is.null(ylabeller)){
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)),
                  ncol=7, axes = "all",
                  remove_labels = "x",
                  scales = "free_y",
                  strip.position = "left",
                  labeller = as_labeller(ylabeller, label_parsed)
      )
    ylab <- NULL
  } else if (!is.null(facetgroup2)){
    if (independenty){
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    independent = "y",
                    remove_labels = removelabs,
                    scales = scales)
      
    } else{
      plt <- plt +
        facet_grid2(as.formula(paste(facetgroup,"~",facetgroup2)),
                    axes = "all", 
                    remove_labels = removelabs,
                    scales = scales)
    } 
    
  } else{
    plt <- plt +
      facet_wrap2(as.formula(paste("~",facetgroup)), 
                  ncol = ncol, 
                  axes = "all", 
                  remove_labels = "x",
                  scales = scales)
    
  } 
  
  plt <- plt + 
    labs(x=xlab, y=ylab) +
    scale_colour_manual("",values=c("#E41A1C","#E41A1C","#E41A1C","#377EB8","#377EB8","#377EB8")) +
    scale_linetype_manual("",values=c(1,2,3,1,2,3)) +
    scale_shape_manual("",values=c(16,17,18,16,17,18)) +
    theme_bw()
  
  plt <- plt +
    guides(linetype = guide_legend(override.aes = list(alpha = 1, fill = c(NA, NA, NA, NA, NA, NA))),
           shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(
      plot.title = element_text(hjust = 0.5, 
                                size = font.size, 
                                face="bold"),
      text = element_text(size=font.size, family = "Arial"),
      strip.text.x = element_text(size=font.size,
                                  face="bold"),
      strip.text.y = element_text(size=font.size),
      axis.text = element_text(size=font.size - 4),
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "black"),
      # facet_wrap without boxes
      strip.background = element_blank(),
      legend.key.size = unit(leg.size/2,"line"),
      legend.key.width = unit(leg.size,"line"),
      strip.placement = "outside",
      panel.spacing.x = unit(2,"lines"),
      aspect.ratio = 1)
  
  if(!is.null(title)){
    plt <- plt +
      labs(title = title)
  }
  
  if (!legend){
    plt <- plt +
      theme(legend.position = "none")
  }
  
  if (y.axis.scientific) {
    plt <- plt + 
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
  }
  
  if(!is.null(figname)){
    height <- length(unique(dt[,facetgroup]))*2
    width <- length(unique(dt[,facetgroup2]))*3
    print(width)
    ggsave(file=figname,
           height = height,# 4 each plot 
           width = width,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 600,
           device = "png",
           plt)
  }
  
  return(plt)  
  
  # facet_wrap(as.formula(paste("Gender",facetgroup,sep = "~")), ncol = ncol, 
  #            scales = "free_x") +
  # labs(x="Part", y="FA") + 
  # scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
  # scale_linetype_manual("",values=c(1,2,1,2)) +
  # scale_shape_manual("",values=c(16,17,16,17)) +
  # theme_bw()
}


process.data <- function(dat,
                         basedir,
                         DB.file,
                         graythalamicpath,
                         rdata.file,
                         correction){
  dat$tractlabel <- sub('.*_','',dat$tract)
  dat[grepl('Left', dat$tract, fixed = TRUE),"Hemisphere"] <- "Left"
  dat[grepl('Right', dat$tract, fixed = TRUE),"Hemisphere"] <- "Right"
  
  dat[dat$tractlabel == 'V1V2','tractlabel'] <- 'OR' 
  
  dat[dat$tractlabel == 'OR','fulltractlabel'] <- 'Optic Radiation'
  dat[dat$tractlabel == 'MR','fulltractlabel']   <- 'Motor Radiation'
  dat[dat$tractlabel == 'DT','fulltractlabel']   <- 'Dentatothalamic'
  dat[dat$tractlabel == 'AR','fulltractlabel']   <- 'Acoustic Radiation'
  dat[dat$tractlabel == 'SR','fulltractlabel']   <- 'Somatosensory Radiation'

  # DT starts from cortex to thalamus, but it should be from thalamus to cortex 
  # to be consistent with the other tracts, so here I flip with 'rev' the values
  temp <- dat[dat$tractlabel == "DT",]
  for (l in 1:nrow(temp)){
    temp$val[[l]] <- rev(temp$val[[l]])
  }
  dat[dat$tractlabel == "DT",] <- temp
  
  # decode the TCK names...
  projPath <- file.path(file.path(basedir,"DATA","macroresults"),
                        "MINI",
                        paste("analysis",'HO',sep="-"))
  MD.tractparams <- read.csv(file.path(projPath,'tractparams.csv'))
  MD.TCK.dictionary <- MD.tractparams[,c('label','roi2')] 
  
  
  for (roi2 in MD.tractparams$roi2){
    label <- as.character(MD.dictionary[sub("^[^_]*_","",roi2)])
    tract <- MD.tractparams[MD.tractparams$roi2 == roi2,'label'] 
    if (strsplit(roi2,split="_")[[1]][1] == 'L'){
      dat[dat$tract == tract,'Hemisphere'] <- 'Left' 
      dat[dat$tract == tract,'fulltractlabel'] <- label
    } else if (strsplit(roi2,split="_")[[1]][1] == 'R'){
      dat[dat$tract == tract,'Hemisphere'] <- 'Right' 
      dat[dat$tract == tract,'fulltractlabel'] <- label
    } 
  }
  
  DB_devtraj <- read.csv(file.path(basedir,"DATA",DB.file),sep=',')
  dt <- dat
  dt <- dt[dt$sub %in% DB_devtraj$FS_ID,] # only process those that are in the DB
  # subs <- unique(dt$sub)
  projects <- unique(dt$project)
  
  for (proj in projects){
    for (sub in unique(dt[dt$project == proj ,'sub'])){
      aux <- subset(DB_devtraj,PROYECTO == proj)
      dt[dt$sub == sub & dt$project == proj,'oldFS_ID'] <- 
        aux[aux$FS_ID == sub,'oldFS_ID']
      dt[dt$sub == sub & dt$project == proj,'AGE'] <- 
        aux[aux$FS_ID == sub,'Age.good']
      dt[dt$sub == sub & dt$project == proj,'Gender'] <- 
        aux[aux$FS_ID == sub,'Gender.good']
    }
  }
  print("done")
  rm(aux)
  
  # convert strings to float format, consider comma as decimal delimiter
  dt$AGE <- as.numeric(sub(",", ".", dt$AGE, fixed = TRUE))
  dt$hemgen <- paste0(dt$Hemisphere," - ",dt$Gender,"  ")
  dt$hemgen <- as.factor(dt$hemgen)
  
  thalsurface <- c()
  body <- c()
  corticalsurface <- c()
  allprofile <- c()
  AGE <- c()
  Gender <- c()
  
  for (r in 1:nrow(dt)){
    # thalsurface <- append(thalsurface,mean(dt$val[[r]][1:20]))
    # body <- append(body,mean(dt$val[[r]][21:80]))
    # corticalsurface <- append(corticalsurface,mean(dt$val[[r]][81:100]))
    thalsurface <- append(thalsurface,mean(dt$val[[r]][12:37]))
    body <- append(body,mean(dt$val[[r]][38:63]))
    corticalsurface <- append(corticalsurface,mean(dt$val[[r]][64:89]))
    allprofile <- append(allprofile,mean(dt$val[[r]]))
  }
  
  dt <- cbind(dt,thalsurface,body,corticalsurface,allprofile)
  
  feat <- c("PROYECTO","ID","AGE","Gender","ICV","CSF","TBV")
  
  #-----------------------------------------------------------------------------
  # Read data without ICV correction
  #-----------------------------------------------------------------------------
  ATLAS_clean <- ATLAS_2_dfHem(path=file.path(graythalamicpath,"data",rdata.file),
                               feat2melt = c("Left.Thalamus.Proper",
                                             "Right.Thalamus.Proper",
                                             "Left.GrayM", 
                                             "Right.GrayM",
                                             "Left.CortexGrayM",
                                             "Right.CortexGrayM",
                                             "Left.SubCortexGrayM",
                                             "Right.SubCortexGrayM",
                                             "Left.WhiteM",
                                             "Right.WhiteM",
                                             "Left.mean.thickness",
                                             "Right.mean.thickness"),
                               feat=feat,
                               deleteWT = "no",
                               correction = correction,
                               scaling = "no")
  ATLAS_clean$hemgen <- paste0(ATLAS_clean$Hemisphere," - ", 
                               ATLAS_clean$Gender,"  ")
  ATLAS_clean$hemgen <- as.factor(ATLAS_clean$hemgen)
  
  feat <- names(ATLAS_clean)[!(names(ATLAS_clean) %in% 
                                 c('PROYECTO',
                                   'Hemisphere',
                                   'ID',
                                   'AGE',
                                   'Gender',
                                   'hemgen'))]
  dt[,feat] <- NA
  
  for (proj in unique(dt$project)){
    print(proj)
    subs <- unique(dt[dt$project == proj,'oldFS_ID'])
    for (sub in subs){
      print(sub)
      dt[dt$oldFS_ID == sub & dt$project == proj & dt$Hemisphere == "Right",feat] <-  
        ATLAS_clean[ATLAS_clean$ID == sub & ATLAS_clean$PROYECTO == proj & 
                      ATLAS_clean$Hemisphere == "Right",feat]
      
      dt[dt$oldFS_ID == sub & dt$project == proj & dt$Hemisphere == "Left",feat] <-  
        ATLAS_clean[ATLAS_clean$ID == sub & ATLAS_clean$PROYECTO == proj & 
                      ATLAS_clean$Hemisphere == "Left",feat]
    }
  }  
  return(dt)
} 

join.white.gray.projects <- function(dat, 
                                     basedir, 
                                     DB.file, 
                                     graythalamicpath,
                                     rdata.file,
                                     correction){
  DB_devtraj <- read.csv(file.path(basedir,"DATA",DB.file),sep=',')
  dt <- dat
  dt <- dt[dt$sub %in% DB_devtraj$FS_ID,] # only process those that are in the DB
  
  subs <- unique(dt$sub)
  projects <- unique(dt$project)
  
  for (proj in projects){
    for (sub in subs){
      aux <- subset(DB_devtraj,PROYECTO == proj)
      dt[dt$sub == sub & dt$project == proj,'oldFS_ID'] <- 
        aux[aux$FS_ID == sub,'oldFS_ID']
      dt[dt$sub == sub & dt$project == proj,'AGE'] <- 
        aux[aux$FS_ID == sub,'Age.good']
      dt[dt$sub == sub & dt$project == proj,'Gender'] <- 
        aux[aux$FS_ID == sub,'Gender.good']
    }
  }
  
  rm(aux)
  
  # convert strings to float format, consider comma as decimal delimiter
  dt$AGE <- as.numeric(sub(",", ".", dt$AGE, fixed = TRUE))
  dt$hemgen <- paste0(dt$Hemisphere," - ",dt$Gender,"  ")
  dt$hemgen <- as.factor(dt$hemgen)
  
  thalsurface <- c()
  body <- c()
  corticalsurface <- c()
  allprofile <- c()
  AGE <- c()
  Gender <- c()
  
  for (r in 1:nrow(dt)){
    # thalsurface <- append(thalsurface,mean(dt$val[[r]][1:20]))
    # body <- append(body,mean(dt$val[[r]][21:80]))
    # corticalsurface <- append(corticalsurface,mean(dt$val[[r]][81:100]))
    thalsurface <- append(thalsurface,mean(dt$val[[r]][12:37]))
    body <- append(body,mean(dt$val[[r]][38:63]))
    corticalsurface <- append(corticalsurface,mean(dt$val[[r]][64:89]))
    allprofile <- append(allprofile,mean(dt$val[[r]]))
  }
  
  dt <- cbind(dt,thalsurface,body,corticalsurface,allprofile)
  
  feat <- c("PROYECTO","ID","AGE","Gender","ICV","CSF","TBV")
  
  #-----------------------------------------------------------------------------
  # Read data without ICV correction
  #-----------------------------------------------------------------------------
  ATLAS_clean <- ATLAS_2_dfHem(path=file.path(graythalamicpath,"data",rdata.file),
                               feat2melt = c("Left.Thalamus.Proper",
                                             "Right.Thalamus.Proper",
                                             "Left.GrayM", 
                                             "Right.GrayM",
                                             "Left.CortexGrayM",
                                             "Right.CortexGrayM",
                                             "Left.SubCortexGrayM",
                                             "Right.SubCortexGrayM",
                                             "Left.WhiteM",
                                             "Right.WhiteM",
                                             "Left.mean.thickness",
                                             "Right.mean.thickness"),
                               feat=feat,
                               deleteWT = "no",
                               correction = correction,
                               scaling = "no")
  ATLAS_clean$hemgen <- paste0(ATLAS_clean$Hemisphere," - ", 
                               ATLAS_clean$Gender,"  ")
  ATLAS_clean$hemgen <- as.factor(ATLAS_clean$hemgen)
  
  feat <- names(ATLAS_clean)[!(names(ATLAS_clean) %in% 
                                 c('PROYECTO',
                                   'Hemisphere',
                                   'ID',
                                   'AGE',
                                   'Gender',
                                   'hemgen'))]
  dt[,feat] <- NA
  
  for (proj in unique(dt$project)){
    print(proj)
    subs <- unique(dt[dt$project == proj,'oldFS_ID'])
    for (sub in subs){
      print(sub)
      dt[dt$oldFS_ID == sub & dt$project == proj & dt$Hemisphere == "Right",feat] <-  
        ATLAS_clean[ATLAS_clean$ID == sub & ATLAS_clean$PROYECTO == proj & 
                      ATLAS_clean$Hemisphere == "Right",feat]
      
      dt[dt$oldFS_ID == sub & dt$project == proj & dt$Hemisphere == "Left",feat] <-  
        ATLAS_clean[ATLAS_clean$ID == sub & ATLAS_clean$PROYECTO == proj & 
                      ATLAS_clean$Hemisphere == "Left",feat]
    }
  }
  return(dt)  
} 



mean.bootstraped.linear.coeff <- function(ATLAS,
                                      x,
                                      y){
  
  females <- subset(ATLAS,Gender == "Female")
  left.females <- subset(females,Hemisphere=="Left")
  right.females <- subset(females,Hemisphere=="Right")
  males <- subset(ATLAS,Gender == "Male")
  left.males <- subset(males,Hemisphere=="Left")
  right.males <- subset(males,Hemisphere=="Right")
  
  #-----------------------------------------------------------------------------
  # Bootstrap - adjusted Whole thalamus ~ Age
  #-----------------------------------------------------------------------------
  set.seed(100)
  left.females.boots <- boots_slope(left.females[,x],
                                    left.females[,y],
                                    1000)
  set.seed(100)
  right.females.boots <- boots_slope(right.females[,x],
                                     right.females[,y],
                                     1000)
  set.seed(100)
  left.males.boots <- boots_slope(left.males[,x],
                                  left.males[,y],
                                  1000)
  set.seed(100)
  right.males.boots <- boots_slope(right.males[,x],
                                   right.males[,y],
                                   1000)
  
  females.hemispheres <- dplyr::bind_rows(list(Left=left.females.boots,
                                               Right=right.females.boots),
                                          .id = 'Hemisphere')
  females.hemispheres$Gender <- 'Female'
  
  males.hemispheres <- dplyr::bind_rows(list(Left=left.males.boots,
                                             Right=right.males.boots), 
                                        .id = 'Hemisphere')
  males.hemispheres$Gender <- 'Male'
  
  y.x <- rbind(females.hemispheres,males.hemispheres)
  
  y.x <- aggregate(VLa.linear.coeff[,c("intercept","slopes")],
                   list(VLa.linear.coeff$Hemisphere,
                        VLa.linear.coeff$Gender),mean)
  y.x <- setNames(y.x,
                  c("Hemisphere",
                    "Gender",
                    "intercept",
                    "slopes"))
  
  y.x$hemgen <- paste0(y.x$Hemisphere," - ", 
                       y.x$Gender,"  ")
  y.x$hemgen <- as.factor(y.x$hemgen)
  return(y.x)
  
}  

plt.models2 <- function(ATLAS,
                       x,
                       y,
                       model.fem.left,
                       model.fem.right,
                       model.male.left,
                       model.male.right,
                       xlab,
                       ylab,
                       line.width,
                       title,
                       legend = TRUE,
                       hide.x.axis = FALSE,
                       toWT = FALSE,
                       tag,
                       tag.size,
                       font.size,
                       linear.coefficients = NULL) {
  
  if (missing(xlab)){
    xlab="Age"
  }
  
  if (missing(x)){
    x="AGE"
  }
  
  if (missing(line.width)){
    line.width = 0.75
  }
  
  if(toWT) {
    ATLAS$yfrac_WT <- (ATLAS[,y]/ ATLAS$Whole_thalamus)*100
    ylab = bquote(frac(Vol[.(y)], Vol['Thalamus']) ~ '[%]')
    y = "yfrac_WT"
  }
  
  if (missing(font.size)) {
    font.size <- 24
  }
  
  plt.vol <- ggplot(ATLAS,aes_string(x=x,
                                     y=y,
                                     colour="hemgen",
                                     shape="hemgen",
                                     linetype="hemgen")) + 
    geom_point(alpha=0.5,key_glyph = large_points) 
  
  if (!is.null(linear.coefficients)){
    plt.vol <- plt.vol + geom_abline(data=linear.coefficients,
                                     aes(intercept = intercept, 
                                         slope = slopes,
                                         color = "hemgen"),
                                         size = line.width)
  } else{
    plt.vol <- plt.vol +
      # females-left
      stat_smooth(data=subset(ATLAS,Hemisphere == "Left" & Gender=="Female"),
                  method = "lm", formula = model.fem.left, 
                  alpha=0, size = line.width) +
      # females-right
      stat_smooth(data=subset(ATLAS,Hemisphere == "Right" & Gender=="Female"),
                  method = "lm", formula = model.fem.right, 
                  alpha=0, size = line.width) +
      # males-left
      stat_smooth(data=subset(ATLAS,Hemisphere == "Left" & Gender=="Male"),
                  method = "lm", formula = model.male.left, 
                  alpha=0, size = line.width) +
      # males-right
      stat_smooth(data=subset(ATLAS,Hemisphere == "Right" & Gender=="Male"),
                  method = "lm", formula = model.male.right, 
                  alpha=0, size = line.width) +
      theme_bw() + labs(x=xlab, y=ylab)
  }  
  
  
  plt.vol <- plt.vol +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_colour_manual("",values=c("#E41A1C","#E41A1C","#377EB8","#377EB8")) +
    scale_linetype_manual("",values=c(1,2,1,2)) +
    scale_shape_manual("",values=c(16,17,16,17)) +
    guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
           shape=guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(plot.title = element_text(hjust = 0.5, size = font.size),
          text = element_text(size=font.size, family = "Arial"),
          strip.text.x = element_text(size = font.size),
          axis.text = element_text(size = font.size - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank(),
          legend.key.size = unit(2.5,"line"),
          legend.key.width = unit(5,"line"),
          aspect.ratio=1)
  
  if (!missing(title)){
    plt.vol <- plt.vol + ggtitle(title)
  }
  
  if (!legend) {
    plt.vol <- plt.vol + theme(legend.position = "none")
  }
  
  if (hide.x.axis) {
    plt.vol <- plt.vol + theme(axis.ticks.x = element_blank(),
                               axis.text.x = element_text(size=20, 
                                                          colour = "white"),
                               axis.title.x = element_text(colour = "white"))
  }
  
  if (!missing(tag)) {
    plt.vol <- plt.vol + labs(tag = tag) 
    if(!missing(tag.size)) {
      plt.vol <- plt.vol + theme(plot.tag = element_text(size = tag.size))
    } else {
      plt.vol <- plt.vol + theme(plot.tag = element_text(size = 30))
    }
  }
  
  return(plt.vol)
}

get.MD.data <- function(dt, 
                        parts.names = NULL, 
                        clean.outliers = FALSE){
  if (clean.outliers){
    dt <- subset(dt, outlier == FALSE)
  } 
  
  dt.MD <- dt[dt$fulltractlabel %in% MD.labels,]
  dt.MD[dt.MD$fulltractlabel %in% dPFC.label,"PFCgroup"] <- "dPFC"
  dt.MD[dt.MD$fulltractlabel %in% mPFC.label,"PFCgroup"] <- "mPFC"
  dt.MD[dt.MD$fulltractlabel %in% OrbPoPFC.label,"PFCgroup"] <- "OrbPoPFC"
  dt.MD[dt.MD$fulltractlabel %in% IFC.label,"PFCgroup"] <- "IFC"
  
  # Now get the meas mean for each group
  dt.MD.mean <- aggregate(dt.MD[,c("thalsurface",
                                   "body",
                                   "corticalsurface",
                                   "allprofile")],
                          list(dt.MD$sub,
                               dt.MD$meas,
                               dt.MD$hemgen,
                               dt.MD$Hemisphere,
                               dt.MD$Gender,
                               dt.MD$PFCgroup,
                               dt.MD$MDl,
                               dt.MD$MDm,
                               dt.MD$Whole_thalamus,
                               dt.MD$AGE,
                               dt.MD$agegroup,
                               dt.MD$hemagegroup),
                               # dt.MD$streamlines_count),
                          mean)
  
  dt.MD.mean <- setNames(dt.MD.mean,
                         c("sub",
                           "meas",
                           "hemgen",
                           "Hemisphere",
                           "Gender",
                           "PFCgroup",
                           "MDl",
                           "MDm",
                           "Whole_thalamus",
                           "AGE",
                           "agegroup",
                           "hemagegroup",
                           # "streamlines_count",
                           "thalsurface",
                           "body",
                           "corticalsurface",
                           "allprofile"))
  
  dt.MD.mean$MD <- dt.MD.mean$MDl + dt.MD.mean$MDm
  
  library(reshape2)
  feat <- names(dt.MD.mean) [!(names(dt.MD.mean) %in% 
                                 c("thalsurface",
                                   "body",
                                   "corticalsurface"))]
  dt.MD.mean.parts <- melt(dt.MD.mean, 
                           id.var = feat,
                           variable.name='part')
  
  if (!is.null(parts.names)){
    levels(dt.MD.mean.parts$part) <- parts.names
    
  } else{
    levels(dt.MD.mean.parts$part) <- c('Thalamic proximity',
                                       'Tract body',
                                       'Cortical proximity')
  } 
  
  dt.MD.mean.parts$meas2vol <- dt.MD.mean.parts$value / 
    (dt.MD.mean.parts$MD / dt.MD.mean.parts$Whole_thalamus)
  # dt.MD.mean.parts$meas2streamlines <- dt.MD.mean.parts$value /
  #   # (dt.MD.mean.parts$MD / dt.MD.mean.parts$streamlines_count)
  #   dt.MD.mean.parts$streamlines_count
  
  dt.MD.mean.parts <- transform(
    dt.MD.mean.parts,
    PFCgroup = factor(
      PFCgroup,
      levels = c(
        "dPFC",
        "mPFC",
        "OrbPoPFC",
        "IFC"
      )
    )
  )
  return(dt.MD.mean.parts)
} 

get.MD.data2 <- function(
    dt, 
    parts.names = NULL,
    agegroup.levels = c(
      "Young",
      "Adult",
      "Elder"
    ), 
    clean.outliers = FALSE
    ){
  if (clean.outliers){
    dt <- subset(dt, outlier == FALSE)
  } 
  
  # dt <- dt %>%
  #   group_by(sub, group, meas) %>%
  #   filter(all(c("Left", "Right") %in% Hemisphere)) %>%  # Then check for both hemispheres
  #   ungroup()
  # dt <- as.data.frame(dt)
  
  dt.MD <- dt[dt$fulltractlabel %in% MD.labels,]
  
  # Now get the meas mean for each group
  dt.MD.mean <- aggregate(
    dt.MD[,c(
      "thalsurface",
      "body",
      "corticalsurface",
      "allprofile"
    )],
    list(
      dt.MD$sub,
      dt.MD$project,
      dt.MD$meas,
      dt.MD$hemgen,
      dt.MD$Hemisphere,
      dt.MD$Gender,
      dt.MD$fulltractlabel,
      dt.MD$MDl,
      dt.MD$MDm,
      dt.MD$Whole_thalamus,
      dt.MD$AGE,
      dt.MD$agegroup,
      dt.MD$hemagegroup
    ),
    #dt.MD$streamlines_count),
    mean
  )
  
  dt.MD.mean <- setNames(
    dt.MD.mean,
    c(
      "sub",
      "project",
      "meas",
      "hemgen",
      "Hemisphere",
      "Gender",
      "fulltractlabel",
      "MDl",
      "MDm",
      "Whole_thalamus",
      "AGE",
      "agegroup",
      "hemagegroup",
      #"streamlines_count",
      "thalsurface",
      "body",
      "corticalsurface",
      "allprofile"
    )
  )
  
  dt.MD.mean[dt.MD.mean$fulltractlabel %in% dPFC.label,"PFCgroup"] <- "dPFC"
  dt.MD.mean[dt.MD.mean$fulltractlabel %in% mPFC.label,"PFCgroup"] <- "mPFC"
  dt.MD.mean[dt.MD.mean$fulltractlabel %in% OrbPoPFC.label,"PFCgroup"] <- "OrbPoPFC"
  dt.MD.mean[dt.MD.mean$fulltractlabel %in% IFC.label,"PFCgroup"] <- "IFC"

  dt.MD.mean$MD <- dt.MD.mean$MDl + dt.MD.mean$MDm

  feat <- names(dt.MD.mean) [!(names(dt.MD.mean) %in%
                                 c("thalsurface",
                                   "body",
                                   "corticalsurface"))]
  dt.MD.mean.parts <- reshape2::melt(
    dt.MD.mean,
    id.var = feat,
    variable.name='part'
    )

  if (!is.null(parts.names)){
    levels(dt.MD.mean.parts$part) <- parts.names
    dt.MD.mean.parts$hempart <- factor(
      paste0(
        dt.MD.mean.parts$Hemisphere,
        " - ",
        dt.MD.mean.parts$part
      ),
      levels = c(
        paste("Left",parts.names[1],sep=" - "),
        paste("Left",parts.names[2],sep=" - "),
        paste("Left",parts.names[3],sep=" - "),
        paste("Right",parts.names[1],sep=" - "),
        paste("Right",parts.names[2],sep=" - "),
        paste("Right",parts.names[3],sep=" - ")
      )
    )

  } else{
    levels(dt.MD.mean.parts$part) <- c(
      'Thalamic proximity',
      'Tract body',
      'Cortical proximity'
    )
    dt.MD.mean.parts$hempart <- factor(
      paste0(
        dt.MD.mean.parts$Hemisphere,
        " - ",
        dt.MD.mean.parts$part,"  "
      ),
      levels = c(
        "Left - Thalamic proximiy  ",
        "Left - Tract body  ",
        "Left - Cortical proximity  ",
        "Right - Thalamic proximity  ",
        "Right - Tract body  ",
        "Right - Cortical proximity  "
      )
    )
  }

  dt.MD.mean.parts$meas2vol <- dt.MD.mean.parts$value /
    (dt.MD.mean.parts$MD / dt.MD.mean.parts$Whole_thalamus)
  # dt.MD.mean.parts$meas2streamlines <- dt.MD.mean.parts$value /
  #   # (dt.MD.mean.parts$MD / dt.MD.mean.parts$streamlines_count)
  #   dt.MD.mean.parts$streamlines_count

  dt.MD.mean.parts$MD2WT <- dt.MD.mean.parts$MD / dt.MD.mean.parts$Whole_thalamus

  dt.MD.mean.parts <- transform(
    dt.MD.mean.parts,
    PFCgroup = factor(
      PFCgroup,
      levels=c(
        "dPFC",
        "mPFC",
        "IFC",
        "OrbPoPFC"
      )
    )
  )

  dt.MD.mean.parts$agegroup <- factor(
    dt.MD.mean.parts$agegroup,
    levels = agegroup.levels
  )

  return(dt.MD.mean.parts)
} 

# asymmetry indices
get.AI <- function(
    group = "tractlabel",
    groups = c("OR","MR","DT","AR","SR"),
    meas = "fa",
    y = "val",
    dt,
    extra.features = NULL,
    parts.names = c("1st","2nd","3rd")){
  
  dt <- as.data.frame(dt)
  dt.selection <- dt[dt[,group] %in% groups & dt[,"meas"] == meas,] 
  dt.selection <-  as.data.frame(dt.selection) 
  subs <- unique(dt.selection$sub)
  TCKS <- unique(dt.selection[,group])
  
  features <- c("Gender","meas",group,"sub","AGE","ses","agegroup","outlier") 
  if (!is.null(extra.features)){features <- c(features,extra.features)} 
  
  data.AI <- c()
  for (sub in subs){
    dt.sub <- dt.selection[dt.selection$sub == sub,]
    aux <- dt.sub[,names(dt.sub) %in% features]
    aux <- unique(aux) 
    
    aux$AI <- vector(mode = "list",length = nrow(aux))
    aux$xlen <- vector(mode = "list",length = nrow(aux))
    
    for (TCK in TCKS){
      Left <- subset(dt.sub[dt.sub[,group] == TCK,],
                     Hemisphere=="Left")
      Right <- subset(dt.sub[dt.sub[,group] == TCK,],
                      Hemisphere=="Right")
      if (nrow(Left) == 0 || nrow(Right) == 0){
        AI <- NA
        aux <- aux[!(aux[,group]  == TCK),] 
      } else{
        AI <- 200*((Right[,y][[1]] - Left[,y][[1]])/(Left[,y][[1]] + Right[,y][[1]]))
        aux$xlen[[which(aux[,group] == TCK )]] <- c(1:length(AI))
        aux$AI[[which(aux[,group] == TCK )]] <- AI
      } 
    }
    data.AI <- rbind(data.AI,aux)
  }
  
  thalsurface <- c()
  body <- c()
  corticalsurface <- c()
  allprofile <- c()
  AGE <- c()
  Gender <- c()
  
  for (r in 1:nrow(data.AI)){
    thalsurface <- append(thalsurface,mean(data.AI$AI[[r]][12:37]))
    body <- append(body,mean(data.AI$AI[[r]][38:63]))
    corticalsurface <- append(corticalsurface,mean(data.AI$AI[[r]][64:89]))
    allprofile <- append(allprofile,mean(data.AI$AI[[r]]))
  }

  data.AI.mean.parts <- cbind(data.AI,thalsurface,body,corticalsurface,allprofile)
  
  feat <- names(data.AI.mean.parts)[
    !(names(data.AI.mean.parts) %in%
        c("thalsurface",
          "body",
          "corticalsurface"))
  ]
  data.AI.mean.parts <- melt(data.AI.mean.parts,
                             id.var = feat,
                             variable.name='part')
  levels(data.AI.mean.parts$part) <- parts.names
  
  return(list(data.AI = data.AI, data.AI.mean.parts = data.AI.mean.parts))
  
}

optimal.BIC.table <- function(dt,vol,group2=NULL,y=NULL){
  colnames <- c("Gender","model","N","y","sd","se","ci","vol")
  dt.models <- data.frame(matrix(ncol=length(colnames),
                                    nrow=0, 
                                    dimnames=list(NULL, colnames)))
  
  TCKS <- unique(dt[,vol])
  print(TCKS)
  
  if (!is.null(group2)){
    group2.levels <- unique(dt[,group2])
    print(group2.levels)
  }
  
  if (is.null(y)){
    y <- "value"
  } 
  
  # TCKS <- TCKS[2:length(TCKS)]
  for (lv2 in 1:length(group2.levels)){
    for (tck in 1:length(TCKS)){
      d <- dt[dt[,vol] == TCKS[tck] & dt[,group2] == group2.levels[lv2],]
      aux <- models.BIC(dataset=d,
                        x="AGE",
                        y=y,
                        R_bootstrap = 1000)
      aux$vol <- TCKS[tck]
      aux[,group2]  <- group2.levels[lv2]
      dt.models <- rbind(dt.models,aux)
    }
  } 
  rm(aux)
  
  dt.models$model <- mapvalues(dt.models$model,
                                  from=c("Linear",
                                         "Quadratic",
                                         "Cubic"),
                                  to = c("model.linear",
                                         "model.quadratic",
                                         "model.cubic"))
  
  optimal.BIC <- dt.models %>%
    group_by(Gender,Hemisphere,vol,meas) %>%
    group_modify(~diff.BIC(data=.x,
                           variable="y"))
  optimal.BIC$model <- as.factor(optimal.BIC$model)
  optimal.BIC <- as.data.frame(optimal.BIC)
  return(optimal.BIC)
} 

optimal.BIC.table2 <- function(dt, vol, group2 = NULL, group3 = NULL, y = NULL){
  colnames <- c("Gender","model","N","y","sd","se","ci","vol")
  dt.models <- data.frame(matrix(ncol=length(colnames),
                                 nrow=0, 
                                 dimnames=list(NULL, colnames)))
  
  TCKS <- unique(dt[,vol])
  print(TCKS)
  
  if (!is.null(group2)){
    group2.levels <- unique(dt[,group2])
    print(group2.levels)
  }
  
  if (!is.null(group3)){
    group3.levels <- unique(dt[,group3])
    print(group3.levels)
  }
  
  if (is.null(y)){
    y <- "value"
  } 
  
  # TCKS <- TCKS[2:length(TCKS)]
  for (lv2 in 1:length(group2.levels)){
    for (lv3 in 1:length(group3.levels)){
      for (tck in 1:length(TCKS)){
        d <- dt[dt[,vol] == TCKS[tck] & 
                  dt[,group2] == group2.levels[lv2] &
                  dt[,group3] == group3.levels[lv3],]
        aux <- models.BIC(dataset=d,
                          x="AGE",
                          y=y,
                          R_bootstrap = 1000)
        aux$vol <- TCKS[tck]
        aux[,group2]  <- group2.levels[lv2]
        aux[,group3] <- group3.levels[lv3]
        dt.models <- rbind(dt.models,aux)
      }
    }
  } 
  rm(aux)
  
  dt.models$model <- mapvalues(dt.models$model,
                               from=c("Linear",
                                      "Quadratic",
                                      "Cubic"),
                               to = c("model.linear",
                                      "model.quadratic",
                                      "model.cubic"))
  
  optimal.BIC <- dt.models %>%
    group_by(Gender,Hemisphere,vol,meas,part) %>%
    group_modify(~diff.BIC(data=.x,
                           variable="y"))
  optimal.BIC$model <- as.factor(optimal.BIC$model)
  optimal.BIC <- as.data.frame(optimal.BIC)
  return(optimal.BIC)
} 

get.FO.data <- function(
    dt,
    parts.names = NULL,
    agegroup.levels = c(
      "Young", 
      "Adult", 
      "Elder"
    ),
    clean.outliers = FALSE
    ) {
  
  if (clean.outliers) {
    dt <- subset(dt, outlier == FALSE)
  }
  
  dt$VLN <- dt$VLa + dt$VLp
  dt.FO <- dt[dt$tractlabel %in% FO.tracts, ]
  
  nuclei_dict_list <- list(
    OR = "LGN",
    AR = "MGN",
    MR = "VLN",
    SR = "VPL",
    DT = "VLp"
  )
  
  for (i in nuclei_dict_list) {
    tract <- names(nuclei_dict_list)[nuclei_dict_list == i]
    dt.FO[dt.FO$tractlabel == tract, "nuclei"] <- i
    dt.FO[dt.FO$tractlabel == tract, "nucleivolume"] <-
      dt.FO[dt.FO$tractlabel == tract, i]
  }

  feat <- names(dt.FO)[!(names(dt.FO) %in% c(
    "thalsurface",
    "body",
    "corticalsurface"
  ))]
  dt.FO.parts <- reshape2::melt(dt.FO, id.var = feat, variable.name = "part")

  if (!is.null(parts.names)) {
    levels(dt.FO.parts$part) <- parts.names
  } else {
    levels(dt.FO.parts$part) <- c(
      "Thalamic proximity",
      "Tract body",
      "Cortical proximity"
    )
  }

  dt.FO.parts <- transform(dt.FO.parts,
    tractlabel = factor(
      tractlabel,
      levels = FO.tracts
    )
  )

  for (i in nuclei_dict_list) {
    tract <- names(nuclei_dict_list)[nuclei_dict_list == i]
    dt.FO.parts[dt.FO.parts$tractlabel == tract, "meas2vol"] <-
      dt.FO.parts[dt.FO.parts$tractlabel == tract, "value"] /
        (dt.FO.parts[dt.FO.parts$tractlabel == tract, i] /
          dt.FO.parts[dt.FO.parts$tractlabel == tract, "Whole_thalamus"])

    dt.FO.parts[dt.FO.parts$tractlabel == tract, "nucleivolume2WT"] <-
      dt.FO.parts[dt.FO.parts$tractlabel == tract, i] /
        dt.FO.parts[dt.FO.parts$tractlabel == tract, "Whole_thalamus"]
  }

  dt.FO.parts$agegroup <- factor(
    dt.FO.parts$agegroup,
    levels = agegroup.levels
  )
  
  dt.FO.parts$tractlabel
  
  # some subjects can have the left tract but not the right and the other way around
  # subjects missing an Hemisphere are automatically removed
  
  # Remove subjects that do not have both hemispheres for every tract
  dt.FO.parts <- dt.FO.parts %>%
    group_by(sub, tractlabel,meas) %>%
    filter(all(c("Left", "Right") %in% Hemisphere)) %>%
    ungroup()
  
  # Ensure 'tractlabel' is a factor with the correct levels
  dt.FO.parts <- dt.FO.parts %>%
    mutate(
      tractlabel = factor(
        tractlabel, 
        levels = levels(dt.FO.parts$tractlabel
      ))
    ) %>%
    arrange(tractlabel)
  
  dt.FO.parts <- as.data.frame(
    dt.FO.parts
  )
  
  return(dt.FO.parts)
}

anovas.repeatedmeasures <- function(
    data, 
    tractname = "dPFC",
    tractgroup = "PFCgroup",
    nanovas4BonferroniCorrection = 9,
    plotnormality = FALSE,
    plots2interactions = FALSE,
    plots3interactions = FALSE,
    text.size = 36,
    groupping = "Hemisphere",
    figpath
  ){                    
  dt.FA <- subset(data, meas == "fa")
  #dt.FA.tract <- subset(dt.FA, PFCgroup == tractname)
  dt.FA.tract <- dt.FA[dt.FA[,tractgroup] == tractname,]
  
  dt.FA.tract.mean <- dt.FA.tract %>%
    group_by_at(c("part",
                  "agegroup",
                  tractgroup,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  "sub")) %>%
    summarise_at(vars(value), list(value = mean))
  
  dt.FA.tract.mean <- dt.FA.tract.mean %>%
    convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  
  dt.FA.tract.mean <- as.data.frame(dt.FA.tract.mean)
  
  res.aov <- anova_test(
    data = dt.FA.tract.mean, dv = value, wid = sub,
    within = c("part","Hemisphere"),
    between = c("agegroup","Gender"),
    detailed = TRUE
  )
  
  res.aov2 <- aov_ez(
    data = dt.FA.tract.mean,
    id = "sub",
    dv = "value",
    between = c("agegroup","Gender"),
    within = c("part","Hemisphere")
  )

  # res.aov2 <-  ezANOVA(data = dt.FA.tract.mean,
  #                  dv = .(value),
  #                  wid = .(sub),
  #                  within = .(part, Hemisphere),
  #                  between = .(agegroup, Gender),
  #                  type = 3,
  #                  detailed = TRUE)

  res.aov3 <- aov_car(value ~ agegroup*Gender*part*Hemisphere+ Error(sub/part*Hemisphere), 
                      data=dt.FA.tract.mean)
  
  # lmer()

  p.corrected <- res.aov$ANOVA["p"]
  p.corrected[p.corrected < (0.05/nanovas4BonferroniCorrection)] <- "*"
  p.corrected[!(p.corrected < (0.05/nanovas4BonferroniCorrection))] <- ""
  p.corrected <- as.data.frame(p.corrected)
  colnames(p.corrected) <- "p.corrected"
  
  res.aov$ANOVA <- cbind(res.aov$ANOVA,p.corrected)

  if (plots3interactions) {
    # pwc <- dt.FA.tract.mean %>%
    #   group_by(Gender, agegroup, Hemisphere) %>%
    #   pairwise_t_test(value ~ part, paired = TRUE, p.adjust.method = "bonferroni") %>%
    #   select(-df, -statistic)
    # pwc <- pwc %>% add_xy_position(x = "agegroup", dodge = 1)
    # tract.boxplots <- ggplot(dt.FA.tract.mean, 
    #                          aes(y = value, 
    #                              x = agegroup,
    #                              color = part)) +
    #   
    #   geom_violin(trim = TRUE, position = position_dodge(1)) +
    #   geom_boxplot(width = 0.22, position = position_dodge(1)) +
    #   geom_point(shape = 1, position = position_jitterdodge(0.5, dodge.width = 1)) +
    #   stat_pvalue_manual(pwc, tip.length = 0.01, hide.ns = FALSE, 
    #                      label = "{p.adj.signif}") +
    #   ylab("FA") + xlab("Agegroup") +
    #   theme_bw() +
    #   facet_nested( ~ Gender + Hemisphere)
    # ggsave(file = file.path(figpath,paste0("Fig_",tractname,"-boxplots_PartsPairWise.png")),
    #        height = 6,# 4 each plot
    #        width = 18,# 4 each plot + 4 legend
    #        bg = "white",
    #        units = "in",
    #        dpi = 600,
    #        device = "png",
    #        tract.boxplots)
    # #-----------------------------------------------------------------------------
    # pwc2 <- dt.FA.tract.mean %>%
    #   group_by(Gender, part, Hemisphere) %>%
    #   pairwise_t_test(value ~ agegroup, paired = FALSE, p.adjust.method = "bonferroni")
    # pwc2 <- pwc2 %>% add_xy_position(x = "part", dodge = 1)
    # tract.boxplots2 <- ggplot(dt.FA.tract.mean, 
    #                           aes(y = value, 
    #                               x = part,
    #                               color = agegroup)) +
    #   geom_violin(trim = TRUE, position = position_dodge(1)) +
    #   geom_boxplot(width = 0.22, position = position_dodge(1)) +
    #   geom_point(shape = 1, position = position_jitterdodge(0.5, dodge.width = 1)) +
    #   stat_pvalue_manual(pwc2, tip.length = 0.02, hide.ns = FALSE,
    #                      label = "{p.adj.signif}") +
    #   ylab("FA") + xlab("Part") +
    #   theme_bw() +
    #   facet_nested( ~ Gender + Hemisphere)
    # ggsave(file = file.path(figpath,paste0("Fig_",tractname,"-boxplots_AgegroupPairWise.png")),
    #        height = 6,# 4 each plot
    #        width = 18,# 4 each plot + 4 legend
    #        bg = "white",
    #        units = "in",
    #        dpi = 600,
    #        device = "png",
    #        tract.boxplots2)
    
    #-------------------------------------------------------------------------------
    # in X = part
    pwc.agegroup <- dt.FA.tract.mean %>%
      group_by_at(c("part", groupping)) %>%
      pairwise_t_test(value ~ agegroup, paired = FALSE, p.adjust.method = "bonferroni")
    pwc.agegroup <- pwc.agegroup %>% add_xy_position(x = "part", dodge = 1)
    pwc.agegroup <- as.data.frame(pwc.agegroup)
    cohensd <- dt.FA.tract.mean %>%
      group_by_at(c("part", groupping)) %>% cohens_d(value ~ agegroup, var.equal = TRUE)
    cohensd <- as.data.frame(cohensd)
    effsize <- round(abs(cohensd$effsize), digits = 2)
    magnitude <- cohensd$magnitude
    if (length(unique(pwc.agegroup$group1 == cohensd$group1)) == 1 &&
        length(unique(pwc.agegroup$group2 == cohensd$group2)) == 1) {
      pwc.agegroup <- cbind(pwc.agegroup, effsize, magnitude)
      tract.boxplots <- ggplot(dt.FA.tract.mean, 
                               aes(y = value, 
                                   x = part,
                                   color = agegroup)) +
        geom_violin(trim = TRUE, position = position_dodge(1)) +
        geom_boxplot(width = 0.22, position = position_dodge(1)) +
        geom_point(shape = 1, position = position_jitterdodge(0.5, dodge.width = 1)) +
        stat_pvalue_manual(pwc.agegroup, tip.length = 0.02, hide.ns = FALSE,
                           label = "{p.adj.signif}, |d| = {effsize}") +
        ylab("FA") + xlab("Section") + labs(title = tractname, colour = "Age group") +
        theme_bw() +
        facet_nested(formula(paste0("~ ",groupping))) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              text = element_text(size = text.size),
              strip.text.x = element_text(size = text.size, face = "bold"),
              strip.text.y = element_text(size = text.size),
              axis.text = element_text(size = text.size - 4),
              # Hide panel borders and remove grid lines
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              # Change axis line
              axis.line = element_line(colour = "black"),
              # facet_wrap without boxes
              strip.background = element_blank())
      
      ggsave(file = file.path(figpath,paste0("Fig_3interactions-",tractname,"-boxplots_x=Part_color=Agegroup_facet=",groupping,".png")),
             height = 6,# 4 each plot
             width = 16,# 4 each plot + 4 legend (18)
             bg = "white",
             units = "in",
             dpi = 600,
             device = "png",
             tract.boxplots)
    }
    # in X = agegroup
    pwc.part <- dt.FA.tract.mean %>%
      group_by_at(c("agegroup", groupping)) %>%
      pairwise_t_test(value ~ part, paired = TRUE, p.adjust.method = "bonferroni")
    pwc.part <- pwc.part %>% add_xy_position(x = "agegroup", dodge = 1)
    pwc.part <- as.data.frame(pwc.part)
    cohensd <- dt.FA.tract.mean %>%
      group_by_at(c("agegroup", groupping)) %>% cohens_d(value ~ part, paired = TRUE)
    cohensd <- as.data.frame(cohensd)
    effsize <- round(abs(cohensd$effsize), digits = 2)
    magnitude <- cohensd$magnitude
    if (length(unique(pwc.part$group1 == pwc.part$group1)) == 1 &&
        length(unique(pwc.part$group2 == pwc.part$group2)) == 1) {
      pwc.part <- cbind(pwc.part, effsize, magnitude)
      
      tract.boxplots <- ggplot(dt.FA.tract.mean, 
                               aes(y = value, 
                                   x = agegroup,
                                   color = part)) +
        geom_violin(trim = TRUE, position = position_dodge(1)) +
        geom_boxplot(width = 0.22, position = position_dodge(1)) +
        geom_point(shape = 1, position = position_jitterdodge(0.5, dodge.width = 1)) +
        stat_pvalue_manual(pwc.part, tip.length = 0.02, hide.ns = FALSE,
                           label = "{p.adj.signif}, |d| = {effsize}") +
        ylab("FA") + xlab("Age group") + labs(title = tractname, colour = "Age group") +
        theme_bw() +
        # facet_nested(formula(paste0("~ ",groupping))) +
        facet_grid2(as.formula(paste(facet.y,"~ ",groupping)),#fulltractlabel ~ Gender + Hemisphere,
                    axes = "all", 
                    # independent = "y",
                    # remove_labels = "x",
                    remove_labels = "all",
                    scales = "free")
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              text = element_text(size = text.size),
              strip.text.x = element_text(size = text.size, face = "bold"),
              strip.text.y = element_text(size = text.size),
              axis.text = element_text(size = text.size - 4),
              # Hide panel borders and remove grid lines
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              # Change axis line
              axis.line = element_line(colour = "black"),
              # facet_wrap without boxes
              strip.background = element_blank())
      ggsave(file = file.path(figpath,paste0("Fig_3interactions-",tractname,"-boxplots_x=Agegroup_color=Part_facet=",groupping,".png")),
             height = 6,# 4 each plot
             width = 16,# 4 each plot + 4 legend
             bg = "white",
             units = "in",
             dpi = 600,
             device = "png",
             tract.boxplots)
    }
  }
  
  if (plots2interactions){
    # in X = part; color Hemisphere
    sub.Left <- unique(dt.FA.tract.mean[dt.FA.tract.mean$Hemisphere == "Left", "sub"])
    sub.Right <- unique(dt.FA.tract.mean[dt.FA.tract.mean$Hemisphere == "Right", "sub"])
    
    subs <- intersect(sub.Left,sub.Right)
    dt.FA.tract.mean <- dt.FA.tract.mean[dt.FA.tract.mean[,"sub"] %in% subs,]
  
    pwc.hemisphere <- dt.FA.tract.mean %>%
      group_by_at(c("part")) %>%
      pairwise_t_test(value ~ Hemisphere, paired = TRUE, p.adjust.method = "bonferroni")
    pwc.hemisphere <- pwc.hemisphere %>% add_xy_position(x = "part", dodge = 1)
    pwc.hemisphere <- as.data.frame(pwc.hemisphere)
    cohensd <- dt.FA.tract.mean %>%
      group_by_at(c("part")) %>% cohens_d(value ~ Hemisphere, paired = TRUE)
    cohensd <- as.data.frame(cohensd)
    effsize <- round(abs(cohensd$effsize), digits = 2)
    magnitude <- cohensd$magnitude
    if (length(unique(pwc.hemisphere$group1 == cohensd$group1)) == 1 &&
        length(unique(pwc.hemisphere$group2 == cohensd$group2)) == 1) {
      pwc.hemisphere <- cbind(pwc.hemisphere, effsize, magnitude)
      tract.boxplots <- ggplot(dt.FA.tract.mean,
                               aes(y = value,
                                   x = part,
                                   color = Hemisphere)) +
        geom_violin(trim = TRUE, position = position_dodge(1)) +
        geom_boxplot(width = 0.22, position = position_dodge(1)) +
        geom_point(shape = 1, position = position_jitterdodge(0.5, dodge.width = 1)) +
        stat_pvalue_manual(pwc.hemisphere, tip.length = 0.02, hide.ns = FALSE,
                           label = "{p.adj.signif}, |d| = {effsize}") +
        scale_colour_manual("Hemisphere",values=c("#E41A1C","#377EB8")) +
        ylab("FA") + xlab("Section") + 
        theme_bw()
      ggsave(file = file.path(figpath,paste0("Fig_2interactions_",tractname,"-boxplots_x=Part_color=Hemisphere.png")),
             height = 6,# 4 each plot
             width = 18,# 4 each plot + 4 legend
             bg = "white",
             units = "in",
             dpi = 600,
             device = "png",
             tract.boxplots)
    }

    # in X = part; color Gender
    pwc.gender <- dt.FA.tract.mean %>%
      group_by_at(c("part")) %>%
      pairwise_t_test(value ~ Gender, paired = FALSE, p.adjust.method = "bonferroni")
    pwc.gender <- pwc.gender %>% add_xy_position(x = "part", dodge = 1)
    pwc.gender <- as.data.frame(pwc.gender)
    cohensd <- dt.FA.tract.mean %>%
      group_by_at(c("part")) %>% cohens_d(value ~ Gender, var.equal = TRUE)
    cohensd <- as.data.frame(cohensd)
    effsize <- round(abs(cohensd$effsize), digits = 2)
    magnitude <- cohensd$magnitude
    if (length(unique(pwc.gender$group1 == cohensd$group1)) == 1 &&
        length(unique(pwc.gender$group2 == cohensd$group2)) == 1) {
      pwc.gender <- cbind(pwc.gender, effsize, magnitude)
      tract.boxplots <- ggplot(dt.FA.tract.mean,
                               aes(y = value,
                                   x = part,
                                   color = Gender)) +
        geom_violin(trim = TRUE, position = position_dodge(1)) +
        geom_boxplot(width = 0.22, position = position_dodge(1)) +
        geom_point(shape = 1, position = position_jitterdodge(0.5, dodge.width = 1)) +
        stat_pvalue_manual(pwc.gender, tip.length = 0.02, hide.ns = FALSE,
                           label = "{p.adj.signif}, |d| = {effsize}") +
        scale_colour_manual("Gender",values=c("#F8766D","#00BA38")) +
        ylab("FA") + xlab("Section") +
        theme_bw()
      ggsave(file = file.path(figpath,paste0("Fig_2interactions_",tractname,"-boxplots_x=Part_color=Gender.png")),
             height = 6,# 4 each plot
             width = 18,# 4 each plot + 4 legend
             bg = "white",
             units = "in",
             dpi = 600,
             device = "png",
             tract.boxplots)
    }

    pwc.agegroup <- dt.FA.tract.mean %>%
      group_by_at(c("part")) %>%
      pairwise_t_test(value ~ agegroup, paired = FALSE, p.adjust.method = "bonferroni")
    pwc.agegroup <- pwc.agegroup %>% add_xy_position(x = "part", dodge = 1)
    pwc.agegroup <- as.data.frame(pwc.agegroup)
    cohensd <- dt.FA.tract.mean %>%
      group_by_at(c("part")) %>% cohens_d(value ~ agegroup, var.equal = TRUE)
    cohensd <- as.data.frame(cohensd)
    effsize <- round(abs(cohensd$effsize), digits = 2)
    magnitude <- cohensd$magnitude
    if (length(unique(pwc.agegroup$group1 == cohensd$group1)) == 1 &&
        length(unique(pwc.agegroup$group2 == cohensd$group2)) == 1) {
      pwc.agegroup <- cbind(pwc.agegroup, effsize, magnitude)
      tract.boxplots <- ggplot(dt.FA.tract.mean,
                               aes(y = value,
                                   x = part,
                                   color = agegroup)) +
        geom_violin(trim = TRUE, position = position_dodge(1)) +
        geom_boxplot(width = 0.22, position = position_dodge(1)) +
        geom_point(shape = 1, position = position_jitterdodge(0.5, dodge.width = 1)) +
        stat_pvalue_manual(pwc.agegroup, tip.length = 0.02, hide.ns = FALSE,
                           label = "{p.adj.signif}, |d| = {effsize}") +
        ylab("FA") + xlab("Section") +
        theme_bw()
      ggsave(file = file.path(figpath,paste0("Fig_2interactions_",tractname,"-boxplots_x=Part_color=Agegroup.png")),
             height = 6,# 4 each plot
             width = 18,# 4 each plot + 4 legend
             bg = "white",
             units = "in",
             dpi = 600,
             device = "png",
             tract.boxplots)
    }
  }
  
  if (plotnormality) {
    names(dt.FA.tract.mean)[names(dt.FA.tract.mean) == "value"] <- "score"
    normality.test <- dt.FA.tract.mean %>%
      group_by(agegroup, part, Gender, Hemisphere) %>%
      shapiro_test(score)
    normality.test <- as.data.frame(normality.test)
    normality.test[normality.test$p < 0.05,]
    
    tract.qqplot <- ggqqplot(dt.FA.tract.mean, "score",
                             color = "Hemisphere", 
                             palette = c("#E41A1C","#377EB8"),
                             ggtheme = theme_bw()) +
      facet_grid(part ~ Gender + Hemisphere + agegroup, labeller = "label_both") +
       theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank())
    ggsave(file=file.path(figpath,paste0("Fig_",tractname,"-qqplot.png")),
           height = 8,# 4 each plot
           width = 22,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 600,
           device = "png",
           tract.qqplot)
    
    dt.FA.fullmean <- dt.FA %>%
      group_by_at(c("part",
                    "agegroup",
                    tractgroup,
                    "Hemisphere",
                    "Gender",
                    "meas")) %>%
      summarise_at(vars(value), list(value = mean))
    # dt.FA.fullmean.tract <- subset(dt.FA.fullmean, PFCgroup == tractname)
    dt.FA.fullmean.tract <- dt.FA.fullmean[dt.FA.fullmean[,tractgroup] == tractname,]
    
    tract.densityplots <- ggplot(dt.FA.tract.mean, 
                                 aes(x = score, 
                                     color = Hemisphere,
                                     fill = Hemisphere)) +
      geom_vline(data = dt.FA.fullmean.tract, 
                 aes(xintercept = value, 
                     color = Hemisphere),
                 linetype = "dashed") +
      # geom_vline(data = dt.MD.mean.parts.FA.fullmedian.dPFC, 
      #            aes(xintercept = value),
      #            linetype = "dashed") +
      geom_density(alpha = 0.4) + 
      scale_colour_manual("",values=c("#E41A1C","#377EB8")) +
      scale_fill_manual("",values=c("#E41A1C","#377EB8")) +
      ylab("Density") + xlab("FA") +
      facet_grid(part ~ Gender + Hemisphere + agegroup, labeller = "label_both") +
      theme_bw() +
      theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.key = element_blank())
    ggsave(file=file.path(figpath,paste0("Fig_",tractname,"-densityplots.png")),
           height = 8,# 4 each plot
           width = 22,# 4 each plot + 4 legend
           bg = "white",
           units = "in",
           dpi = 600,
           device = "png",
           tract.densityplots)
  }
  
  # return(kable(get_anova_table(res.aov), digits = 3, format = "pandoc", caption = "ANOVA table"))anova_test
  return(list(ANOVA=res.aov,res.aov2=res.aov2,res.aov3=res.aov3))
}

double.interaction <- function(
    x = "part",
    y = "value",
    group = "tractlabel",
    groups = FO.tracts,
    compare = "Hemisphere",
    legend.title = "Hemisphere",
    abs.cohensd = FALSE,
    text.size = 26,
    stats.size = 5,
    paired = FALSE,
    palette = NULL,
    data,
    figpath = figpath,
    identifier
    ) {
  data <- subset(data, meas == "fa")
  data <- data[data[,group] %in% groups,]

  # if PAIRED check that vectors to compare are the same lenght (= all subIDs have both levels to compare)
  if (paired & length(unique(data[,compare])) == 2){
    data.commonID <- c()
    for (f in unique(data[,group])){

      levels <- unique(data[,compare])
      l1 <- unique(data[data[,compare] == levels[1] & data[,group] == f, "sub"])
      l2 <- unique(data[data[,compare] == levels[2] & data[,group] == f, "sub"])

      subs <- intersect(l1,l2) 
      data.commonID <- rbind(data.commonID, 
                             data[data[,"sub"] %in% subs &
                             data[,group]  == f,])
    }
    data <- data.commonID
  } else if (!(length(unique(data[,compare])) == 2) & paired) {
     stop("Levels to compare must be equal to 2")
  }

  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  "sub")) %>%
    summarise_at(vars(value), list(value = mean))

  data.mean <- data.mean %>%
      convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  data.mean <- as.data.frame(data.mean)

  pwc.comparisons <- data.mean %>%
    group_by_at(c(x, group)) %>%
    pairwise_t_test(as.formula(paste0(y," ~ ",compare)), paired = paired, p.adjust.method = "bonferroni")
  pwc.comparisons <- pwc.comparisons %>% add_xy_position(x = "part", dodge = 1)
  pwc.comparisons <- as.data.frame(pwc.comparisons)
  cohensd <- data.mean %>%
    group_by_at(c(x, group)) %>% 
    cohens_d(as.formula(paste0(y," ~ ",compare)), paired = paired, var.equal = paired)#, var.equal = FALSE)
  cohensd <- as.data.frame(cohensd)
  if (abs.cohensd){
    effsize <- round(abs(cohensd$effsize), digits = 2)
    stat.label <- "{p.adj.signif}, |d| = {effsize}"
  } else{
    effsize <- round(cohensd$effsize, digits = 2)
    stat.label <- "{p.adj.signif}, d = {effsize}"
  } 
  magnitude <- cohensd$magnitude

  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  if (length(unique(pwc.comparisons$group1 == cohensd$group1)) == 1 &&
      length(unique(pwc.comparisons$group2 == cohensd$group2)) == 1) {
    pwc.comparisons <- cbind(pwc.comparisons, effsize, magnitude)
    tract.boxplots <- ggplot(data.mean, 
                            aes(y = .data[[y]], 
                                x = .data[[x]],
                                color = .data[[compare]])) +
      geom_violin(aes(fill = .data[[compare]]), alpha = 0.25, trim = TRUE, position = position_dodge(1)) +
      geom_point(shape = 21, size = 1, position = position_jitterdodge(0.5, dodge.width = 1))+#0.5, dodge.width = 1)) +
      geom_boxplot(width = 0.22, position = position_dodge(1)) +
      # stat_pvalue_manual(pwc.comparisons, 
      #                    tip.length = 0.02, 
      #                    vjust = -0.5, 
      #                    hide.ns = FALSE,
      #                    size = stats.size,# fontface = "bold",
      #                    label = stat.label) +
      add_pvalue(pwc.comparisons, 
                 fontface = "bold",
                 label.size = stats.size,
                 inherit.aes = FALSE, 
                 tip.length = 0.02, 
                 hide.ns = FALSE,
                 label = stat.label) +
      ylab("FA") + xlab("Section") + labs(colour = legend.title, fill = legend.title) +
      theme_bw() +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      # facet_nested(formula(paste0(tractgroup,"~",groupping))) +
      facet_grid2(as.formula(paste0(group," ~ .")),#fulltractlabel ~ Gender + Hemisphere,
                        axes = "all", 
                        # independent = "y",
                        # remove_labels = "x",
                        remove_labels = "all",
                        scales = "free") +
      # facet_wrap2(as.formula(paste(tractgroup,"~",groupping)),#fulltractlabel ~ Gender + Hemisphere,
      #             # axes = "all", 
      #             # # independent = "y",
      #             # # remove_labels = "x",
      #             # remove_labels = "all",
      #             # scales = "free",
      #             ncol = 4,
      #             trim_blank = FALSE) +
    # scale_y_continuous(breaks = c(0,seq(0.2, 0.8, by = 0.1))) +
    # expand_limits(y = c(0.1, 0.9)) +
    scale_y_continuous(breaks=seq(0.2, 1, 0.1), limits=c(NA, NA), expand = expansion(mult = c(0.05, 0.15))) +
    #  ylim(NA,0.9) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            text = element_text(size = text.size),
            strip.text.x = element_text(size = text.size),
            strip.text.y = element_text(size = text.size, face = "bold"),
            axis.text = element_text(size = text.size - 4),
            # Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change axis line
            axis.line = element_line(colour = "black"),
            # facet_wrap without boxes
            strip.background = element_blank())
    
    ggsave(file = file.path(figpath,
                            paste0("Fig_two-way_interactions_boxplots_",identifier,"_x=",x,"_y=",y,"_color=",compare,".png")),
          height = length(unique(data[,group]))*3.5,#20,#15,# 4 each plot
          width = 9,#24,# 4 each plot + 4 legend (18)
          bg = "white",
          units = "in",
          dpi = 600,
          device = "png",
          tract.boxplots)
  }
  return(tract.boxplots)
}

pair.data <- function(
    dt,
    groupBy = c("tractlabel","part"),
    pairBy = "Hemisphere",
    levelspairBy = c("Left","Right"),
    ID = "sub"
    
){
  dt <- dt %>%
    group_by(across(all_of(c(groupBy, ID)))) %>%
    filter(all(levelspairBy %in% !!sym(pairBy))) %>%
    ungroup()
  return(as.data.frame(dt))
}

map.pvalue2stars <- function(df,y,youtput) {
  df[df[,y] <= 0.05, youtput] <- "*"
  df[df[,y] <= 0.01, youtput] <- "**"
  df[df[,y] <= 0.001, youtput] <- "***"
  df[df[,y] > 0.05, youtput] <- "ns"
  return(df)
}

double.interactionV2 <- function(
  x = "part",
  y = "value",
  group = "tractlabel",
  groups = FO.tracts,
  compare = "Hemisphere",
  legend.title = "Hemisphere",
  abs.cohensd = FALSE,
  text.size = 26,
  stats.size = 5,
  paired = FALSE,
  palette = NULL,
  data,
  facet.formula = paste0(group," ~ ."),
  figpath = figpath,
  identifier,
  height = length(unique(data[,group]))*3.5,
  width = 9
) {
  
  data <- subset(data, meas == "fa")
  data <- data[data[,group] %in% groups,]
  
  # if PAIRED check that vectors to compare are the same lenght (= all subIDs have both levels to compare)
  if (paired & length(unique(data[,compare])) == 2){
    data.commonID <- c()
    for (f in unique(data[,group])){

      levels <- unique(data[,compare])
      l1 <- unique(data[data[,compare] == levels[1] & data[,group] == f, "sub"])
      l2 <- unique(data[data[,compare] == levels[2] & data[,group] == f, "sub"])

      subs <- intersect(l1,l2) 
      data.commonID <- rbind(data.commonID, 
                             data[data[,"sub"] %in% subs &
                             data[,group]  == f,])
    }
    data <- data.commonID
    print("Selected subjects with both levels to compare")
  } else if (!(length(unique(data[,compare])) == 2) & paired) {
     stop("Levels to compare must be equal to 2")
  }

  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  "sub")) %>%
    summarise_at(vars(value), list(value = mean))

  data.mean <- data.mean %>%
      convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  data.mean <- as.data.frame(data.mean)
  
  pwc.comparisons <- data.mean %>%
    group_by_at(c(x, group)) %>%
    pairwise_t_test(as.formula(paste0(y," ~ ",compare)), paired = paired, p.adjust.method = "bonferroni")
  pwc.comparisons <- pwc.comparisons %>% add_xy_position(x = x, dodge = 1)
  pwc.comparisons <- as.data.frame(pwc.comparisons)
  pwc.comparisons <- map.pvalue2stars(pwc.comparisons, "p.adj","p.adj.signif")
  
  cohensd <- data.mean %>%
    group_by_at(c(x, group)) %>% 
    cohens_d(as.formula(paste0(y," ~ ",compare)), paired = paired, var.equal = paired)#, var.equal = FALSE)
  cohensd <- as.data.frame(cohensd)
  
  if (abs.cohensd){
    effsize <- round(abs(cohensd$effsize), digits = 2)
    stat.label <- "{p.adj.signif}, |d| = {effsize}"
  } else{
    effsize <- round(cohensd$effsize, digits = 2)
    stat.label <- "{p.adj.signif}, d = {effsize}"
  } 
  magnitude <- cohensd$magnitude

  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  if (length(unique(pwc.comparisons$group1 == cohensd$group1)) == 1 &&
      length(unique(pwc.comparisons$group2 == cohensd$group2)) == 1) {
    pwc.comparisons <- cbind(pwc.comparisons, effsize, magnitude)
    tract.boxplots <- ggplot(data.mean, 
                            aes(y = .data[[y]], 
                                x = .data[[x]],
                                color = .data[[compare]])) +
      geom_violin(aes(fill = .data[[compare]]), alpha = 0.25, trim = TRUE, position = position_dodge(1)) +
      geom_point(shape = 21, size = 1, position = position_jitterdodge(0.5, dodge.width = 1))+#0.5, dodge.width = 1)) +
      geom_boxplot(width = 0.22, position = position_dodge(1)) +
      # stat_pvalue_manual(pwc.comparisons, 
      #                    tip.length = 0.02, 
      #                    vjust = -0.5, 
      #                    hide.ns = FALSE,
      #                    size = stats.size,# fontface = "bold",
      #                    label = stat.label) +
      stat_pvalue_manual(pwc.comparisons, 
                 #fontface = "bold",
                 label.size = stats.size,
                 inherit.aes = FALSE, 
                 tip.length = 0.02, 
                 hide.ns = FALSE,
                 label = stat.label) +
      ylab("FA") + xlab("Section") + labs(colour = legend.title, fill = legend.title) +
      theme_bw() +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      # facet_nested(formula(paste0(tractgroup,"~",groupping))) +
      facet_grid2(as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
                        axes = "all", 
                        # independent = "y",
                        # remove_labels = "x",
                        remove_labels = "all",
                        scales = "free") +
      # facet_wrap2(as.formula(paste(tractgroup,"~",groupping)),#fulltractlabel ~ Gender + Hemisphere,
      #             # axes = "all", 
      #             # # independent = "y",
      #             # # remove_labels = "x",
      #             # remove_labels = "all",
      #             # scales = "free",
      #             ncol = 4,
      #             trim_blank = FALSE) +
    # scale_y_continuous(breaks = c(0,seq(0.2, 0.8, by = 0.1))) +
    # expand_limits(y = c(0.1, 0.9)) +
    scale_y_continuous(breaks=seq(0.2, 1, 0.1), limits=c(NA, NA), expand = expansion(mult = c(0.05, 0.15))) +
    #  ylim(NA,0.9) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            text = element_text(size = text.size),
            strip.text.x = element_text(size = text.size, face =  "bold"),
            strip.text.y = element_text(size = text.size, face = "bold"),
            axis.text = element_text(size = text.size - 4),
            # Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change axis line
            axis.line = element_line(colour = "black"),
            # facet_wrap without boxes
            strip.background = element_blank())
    
    ggsave(file = file.path(figpath,
                            paste0("Fig_two-way_interactions_boxplots_",identifier,"_x=",x,"_y=",y,"_color=",compare,".png")),
          height = height,#20,#15,# 4 each plot
          width = width,#24,# 4 each plot + 4 legend (18)
          bg = "white",
          units = "in",
          dpi = 600,
          device = "png",
          tract.boxplots)
  }
  return(tract.boxplots)
}

double.interaction.nopaired <- function(
    data,
    x = "part",
    y = "value",
    group = "tractlabel",
    groups = FO.tracts,
    compare = "Hemisphere",
    metric = "fa",
    comparison_order = NULL,
    legend.title = "Hemisphere",
    abs.effsize = FALSE,
    text.size = 26,
    stats.size = 5,
    palette = NULL,
    paired = FALSE,
    facet.formula = paste0(group," ~ ."),
    hide.facet.titles = FALSE,
    figpath = NULL,
    identifier,
    height = length(unique(data[,group]))*3.5,
    width = 9,
    hide.x.axis = FALSE,
    hide.y.axis = FALSE,
    dummy.facet.x = FALSE
) {
  
  data <- subset(data, meas == metric)
  data <- data[data[,group] %in% groups,]
  
  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  "sub")) %>%
    dplyr::summarise(!!y := mean(!!sym(y)))

  data.mean <- data.mean %>%
    convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  data.mean <- as.data.frame(data.mean)
  
  # no paired and non parametric comparisons
  pwc.comparisons <- data.mean %>%
    group_by_at(c(group, x)) %>%
    rstatix::pairwise_wilcox_test(
      as.formula(paste(y,compare, sep = "~")),
      p.adjust.method = "bonf",
      paired = FALSE
    )
  
  # Compute effect size (r)
  r.wilcox <- data.mean %>%
    group_by_at(c(group, x)) %>%
    rstatix::wilcox_effsize(
      as.formula(paste(y, compare, sep = "~")),
      paired = FALSE
    )
  r.wilcox <- as.data.frame(r.wilcox)
  
  pwc.comparisons.effsize <- pwc.comparisons %>%
    left_join(
      r.wilcox, 
      by = c(group, x, "group1", "group2")
    )
  
  if (abs.effsize){
    pwc.comparisons.effsize$effsize <-
      round(abs(pwc.comparisons.effsize$effsize), digits = 2)
    stat.label <- "{p.adj.signif}, |r|={effsize}"
  } else{
    pwc.comparisons.effsize$effsize <- 
      round(pwc.comparisons.effsize$effsize, digits = 2)
    stat.label <- "{p.adj.signif}, r={effsize}"#"r = {effsize}\n{p.adj.signif}"
  } 
  
  # Format the effect size to display without leading zero for values < 1 and show .0 for zero
  # pwc.comparisons.effsize$effsize <- ifelse(
  #   pwc.comparisons.effsize$effsize == 0, ".0", 
  #   sub("^0\\.", ".", as.character(pwc.comparisons.effsize$effsize))
  # )
  
  pwc.comparisons.effsize$effsize <- ifelse(
    pwc.comparisons.effsize$effsize == 0, ".00", 
    sub("^0", "", sprintf("%.2f", pwc.comparisons.effsize$effsize))
  )
  
  pwc.comparisons.effsize <- map.pvalue2stars(
    pwc.comparisons.effsize,
    "p.adj",
    "p.adj.signif"
  )
  
  # magnitude <- r.wilcox$magnitude
  if (!is.null(comparison_order)){
    # Reorder within each part and tractlabel
    pwc.comparisons.effsize <- pwc.comparisons.effsize %>%
      mutate(comparison = paste(group1, group2, sep = "-")) %>%
      mutate(comparison = factor(comparison, levels = comparison_order)) %>%
      arrange_at(c(x, group, "comparison")) %>%
      select(-comparison)  # Remove the helper column
  }
  
  pwc.comparisons.effsize <- pwc.comparisons.effsize %>% 
    add_xy_position(x = x, dodge = 1)
  pwc.comparisons.effsize <- as.data.frame(pwc.comparisons.effsize)
  
  pwc.comparisons.effsize <- pwc.comparisons.effsize %>%
    mutate(color = scales::gradient_n_pal(c("blue", "red"))(effsize))
  pwc.comparisons.effsize <- pwc.comparisons.effsize %>%
    mutate(color = as.character(color))  # Ensure color is a character vector with hex values
  pwc.comparisons.effsize <- as.data.frame(pwc.comparisons.effsize)
  print(pwc.comparisons.effsize)
  
  
  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  if (dummy.facet.x){
    data.mean$dummy <- "dummy" 
  }
  
  n <- length(unique(data[[compare]]))
  if (n == 2){
    x1 <- -0.5
    x2 <- 0.5
  } else if (n == 3){
    x1 <- -1
    x2 <- 1
  }
  # Compute means
  data_line <- data.mean %>%
    group_by(!!sym(group), !!sym(x), !!sym(compare)) %>%
    dplyr::summarise(y = mean(!!sym(y))) |>
    ungroup() |> 
    # Manual dodging 
    mutate(x_num = as.numeric(factor(!!sym(x))) + seq(x1,x2, length.out = n) * 1 / n)
  
  if (length(unique(pwc.comparisons$group1 == r.wilcox$group1)) == 1 &&
      length(unique(pwc.comparisons$group2 == r.wilcox$group2)) == 1) {
    set.seed(100) # for the jitter
    tract.boxplots <- ggplot(
      data.mean,
      aes(
        y = .data[[y]],
        x = .data[[x]],
        color = .data[[compare]]
      )
    ) +
      geom_violin(
        aes(
          fill = .data[[compare]]
        ),
        alpha = 0.22,
        trim = TRUE,
        position = position_dodge(1)
      ) +
      geom_point(
        shape = 21,
        size = 0.85,
        alpha = 0.75,
        position = position_jitterdodge(
          0.5,
          dodge.width = 1
        )
      )+
      geom_boxplot(
        width = 0.22, 
        position = position_dodge(1)
      ) +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 2,
        position = position_dodge(width = 1)
      ) +
      
      geom_line(
        data = data_line,
        aes(
          x = x_num,
          y = y,
          group = .data[[x]]
        ),
        colour = "#003F7D",#"blue"
        size = 1.15
      ) +
      # geom_smooth(
      #   data = data_line,
      #   aes(
      #     x = x_num,
      #     y = y,
      #     group = .data[[x]]
      #   )
      # ) +

      stat_pvalue_manual(
        pwc.comparisons.effsize,
        label.size = stats.size,
        inherit.aes = FALSE,
        tip.length = 0.02,
        hide.ns = FALSE,
        label = stat.label,
        fontface = "bold"#,
        #color = "group1"
      ) +
      ylab("FA") + xlab("Section") +
      labs(
        colour = legend.title,
        fill = legend.title
      ) +
      theme_bw() +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      facet_grid2(
        as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
        axes = "all",
        # independent = "y",
        # remove_labels = "x",
        remove_labels = "all",
        scales = "free"
      ) +
      scale_y_continuous(
        breaks=seq(0.2, 1, 0.1),
        limits=c(NA, NA),
        expand = expansion(mult = c(0.05, 0.15)),
        labels = label_number(
          accuracy = 0.1
        )
      ) +
      #  ylim(NA,0.9) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = text.size),
        strip.text.x = element_text(size = text.size, face =  "bold"),
        strip.text.y = element_text(size = text.size, face = "bold"),
        axis.text = element_text(size = text.size - 4),
        # Hide panel borders and remove grid lines
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour = "black"),
        # facet_wrap without boxes
        strip.background = element_blank(),
        panel.spacing.x = unit(-1, "lines"),
        panel.spacing.y = unit(-1, "lines")
      )
    
    if (dummy.facet.x){
      tract.boxplots <- tract.boxplots +
        labs(title="dummy") +
        theme(
          plot.title = element_text(
            hjust = 0.5, 
            size = text.size+6, 
            color = "white"
          )
        )
      # facet.formula <- paste0(group," ~ dummy")
      # tract.boxplots <- tract.boxplots +
      #   facet_grid2(
      #     as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
      #     axes = "all",
      #     remove_labels = "all",
      #     scales = "free"
      #   ) +
      #   theme(
      #     strip.text.x = element_text(color = "white")
      #   )
    }
    
    if(hide.x.axis) {
      tract.boxplots <- tract.boxplots +
        theme(
          axis.text.x = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white")
        )
    }
    
    if(hide.y.axis) {
      tract.boxplots <- tract.boxplots +
        theme(
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
        )
    }
    
    if (hide.facet.titles){
      if (dummy.facet.x){
        hide.facet.titles <- hide.facet.titles +
          theme(
            strip.text.x = element_text(color = "white"),
            strip.text.y = element_blank()
          )
        
      } else{
        hide.facet.titles <- hide.facet.titles +
          theme(
            strip.text.x = element_blank(),  # Hide horizontal facet titles
            strip.text.y = element_blank()
          )
      }
    }
    
    if (!is.null(figpath)){
      ggsave(
        file = file.path(
          figpath,
          paste0(
            "Fig_two-way_interactions_boxplots_",
            identifier,
            "_x=",x,
            "_y=",y,
            "_color=",compare,
            ".png"
          )
        ),
        height = height,#20,#15,# 4 each plot
        width = width,#24,# 4 each plot + 4 legend (18)
        bg = "white",
        units = "in",
        dpi = 600,
        device = "png",
        tract.boxplots
      )
      
    }
    
  }
  return(
    tract.boxplots
  )
}

double.interaction.lines <- function(
    data,
    metric = "fa",
    compare = "Hemisphere",
    group = "tractlabel",
    x = "part",
    y = "value",
    palette = NULL,
    ylab = "FA",
    xlab = "Section",
    legend.title = "Hemisphere",
    facet.formula = paste0(group," ~ ."),
    x.levels = c("1st", "2nd", "3rd"),
    text.size = 26,
    hide.x.axis = FALSE,
    hide.facet.titles = FALSE,
    dummy.facet.x = FALSE
){
  
  # Subset the data for the specified metric
  summary_data <- data %>%
    filter(meas == metric) %>%
    group_by(!!sym(x), !!sym(compare), !!sym(group)) %>%
    dplyr::summarise(
      mean_value = mean(!!sym(y)),
      # sem = sd(!!sym(y)) / sqrt(n()), 
      # ci_lower = mean_value - sem,  # Error bar lower bound (SEM)
      # ci_upper = mean_value + sem,  # Error bar upper bound (SEM)
      ci_lower = mean_value - 1.96 * (sd(!!sym(y)) / sqrt(n())),
      ci_upper = mean_value + 1.96 * (sd(!!sym(y)) / sqrt(n())),
      # ci_lower = CI(!!sym(y))[1],
      # ci_upper = CI(!!sym(y))[3],
      .groups = "drop"
    ) %>%
    # Make sure the 'part' variable is an ordered factor
    mutate(!!sym(x) := factor(!!sym(x), levels = x.levels))
  
  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  if (dummy.facet.x){
    summary_data$dummy <- "dummy" 
  }
  # Plot the means with CI and connect them
  plt <- ggplot(
    summary_data, 
    aes(
      x = .data[[x]], 
      y = mean_value, 
      color = .data[[compare]], 
      group = .data[[compare]]
    )
  ) +
    geom_point(size = 4) +  # Plot the mean points
    geom_errorbar(
      aes(
        ymin = ci_lower, 
        ymax = ci_upper
      ), 
      width = 0.2
    ) +  # Add confidence intervals
    geom_line(size = 1) +  # Connect the means
    labs(
      y = ylab, 
      x = xlab, 
      color = legend.title,
      fill = legend.title
    ) +
    theme(text = element_text(size = 14)) + 
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    facet_grid2(
      as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
      axes = "all", 
      # independent = "y",
      # remove_labels = "x",
      remove_labels = "all",
      scales = "free"
    ) +
    scale_y_continuous(
      labels = label_number(
        accuracy = 0.1
      )
    ) +
    scale_x_discrete(expand = expansion(mult = c(0.15, 0.15))) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      text = element_text(size = text.size),
      strip.text.x = element_text(size = text.size),
      strip.text.y = element_text(size = text.size, face = "bold"),
      axis.text = element_text(size = text.size - 4),
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "black"),
      # facet_wrap without boxes
      strip.background = element_blank(),
      # legend.position = "none",
      panel.spacing.x = unit(-1, "lines"),
      panel.spacing.y = unit(-1, "lines")
    )
  
  if (dummy.facet.x){
    # facet.formula <- paste0(group," ~ dummy")
    # plt <- plt +
    #   facet_grid2(
    #     as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
    #     axes = "all",
    #     remove_labels = "all",
    #     scales = "free"
    #   ) +
    #   theme(
    #     strip.text.x = element_text(color = "white")
    #   )
    plt <- plt +
      labs(title="dummy") +
      theme(
        plot.title = element_text(
          hjust = 0.5, 
          size = text.size+6, 
          color = "white"
        )
      )
  }
  
  if(hide.x.axis) {
    plt <- plt +
      theme(
        axis.text.x = element_text(colour = "white"),
        axis.title.x = element_text(colour = "white")
      )
  }
  
  if (hide.facet.titles){
    if (dummy.facet.x){
      plt <- plt +
        theme(
          strip.text.x = element_text(color = "white"),
          strip.text.y = element_blank()
        )
      
    } else{
      plt <- plt +
        theme(
          strip.text.x = element_blank(),  # Hide horizontal facet titles
          strip.text.y = element_blank()
        )
    }
  }
  
  return(plt)
}

double.interactionV3 <- function(
    x = "part",
    y = "value",
    group = "tractlabel",
    groups = FO.tracts,
    compare = "Hemisphere",
    legend.title = "Hemisphere",
    abs.cohensd = FALSE,
    text.size = 26,
    stats.size = 5,
    paired = FALSE,
    palette = NULL,
    plt.stats = TRUE,
    data,
    facet.formula = paste0(group," ~ ."),
    figpath = figpath,
    identifier,
    height = length(unique(data[,group]))*3.5,
    width = 9
) {
  
  data <- subset(data, meas == "fa")
  data <- data[data[,group] %in% groups,]
  
  # if PAIRED check that vectors to compare are the same lenght (= all subIDs have both levels to compare)
  if (paired & length(unique(data[,compare])) == 2){
    data.commonID <- c()
    for (f in unique(data[,group])){
      
      levels <- unique(data[,compare])
      l1 <- unique(data[data[,compare] == levels[1] & data[,group] == f, "sub"])
      l2 <- unique(data[data[,compare] == levels[2] & data[,group] == f, "sub"])
      
      subs <- intersect(l1,l2) 
      data.commonID <- rbind(data.commonID, 
                             data[data[,"sub"] %in% subs &
                                    data[,group]  == f,])
    }
    data <- data.commonID
    print("Selected subjects with both levels to compare")
  } else if (!(length(unique(data[,compare])) == 2) & paired) {
    stop("Levels to compare must be equal to 2")
  }
  
  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  "sub")) %>%
    summarise_at(vars(value), list(value = mean))
  
  data.mean <- data.mean %>%
    convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  data.mean <- as.data.frame(data.mean)
  
  # pwc.comparisons <- data.mean %>%
  #   group_by_at(c(x, group)) %>%
  #   pairwise_t_test(as.formula(paste0(y," ~ ",compare)), paired = paired, p.adjust.method = "bonferroni")
  # pwc.comparisons <- pwc.comparisons %>% add_xy_position(x = x, dodge = 1)
  # pwc.comparisons <- as.data.frame(pwc.comparisons)
  # pwc.comparisons <- map.pvalue2stars(pwc.comparisons, "p.adj","p.adj.signif")
  # 
  # cohensd <- data.mean %>%
  #   group_by_at(c(x, group)) %>% 
  #   cohens_d(as.formula(paste0(y," ~ ",compare)), paired = paired, var.equal = paired)#, var.equal = FALSE)
  # cohensd <- as.data.frame(cohensd)
  # 
  # if (abs.cohensd){
  #   effsize <- round(abs(cohensd$effsize), digits = 2)
  #   stat.label <- "{p.adj.signif}, |d| = {effsize}"
  # } else{
  #   effsize <- round(cohensd$effsize, digits = 2)
  #   stat.label <- "{p.adj.signif}, d = {effsize}"
  # } 
  # magnitude <- cohensd$magnitude
  
  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  # if (length(unique(pwc.comparisons$group1 == cohensd$group1)) == 1 &&
  #     length(unique(pwc.comparisons$group2 == cohensd$group2)) == 1) {
    # pwc.comparisons <- cbind(pwc.comparisons, effsize, magnitude)
    tract.boxplots <- ggplot(data.mean, 
                             aes(y = .data[[y]], 
                                 x = .data[[x]],
                                 color = .data[[compare]])) +
      geom_violin(aes(fill = .data[[compare]]), alpha = 0.25, trim = TRUE, position = position_dodge(1)) +
      geom_point(shape = 21, size = 1, position = position_jitterdodge(0.5, dodge.width = 1))+#0.5, dodge.width = 1)) +
      geom_boxplot(width = 0.22, position = position_dodge(1)) +
      geom_smooth(inherit.aes = FALSE, data = data.mean, 
                  aes(x = .data[[x]], 
                      y = .data[[y]],
                      group = interaction(.data[[x]],.data[[compare]])), method="lm")
    # if (plt.stats){
    #   tract.boxplots <- tract.boxplots +
    #     stat_pvalue_manual(pwc.comparisons, 
    #                        #fontface = "bold",
    #                        label.size = stats.size,
    #                        inherit.aes = FALSE, 
    #                        tip.length = 0.02, 
    #                        hide.ns = FALSE,
    #                        label = stat.label)
    # }
    tract.boxplots <- tract.boxplots + 
      ylab("FA") + xlab("Section") + labs(colour = legend.title, fill = legend.title) +
      theme_bw() +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      # facet_nested(formula(paste0(tractgroup,"~",groupping))) +
      facet_grid2(as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
                  axes = "all", 
                  # independent = "y",
                  # remove_labels = "x",
                  remove_labels = "all",
                  scales = "free") +
      # facet_wrap2(as.formula(paste(tractgroup,"~",groupping)),#fulltractlabel ~ Gender + Hemisphere,
      #             # axes = "all", 
      #             # # independent = "y",
      #             # # remove_labels = "x",
      #             # remove_labels = "all",
      #             # scales = "free",
      #             ncol = 4,
      #             trim_blank = FALSE) +
      # scale_y_continuous(breaks = c(0,seq(0.2, 0.8, by = 0.1))) +
      # expand_limits(y = c(0.1, 0.9)) +
      scale_y_continuous(breaks=seq(0.2, 1, 0.1), limits=c(NA, NA), expand = expansion(mult = c(0.05, 0.15))) +
      #  ylim(NA,0.9) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            text = element_text(size = text.size),
            strip.text.x = element_text(size = text.size, face =  "bold"),
            strip.text.y = element_text(size = text.size, face = "bold"),
            axis.text = element_text(size = text.size - 4),
            # Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change axis line
            axis.line = element_line(colour = "black"),
            # facet_wrap without boxes
            strip.background = element_blank())
    
    ggsave(file = file.path(figpath,
                            paste0("Fig_two-way_interactions_boxplots_",identifier,"_x=",x,"_y=",y,"_color=",compare,".png")),
           height = height,#20,#15,# 4 each plot
           width = width,#24,# 4 each plot + 4 legend (18)
           bg = "white",
           units = "in",
           dpi = 600,
           device = "png",
           tract.boxplots)
  # }
  return(tract.boxplots)
}

plt.boxplot.trajectories <- function(
    x = "part",
    y = "value",
    group = "tractlabel",
    groups = FO.tracts,
    compare = "Hemisphere",
    legend.title = "Hemisphere",
    xlabel = "Section",
    text.size = 26,
    palette = NULL,
    data,
    facet.formula = paste0(group," ~ ."),
    figpath = figpath,
    identifier,
    height = length(unique(data[,group]))*3.5,
    width = 9
) {
  
  data <- subset(data, meas == "fa")
  data <- data[data[,group] %in% groups,]
  
  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  "sub")) %>%
    summarise_at(vars(value), list(value = mean))
  
  data.mean <- data.mean %>%
    convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  data.mean <- as.data.frame(data.mean)
  
  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  tract.boxplots <- ggplot(data.mean, 
                           aes(y = .data[[y]], 
                               x = .data[[x]],
                               color = .data[[compare]])) +
    geom_violin(aes(fill = .data[[compare]]), alpha = 0.25, trim = TRUE, position = position_dodge(1)) +
    geom_point(shape = 21, size = 1, position = position_jitterdodge(0.5, dodge.width = 1))+#0.5, dodge.width = 1)) +
    geom_boxplot(width = 0.22, position = position_dodge(1)) +
    geom_smooth(inherit.aes = FALSE, data = data.mean, 
                method = "glm",
                aes(x = as.integer(.data[[x]]), 
                    y = .data[[y]]), formula = y ~ poly(x,2)) +
    # stat_smooth(data=subset(ATLAS,Hemisphere == "Right" & Gender=="Female"),
    #             method = "lm", formula = model.fem.right, 
    #             alpha=0, size = line.width) +
    ylab("FA") + xlab(xlabel) + labs(colour = legend.title, fill = legend.title) +
    theme_bw() +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    # facet_nested(formula(paste0(tractgroup,"~",groupping))) +
    facet_grid2(as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
                axes = "all", 
                # independent = "y",
                # remove_labels = "x",
                remove_labels = "all",
                scales = "free") +

    scale_y_continuous(breaks=seq(0.2, 1, 0.1), limits=c(NA, NA), expand = expansion(mult = c(0.05, 0.15))) +
    #  ylim(NA,0.9) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          text = element_text(size = text.size),
          strip.text.x = element_text(size = text.size, face =  "bold"),
          strip.text.y = element_text(size = text.size, face = "bold"),
          axis.text = element_text(size = text.size - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  ggsave(file = file.path(figpath,
                          paste0("Fig_two-way_interactions_boxplots_",identifier,"_x=",x,"_y=",y,"_color=",compare,".png")),
         height = height,#20,#15,# 4 each plot
         width = width,#24,# 4 each plot + 4 legend (18)
         bg = "white",
         units = "in",
         dpi = 300,
         device = "png",
         tract.boxplots)
  # }
  return(tract.boxplots)
}


double.interactionV3 <- function(
    x = "part",
    y = "value",
    group = "tractlabel",
    groups = FO.tracts,
    compare = "Hemisphere",
    legend.title = "Hemisphere",
    abs.cohensd = FALSE,
    text.size = 26,
    stats.size = 5,
    paired = FALSE,
    palette = NULL,
    data,
    facet.formula = paste0(group," ~ ."),
    figpath = figpath,
    identifier,
    height = length(unique(data[,group]))*3.5,
    width = 9
) {
  
  data <- subset(data, meas == "fa")
  data <- data[data[,group] %in% groups,]
  
  # if PAIRED check that vectors to compare are the same lenght (= all subIDs have both levels to compare)
  if (paired & length(unique(data[,compare])) == 2){
    data.commonID <- c()
    for (f in unique(data[,group])){
      
      levels <- unique(data[,compare])
      l1 <- unique(data[data[,compare] == levels[1] & data[,group] == f, "sub"])
      l2 <- unique(data[data[,compare] == levels[2] & data[,group] == f, "sub"])
      
      subs <- intersect(l1,l2) 
      data.commonID <- rbind(data.commonID, 
                             data[data[,"sub"] %in% subs &
                                    data[,group]  == f,])
    }
    data <- data.commonID
    print("Selected subjects with both levels to compare")
  } else if (!(length(unique(data[,compare])) == 2) & paired) {
    stop("Levels to compare must be equal to 2")
  }
  
  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  "sub")) %>%
    summarise_at(vars(value), list(value = mean))
  
  data.mean <- data.mean %>%
    convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  data.mean <- as.data.frame(data.mean)
  
  pwc.comparisons <- data.mean %>%
    group_by_at(c(x, group)) %>%
    pairwise_t_test(as.formula(paste0(y," ~ ",compare)), paired = paired, p.adjust.method = "bonferroni")
  pwc.comparisons <- pwc.comparisons %>% add_xy_position(x = x, dodge = 1)
  pwc.comparisons <- as.data.frame(pwc.comparisons)
  pwc.comparisons <- map.pvalue2stars(pwc.comparisons, "p.adj","p.adj.signif")
  
  cohensd <- data.mean %>%
    group_by_at(c(x, group)) %>% 
    cohens_d(as.formula(paste0(y," ~ ",compare)), paired = paired, var.equal = paired)#, var.equal = FALSE)
  cohensd <- as.data.frame(cohensd)
  
  cohensd <- merge(pwc.comparisons,cohensd,by=c("group1","group2",group,x,".y.","n1","n2"))
  pwc.comparisons <- cohensd
  
  
  if (abs.cohensd){
    effsize <- round(abs(cohensd$effsize), digits = 2)
    stat.label <- "{p.adj.signif}, |d| = {effsize}"
  } else{
    effsize <- round(cohensd$effsize, digits = 2)
    stat.label <- "{p.adj.signif}, d = {effsize}"
  } 
  magnitude <- cohensd$magnitude
  
  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  if (length(unique(pwc.comparisons$group1 == cohensd$group1)) == 1 &&
      length(unique(pwc.comparisons$group2 == cohensd$group2)) == 1) {
    # pwc.comparisons <- cbind(pwc.comparisons, effsize, magnitude)
    tract.boxplots <- ggplot(data.mean, 
                             aes(y = .data[[y]], 
                                 x = .data[[x]],
                                 color = .data[[compare]])) +
      geom_violin(aes(fill = .data[[compare]]), alpha = 0.25, trim = TRUE, position = position_dodge(1)) +
      geom_point(shape = 21, size = 1, position = position_jitterdodge(0.5, dodge.width = 1))+#0.5, dodge.width = 1)) +
      geom_boxplot(width = 0.22, position = position_dodge(1)) +
      # stat_pvalue_manual(pwc.comparisons, 
      #                    tip.length = 0.02, 
      #                    vjust = -0.5, 
      #                    hide.ns = FALSE,
      #                    size = stats.size,# fontface = "bold",
      #                    label = stat.label) +
      stat_pvalue_manual(pwc.comparisons, 
                         #fontface = "bold",
                         label.size = stats.size,
                         inherit.aes = FALSE, 
                         tip.length = 0.02, 
                         hide.ns = FALSE,
                         label = stat.label) +
      ylab("FA") + xlab("Section") + labs(colour = legend.title, fill = legend.title) +
      theme_bw() +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      # facet_nested(formula(paste0(tractgroup,"~",groupping))) +
      facet_grid2(as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
                  axes = "all", 
                  # independent = "y",
                  # remove_labels = "x",
                  remove_labels = "all",
                  scales = "free") +
      # facet_wrap2(as.formula(paste(tractgroup,"~",groupping)),#fulltractlabel ~ Gender + Hemisphere,
      #             # axes = "all", 
      #             # # independent = "y",
      #             # # remove_labels = "x",
      #             # remove_labels = "all",
      #             # scales = "free",
      #             ncol = 4,
      #             trim_blank = FALSE) +
      # scale_y_continuous(breaks = c(0,seq(0.2, 0.8, by = 0.1))) +
      # expand_limits(y = c(0.1, 0.9)) +
      scale_y_continuous(breaks=seq(0.2, 1, 0.1), limits=c(NA, NA), expand = expansion(mult = c(0.05, 0.15))) +
      #  ylim(NA,0.9) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            text = element_text(size = text.size),
            strip.text.x = element_text(size = text.size, face =  "bold"),
            strip.text.y = element_text(size = text.size, face = "bold"),
            axis.text = element_text(size = text.size - 4),
            # Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change axis line
            axis.line = element_line(colour = "black"),
            # facet_wrap without boxes
            strip.background = element_blank())
    
    ggsave(file = file.path(figpath,
                            paste0("Fig_two-way_interactions_boxplots_",identifier,"_x=",x,"_y=",y,"_color=",compare,".png")),
           height = height,#20,#15,# 4 each plot
           width = width,#24,# 4 each plot + 4 legend (18)
           bg = "white",
           units = "in",
           dpi = 600,
           device = "png",
           tract.boxplots)
  }
  return(tract.boxplots)
}

double.interactionV4 <- function(
    x = "part",
    y = "value",
    group = "tractlabel",
    groups = FO.tracts,
    compare = "Hemisphere",
    legend.title = "Hemisphere",
    abs.cohensd = FALSE,
    text.size = 26,
    stats.size = 5,
    paired = FALSE,
    palette = NULL,
    data,
    facet.formula = paste0(group," ~ ."),
    figpath = figpath,
    identifier,
    height = length(unique(data[,group]))*3.5,
    width = 9
) {
  
  data <- subset(data, meas == "fa")
  data <- data[data[,group] %in% groups,]
  
  # if PAIRED check that vectors to compare are the same lenght (= all subIDs have both levels to compare)
  if (paired & length(unique(data[,compare])) == 2){
    data.commonID <- c()
    for (f in unique(data[,group])){
      
      levels <- unique(data[,compare])
      l1 <- unique(data[data[,compare] == levels[1] & data[,group] == f, "sub"])
      l2 <- unique(data[data[,compare] == levels[2] & data[,group] == f, "sub"])
      
      subs <- intersect(l1,l2) 
      data.commonID <- rbind(data.commonID, 
                             data[data[,"sub"] %in% subs &
                                    data[,group]  == f,])
    }
    data <- data.commonID
    print("Selected subjects with both levels to compare")
  } else if (!(length(unique(data[,compare])) == 2) & paired) {
    stop("Levels to compare must be equal to 2")
  }
  
  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Gender",
                  "meas",
                  "sub")) %>%
    summarise_at(vars(value), list(value = mean))
  
  data.mean <- data.mean %>%
    convert_as_factor(sub, agegroup, part, Gender)
  data.mean <- as.data.frame(data.mean)
  
  pwc.comparisons <- data.mean %>%
    group_by_at(c(x, group)) %>%
    pairwise_t_test(as.formula(paste0(y," ~ ",compare)), paired = paired, p.adjust.method = "bonferroni")
  pwc.comparisons <- pwc.comparisons %>% add_xy_position(x = x, dodge = 1)
  pwc.comparisons <- as.data.frame(pwc.comparisons)
  pwc.comparisons <- map.pvalue2stars(pwc.comparisons, "p.adj","p.adj.signif")
  
  cohensd <- data.mean %>%
    group_by_at(c(x, group)) %>% 
    cohens_d(as.formula(paste0(y," ~ ",compare)), paired = paired, var.equal = paired)#, var.equal = FALSE)
  cohensd <- as.data.frame(cohensd)
  
  if (abs.cohensd){
    effsize <- round(abs(cohensd$effsize), digits = 2)
    stat.label <- "{p.adj.signif}, |d| = {effsize}"
  } else{
    effsize <- round(cohensd$effsize, digits = 2)
    stat.label <- "{p.adj.signif}, d = {effsize}"
  } 
  magnitude <- cohensd$magnitude
  
  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  if (length(unique(pwc.comparisons$group1 == cohensd$group1)) == 1 &&
      length(unique(pwc.comparisons$group2 == cohensd$group2)) == 1) {
    pwc.comparisons <- cbind(pwc.comparisons, effsize, magnitude)
    tract.boxplots <- ggplot(data.mean, 
                             aes(y = .data[[y]], 
                                 x = .data[[x]],
                                 color = .data[[compare]])) +
      geom_violin(aes(fill = .data[[compare]]), alpha = 0.25, trim = TRUE, position = position_dodge(1)) +
      geom_point(shape = 21, size = 1, position = position_jitterdodge(0.5, dodge.width = 1))+#0.5, dodge.width = 1)) +
      geom_boxplot(width = 0.22, position = position_dodge(1)) +
      # stat_pvalue_manual(pwc.comparisons, 
      #                    tip.length = 0.02, 
      #                    vjust = -0.5, 
      #                    hide.ns = FALSE,
      #                    size = stats.size,# fontface = "bold",
      #                    label = stat.label) +
      stat_pvalue_manual(pwc.comparisons, 
                         #fontface = "bold",
                         label.size = stats.size,
                         inherit.aes = FALSE, 
                         tip.length = 0.02, 
                         hide.ns = FALSE,
                         label = stat.label) +
      ylab("FA") + xlab("Section") + labs(colour = legend.title, fill = legend.title) +
      theme_bw() +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      # facet_nested(formula(paste0(tractgroup,"~",groupping))) +
      facet_grid2(as.formula(facet.formula),#fulltractlabel ~ Gender + Hemisphere,
                  axes = "all", 
                  # independent = "y",
                  # remove_labels = "x",
                  remove_labels = "all",
                  scales = "free") +
      # facet_wrap2(as.formula(paste(tractgroup,"~",groupping)),#fulltractlabel ~ Gender + Hemisphere,
      #             # axes = "all", 
      #             # # independent = "y",
      #             # # remove_labels = "x",
      #             # remove_labels = "all",
      #             # scales = "free",
      #             ncol = 4,
      #             trim_blank = FALSE) +
      # scale_y_continuous(breaks = c(0,seq(0.2, 0.8, by = 0.1))) +
      # expand_limits(y = c(0.1, 0.9)) +
      scale_y_continuous(breaks=seq(0.2, 1, 0.1), limits=c(NA, NA), expand = expansion(mult = c(0.05, 0.15))) +
      #  ylim(NA,0.9) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            text = element_text(size = text.size),
            strip.text.x = element_text(size = text.size, face =  "bold"),
            strip.text.y = element_text(size = text.size, face = "bold"),
            axis.text = element_text(size = text.size - 4),
            # Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change axis line
            axis.line = element_line(colour = "black"),
            # facet_wrap without boxes
            strip.background = element_blank())
    
    ggsave(file = file.path(figpath,
                            paste0("Fig_two-way_interactions_boxplots_",identifier,"_x=",x,"_y=",y,"_color=",compare,".png")),
           height = height,#20,#15,# 4 each plot
           width = width,#24,# 4 each plot + 4 legend (18)
           bg = "white",
           units = "in",
           dpi = 600,
           device = "png",
           tract.boxplots)
  }
  return(tract.boxplots)
}


double.interaction.pro <- function(
    x = "part",
    y = "value",
    group = "tractlabel",
    groups = FO.tracts,
    compare = "Hemisphere",
    metric = "fa",
    ID = "sub",
    abs.effsize = FALSE,
    legend.title = "Hemisphere",
    palette = NULL,
    text.size = 26,
    stats.size = 5,
    paired = FALSE,
    data,
    figpath = NULL,
    identifier,
    hide.y.axis = FALSE,
    hide.x.axis = FALSE,
    show.axes = "all",
    dummy.facet.x = FALSE,
    switch.facet.x = FALSE,
    xlabel = NULL
    ) {
  data <- subset(data, meas == metric)
  data <- data[data[,group] %in% groups,]
  
  data.mean <- data %>%
    group_by_at(c("part",
                  "agegroup",
                  group,
                  "Hemisphere",
                  "Gender",
                  "meas",
                  ID)) %>%
    summarise_at(vars(value), list(value = mean))
  
  data.mean <- data.mean %>%
    convert_as_factor(sub, agegroup, part, Gender, Hemisphere)
  data.mean <- as.data.frame(data.mean)

  # if PAIRED check that vectors to compare are the same lenght (= all subIDs have both levels to compare)
  if (paired & length(unique(data.mean[,compare])) == 2){
    data.commonID <- c()
    for (f in unique(data.mean[,group])){

      levels <- unique(data.mean[,compare])
      l1 <- unique(data.mean[data.mean[,compare] == levels[1] & data.mean[,group] == f, ID])
      l2 <- unique(data.mean[data.mean[,compare] == levels[2] & data.mean[,group] == f, ID])

      subs <- intersect(l1,l2) 
      data.commonID <- rbind(
        data.commonID, 
        data.mean[data.mean[,ID] %in% subs &
                    data.mean[,group]  == f,]
      )
    }
    data.mean <- data.commonID
  } else if (!(length(unique(data.mean[,compare])) == 2) & paired) {
     stop("Levels to compare must be equal to 2")
  }

  # no paired and non parametric comparisons
  pwc.comparisons <- data.mean %>%
    group_by_at(c(group, x)) %>%
    rstatix::pairwise_wilcox_test(
      as.formula(paste(y,compare, sep = "~")),
      p.adjust.method = "bonf",
      paired = TRUE
    )
  
  # Compute effect size (r)
  r.wilcox <- data.mean %>%
    group_by_at(c(group, x)) %>%
    rstatix::wilcox_effsize(
      as.formula(paste(y, compare, sep = "~")),
      paired = TRUE
    )
  r.wilcox <- as.data.frame(r.wilcox)
  
  pwc.comparisons.effsize <- pwc.comparisons %>%
    left_join(
      r.wilcox, 
      by = c(group, x, "group1", "group2")
    )
  
  if (abs.effsize){
    pwc.comparisons.effsize$effsize <-
      round(abs(pwc.comparisons.effsize$effsize), digits = 2)
    stat.label <- "{p.adj.signif}, |r|={effsize}"
  } else{
    pwc.comparisons.effsize$effsize <- 
      round(pwc.comparisons.effsize$effsize, digits = 2)
    stat.label <- "{p.adj.signif}, r={effsize}"
  } 
  
  # Format the effect size to display without leading zero for values < 1 and 
  # show .00 for zero
  pwc.comparisons.effsize$effsize <- ifelse(
    pwc.comparisons.effsize$effsize == 0, ".00", 
    sub("^0", "", sprintf("%.2f", pwc.comparisons.effsize$effsize))
  )
  
  pwc.comparisons.effsize <- map.pvalue2stars(
    pwc.comparisons.effsize,
    "p.adj",
    "p.adj.signif"
  )
  
  pwc.comparisons.effsize <- pwc.comparisons.effsize %>% 
    add_xy_position(x = x, dodge = 1)
  pwc.comparisons.effsize <- as.data.frame(pwc.comparisons.effsize)
  
  if (is.null(palette)){
    palette <- brewer.pal(5,"Set1")
    palette <- palette[1:length(unique(data[,compare]))] 
  }
  
  n <- length(unique(data[[compare]]))
  if (n == 2){
    x1 <- -0.5
    x2 <- 0.5
  } else if (n == 3){
    x1 <- -1
    x2 <- 1
  }
  # Compute means
  data_line <- data.mean %>%
    group_by(!!sym(group), !!sym(x), !!sym(compare)) %>%
    dplyr::summarise(y = mean(!!sym(y))) %>%
    ungroup() 
  
  # pwc.comparisons <- data.mean %>%
  #   group_by_at(c(x, group)) %>%
  #   pairwise_t_test(as.formula(paste0(y," ~ ",compare)), paired = paired, p.adjust.method = "bonferroni")
  # pwc.comparisons <- pwc.comparisons %>% add_xy_position(x = compare, dodge = 1)
  # pwc.comparisons <- as.data.frame(pwc.comparisons)
  # cohensd <- data.mean %>%
  #   group_by_at(c(x, group)) %>% 
  #   cohens_d(as.formula(paste0(y," ~ ",compare)), paired = paired, var.equal = paired)#, var.equal = FALSE)
  # cohensd <- as.data.frame(cohensd)
  # if (abs.cohensd){
  #   effsize <- round(abs(cohensd$effsize), digits = 2)
  #   stat.label <- "{p.adj.signif}, |d| = {effsize}"
  # } else{
  #   effsize <- round(cohensd$effsize, digits = 2)
  #   stat.label <- "{p.adj.signif}, d = {effsize}"
  # } 
  # # effsize <- round(abs(cohensd$effsize), digits = 2)
  # magnitude <- cohensd$magnitude
  
  if (length(unique(pwc.comparisons$group1 == r.wilcox$group1)) == 1 &&
      length(unique(pwc.comparisons$group2 == r.wilcox$group2)) == 1) {
    
    set.seed(100) # for the jitter
    
    tract.boxplots <- ggplot(
      data.mean, 
      aes(
        y = .data[[y]], 
        x = .data[[compare]],
        color = .data[[compare]]
      )
    ) +
      geom_violindot(
        aes(fill = .data[[compare]]),
        dots_size = 0, 
        alpha = 0.25,
        position_dots = position_dodge(0.1),
        flip = c(1)
      ) +
      geom_point(
        aes(group = .data[[ID]]),
        shape = 21, 
        size = 1, 
        position = position_dodge(0.2)
      ) +
      geom_boxplot(
        width = 0.22, 
        position = position_dodge(0.1)
      ) +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 2,
        position = position_dodge(width = 1)
      ) +
      geom_line(
        data = data_line,
        aes(
          x = .data[[compare]],
          y = y,
          group = .data[[x]]
        ),
        colour = "#003F7D",#"blue"
        size = 1.15
      ) +
      # stat_pvalue_manual(pwc.comparisons, inherit.aes = FALSE, tip.length = 0.02, hide.ns = FALSE,
      #                   label = "{p.adj.signif}, |d| = {effsize}") +
      add_pvalue(
        pwc.comparisons.effsize, 
        fontface = "bold",
        label.size = stats.size,
        inherit.aes = FALSE, 
        tip.length = 0.02, 
        vjust = -0.5,
        hide.ns = FALSE,
        label = stat.label
      ) +
      geom_line(
        aes(group = .data[[ID]]),
        position = position_dodge(0.2),
        color = "black",
        alpha = 0.075
      ) +                  
      ylab("FA") + 
      labs(
        colour = legend.title, 
        fill = legend.title
      ) +
      theme_bw() +
      # scale_color_brewer(palette="Set1") +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      # facet_nested(formula(paste0(tractgroup,"~",groupping))) +
      facet_grid2(
        as.formula(
          paste0(group," ~ part")
        ),#fulltractlabel ~ Gender + Hemisphere,
        axes = show.axes, 
        # independent = "y",
        # remove_labels = "x",
        remove_labels = "all",
        scales = "free"
      ) +
      # facet_wrap2(as.formula(paste(tractgroup,"~",groupping)),#fulltractlabel ~ Gender + Hemisphere,
      #             # axes = "all", 
      #             # # independent = "y",
      #             # # remove_labels = "x",
      #             # remove_labels = "all",
      #             # scales = "free",
      #             ncol = 4,
      #             trim_blank = FALSE) +
    # scale_y_continuous(breaks = c(0,seq(0.2, 0.8, by = 0.1))) +
    # expand_limits(y = c(0.1, 0.9)) +
  #   ggh4x::facetted_pos_scales(
  #   y = scale_y_continuous(breaks=seq(0.2, 1, 0.1), limits=c(NA, NA))
  # ) +
      scale_y_continuous(
        breaks=seq(0.2, 1, 0.1), 
        limits=c(NA, NA), 
        expand = expansion(mult = c(0.05, 0.15)) 
      ) +
      scale_x_discrete(
        expand = expansion(mult = c(0, 0.85))
      ) +
    #  ylim(NA,0.9) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = text.size),
        strip.text.x = element_text(size = text.size),
        strip.text.y = element_text(size = text.size, face = "bold"),
        axis.text = element_text(size = text.size - 4),
        # Hide panel borders and remove grid lines
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour = "black"),
        # facet_wrap without boxes
        strip.background = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(-1, "lines"),
        panel.spacing.y = unit(-1, "lines")
      )
    
    if (dummy.facet.x){
      tract.boxplots <- tract.boxplots +
        labs(title="dummy") +
        theme(
          plot.title = element_text(
            hjust = 0.5, 
            size = text.size+6, 
            color = "white"
          )
        )
    }
    
    if(hide.y.axis) {
      tract.boxplots <- tract.boxplots +
        theme(
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          # axis.text.y = element_text(
          #   size=text.size-4, 
          #   colour = "white"
          # ),
          # axis.title.y = element_text(
          #   size=text.size-4, 
          #   colour = "white"
          # ),
          panel.spacing.x = unit(0, "lines"),
          panel.spacing.y = unit(-1, "lines")
        )
    }
    
    if(hide.x.axis && is.null(xlabel)) {
      tract.boxplots <- tract.boxplots +
        theme(
          axis.text.x = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white")
        )
    }
    
    if (switch.facet.x) {
      tract.boxplots <- tract.boxplots +
        facet_grid2(
          as.formula(
            paste0(group," ~ part")
          ),
          axes = "x", 
          remove_labels = "all",
          scales = "free",
          switch="x"
        ) +
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_text(
            family = "sans",
            size = text.size
          ),
          axis.ticks.x = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(
            family = "sans",
            size = text.size-4, 
            color = "grey30",
            margin = margin(t = 0, b = 0)
          ),
          panel.spacing.x = unit(0, "lines"),
          panel.spacing.y = unit(0.83, "lines")
        ) 
    }
    
    if (!is.null(xlabel)){
      tract.boxplots <- tract.boxplots +
        labs(x=xlabel)
    }

    if (!is.null(figpath)){
      ggsave(
        file = file.path(
          figpath,
          paste0(
            "Fig_two-way_interactions_boxplots_",
            identifier,
            "_x=",x,
            "_y=",y,
            "_color=",compare,
            ".png"
          )
        ),
        height = length(unique(data[,group]))*3.5,#20,#15,# 4 each plot
        width = 5*2,#24,# 4 each plot + 4 legend (18)
        bg = "white",
        units = "in",
        dpi = 600,
        device = "png",
        tract.boxplots
      )
    }
      
  }
    
  return(tract.boxplots)
  
}



average.col.list <- function(x) {
  df <- data.frame(matrix(ncol = 2, nrow = 1))
  colnames(df) <- c("avgvector","xlen")
  df$avgvector <- vector(mode = "list",length=nrow(df))
  df$xlen <- vector(mode = "list",length=nrow(df))
                         
  x <- as.data.frame(x)
  
  avg.vector <- colMeans(do.call(rbind,x[,"val"]))
  df$avgvector[[1]] <- avg.vector
  df$xlen[[1]] <- c(1:length(avg.vector))
  
  return(df)
}

nlmer.results <- function(
    data,
    dependent.variable = "Whole_thalamus",
    fixed_effects = "AGE* Hemisphere * Gender",
    nonlinear.dependent.var = "part",
    random.var = NULL
){
  
  if (!is.null(random.var)){
    random.effects = as.formula(
      paste0(
        "A + B + C ~ ",
        nonlinear.dependent.var,
        " | ",
        random.var
      )
    )
  } else{
    random.effects = NULL
  }
  
  fixed.effects = as.formula(
    paste0(
      "A + B + C ~ ",
      fixed_effects
    )
  )
  
  ##############################################################################
  # QUADRATIC MODEL
  ##############################################################################
  quadr.eq = as.formula(
    paste0(
      dependent.variable,
      " ~ ",
      "A *", 
      nonlinear.dependent.var,
      "^2 + B * ",
      nonlinear.dependent.var,
      " + C"
    )
  )
  
  print(quadr.eq)
  print(fixed.effects)
  print(random.effects)
  
  # Step 1: Extract the min and max values and their positions
  valmax <- max(data[[dependent.variable]], na.rm = TRUE)  # Maximum value of valmean
  valmin <- min(data[[dependent.variable]], na.rm = TRUE)  # Minimum value of valmean
  print(c(valmin,valmax))

  # Find the positions of the max and min
  # pos_max <- which.max(data[[dependent.variable]])  # Index of max
  # pos_min <- which.min(data[[dependent.variable]])  # Index of min

  # Step 2: Identify the "Part" values corresponding to max and min
  part_max <- data[data[[dependent.variable]] == valmax, nonlinear.dependent.var]
  part_min <- data[data[[dependent.variable]] == valmin, nonlinear.dependent.var]
  print(c(part_max,part_min))

  # Step 3: Guess starting parameters based on max, min, and positions
  # Let's initialize A, B, and C based on these extreme values:

  # Estimate A, B, C from max and min values
  A_guess <- (valmax - valmin) / ((part_max^2 - part_min^2) * (part_max - part_min))  # This estimates the curvature
  B_guess <- (valmax - valmin - A_guess * (part_max^2 - part_min^2)) / (part_max - part_min)  # Linear term estimate
  C_guess <- valmin - A_guess * part_min^2 - B_guess * part_min  # Constant term estimate

  # Print the guessed parameters
  cat("Estimated starting values:\n")
  cat("A = ", A_guess, "\n")
  cat("B = ", B_guess, "\n")
  cat("C = ", C_guess, "\n")
  
  quad_model <- nlme(
    quadr.eq,  # Quadratic equation
    data = data,
    fixed = fixed.effects,#A + B + C ~ agegroup * Hemisphere * Gender,  # Fixed effects
    random = random.effects,  # Random effects on A (subject-level variation)
    start = c(
      A = -0.1,#A_guess,
      B = 0.5,#B_guess,
      C = 1,#C_guess
      control = nlmeControl(maxIter = 500, msMaxIter = 500, pnlsTol = 1e-5),
      method = "REML"
    )  # Initial estimates
  )

  # return(quad_model)
  
  ##############################################################################
  # SIGMOID MODEL
  ##############################################################################
  sigmoid.eq = as.formula(
    paste0(
      dependent.variable,
      " ~ ",
      "A / (1 + exp(-B*(",
      nonlinear.dependent.var,
      "- C)))"
    )
  )

  # sigmoid_model <- nlme(
  #   sigmoid.eq,  # Sigmoid function
  #   data = data,
  #   fixed = fixed.effects,
  #   random = random.effects,
  #   start = c(
  #     A = max(
  #       data[[nonlinear.dependent.var]]
  #     ),
  #     B = 1,
  #     C = mean(
  #       data[[nonlinear.dependent.var]]
  #     )
  #   )  # Initialize within 0.4-0.7
  # )
  
  return(quad_model)
}

anova.lmer.results <- function(
    data,
    dependent_variable = "Whole_thalamus",
    fixed_effects = "AGE * Hemisphere * Gender",
    random_effects = NULL, # "(1 | ID)"
    bonferroni_comparisons = NULL
){
  
  if (is.null(random_effects)) {
    eq <- paste(
      dependent_variable,
      fixed_effects,
      sep="~"
    )
  } else {
    eq <- as.formula(
      paste(
        paste(
          dependent_variable,
          fixed_effects,
          sep="~"
        ), 
        random_effects, 
        sep = " + "
      )
    )
  }
  
  print(eq)
  
  # MODEL
  model.lmer <- lmer(
    eq,
    data = data
  )
  
  # ANOVA
  # LMER
  aov.lmer <- stats::anova(model.lmer, type="III")
  num_participants <- length(unique(data[["sub"]]))
  cat("Number of participants: ", num_participants, "\n")
  aov.lmer.table <- data.frame(aov.lmer)
  aov.lmer.table <- cbind(
    effects = row.names(aov.lmer.table),
    aov.lmer.table
  )
  row.names(aov.lmer.table) <- NULL
  pval.lmer.agebyhem <- aov.lmer.table[
    aov.lmer.table$effects == "AGE:Hemisphere","Pr..F."
  ]
  
  t <- c()
  for (r in 1:nrow(aov.lmer.table)){
    txt <- paste0(
      aov.lmer.table[r,"effects"],
      " : ",
      "F(",
      aov.lmer.table[r,"NumDF"],
      ",",
      round(aov.lmer.table[r,"DenDF"]),
      ") = ",
      round(aov.lmer.table[r,"F.value"], digits = 3),
      ", ",
      if(!is.null(bonferroni_comparisons)){
        bonferroni_p_value_stars(
          aov.lmer.table[r,"Pr..F."],
          alpha = 0.05,
          n_tests = bonferroni_comparisons
        )
      }else{
        p_value_stars(aov.lmer.table[r,"Pr..F."])
      }#p_value_stars(aov.lmer.table[r,"Pr..F."])
    )
    t <- rbind(t,txt)
  }
  
  t <- data.frame(ANOVA = t)
  
  row.names(t) <- NULL
  
  aov.lmer.table2 <- aov.lmer.table
  aov.lmer.table2$F.value <- paste0(
    "F(",
    aov.lmer.table2[,"NumDF"],
    ",",
    round(aov.lmer.table2[,"DenDF"]),
    ") = ",
    round(aov.lmer.table2[,"F.value"], 
          digits = 3)
  )
  aov.lmer.table2 <- aov.lmer.table2[,-c(2,3,4,5)]
  
  aov.lmer.table2[,"Pr..F."] <- sapply(
    aov.lmer.table2[,"Pr..F."], 
    function(p) {
      if (!is.null(bonferroni_comparisons)) {
        bonferroni_p_value_stars(
          p, 
          alpha = 0.05, 
          n_tests = bonferroni_comparisons
        )
      } else {
        p_value_stars(p)
      }
    }
  )
  
  names(aov.lmer.table2)[
    names(aov.lmer.table2) == "effects"
  ] <- "Effects"
  names(aov.lmer.table2)[
    names(aov.lmer.table2) == "F.value"
  ] <- "F-value"
  names(aov.lmer.table2)[
    names(aov.lmer.table2) == "Pr..F."
  ] <- "P-value"
  
  print(aov.lmer.table2)
  
  aov.lmer.table2.print <- flextable(data = aov.lmer.table2)
  aov.lmer.table2.print <- aov.lmer.table2.print %>%
    width(j = 2,
          width = 1.75) %>%
    width(j = 3,
          width = 1)
  
  return(
    list(
      anova.table = aov.lmer.table,
      pval.agebyhem = pval.lmer.agebyhem,
      anova.stats = t,
      anova.stats.table = aov.lmer.table2,
      anova.stats.table.print = aov.lmer.table2.print
    )
  )
  
}


p_value_stars <- function(p) {
  if(p < 0.001) {
    return("P<.001 ***")
  } else if(p < 0.01) {
    return("P<.01 **")
  } else if(p < 0.05) {
    return("P<.05 *")
  } else {
    return(
      sprintf(
        "P=.%03d ns", 
        round(p * 1000)
      )
    )  # Not significant
  }
}

bonferroni_p_value_stars <- function(
    p, 
    alpha, 
    n_tests
    ) {
  
  threshold <- alpha / n_tests  # Compute Bonferroni-corrected threshold
  threshold <- round(threshold, 4) # round threshold
  threshold_str <- sprintf("%.4f", threshold)  # Format threshold to 4 decimals
  
  # Determine significance based on the Bonferroni threshold
  if (p < threshold) {
    if (p < 0.001) {
      return("P<.001 ***")
    } else if (p < 0.01) {
      return("P<.01 **")
    } else {
      return("P<.05 *")
    }
  } else {
    # return(sprintf("P=%.4f ns", p))  # Not significant
    return(
      sprintf(
        "P=.%03d ns", 
        round(p * 1000)
      )
    )
  }
}


bonferroni_p_value_stars_V2 <- function(
    p, 
    alpha, 
    n_tests
) {
  
  threshold <- alpha / n_tests  # Compute Bonferroni-corrected threshold
  threshold <- round(threshold, 4) # round threshold
  threshold_str <- sprintf("%.4f", threshold)  # Format threshold to 4 decimals
  
  # Determine significance based on the Bonferroni threshold
  if (p < threshold) {
    if (p < 0.001) {
      return("P<.001 ***")
    } else if (p < 0.01) {
      if (threshold < 0.01 & threshold > 0.001){
        return(
          sprintf(
            "P=<.%03d **", 
            round(threshold, digits = 3)*1000
          )
        )
      } else {
        return("P<.01 **")
      }
      
    } else {
      if(threshold < 0.05 & threshold > 0.01){
        return(
          sprintf(
            "P=<.%03d *", 
            round(threshold, digits = 3)*1000
          )
        )
      } else {
        return("P<.05 *")
      }
    }
  } else {
    # return(sprintf("P=%.4f ns", p))  # Not significant
    return(
      sprintf(
        "P=.%03d ns", 
        round(p * 1000)
      )
    )
  }
}

run_aov_analysis <- function(
    data,
    group,
    dependent_variable = "value",
    fixed_effects = "agegroup * Gender * part * Hemisphere",
    random_effects = "(1  | sub)+ (1|part:sub) + (1|Hemisphere:sub)",
    bonferroni_comparisons = NULL
) {
  # Create an empty list to store results for each tract
  results_list <- list()
  
  levels <- unique(data[[group]])
  
  # Loop through each FO.tract
  for (l in levels) {
    print(l)
    # Subset the data for the current tract
    data_level <- data[
      data[[group]] == l,
    ]
    
    # Run the analysis for the current tract and store it in the results list
    results_list[[l]] <- anova.lmer.results(
      data = data_level,
      dependent_variable = dependent_variable,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      bonferroni_comparisons = bonferroni_comparisons
    )
  }
  
  # Return the list with results for each tract
  return(results_list)
}

run_aov_analysis.V2 <- function(
    data,
    group,   # vector of 1 or more column names
    dependent_variable = "value",
    fixed_effects = "agegroup * Gender * part * Hemisphere",
    random_effects = "(1 | sub) + (1 | part:sub) + (1 | Hemisphere:sub)",
    bonferroni_comparisons = NULL
) {
  # Create an empty list to store results for each group combination
  results_list <- list()
  
  # Get all unique combinations of the grouping columns
  levels <- unique(data[, group, drop = FALSE])
  
  # Loop through each combination of group levels
  for (i in seq_len(nrow(levels))) {
    level_vals <- levels[i, ]
    print(level_vals)
    
    # Subset the data for this combination
    data_level <- data
    for (col in group) {
      data_level <- data_level[data_level[[col]] == level_vals[[col]], ]
    }
    
    # Run the analysis and store the result
    results_list[[paste(sapply(level_vals, as.character), collapse = "_")]] <- anova.lmer.results(
      data = data_level,
      dependent_variable = dependent_variable,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      bonferroni_comparisons = bonferroni_comparisons
    )
  }
  
  return(results_list)
}


color_scale <- function(
    x,
    threshold = 0.05/9,
    min.pval
) {
  ifelse(
    x > threshold, "#D3D3D3", 
    col_numeric(
      palette = c(
        "#E57373",
        "wheat"
      ), 
      domain = c(
        min.pval, 
        threshold
      )
    )(x)
  )
}

# Function to apply background color based on p-value
apply_background_color <- function(p_value, threshold, min.pval) {
  color_scale(p_value, threshold, min.pval) # Use the continuous color scale
}

build_anova_table <- function(
    anova.results,
    elements,
    threshold = 0.05/9,
    desired.order = NULL,
    effects_names = NULL
){
  # Function to format F and P values together
  format_f_p <- function(f_value, p_value) {
    paste0(f_value, "\n", p_value)
  }
  
  # Process each tract's results
  formatted_results <- lapply(elements, function(tract) {
    df <- anova.results[[tract]]$anova.stats.table  # Extract the tract's dataframe
    df %>%
      mutate(!!tract := format_f_p(`F-value`, `P-value`)) %>%  # Format F and P values together
      select(Effects, !!tract)  # Keep only Effects and formatted values
  })
  
  # Merge all tract data into one wide-format table
  anova_wide <- Reduce(
    function(df1, df2) full_join(df1, df2, by = "Effects"), 
    formatted_results
  )
  
  # Replace "\n" with actual line breaks in all columns except "Effects"
  anova_wide <- anova_wide %>%
    mutate(across(-Effects, ~ gsub("\\\\n", "\n", .))) 
  print(anova_wide)
  ################################################################################
  # Put the p-values in a dataframe and reorder them according to the effects vector
  p_values_df <- lapply(elements, function(element) {
    df <- anova.results[[element]]$anova.table  # Extract the elements's dataframe
    df %>%
      mutate(!!element := `Pr..F.`) %>%  # Format F and P values together
      select(effects, !!element)  # Keep only Effects and formatted values
  })
  
  # Combine all p-values into one data frame, joining by the 'Effects' column
  p_values_df <- Reduce(function(x, y) {
    left_join(x, y, by = "effects")  # Join by 'Effects' to merge the results
  }, p_values_df)
  
  if (!is.null(desired.order)){
    # Reorder the "Effects" column according to the desired order
    anova_wide <- anova_wide %>%
      mutate(Effects = factor(Effects, levels = desired.order)) %>%
      arrange(Effects)  # Arrange rows based on the new factor levels
    
    # Reorder the "Effects" column according to the desired order
    p_values_df <- p_values_df %>%
      mutate(effects = factor(effects, levels = desired.order)) %>%
      arrange(effects)  # Arrange rows based on the new factor levels
  }
  print(p_values_df)
  # now color the background
  # Create a continuous color scale based on the p-values
  min.pvalue <- min(p_values_df[,-1], na.rm = TRUE)
  
  # replace effects names values
  if (!is.null(effects_names)){
    anova_wide$Effects <- effects_names
  }
  
  # Convert to a flextable
  anova_flextable <- anova_wide %>%
    flextable() %>%
    autofit()  # Adjust column sizes automatically
  
  anova_flextable <- bg(
    anova_flextable,
    j = names(anova_wide[2:ncol(anova_wide)]),
    bg = 
      sapply(p_values_df[,2:ncol(anova_wide)], function(x) apply_background_color(x, threshold,min.pvalue)), 
    part = "body"
  )
  
  # Set column widths (in centimeters)
  anova_flextable <- anova_flextable %>%
    flextable::set_table_properties(
      width = 0.95, # here convert 3.16 cm to inches 
      layout = "autofit"
    )
  
  return(anova_flextable)
}
