# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # # # # # # # # # # # # # REQUIREMENTS # # # # # # # # # # # # # # # # #     
# **************************************************************************** #
# ------------------------------------------------------------------------------
libs.ver <- data.frame(
  libs = c('Rmisc','ggplot2','ggpmisc','reshape','dplyr','ggpubr',
           'tidyverse','ggstatsplot','cowplot','car','rsq',
           'RColorBrewer','pracma','patchwork','ggsci','effsize',
           'svglite','data.table','XML','scales','ggrepel'), 
  ver = c('1.5','3.4.4','0.3.6','0.8.9','1.0.9','0.4.0','1.3.0',
          '0.9.3','1.1.1','3.1.0','2.5','1.1.3','2.3.3','1.1.1',
          '2.9','0.8.1','2.1.1','1.14.2','3.99.0.13','1.2.0','0.9.1')
)

libs <- c('Rmisc','ggplot2','ggpmisc','reshape','dplyr','ggpubr',
          'tidyverse','ggstatsplot','cowplot','car','rsq',
          'RColorBrewer','pracma','patchwork','ggsci','effsize',
          'svglite','data.table','XML','scales','ggrepel')

requirements <- function(libraries,
                         BCBL = TRUE,
                         user){
  if(BCBL){
    .libPaths( c( file.path('/scratch',user) , .libPaths() ) )
  } 
  
  # install this package to manage installation of required libraries
  install.packages('devtools')
  library(devtools)
  # install all the packages:
  for (i in 1:nrow(libraries)){
    devtools::install_version(
      libraries[i,'libs'], 
      libraries[i,'ver'],
      upgrade = TRUE
    )
  } 
} 

requirementsV2 <- function(
    libraries,
    libspath,
    install = FALSE
  ){
  if (!dir.exists(libspath)){
    dir.create(libspath, recursive = TRUE)
  }
 
  .libPaths(
    c(libspath,
    .libPaths())
  )
  
  # Path to user .Rprofile
  rprofile_path <- path.expand("~/.Rprofile")
  
  # Content to write
  rprofile_content <- sprintf(
    '# Automatically set library path\n.libPaths("%s")\nmessage("Active library paths: ", paste(.libPaths(), collapse = ", "))\n',
    libspath
  )
  
  # Write (overwrite) .Rprofile
  writeLines(rprofile_content, rprofile_path)
  
  if (install){
    # install this package to manage installation of required libraries
    if (!"devtools" %in% installed.packages(lib.loc = libspath)[, 1]) {
      install.packages("devtools", lib = libspath, dependencies = TRUE)
    } else {
      library(devtools, lib.loc = libspath)
      # library(remotes, lib.loc = libspath)
    }
    
    # install all the packages:
    for (i in 1:nrow(libraries)){
      devtools::install_version(
        libraries[i,'libs'],
        upgrade = TRUE
      )
      
      # remotes::install_version(
      #   libraries[i,'libs'], 
      #   libraries[i,'ver']
      # )
      
    }
  } 
} 
  

# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # # # # # # # # # Functions for  data cleaning # # # # # # # # # # # # #
# **************************************************************************** #
# ------------------------------------------------------------------------------

# var_correction
# - Input X as data for VAR correction and Y as the corresponding VAR data
# ******************************************************************************
var_correction <- function(x,y){
  tempM <- na.omit(cbind(x, y))
  xn <- x - lm(eval(x ~ y))$coefficients[2][[1]] * (y - mean(tempM[,2]))
  return(xn)
}

# norm_correction
# - Input X as data for X correction and Y as the corresponding X data
# ******************************************************************************
norm_correction <- function(x,y){
  value <- x/y
  return(value)
}

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# nuclei names
# ******************************************************************************
# nuclei names
nuclei_names <- c("MGN","LGN","PuI","PuM","L.Sg","VPL","CM","VLa","PuA","MDm",
                  "Pf","VAmc","MDl","CeM","VA","MV.Re","VM","CL","PuL","Pt",
                  "AV","Pc","VLp","LP","LD")

# clean_data
# - Input: path where is the .RData
# This function cleans the data by specifying the correction_type and scaling
# Clean data function accepts the following corrections:
# 1. ICV_correction: remove ICV effect by regressing out
#    the whole thalamus ~ ICV relationship
# ******************************************************************************
clean_data <- function(path, correction_type, scaling){
  ATLAS <- loadRData(path)
  dimensions <- dim(ATLAS)

  r <- c(0)
  j=1;
  for ( i in 1:dimensions[1]){
    if(ATLAS[i,"Eliminar_1"]==1){
      r[j] <- c(i)
      j = j+1
    }
  }
  ATLAS <- ATLAS[-c(r),]

  ## Removing the Subject in which there is any movement in the MRI images
  # (values 2 and 3)
  rr <- c(0)
  j=1;
  for (i in 1:dim(ATLAS)[1]) {
    if (ATLAS[i, "DICOM_MOV"]!=1){
      rr[j] <- c(i)
      j=j+1
    }
  }
  ATLAS <- ATLAS[-c(rr),]

  # Removing the subject with bad segmentation
  rr <- c(0)
  j=1;
  for (i in 1:dim(ATLAS)[1]) {
    if (ATLAS[i, "Bad_Segmentation"]==1){
      rr[j] <- c(i)
      j=j+1
    }
  }
  ATLAS <- ATLAS[-c(rr),]
  ATLAS_Norm <- data.frame(ATLAS)
  
  left_nuclei <- paste("Left",nuclei_names,sep=".")
  right_nuclei <- paste("Right", nuclei_names, sep=".")
  l <- c(left_nuclei,"Left.Whole_thalamus")
  r <- c(right_nuclei,"Right.Whole_thalamus")
  all_nuclei <- c(c(l,r))
  
  # convert mm3 to cm3
  # all thalamus  
  ATLAS_Norm[,all_nuclei] <- ATLAS_Norm[,all_nuclei]/1000
  # aseg atlas
  ATLAS_Norm$Right.Thalamus.Proper <- ATLAS_Norm$Right.Thalamus.Proper/1000
  ATLAS_Norm$Left.Thalamus.Proper <- ATLAS_Norm$Left.Thalamus.Proper/1000
  # Intracranial Volume
  ATLAS_Norm$ICV <- ATLAS_Norm$ICV/1000
  # gray matter volume  
  # ATLAS_Norm$rhCortexVol <- ATLAS_Norm$rhCortexVol/1000
  # ATLAS_Norm$rhSubCortGrayVol <- ATLAS_Norm$rhSubCortGrayVol/1000
  # ATLAS_Norm$lhCortexVol <- ATLAS_Norm$lhCortexVol/1000
  # ATLAS_Norm$lhSubCortGrayVol <- ATLAS_Norm$lhSubCortGrayVol/1000
  ATLAS_Norm$Right.CortexGrayM <- ATLAS_Norm$rhCortexVol/1000
  ATLAS_Norm$Right.SubCortexGrayM <- ATLAS_Norm$rhSubCortGrayVol/1000
  ATLAS_Norm$Left.CortexGrayM <- ATLAS_Norm$lhCortexVol/1000
  ATLAS_Norm$Left.SubCortexGrayM <- ATLAS_Norm$lhSubCortGrayVol/1000
  # ATLAS_Norm$Right.GrayM <-  ATLAS_Norm$rhCortexVol + ATLAS_Norm$rhSubCortGrayVol
  # ATLAS_Norm$Left.GrayM <- ATLAS_Norm$lhCortexVol + ATLAS_Norm$lhSubCortGrayVol
  ATLAS_Norm$Right.GrayM <-  ATLAS_Norm$Right.CortexGrayM + ATLAS_Norm$Right.SubCortexGrayM
  ATLAS_Norm$Left.GrayM <- ATLAS_Norm$Left.CortexGrayM + ATLAS_Norm$Left.SubCortexGrayM
  # white matter volume
  ATLAS_Norm$Right.WhiteM <- ATLAS_Norm$totalRight.WhiteMatter/1000
  ATLAS_Norm$Left.WhiteM <- ATLAS_Norm$totalLeft.WhiteMatter/1000
  
  if(correction_type == "ICV_correction"){
    ATLAS_Norm_F <- ATLAS_Norm[which(ATLAS_Norm$Gender=='Female'),]
    ATLAS_Norm_M <- ATLAS_Norm[which(ATLAS_Norm$Gender=='Male'),]
    for (i in 1:length(all_nuclei)){
      ATLAS_Norm_F[,all_nuclei[i]] <- var_correction(
        ATLAS_Norm_F[,all_nuclei[i]], 
        ATLAS_Norm_F$ICV
      )
      ATLAS_Norm_M[,all_nuclei[i]] <- var_correction(
        ATLAS_Norm_M[,all_nuclei[i]], 
        ATLAS_Norm_M$ICV
      )
    }
    ATLAS_Norm <- rbind(ATLAS_Norm_F,ATLAS_Norm_M)
    
  # } else if(correction_type == "ICV_correction_full"){
  #   ATLAS_Norm_F_L <- subset(
  #     ATLAS_Norm, 
  #     Hemisphere == "Left" & Gender == "Female"
  #   )
  #   ATLAS_Norm_M_L <- subset(
  #     ATLAS_Norm, 
  #     Hemisphere == "Left" & Gender == "Male"
  #   )
  #   ATLAS_Norm_F_R <- subset(
  #     ATLAS_Norm, 
  #     Hemisphere == "Right" & Gender == "Female"
  #   )
  #   ATLAS_Norm_M_R <- subset(
  #     ATLAS_Norm, 
  #     Hemisphere == "Right" & Gender == "Male"
  #   )
  #   print(nrow(ATLAS_Norm_F_L))
  #   print(nrow(ATLAS_Norm_F_R))
  #   print(nrow(ATLAS_Norm_M_L))
  #   print(nrow(ATLAS_Norm_M_R))
  #   
  #   for (i in 1:length(all_nuclei)){
  #     ATLAS_Norm_F_L[,all_nuclei[i]] <- var_correction(
  #       ATLAS_Norm_F_L[,all_nuclei[i]], 
  #       ATLAS_Norm_F_L$ICV
  #     )
  #     ATLAS_Norm_M_L[,all_nuclei[i]] <- var_correction(
  #       ATLAS_Norm_M_L[,all_nuclei[i]], 
  #       ATLAS_Norm_M_L$ICV
  #     )
  #     ATLAS_Norm_F_R[,all_nuclei[i]] <- var_correction(
  #       ATLAS_Norm_F_R[,all_nuclei[i]], 
  #       ATLAS_Norm_F_R$ICV
  #     )
  #     ATLAS_Norm_M_R[,all_nuclei[i]] <- var_correction(
  #       ATLAS_Norm_M_R[,all_nuclei[i]], 
  #       ATLAS_Norm_M_R$ICV
  #     )
  #   }
  #   ATLAS_Norm <- rbind(
  #     ATLAS_Norm_F_L,
  #     ATLAS_Norm_M_L,
  #     ATLAS_Norm_F_R,
  #     ATLAS_Norm_M_R
  #   )
  #     

  } else if(correction_type == "None"){
    
  } else{
    print("Type ICV_correction or None in 'correction' argument")
  }
  
  if(scaling=="yes"){
    for (i in 1:length(all_nuclei)) {
      ATLAS_Norm[,all_nuclei[i]] <- scale(
        ATLAS_Norm[,all_nuclei[i]], 
        center=TRUE, scale=TRUE
      )
    }
    # ATLAS_Norm["ICV"] <- scale(ATLAS_Norm["ICV"], 
    #                            center=TRUE, scale=TRUE)
    # ATLAS_Norm["TotalGrayVol"] <- scale(ATLAS_Norm["TotalGrayVol"], 
    #                                     center=TRUE, scale=TRUE)
    # # delete thalamus volumes with more than 2 NA values
    # ATLAS_Norm <- ATLAS_Norm[(colSums(is.na(ATLAS_Norm)) < 3)]
    return(ATLAS_Norm)
  } else {
    # # delete thalamus volumes with more than 2 NA values
    # ATLAS_Norm <- ATLAS_Norm[(colSums(is.na(ATLAS_Norm)) < 3)]
    return(ATLAS_Norm)
  }
}

# ATLAS to nice dataframes with all thalamic volumes + features selected by
# the user with a column classifying each hemisphere
# feat2melt <- the variable names must contain Right and Left, so the function
# can work properly when processing the dataframe into Left-Right hemispheres
# ******************************************************************************
ATLAS_2_dfHem <- function(df,path,correction,scaling,feat,feat2melt,deleteWT,
                          nuclei){
  if(!missing(df)){
    corrected <- df
  }else if(!missing(path)){
    corrected <- clean_data(path,correction,scaling)
  }
  
  nuclei_names <- c("MGN","LGN","PuI","PuM","L.Sg","VPL","CM","VLa","PuA","MDm",
                    "Pf","VAmc","MDl","CeM","VA","MV.Re","VM","CL","PuL","Pt",
                    "AV","Pc","VLp","LP","LD")
  # smallest nuclei
  nuclei.discard <- c("L.Sg","CM","Pf","VAmc","CeM","MV.Re","CL","Pc","Pt")
  # remove smallest nuclei
  nuclei_names <- nuclei_names[!(nuclei_names %in% nuclei.discard)]
  left_nuclei <- paste("Left",nuclei_names,sep=".")
  right_nuclei <- paste("Right", nuclei_names, sep=".")
  l <- c(left_nuclei,"Left.Whole_thalamus")
  r <- c(right_nuclei,"Right.Whole_thalamus")
  all_nuclei <- c(l,r)
  
  # get all nuclei thalamus volumes. Besides, I get relevant features (feat)
  # user wants.
  if(!missing(feat2melt)){
    corrected <- corrected[,c(all_nuclei,feat2melt,feat)]
  }else{
    corrected <- corrected[,c(all_nuclei,feat)]
  }
  # delete whole thalamus
  if(missing(deleteWT)){
    corrected <- corrected[, -grep("Whole_thalamus$", all_nuclei)]
  }
  
  Left <- corrected[c(grep('Left', names(corrected)),
                      grep(paste(feat,collapse="|"),
                           names(corrected)))]
  names(Left) <- sub("Left*.", "", names(Left)) # delete Left prefix
  Right <- corrected[c(grep('Right', names(corrected)),
                       grep(paste(feat,collapse="|"),
                            names(corrected)))]
  names(Right) <- sub("Right*.", "", names(Right)) # delete Right prefix
  
  corrected <- dplyr::bind_rows(list(Right=Right,
                                     Left=Left), .id = 'Hemisphere')
  if(!missing(nuclei)){
    corrected <- corrected[-grep(paste(nuclei_names,collapse="|"),
                                 names(corrected))]
  }
  # factorize
  corrected$Gender <- factor(corrected$Gender)
  corrected$Hemisphere <- factor(corrected$Hemisphere)
  return(corrected)
}

# Check if there are duplicated subs and correct them
fix_duplicated_IDs <- function(
    data,
    id = "ID",
    prefix = "PROYECTO"
    ){
  # right.hem <- unique(subset(data, Hemisphere == "Right"))
  duplicated.subs <- data[duplicated(data[,id]),id]
  if (length(duplicated.subs) > 0){
    cat(sprintf("%d duplicated sub IDs found.", length(duplicated.subs)),"\n")
    cat(
      sprintf(
        "Taking first letters of their %s variable to prefix them.",
        "PROYECTO"
      ),
      "\n"
    )
    data[data[,id] %in% duplicated.subs,id] <-
      paste(
        substr(
          data[data[,id] %in% duplicated.subs, prefix],
          start = 1, 
          stop = 3
        ),
        data[data[,id] %in% duplicated.subs, id],
        sep = "_"
      )
    
    # right.hem <- unique(subset(data, Hemisphere == "Right"))
    duplicated.subs <- data[duplicated(data[,id]),id]
    
    if (length(duplicated.subs) == 0){
      cat("Fixed. No duplicated subs now.","\n")
      cat("================================","\n")
    } else {
      cat("Still duplicated subjects, check your dataframe further.","\n")
      cat("================================","\n")
    }
  } else{
    cat("No duplicated subs.","\n")
    cat("================================","\n")
  }
  return(data)
}
# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # # # # # # # # Density plots for Figure 1.A.B # # # # # # # # # # # # .
# **************************************************************************** #
# ------------------------------------------------------------------------------
vol.dist.factor.hem <- function(ATLAS,factor,volume,font.size){
  factorlabels <- levels(ATLAS[,factor])
  stopifnot(length(factorlabels) == 2)
  
  # Compute d and VR between hemispheres for each level of the factor
  # ****************************************************************************
  a_df <- ATLAS[ATLAS[,factor] == factorlabels[1],]
  b_df <- ATLAS[ATLAS[,factor] == factorlabels[2],] 
  
  vol.hem.exp <- as.formula(paste(volume,'Hemisphere',sep="~"))
  
  # a_cohend <- cohen.d(data=a_df, vol.hem.exp)
  a_cohend <- rstatix::cohens_d(data=a_df, vol.hem.exp, paired = TRUE)
  a_vr <- var.test(vol.hem.exp, a_df, alternative="two.sided")
  # b_cohend <-cohen.d(data=b_df, vol.hem.exp)
  b_cohend <- rstatix::cohens_d(data=b_df, vol.hem.exp, paired = TRUE)
  b_vr <- var.test(vol.hem.exp, b_df, alternative="two.sided")
  # create labels to plot
  a_d_vr <- paste(paste("d = ",round(abs(a_cohend$effsize),3)),
                     paste(" VR = ",round(a_vr$estimate,3)),sep="\n")
  b_d_vr <- paste(paste("d = ",round(abs(b_cohend$effsize),3)),
                         paste(" VR = ",round(b_vr$estimate,3)),sep="\n")
  d_vr_factor <- data.frame(
    label = c(a_d_vr, b_d_vr),
    factor   = factorlabels
  )
  names(d_vr_factor)[names(d_vr_factor) == "factor"] <- factor
  
  # Compute d and VR between atlases for each hemisphere: left and right
  # ****************************************************************************
  left_df <- ATLAS[ATLAS$Hemisphere == "Left",]
  right_df <- ATLAS[ATLAS$Hemisphere == "Right",]
  
  vol.factor.exp <- as.formula(paste(volume,factor,sep="~"))
  
  left_cohend<-cohen.d(data=left_df, vol.factor.exp)
  left_vr <- var.test(vol.factor.exp, left_df, alternative="two.sided")
  right_cohend<-cohen.d(data=right_df, vol.factor.exp)
  right_vr <- var.test(vol.factor.exp, right_df, alternative="two.sided")
  
  # create labels to plot
  left_d_vr <- paste(paste("d = ",round(abs(left_cohend$estimate),3)),
                     paste(" VR = ",round(left_vr$estimate,3)),sep="\n")
  right_d_vr <- paste(paste("d = ",round(abs(right_cohend$estimate),3)),
                      paste(" VR = ",round(right_vr$estimate,3)),sep="\n")
  d_vr_hemisphere <- data.frame(
    label = c(left_d_vr, right_d_vr),
    Hemisphere   = c("Left","Right")
  )
  
  # Normalize each distribution
  # ****************************************************************************
  nlength = 512
  tsteps_a_left <- seq(min(a_df[a_df$Hemisphere == 'Left',volume]),
                       max(a_df[a_df$Hemisphere == 'Left',volume]),
                       length.out = nlength)
  tsteps_a_right <- seq(min(a_df[a_df$Hemisphere == 'Right',volume]),
                        max(a_df[a_df$Hemisphere == 'Right',volume]),
                        length.out = nlength)
  tsteps_b_left <- seq(min(b_df[b_df$Hemisphere == 'Left',volume]),
                       max(b_df[b_df$Hemisphere == 'Left',volume]),
                       length.out = nlength)
  tsteps_b_right <- seq(min(b_df[b_df$Hemisphere == 'Right',volume]),
                        max(b_df[b_df$Hemisphere == 'Right',volume]),
                        length.out = nlength)
  
  density_a_left <- density(a_df[a_df$Hemisphere == 'Left',volume],n=nlength)$y
  density_a_right <- density(a_df[a_df$Hemisphere == 'Right',volume],n=nlength)$y
  density_b_left <- density(b_df[b_df$Hemisphere == 'Left',volume],n=nlength)$y
  density_b_right <- density(b_df[b_df$Hemisphere == 'Right',volume],n=nlength)$y
  
  norm_density_a_left <- density_a_left/trapz(tsteps_a_left,density_a_left)
  norm_density_a_right <- density_a_right/trapz(tsteps_a_right,density_a_right)
  norm_density_b_left <- density_b_left/trapz(tsteps_b_left,density_b_left)
  norm_density_b_right <- density_b_right/trapz(tsteps_b_right,density_b_right)
  
  density_a_left <- data.frame(tsteps_a_left,norm_density_a_left)
  colnames(density_a_left) <- c('x','y')
  density_a_left$Hemisphere <- 'Left'
  density_a_left[,factor]  <- factorlabels[1] 
  density_a_right <- data.frame(tsteps_a_right,norm_density_a_right)
  colnames(density_a_right) <- c('x','y')
  density_a_right$Hemisphere <- 'Right'
  density_a_right[,factor] <- factorlabels[1]
  
  density_b_left <- data.frame(tsteps_b_left,norm_density_b_left)
  colnames(density_b_left) <- c('x','y')
  density_b_left$Hemisphere <- 'Left'
  density_b_left[,factor] <- factorlabels[2]
  density_b_right <- data.frame(tsteps_b_right,norm_density_b_right)
  colnames(density_b_right) <- c('x','y')
  density_b_right$Hemisphere <- 'Right'
  density_b_right[,factor] <- factorlabels[2] 
  
  density_df <- rbind(density_b_left,
                      density_b_right,
                      density_a_left,
                      density_a_right)
  expr <- as.formula(paste(volume, paste(c("Hemisphere",factor), 
                                         collapse=" * "), sep=" ~ "))
  mu <- aggregate(expr,data=ATLAS,FUN=median)
  
  if (missing(font.size)) {
    font.size <- 24
  }
  
  # FIGURE A - Hemispheres comparison for each level
  # ****************************************************************************
  fig1_A <- ggplot(density_df, aes_string(x='x',
                                          y='y',
                                          fill = 'Hemisphere',
                                          alpha = factor)) + 
    geom_line(size = 1.5) + geom_area() + theme_bw() + 
    labs(x=expression(paste("Thalamic volume [cm"^"3"*"]")), y='Density') +
    geom_vline(data=mu, aes_string(xintercept=volume,
                                   alpha=factor,
                                   color='Hemisphere'), 
               linetype="dashed", size=1.5) +
    scale_color_brewer(palette="Set1") +  scale_fill_brewer(palette="Set1") +
    scale_alpha_discrete(range=c(0.5, 0.25)) +
    geom_text(inherit.aes=FALSE, data = d_vr_factor,
              mapping = aes(x = Inf, y = Inf, label = label),
              hjust   = 1,
              vjust   = 1,
              size    = 7) + 
    facet_grid(reformulate(factor)) + 
    guides(color="none", fill=guide_legend(override.aes = list(alpha=1)),
           alpha = "none") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = font.size),
          strip.text.x = element_text(size = font.size),
          axis.text = element_text(size = font.size - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  # FIGURE B - Between-levels comparison for each hemisphere
  # ****************************************************************************
  fig1_B <- ggplot(density_df, aes_string(x='x',
                                          y='y',
                                          fill='Hemisphere',
                                          alpha=factor)) + 
    geom_line(size = 1.5) + geom_area() + theme_bw() + 
    labs(x=expression(paste("Thalamic volume [cm"^"3"*"]")), y='Density') +
    geom_vline(data=mu, aes_string(xintercept=volume,
                                   alpha=factor,
                                   color='Hemisphere'), 
               linetype="dashed", size=1.5) +
    scale_color_brewer(palette="Set1")+scale_fill_brewer(palette = "Set1") + 
    scale_alpha_discrete(range=c(0.5, 0.25)) +
    geom_text(inherit.aes = FALSE, data = d_vr_hemisphere,
              mapping = aes(x = Inf, y = Inf, label = label),
              hjust   = 1,
              vjust   = 1,
              size    = 7) +
    facet_grid(~Hemisphere) + guides(fill="none",color="none",
                                     linetype="none",alpha = "none") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = font.size),
          strip.text.x = element_text(size = font.size),
          axis.text = element_text(size = font.size - 4),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          # facet_wrap without boxes
          strip.background = element_blank())
  
  return(list(fig1_A = fig1_A, fig1_B = fig1_B))
} 

# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # Bootstrapping for slope estimate in linear models as default # # # # #
# **************************************************************************** #
# ------------------------------------------------------------------------------
boots_slope <- function(
    x, 
    y, 
    R_bootstrap, 
    model = 1,
    age_range = c(6.37, 81.59) # this is to calculate the averaged slope for
                               # quadratic and cubic regressions
    ){
  
  intercept <- array(0,dim=R_bootstrap)
  linear.term <- array(0,dim=R_bootstrap)
  adj.rsquared <- array(0,dim=R_bootstrap)
  pearson <- array(0,dim=R_bootstrap)
  BIC <- array(0,dim=R_bootstrap)
  # spearman <- array(0,dim=R_bootstrap)
  # fstatistic <- array(0,dim=R_bootstrap)
  if (model > 1) {
    quadratic.term <- array(0,dim=R_bootstrap)
    avg.slope <- array(0,dim=R_bootstrap)
  }
  # also if model == 3 add cubic term
  if (model == 3){
    cubic.term <- array(0,dim=R_bootstrap)
  }
  
  id <- array(0,dim=R_bootstrap)
  for (k in 1:R_bootstrap){
    idx <- sample(1:length(x), length(x), replace=TRUE)
    fit <- glm(y[idx]~poly(x[idx], model, raw=TRUE))
    adj.rsquared[k] <- rsq(fit,adj=TRUE)
    intercept[k] <- fit$coefficients[1]
    linear.term[k] <- fit$coefficients[2]
    if (model == 2) {
      quadratic.term[k] <- fit$coefficients[3]
      # averaged slope
      a <- quadratic.term[k]
      b <- linear.term[k]
      x2 <- max(age_range)
      x1 <- min(age_range)
      avg.slope[k] <- average_slope <- (a * (x2^2 - x1^2) + b * (x2 - x1)) / (x2 - x1)
      
    } else if (model == 3){
      quadratic.term[k] <- fit$coefficients[3]
      cubic.term[k] <- fit$coefficients[4]
      # averaged slope
      a <- cubic.term[k]
      b <- quadratic.term[k]
      c <- linear.term[k]
      x2 <- max(age_range)
      x1 <- min(age_range)
      average_slope <- (a * (x2^3 - x1^3) + b * (x2^2 - x1^2) + c * (x2 - x1)) / (x2 - x1)
    }
    intercept[k] <- fit$coefficients[1]
    BIC[k] <- BIC(fit)
    # linear.term[k] <- fit$coefficients[2]
    # fstatistic[k] <- summary(fit)$fstatistic[1]
    pearson[k] <- cor.test(x[idx],y[idx] ,method='pearson')$estimate
    # spearman[k] <-  cor.test(x[idx],y[idx],method='spearman')$estimate
    id[k] <- k
  }
  
  if (model == 2) {
    data <- data.frame(
      "id"=id,
      "intercept"=intercept,
      "linear_term"=linear.term,
      "quadratic_term"=quadratic.term,
      "slopes"=avg.slope,
      "BIC" = BIC,
      "adj_rsquared"=adj.rsquared,
      "pearson"=pearson
    )
  } else if (model == 3){
    data <- data.frame(
      "id"=id,
      "intercept"=intercept,
      "linear_term"=linear.term,
      "quadratic_term"=quadratic.term,
      "cubic_term"=cubic.term,
      "slopes"=avg.slope,
      "BIC" = BIC,
      "adj_rsquared"=adj.rsquared,
      "pearson"=pearson
    ) 
  } else {
    data <- data.frame(
      "id"=id,
      "intercept"=intercept,
      "linear_term"=linear.term,
      "slopes"=linear.term,
      "BIC" = BIC,
      "adj_rsquared"=adj.rsquared,
      "pearson"=pearson
    )
  }
  
  return(data) #,
  #  "spearman"=spearman, "fstatistic"=fstatistic))
}

# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # # # # # # # # # # # # # BIC  UTILITIES # # # # # # # # # # # # # # # #
# **************************************************************************** #
# ------------------------------------------------------------------------------
models.boots <- function(dataset,x,y,R_bootstrap){
  
  if(!missing(dataset)){
    dataset <- as.data.frame(dataset)
    x <- dataset[,x]
    y <- dataset[,y]
    
    adj.rsquared.linear <- array(0,dim=R_bootstrap)
    adj.rsquared.quadratic <- array(0,dim=R_bootstrap)
    adj.rsquared.cubic <- array(0,dim=R_bootstrap)
    BIC.linear <- array(0,dim=R_bootstrap)
    BIC.quadratic <- array(0,dim=R_bootstrap)
    BIC.cubic <- array(0,dim=R_bootstrap)
    id <- array(0,dim=R_bootstrap)
    
    set.seed(100)
    
    for (k in 1:R_bootstrap){
      idx <- sample(1:length(x), length(x), replace=TRUE)
      
      fit.linear <- glm(
        y[idx]~poly(x[idx],1,raw=TRUE)
      )
      # adj.rsquared.linear[k] <- summary(fit.linear)$adj.r.squared
      adj.rsquared.linear[k] <- rsq(fit.linear,adj=TRUE)
      BIC.linear[k] <- BIC(fit.linear) 
      
      fit.quadratic <- glm(
        y[idx]~poly(x[idx],2,raw=TRUE)
      )
      # adj.rsquared.quadratic[k] <- summary(fit.quadratic)$adj.r.squared
      adj.rsquared.quadratic[k] <- rsq(fit.quadratic,adj=TRUE)
      BIC.quadratic[k] <- BIC(fit.quadratic)
      
      fit.cubic <- glm(
        y[idx]~poly(x[idx],3,raw=TRUE)
      )
      # adj.rsquared.cubic[k] <- summary(fit.cubic)$adj.r.squared
      adj.rsquared.cubic[k] <- rsq(fit.cubic,adj=TRUE)
      BIC.cubic[k] <- BIC(fit.cubic)
      
      id[k] <- k
    }
    
    return(
      data.frame(
        "id"=id,
        "adj_rsquaredlinear"=adj.rsquared.linear,
        "adj_rsquaredquadratic"=adj.rsquared.quadratic,
        "adj_rsquaredcubic"=adj.rsquared.cubic,
        "BIClinear"=BIC.linear,
        "BICquadratic"=BIC.quadratic,
        "BICcubic"=BIC.cubic
      )
    )
  } else {
    print("No dataset provided.")
  }
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

models.BIC.gender <- function(dataset,x,y,R_bootstrap){
  models.data <- dataset %>%
    group_by(Gender) %>%
    group_modify(
      ~models.boots(
        dataset=.x,
        x="AGE",
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
    groupvars=c("Gender","model")
  )
  
  BIC$vol <- y
  return(BIC)
}

diff.BIC <- function(data,variable){
  data <- as.data.frame(data)
  vector <- data[,variable]
  diffvalues <- -diff(vector)
  
  if(sum(diffvalues > 2) == length(diffvalues)){
    n=length(diffvalues)+1
  } else if (sum(diffvalues>2) == 0){
    n=1
  } else {
    n=which(diffvalues>2)+1
    #check it to the linear model
    if((vector[1] - vector[n]) > 2) {
      n=n
    } else {
      n=1
    }
  }
  return(data.frame("model"=n))
}

# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # # # # # # # # # # # # # PLOT UTILITIES # # # # # # # # # # # # # # # #
# **************************************************************************** #
# ------------------------------------------------------------------------------
plot.box.boots <- function(
    data,
    x,
    y,
    compare,
    joinid = FALSE,
    idstr,
    xylabels,
    legtitle,
    legcontent,
    title,
    saveimage,
    dimensions,
    # test,
    testlab,
    pairs = FALSE,
    # pairwiselist,
    palette,
    colors,
    legend = TRUE,
    hide.x.axis=TRUE,
    hide.y.axis=TRUE
    ){
  
  pmain <- ggplot(
    data = data, 
    aes(
      x=x,
      y=y,
      colour=compare
    )
  ) +
    geom_violin(
      aes(fill=compare),
      position=position_dodge(1),
      alpha=0.3,
      width=0.8
    ) +
    geom_boxplot(
      width=0.5, 
      notch = TRUE,
      position=position_dodge(1),
      outlier.shape = 18,
      outlier.size = 3,
      outlier.alpha = 0.7
    ) +
    geom_point(
      alpha=0.1,
      size=1
    )+
    stat_summary(
      aes(
        fill=compare
      ),
      fun=mean,
      geom="point",
      shape=23,
      size=4,
      colour='purple',
      alpha=0.5,
      position=position_dodge(1)
    )+
    stat_summary(
      aes(
        group=compare
      ),
      fun.data=mean_se, 
      geom="crossbar",
      width=0.5, 
      color="black",
      position = position_dodge(width = 1)
    )
  
  # # Perform the comparison and calculate p-values manually
  # comparison_results <- compare_means(
  #   as.formula(paste(y,x,sep="~")),
  #   data = data, 
  #   method = "t.test",
  #   paired = pairs
  # )
  # # Format p-values using the p_value_stars function
  # comparison_results$p.formatted <- sapply(
  #   comparison_results$p, 
  #   p_value_stars
  # )
  
  if(joinid){
    pmain <- pmain + geom_line(aes(group = idstr), color="gray", size=0.1)
  } 
  
  if(!missing(testlab)){
    pmain <- pmain + 
      # geom_text(
      #   data = comparison_results,
      #   aes(
      #     x = label_x, 
      #     y = label_y, 
      #     label = p.formatted
      #   ),
      #   size = 6
      # )
      stat_compare_means(
        aes(group=compare),
        method = "t.test",
        # label = function(p) p_value_stars(p),
        paired = pairs,
        size=6,
        label.x.npc = testlab[1],
        label.y.npc = testlab[2]
      )
  # } else if(!missing(pairwiselist)){
  #   pmain <- pmain + stat_compare_means(comparisons=pairwiselist, 
  #                                       method = "t.test",
  #                                       paired=TRUE,label = "p.signif")
  } else{
    pmain <- pmain + 
      stat_compare_means(
        aes(group=compare),
        method = "t.test",
        # label = function(p) p_value_stars(p),
        paired = pairs, # p.adjust.method = "bonferroni", 
        size=6
      )
    # if(!missing(test)){
    #   pmain <- pmain
    # } else{
    #   if(!missing(id)){
    #     pmain <- pmain + stat_compare_means(aes(group=compare), 
    #                                         method = "t.test", paired=TRUE)
    #   } else{
    #     pmain <- pmain + stat_compare_means(aes(group=compare),
    #                                         method = "t.test", paired=FALSE,
    #                                         size=6)
    #   }  
    # }
  }
  pmain <- pmain + 
    stat_summary(
      fun.data=mean_se,
      geom="errorbar",
      size=1,
      width=0.5,alpha=0.5
    )+
    ggtitle(title) + labs(x=xylabels[1],y=xylabels[2]) +
    theme_bw(base_size=14) +
    theme(plot.title=element_text(hjust = 0.5, size=24),
          text = element_text(size=24, family = "Arial"),
          strip.text.x = element_text(size=24),
          axis.text = element_text(size=20),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          aspect.ratio = 1)
  
  if(hide.x.axis) {
    pmain <- pmain +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"))
  }
  
  if(hide.y.axis) {
    pmain <- pmain +
      theme(axis.title.y = element_text(colour = "white"))
  }
  
  if (!legend) {
    pmain <- pmain + theme(legend.position = "none")
  }
  
  mu <- aggregate(y,list(compare),mean)
  yhist <- axis_canvas(pmain,axis="y", mapping=aes(x=y,color=compare),
                       coord_flip = TRUE) +
    geom_histogram(aes(y=..density..,fill=compare),alpha=0.3,size=0.2,
                   position="identity",bins=60) + 
    geom_density()+
    geom_vline(data=mu, aes(xintercept=x, color=Group.1),
               linetype="dashed") + geom_rug(x=y,aes(color=compare),
                                             length = unit(0.05, "npc")) +
    coord_flip()
  
  if(!missing(legtitle) && !missing(legcontent)){
    if (!missing(colors)) {
      pmain <- pmain+
        scale_color_manual(legtitle,values=colors,labels=legcontent) +
        scale_fill_manual(legtitle,values=colors,labels=legcontent)
      yhist <- yhist + scale_color_manual(values=colors) +
        scale_fill_manual(values=colors)
    } else if(!missing(palette)){
      pmain <- pmain+
        scale_color_brewer(palette=palette,name = legtitle,labels = legcontent)+
        scale_fill_brewer(palette=palette,name=legtitle,labels=legcontent)
      yhist <- yhist + scale_color_brewer(palette=palette)+
        scale_fill_brewer(palette=palette)
    } else{
      pmain <- pmain+scale_color_discrete(name = legtitle,labels = legcontent)+
        scale_fill_discrete(name = legtitle,labels = legcontent)
    }
  } else{
    if(!missing(palette)){
      pmain <- pmain+
        scale_color_brewer(palette=palette)+scale_fill_brewer(palette=palette)
      yhist <- yhist + scale_color_brewer(palette=palette)+
        scale_fill_brewer(palette=palette)
    }
  }
  
  p2 <- insert_yaxis_grob(pmain, yhist, grid::unit(.2, "null"), 
                          position = "right")
  ggdraw(p2)
  
  if(!missing(saveimage)){
    if(!missing(dimensions)){
      ggsave(file=saveimage,width=dimensions[1],height = dimensions[2])
    } else
      ggsave(file=saveimage,width=8, height=5)
  }
  return(ggdraw(p2))
}

plot.box.raneffects <- function(
    data,
    data.raneffects,
    data.fixed,
    x,
    y,
    compare,
    joinid = FALSE,
    idstr,
    xylabels,
    legtitle,
    legcontent,
    title,
    saveimage,
    dimensions,
    testlab,
    pairs = FALSE,
    palette,
    colors,
    legend = TRUE,
    hide.x.axis=TRUE,
    hide.y.axis=TRUE
){
  
  pmain <- ggplot(data = data, aes(x=x,y=y,colour=compare)) +
    geom_violin(aes(fill=compare),position=position_dodge(1),
                alpha=0.3,width=0.8) +
    geom_boxplot(width=0.5, notch = TRUE,position=position_dodge(1),
                 outlier.shape = 18,outlier.size = 3,outlier.alpha = 0.7) +
    geom_point(alpha=0.1,size=1)+
    stat_summary(aes(fill=compare),fun=mean,geom="point",shape=23,size=4,
                 colour='purple',alpha=0.5,position=position_dodge(1))+
    stat_summary(aes(group=compare),fun.data=mean_se, geom="crossbar",
                 width=0.5, color="black",position = position_dodge(width = 1))
  if(joinid){
    pmain <- pmain + geom_line(aes(group = idstr), color="gray", size=0.1)
  } 
  
  if(!missing(testlab)){
    pmain <- pmain + stat_compare_means(aes(group=compare), 
                                        method = "t.test",
                                        paired = pairs, size=6,
                                        label.x.npc = testlab[1],
                                        label.y.npc = testlab[2])
  } else{
    pmain <- pmain + stat_compare_means(aes(group=compare),
                                        method = "t.test", 
                                        paired = pairs,
                                        size=6)
  }
  pmain <- pmain + stat_summary(fun.data=mean_se,geom="errorbar",size=1,
                                width=0.5,alpha=0.5)+
    ggtitle(title) + labs(x=xylabels[1],y=xylabels[2]) +
    theme_bw(base_size=14) +
    theme(plot.title=element_text(hjust = 0.5, size=24),
          text = element_text(size=24, family = "Arial"),
          strip.text.x = element_text(size=24),
          axis.text = element_text(size=20),
          # Hide panel borders and remove grid lines
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black"),
          aspect.ratio = 1)
  
  for (i in 1:nrow(data.raneffects)) {
    intercept <- data.fixed$Intercept + data.raneffects$Intercept[i]
    slope <- data.fixed$Slope + data.raneffects$Slope[i]
    
    # Plot a line for each subject (random intercept + slope)
    pmain <- pmain + 
      geom_abline(
        intercept = intercept, 
        slope = slope, 
        color = "black", 
        alpha = 0.3, 
        linetype = "dashed"
      )
  }
  
  
  if(hide.x.axis) {
    pmain <- pmain +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"))
  }
  
  if(hide.y.axis) {
    pmain <- pmain +
      theme(axis.title.y = element_text(colour = "white"))
  }
  
  if (!legend) {
    pmain <- pmain + theme(legend.position = "none")
  }
  
  mu <- aggregate(y,list(compare),mean)
  yhist <- axis_canvas(pmain,axis="y", mapping=aes(x=y,color=compare),
                       coord_flip = TRUE) +
    geom_histogram(aes(y=..density..,fill=compare),alpha=0.3,size=0.2,
                   position="identity",bins=60) + 
    geom_density()+
    geom_vline(data=mu, aes(xintercept=x, color=Group.1),
               linetype="dashed") + geom_rug(x=y,aes(color=compare),
                                             length = unit(0.05, "npc")) +
    coord_flip()
  
  if(!missing(legtitle) && !missing(legcontent)){
    if (!missing(colors)) {
      pmain <- pmain+
        scale_color_manual(legtitle,values=colors,labels=legcontent) +
        scale_fill_manual(legtitle,values=colors,labels=legcontent)
      yhist <- yhist + scale_color_manual(values=colors) +
        scale_fill_manual(values=colors)
    } else if(!missing(palette)){
      pmain <- pmain+
        scale_color_brewer(palette=palette,name = legtitle,labels = legcontent)+
        scale_fill_brewer(palette=palette,name=legtitle,labels=legcontent)
      yhist <- yhist + scale_color_brewer(palette=palette)+
        scale_fill_brewer(palette=palette)
    } else{
      pmain <- pmain+scale_color_discrete(name = legtitle,labels = legcontent)+
        scale_fill_discrete(name = legtitle,labels = legcontent)
    }
  } else{
    if(!missing(palette)){
      pmain <- pmain+
        scale_color_brewer(palette=palette)+scale_fill_brewer(palette=palette)
      yhist <- yhist + scale_color_brewer(palette=palette)+
        scale_fill_brewer(palette=palette)
    }
  }
  
  p2 <- insert_yaxis_grob(pmain, yhist, grid::unit(.2, "null"), 
                          position = "right")
  ggdraw(p2)
  
  if(!missing(saveimage)){
    if(!missing(dimensions)){
      ggsave(file=saveimage,width=dimensions[1],height = dimensions[2])
    } else
      ggsave(file=saveimage,width=8, height=5)
  }
  return(ggdraw(p2))
}

compare.slopes.hem.gender <- function(
    ATLAS,
    x,
    y,
    title,
    test.label.pos,
    color.left,
    color.right,
    legend = FALSE,
    nboots = 1000,
    models = c(
      1, # F.left
      1, # F.right
      1, # M.left
      1  # M.right
    ),
    joinid = FALSE,
    paired = FALSE,
    hide.x.axis.males=TRUE,
    hide.y.axis.males=TRUE,
    hide.x.axis.females=TRUE,
    hide.y.axis.females=TRUE,
    hide.x.axis.left=TRUE,
    hide.y.axis.left=TRUE,
    hide.x.axis.right=TRUE,
    hide.y.axis.right=TRUE
    ){
  
  females <- subset(ATLAS,Gender == "Female")
  left.females <- subset(females,Hemisphere=="Left")
  right.females <- subset(females,Hemisphere=="Right")
  males <- subset(ATLAS,Gender == "Male")
  left.males <- subset(males,Hemisphere=="Left")
  right.males <- subset(males,Hemisphere=="Right")
  
  #-----------------------------------------------------------------------------
  # Bootstrap - Y ~ X
  #-----------------------------------------------------------------------------
  set.seed(100)
  left.females.boots <- boots_slope(
    left.females[,x],
    left.females[,y],
    nboots,
    model = models[1]
  )
  set.seed(100)
  right.females.boots <- boots_slope(
    right.females[,x],
    right.females[,y],
    nboots,
    model = models[2]
  )
  set.seed(100)
  left.males.boots <- boots_slope(
    left.males[,x],
    left.males[,y],
    nboots,
    model = models[3]
  )
  set.seed(100)
  right.males.boots <- boots_slope(
    right.males[,x],
    right.males[,y],
    nboots,
    model = models[4]
  )
  
  cat("Boostraps for each Hemisphere - Gender combination done.","\n")
  
  features <- c(
    "id","intercept","slopes","BIC","adj_rsquared","pearson"
  )
  
  females.hemispheres <- dplyr::bind_rows(
    list(
      Left=left.females.boots[,features],
      Right=right.females.boots[,features]
    ),
    .id = 'Hemisphere'
  )
  females.hemispheres$Gender <- 'Female'
  
  males.hemispheres <- dplyr::bind_rows(
    list(
      Left=left.males.boots[,features],
      Right=right.males.boots[,features]
    ), 
    .id = 'Hemisphere'
  )
  males.hemispheres$Gender <- 'Male'
  
  y.x <- rbind(females.hemispheres,males.hemispheres)
  # y.x <- y.x[,-2]
  
  if (test.label.pos == "middle"){
    testlab=c(0.26,0.025)
  } else if (test.label.pos == "left") {
    testlab=c(0,0.025)
  }
  
  #-----------------------------------------------------------------------------
  # Compare gender for each hemisphere
  #-----------------------------------------------------------------------------
  left.gender <- y.x[y.x$Hemisphere == 'Left',] 
  # cohend.left <- cohen.d(data=left.gender, slopes~Gender)
  cohend.left <- rstatix::cohens_d(
    data = left.gender,
    slopes ~ Gender,
    paired = FALSE # between subjects paired always FALSE
  )
  right.gender <- y.x[y.x$Hemisphere == 'Right',] 
  # cohend.right <- cohen.d(data=right.gender, slopes~Gender)
  cohend.right <- rstatix::cohens_d(
    data = right.gender,
    slopes ~ Gender,
    paired = FALSE # between subjects paired always FALSE
  )
  
  plt.left <- plot.box.boots(
    left.gender,
    x = left.gender$Gender,
    y = left.gender$slopes,
    compare = left.gender$Gender,
    xylabels = c("Gender","Slope"),
    title=paste(title," | Left (d = ",
                round(abs(cohend.left$effsize),
                      3),")", sep=""),
    legtitle = "Gender",
    legcontent = c("Female","Male"),
    testlab = testlab,
    colors = rep(color.left,length(unique(y.x$Gender))),
    legend = legend,
    hide.x.axis = hide.x.axis.left,
    hide.y.axis = hide.y.axis.left
  )
  
  plt.right <- plot.box.boots(
    right.gender,
    x=right.gender$Gender,
    y=right.gender$slopes,
    compare=males.hemispheres$Hemisphere,
    xylabels=c("Gender","Slope"),
    title=paste(title," | Right (d = ",
                round(abs(cohend.right$effsize),
                      3),")", sep=""),
    legtitle = "Gender",
    legcontent = c("Female","Male"),
    testlab = testlab,
    colors = rep(color.right,length(unique(y.x$Gender))),
    legend = legend,
    hide.x.axis = hide.x.axis.right,
    hide.y.axis = hide.y.axis.right
  )
  
  #-----------------------------------------------------------------------------
  # Compare hemispheres for each gender
  #-----------------------------------------------------------------------------
  males.hemispheres <- y.x[y.x$Gender == 'Male',]
  # cohend.males <- cohen.d(data=males.hemispheres, slopes~Hemisphere)
  cohend.males <- rstatix::cohens_d(
    data = males.hemispheres,
    slopes ~ Hemisphere,
    paired = paired
  )
  
  females.hemispheres <- y.x[y.x$Gender == 'Female',]
  # cohend.females <- cohen.d(data=females.hemispheres, slopes~Hemisphere)
  cohend.females <- rstatix::cohens_d(
    data = females.hemispheres,
    slopes ~ Hemisphere,
    paired = paired
  )
  
  plt.females <- plot.box.boots(
    females.hemispheres,
    x=females.hemispheres$Hemisphere,
    y=females.hemispheres$slopes,
    compare=females.hemispheres$Hemisphere,
    xylabels=c("Hemisphere","Slope"),
    title=paste(title," | Females (d = ",
                round(abs(cohend.females$effsize),
                      3),")", sep=""),
    legtitle = "Hemisphere",
    legcontent = c("Left","Right"),
    testlab = testlab,
    pairs = paired,
    joinid = joinid,
    # idstr = females.hemispheres$id,
    palette="Set1",
    legend = legend,
    hide.x.axis = hide.x.axis.females,
    hide.y.axis = hide.y.axis.females
  )
  
  plt.males <- plot.box.boots(
    males.hemispheres,
    x = males.hemispheres$Hemisphere,
    y = males.hemispheres$slopes,
    compare=males.hemispheres$Hemisphere,
    xylabels=c("Hemisphere","Slope"),
    title=paste(title," | Males (d = ",
                round(abs(cohend.males$effsize),
                      3),")", sep=""),
    legtitle = "Hemisphere",
    legcontent = c("Left","Right"),
    testlab = testlab,
    pairs = paired,
    joinid = joinid,
    # idstr = males.hemispheres$id,
    palette="Set1",
    legend = legend,
    hide.x.axis = hide.x.axis.males,
    hide.y.axis = hide.y.axis.males
  )
  
  return(list(plt.left = plt.left,
              plt.right = plt.right,
              plt.females = plt.females,
              plt.males = plt.males))
}

plt.models <- function(
    ATLAS,
    x,
    y,
    model.fem.left,
    model.fem.right,
    model.male.left,
    model.male.right,
    xlab,
    ylab = NULL,
    line.width,
    title,
    legend = TRUE,
    hide.x.axis = FALSE,
    toWT = FALSE,
    tag,
    tag.size,
    font.size
    ) {
  
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
    if(is.null(ylab)){
      ylab = bquote(frac(Vol[.(y)], Vol['Thalamus']) ~ '[%]')
    }
    y = "yfrac_WT"
  }
  
  if (missing(font.size)) {
    font.size <- 24
  }
  
  a <- levels(ATLAS[,"hemgen"])
  
  custom.colors <- c()
  for (i in a){ 
    if (grepl("Left",i,fixed=TRUE)){ 
      custom.colors <- c(custom.colors,"#E41A1C")
    } else { 
      custom.colors <- c(custom.colors,"#377EB8")
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
  
  custom.shape <- c()
  for (i in a){ 
    if (grepl("Female",i,fixed=TRUE)){ 
      custom.shape <- c(custom.shape,16)
    } else { 
      custom.shape <- c(custom.shape,17)
    }
  }
  
  plt.vol <- ggplot(ATLAS,aes_string(x=x,
                                     y=y,
                                     colour="hemgen",
                                     shape="hemgen",
                                     linetype="hemgen")) + 
    geom_point(alpha=0.5,key_glyph = large_points) + 
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
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_colour_manual("",values=custom.colors) + # c("#E41A1C","#E41A1C","#377EB8","#377EB8")
    scale_linetype_manual("",values=custom.linetype) + # c(1,2,1,2)
    scale_shape_manual("",values=custom.shape) + # c(16,17,16,17)
    guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA, NA))),
           shape=guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(plot.title = element_text(hjust = 0.5, size = font.size, face = "bold"),
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
    plt.vol <- plt.vol + 
      theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=20, 
                                   colour = "white"),
        axis.title.x = element_text(colour = "white")
      )
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

plt.models.gender <- function(
    ATLAS,
    y,
    x,
    model.female,
    model.male,
    xlab,
    ylab,
    line.width,
    title,
    facewrap=FALSE,
    legend=TRUE,
    hide.x.axis = FALSE,
    tag,
    tag.size
) {
  
  if (missing(xlab)){
    xlab="Age"
  }
  
  if (missing(x)){
    x="AGE"
  }
  
  if (missing(line.width)){
    line.width= 0.75
  }
  
  plt.vol <- ggplot(ATLAS,aes_string(x=x,
                                     y=y,
                                     shape="Gender")) + 
    geom_point(alpha=0.5,key_glyph = large_points) + 
    # females
    stat_smooth(data=subset(ATLAS,Gender=="Female"),
                aes_string(linetype="Gender"),method = "lm", 
                formula = model.female, alpha=0, size=line.width) +
    # males
    stat_smooth(data=subset(ATLAS,Gender=="Male"),
                aes_string(linetype="Gender"),method = "lm", 
                formula = model.male, alpha=0, size=line.width) +
    scale_linetype_manual("",values=c(1,2)) +
    scale_shape_manual("",values=c(16,17)) +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_color_brewer(palette="Set1") +  scale_fill_brewer(palette="Set1") +
    guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA))),
           shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(plot.title = element_text(hjust = 0.5, 
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
          legend.key.width = unit(5,"line"),
          aspect.ratio=1)
  
  if (facewrap) {
    plt.vol <- plt.vol + facet_wrap(~Gender)
  }
  
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

plt.models.gender.V2 <- function(
    ATLAS,
    y,
    x,
    model.female,
    model.male,
    xlab,
    ylab,
    line.width,
    title,
    facewrap=FALSE,
    legend=TRUE,
    hide.x.axis = FALSE,
    font.size,
    tag,
    tag.size,
    custom.colors = NULL
    ) {
  
  if (missing(xlab)){
    xlab="Age"
  }
  
  if (missing(x)){
    x="AGE"
  }
  
  if (missing(line.width)){
    line.width= 0.75
  }
  
  if (missing(font.size)) {
    font.size <- 24
  }
  
  ATLAS[,"hemgen"] <- droplevels(ATLAS[,"hemgen"])
  
  a <- levels(ATLAS[,"hemgen"])
  
  if(is.null(custom.colors)){
    custom.colors <- c()
    for (i in a){ 
      if (grepl("Left",i,fixed=TRUE)){ 
        custom.colors <- c(custom.colors,"#E41A1C")
      } else { 
        custom.colors <- c(custom.colors,"#377EB8")
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
  
  custom.shape <- c()
  for (i in a){ 
    if (grepl("Female",i,fixed=TRUE)){ 
      custom.shape <- c(custom.shape,16)
    } else { 
      custom.shape <- c(custom.shape,17)
    }
  }
  
  plt.vol <- ggplot(ATLAS,aes_string(x=x,
                                     y=y,
                                     colour="hemgen",
                                     shape="hemgen",
                                     linetype="hemgen")) + 
    geom_point(alpha=0.5,key_glyph = large_points) + 
    # Female
    stat_smooth(data=subset(ATLAS,Gender == "Female"),
                method = "lm", formula = model.female, 
                alpha=0, size = line.width) +
    # Male
    stat_smooth(data=subset(ATLAS,Gender == "Male"),
                method = "lm", formula = model.male, 
                alpha=0, size = line.width) +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_colour_manual("",values=custom.colors) + # c("#E41A1C","#E41A1C","#377EB8","#377EB8")
    scale_linetype_manual("",values=custom.linetype) + # c(1,2,1,2)
    scale_shape_manual("",values=custom.shape) + # c(16,17,16,17)
    guides(linetype = guide_legend(override.aes = list(fill = c(NA,NA))),
           shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = font.size, 
                                    face="bold"),
          text = element_text(size=font.size, family = "Arial"),
          strip.text.x = element_text(size=font.size),
          axis.text = element_text(size=font.size - 4 ),
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
  
  if (facewrap) {
    plt.vol <- plt.vol + facet_wrap(~Gender)
  }
  
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

plt.models.hem <- function(
    ATLAS,
    y,
    x,
    model.left,
    model.right,
    xlab,
    ylab,
    line.width,
    title,
    facewrap=FALSE,
    legend=TRUE,
    hide.x.axis = FALSE,
    font.size,
    tag,
    tag.size
) {
  
  if (missing(xlab)){
    xlab="Age"
  }
  
  if (missing(x)){
    x="AGE"
  }
  
  if (missing(line.width)){
    line.width= 0.75
  }
  
  if (missing(font.size)) {
    font.size <- 24
  }
  
  ATLAS[,"hemgen"] <- droplevels(ATLAS[,"hemgen"])
  
  a <- levels(ATLAS[,"hemgen"])
  
  custom.colors <- c()
  for (i in a){ 
    if (grepl("Left",i,fixed=TRUE)){ 
      custom.colors <- c(custom.colors,"#E41A1C")
    } else { 
      custom.colors <- c(custom.colors,"#377EB8")
    }
  }
  print(custom.colors)
  custom.linetype <- c()
  for (i in a){ 
    if (grepl("Female",i,fixed=TRUE)){ 
      custom.linetype <- c(custom.linetype,1)
    } else { 
      custom.linetype <- c(custom.linetype,2)
    }
  }
  
  custom.shape <- c()
  for (i in a){ 
    if (grepl("Female",i,fixed=TRUE)){ 
      custom.shape <- c(custom.shape,16)
    } else { 
      custom.shape <- c(custom.shape,17)
    }
  }
  
  plt.vol <- ggplot(ATLAS,aes_string(x=x,
                                     y=y,
                                     colour="hemgen",
                                     shape="hemgen",
                                     linetype="hemgen")) + 
    geom_point(alpha=0.5,key_glyph = large_points) + 
    # left
    stat_smooth(data=subset(ATLAS,Hemisphere == "Left"),
                method = "lm", formula = model.left, 
                alpha=0, size = line.width) +
    # right
    stat_smooth(data=subset(ATLAS,Hemisphere == "Right"),
                method = "lm", formula = model.right, 
                alpha=0, size = line.width) +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_colour_manual("",values=custom.colors) + # c("#E41A1C","#E41A1C","#377EB8","#377EB8")
    scale_linetype_manual("",values=custom.linetype) + # c(1,2,1,2)
    scale_shape_manual("",values=custom.shape) + # c(16,17,16,17)
    guides(linetype = guide_legend(override.aes = list(fill = c(NA,NA))),
           shape = guide_legend(override.aes = list(alpha=0.5,size=1.5))) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = font.size, 
                                    face="bold"),
          text = element_text(size=font.size, family = "Arial"),
          strip.text.x = element_text(size=font.size),
          axis.text = element_text(size=font.size-4),
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
  
  if (facewrap) {
    plt.vol <- plt.vol + facet_wrap(~Hemisphere)
  }
  
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
  
  
  plt.female <- ggplot(data=female,aes(x=model,y=y,
                                       colour=Hemisphere,
                                       fill=Hemisphere,
                                       group=Hemisphere)) + 
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
    plt.female <- plt.female + ggtitle(volume)
    plt.male <- plt.male + ggtitle(volume)
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

plt.BIC.gender <- function(dataset,
                           volume,
                           xlab,
                           ylab,
                           title,
                           legend=FALSE,
                           hide.x.axis=TRUE){
  dataset$model <- mapvalues(dataset$model,
                             from=c("Linear",
                                    "Quadratic",
                                    "Cubic"),
                             to = c("1","2","3"))
  
  dataset <- subset(dataset,vol == volume)
  
  optimal.BIC <- dataset %>%
    group_by(Gender) %>%
    group_modify(~diff.BIC(data=.x,
                           variable="y"))
  optimal.BIC$model <- as.factor(optimal.BIC$model)
  
  plt <- ggplot(data=dataset,aes(x=model,y=y,group=Gender)) +
    geom_vline(data=optimal.BIC,aes(xintercept = model),color="red")+
    geom_ribbon(aes(ymin=(y-ci),ymax=(y+ci)),colour=NA,alpha=0.25) +
    geom_line(aes(linetype=Gender),alpha=1,size=1) +
    scale_linetype_manual(values=c(1, 2)) +
    geom_point(shape=16,size=2.25,key_glyph = large_points) +
    theme_bw() + labs(x=xlab, y=ylab) +
    scale_y_continuous(breaks= pretty_breaks()) +
    facet_wrap(~Gender,scales="free_y") +
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
    plt <- plt + ggtitle(volume)
  } else {
    plt <- plt + ggtitle(title)
  }
  
  if (!legend) {
    plt <- plt + theme(legend.position = "none")
  }
  
  if (hide.x.axis) {
    plt <- plt + theme(axis.ticks.x = element_blank(),
                       axis.text.x = element_text(size=20, colour = "white"),
                       axis.title.x = element_text(colour = "white"))
  }
  
  return(plt)
}

# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # # # # # # # # # # # # # # ASYM INDEX # # # # # # # # # # # # # # # # #
# **************************************************************************** #

# asymmetry index computation

asym_index <- function(atlas,scale,nuclei_names,feat){
  l <- atlas[which(atlas$Hemisphere=='Left'),]
  r <- atlas[which(atlas$Hemisphere=='Right'),]
  
  asym_atlas <- list()
  
  if(scale=="yes"){
    for (i in 1:length(nuclei_names)){
      nuc <- nuclei_names[i] 
      asym_atlas[[i]] <- scale(200*((r[nuc]-l[nuc])/(l[nuc]+r[nuc])), 
                               center=TRUE, scale=TRUE)
    } 
  } else{
    for (i in 1:length(nuclei_names)){
      nuc <- nuclei_names[i] 
      asym_atlas[[i]] <- 200*((r[nuc]-l[nuc])/(l[nuc]+r[nuc]))
    } 
  }
  asym_atlas <- data.frame(asym_atlas)
  asym_atlas <- cbind(asym_atlas,l[feat])
  return(asym_atlas)
}  

# ------------------------------------------------------------------------------
# **************************************************************************** #
# # # # # # # # # # # # # # # # DIAGNOSTIC  TOOL # # # # # # # # # # # # # # # #
# **************************************************************************** #
# input dataframe with the residuals for each Y-X value
# the column must be "residuals_normalized@
plt.diagnostic <- function(
    data,
    residuals = "residuals_standardized",
    residuals.type = "Standardized Residuals",
    fitted = NULL,
    title = "",
    title.size = 16,
    sub.titles = TRUE,
    hide.ylabels = FALSE
) {
  
  if (sub.titles) {
    color.title <- "black"
  } else {
    color.title <- "white"
  }
  
  if (hide.ylabels){
    color.ylabs <- "white"
  } else {
    color.ylabs <- "black"
  }
  
  x_limits <- range(data[,residuals])
  p1 <- ggplot(
    data, 
    aes_string(x = residuals)
  ) +
    geom_boxplot(
      width = 0.1, 
      color = "darkblue", 
      fill = "lightblue", 
      outlier.size = 1.75
    ) +
    geom_vline(xintercept=0, linetype="dotted") +
    coord_cartesian(xlim = x_limits) +  # Add consistent limits
    labs(
      title = paste0("Boxplot of ", residuals.type),
      # title = paste0("<span style='color:red;'>Boxplot of ", residuals.type, "</span>"), 
      x = "",
      y = ""
    ) +
    theme_minimal() + 
    theme(
      plot.title = element_text(
        size = title.size - 2,
        color = color.title
      ),
      axis.text = element_text(
        size = title.size - 2
      ),
      panel.grid = element_blank(),
      axis.text.y =element_blank(),
      axis.ticks.y =element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  p2 <- ggplot(
    data,
    aes_string(x = residuals)
  ) +
    geom_histogram(
      aes(y = ..density..),
      bins = 40, 
      fill = "lightgray", 
      color = "black"
    ) +
    # Density line
    geom_density(color = "blue", size = 1) +
    geom_vline(xintercept=0, linetype="dotted") +
    labs(
      title = paste0("Histogram of ", residuals.type), 
      x = "", 
      y = "Density"
    ) +
    coord_cartesian(xlim=x_limits) + 
    theme_minimal() +
    theme(
      plot.title = element_text(
        size = title.size - 2,
        color = color.title
      ),
      axis.title = element_text(
        size = title.size - 2
      ),
      axis.text = element_text(
        size = title.size - 2
      ),
      axis.title.y = element_text(
        color = color.ylabs
      ),
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) 

  p3 <- ggplot(
    data, 
    aes_string(sample = residuals)
  )
  # p3 <- p3 + 
  #   stat_qq(
  #     aes(x = ..sample.., y = ..theoretical..),
  #     size = 1.75, 
  #     alpha = 0.6, 
  #     color = "blue"
  #   ) +
  #   stat_qq_line(
  #     # aes(x = ..theoretical.., y = ..sample..),
  #     color = "red",
  #     linetype = "dashed"
  #   ) +
  #   # coord_cartesian(xlim=x_limits) +
  #   geom_vline(xintercept=0, linetype="dotted") +
  #   # coord_flip() +
  #   labs(
  #     title = paste0("QQ plot of ",residuals.type), 
  #     x = "Quantiles of standard normal", 
  #     y = residuals.type
  #   ) +
  #   theme_minimal() + 
  #   theme(panel.grid = element_blank())
  
  # Compute Q-Q plot data
  qq_data <- qqnorm(y = data[[residuals]], plot.it = FALSE)
  qq_df <- data.frame(
    theoretical = qq_data$x,  # Theoretical quantiles
    sample = qq_data$y        # Sample quantiles
  )
  
  # Manually compute Q-Q line (line through 1st and 3rd quartiles)
  line_df <- data.frame(
    x = quantile(qq_df$theoretical, probs = c(0.25, 0.75)),
    y = quantile(qq_df$sample, probs = c(0.25, 0.75))
  )
  
  # Compute the slope and intercept using the 1st and 3rd quantiles
  slope <- diff(quantile(qq_df$sample, probs = c(0.25, 0.75))) / 
    diff(quantile(qq_df$theoretical, probs = c(0.25, 0.75)))
  intercept <- quantile(qq_df$sample, probs = 0.25) - slope * quantile(qq_df$theoretical, probs = 0.25)
  
  # Get y-limits
  y_limits <- c(min(qq_df$theoretical), max(qq_df$theoretical))
  
  # Calculate the corresponding x-values for the y-limits using the line equation y = mx + b
  x_min <- (y_limits[1] - intercept) / slope
  x_max <- (y_limits[2] - intercept) / slope
  
  # Create the extended line based on the x-values corresponding to the y-limits
  x_range <- c(x_min, x_max)
  y_range <- intercept + slope * x_range
  
  # x_range <- seq(min(qq_df$sample), max(qq_df$sample), length.out = 100)
  # y_range <- intercept + slope * x_range
  
  line_df <- data.frame(x = x_range, y = y_range)
  
  # Custom Q-Q plot with flipped axes
  p3 <- ggplot(qq_df, aes(x = sample, y = theoretical)) +  # Swap x and y
    geom_point(
      size = 1.75, 
      alpha = 0.6, 
      color = "blue"
    ) +
    geom_line(
      data = line_df, 
      aes(x = y, y = x), 
      color = "red", 
      size = 0.5, 
      linetype = "dashed"
    ) +
    geom_vline(xintercept=0, linetype="dotted") +
    coord_cartesian(xlim=x_limits) +
    labs(
      # title = paste0("Q-Q plot of ",residuals.type),
      title = "Normal Q-Q plot",
      # y = "Quantiles of standard normal", 
      y = "Theoretical quantiles", 
      x = residuals.type
    ) +
    theme_minimal() + 
    theme(
      plot.title = element_text(
        size = title.size - 2,
        color = color.title
      ),
      axis.title = element_text(
        size = title.size - 2
      ),
      axis.text = element_text(
        size = title.size - 2
      ),
      axis.title.y = element_text(
        color = color.ylabs
      ),
      panel.grid = element_blank()
      )
  
  if (!is.null(fitted)){
    p.residuals_fitted <- 
      ggplot(
        data = data,
        aes_string(
          x = fitted, 
          y = residuals
        )
      ) + 
      geom_hline(
        yintercept = 0, 
        color = "black", 
        linetype = "dashed", 
        size = 1
      ) +
      geom_smooth(method = "loess", color = "red", se = FALSE) +
      guides(alpha="none") +
      labs(title = paste0(residuals.type," vs Fitted"), 
           x = "Fitted values", 
           y = residuals.type) +
      geom_point(size = 1.75, alpha = 0.6, color = "blue") + 
      theme_minimal() + 
      theme(
        plot.title = element_text(
          size = title.size - 2,
          color = color.title
        ),
        axis.text = element_text(
          size = title.size - 4
        ),
        axis.title = element_text(
          size = title.size - 2
        ),
        axis.title.y = element_text(
          color = color.ylabs
        ),
        panel.grid = element_blank()
        )
    
    p <- p1 / p2 / p3 / p.residuals_fitted +
      plot_layout(
        heights = c(0.25, 1, 1, 1),
        widths = c(1,1,1,1)
      ) +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(
            hjust = 0.5, 
            face = "bold", 
            size = title.size
          )
        )
        )
  } else {
    p <- p1 / p2 / p3 + 
      plot_layout(
        heights = c(0.25, 1, 1),
        widths = c(1,1,1)
      ) +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(
            hjust = 0.5, 
            face = "bold", 
            size = title.size
          )
        )
      )
  }
  
  p <-  wrap_elements(full = p)
  
  return(p)
}


# utils
# ------------------------------------------------------------------------------
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and 
## confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be 
##               summariezed
##   groupvars: a vector containing names of columns that contain grouping 
##              variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval 
##                  (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

large_points <- function(data, params, size) {
  # Multiply by some number
  data$size <- data$size * 4
  draw_key_point(data = data, params = params, size = size)
}

critical.points.quadratic.cubic <- function(
    intercept,
    linear_term,
    quadratic_term,
    cubic_term
  ){
  
  if (missing(intercept)){
    print("Error, no intercept found")
  }else if (!missing(linear_term) & !missing(quadratic_term) & missing(cubic_term)){
    x_critical_point <- -linear_term / (2 * quadratic_term)
    y_critical_point <- intercept + linear_term*x_critical_point + quadratic_term*x_critical_point^2
    # cat(sprintf("Critical point is at age %7.3f \n", x_critical_point))
    # cat(sprintf("Y at critical point is %7.3f \n", y_critical_point))
    return(
      list(
        x_critical = x_critical_point,
        y_critical = y_critical_point
      )
    )
    
  } else if(!missing(linear_term) & !missing(quadratic_term) & !missing(cubic_term)){
    # Solve for critical points using the first derivative (where f'(x) = 0)
    # First, let's solve the quadratic equation 3ax^2 + 2bx + c = 0 using the quadratic formula.
    # use rule of thumb . the term under the square is called the discriminant,
    # which tells us about the nature of the roots (real or complex)
    # Positive discriminant: the equation has two distinct real roots (= 2 
    # critical points)
    # Discriminant = 0, the equation has exactly one real root ( = 1 critical 
    # point)
    # Negative discriminant: the equation has no real roots
    
    discriminant <- quadratic_term^2 - 3 * cubic_term * linear_term
    if (discriminant > 0){
      root1 <- (-quadratic_term + sqrt(discriminant)) / (3 * cubic_term)
      root2 <- (-quadratic_term - sqrt(discriminant)) / (3 * cubic_term)
      
      # Check whether these points are maxima or minima using the second derivative
      second_deriv1 <- 6*cubic_term*root1 + 2*quadratic_term
      second_deriv2 <- 6*cubic_term*root2 + 2*quadratic_term
      
      # cat("Second derivative at root 1:", second_deriv1, "\n")
      # cat("Second derivative at root 2:", second_deriv2, "\n")
      
      # Classify points as maximum or minimum
      if (second_deriv1 < 0) {
        # cat("Root 1 is a maximum (plateau).\n")
        x_max <- root1
      } else if (second_deriv1 > 0) {
        # cat("Root 1 is a minimum (valley).\n")
        x_min <- root1
      }
      
      if (second_deriv2 < 0) {
        # cat("Root 2 is a maximum (plateau).\n")
        x_max <- root2
      } else if (second_deriv2 > 0) {
        # cat("Root 2 is a minimum (valley).\n")
        x_min <- root2
      }
      
      y_max <- cubic_term*x_max^3+quadratic_term*x_max^2+linear_term*x_max+intercept
      y_min <- cubic_term*x_min^3+quadratic_term*x_min^2+linear_term*x_min+intercept
      
      # cat(sprintf("Critical point for y_max is at age %7.3f \n", x_max))
      # cat(sprintf("Y max is %7.3f \n", y_max))
      # 
      # cat(sprintf("Critical point for y_min is at age %7.3f \n", x_min))
      # cat(sprintf("Y min is %7.3f \n", y_min))
      # 
      return(
        list(
          x_max = x_max,
          y_max = y_max,
          x_min = x_min,
          y_min = y_min
        )
      )
      
    } else if(discriminant == 0){
      # only 1 solution 
      x <- quadratic_term / (3 * cubic_term)
      y <- cubic_term*x^3+quadratic_term*x^2+linear_term*x+intercept
      # we'll assume that is max for code structure purposes in
      # bootstrap_regressions.gender when entering the cubic mode
      return(
        list(
          x_max = x,
          y_max = y
        )
      )
    } else{
      cat("No real critical points found.\n")
    }
  }
  
  # return(
  #   list(
  #     x_critical = x_critical_point,
  #     y_critical = y_critical_point
  #   )
  # )
}


bootstrap_regressions.gender <- function(
    ATLAS,
    x,
    y,
    # title,
    # test.label.pos,
    # colorsleft,
    # colorsright,
    nboots = 1000,
    models = c( 
      1, # Female 
      1, # Male
    )){
  females <- subset(ATLAS, Gender == "Female")
  males <- subset(ATLAS, Gender == "Male")
  
  #-----------------------------------------------------------------------------
  # Bootstrap - Y ~ X
  #-----------------------------------------------------------------------------
  set.seed(100)
  females.boots <- boots_slope(
    females[,x],
    females[,y],
    nboots,
    model = models[1]
  )
  
  set.seed(100)
  males.boots <- boots_slope(
    males[,x],
    males[,y],
    nboots,
    model = models[2]
  )
  
  females.boots$Gender <- 'Female'
  males.boots$Gender <- 'Male'
  
  y.x <- rbind.fill(females.boots,males.boots)
  
  # y.x$hemgen <- paste0(y.x$Hemisphere," - ", 
  #                      y.x$Gender,"  ")
  # y.x$hemgen <- as.factor(y.x$hemgen)
  
  x_vals <- sort(unique(ATLAS[,x]))
  
  ###
  
  boot_lines <- do.call(rbind, lapply(1:nrow(y.x), function(i) {
    
  dat <- y.x[i,!is.na(y.x[i,])]
  if (!("quadratic_term" %in% names(dat))) {
    data.frame(
      AGE = x_vals,
      y = y.x$intercept[i] + y.x$linear_term[i] * x_vals,
      Gender = y.x$Gender[i],
      Line = i
    )
  } else if (("quadratic_term" %in% names(dat)) && !("cubic_term" %in% names(dat))){
    data.frame(
      AGE = x_vals,
      y = y.x$intercept[i] + y.x$linear_term[i] * x_vals + y.x$quadratic_term[i] * x_vals^2,
      Gender = y.x$Gender[i],
      Line = i
    )
  } else if ("cubic_term" %in% names(dat)){
    data.frame(
      AGE = x_vals,
      y = y.x$intercept[i] + y.x$linear_term[i] * x_vals + y.x$quadratic_term[i] * x_vals^2 + y.x$cubic_term[i] * x_vals^3,
      Gender = y.x$Gender[i],
      Line = i
    )
  }
}))
  return(
    list(
      boot_lines = boot_lines, 
      yx = y.x
    )
  )
  
}

bootstrap_regressions.gender.V2 <- function(
    ATLAS,
    x,
    y,
    # title,
    # test.label.pos,
    # colorsleft,
    # colorsright,
    nboots = 1000,
    models = c( 
      1, # Female 
      1, # Male
    )){
  females <- subset(ATLAS, Gender == "Female")
  males <- subset(ATLAS, Gender == "Male")
  
  #-----------------------------------------------------------------------------
  # Bootstrap - Y ~ X
  #-----------------------------------------------------------------------------
  set.seed(100)
  females.boots <- boots_slope(
    females[,x],
    females[,y],
    nboots,
    model = models[1]
  )
  
  set.seed(100)
  males.boots <- boots_slope(
    males[,x],
    males[,y],
    nboots,
    model = models[2]
  )
  
  females.boots$Gender <- 'Female'
  males.boots$Gender <- 'Male'
  
  y.x <- rbind.fill(females.boots,males.boots)
  
  # y.x$hemgen <- paste0(y.x$Hemisphere," - ", 
  #                      y.x$Gender,"  ")
  # y.x$hemgen <- as.factor(y.x$hemgen)
  
  x_vals <- sort(unique(ATLAS[,x]))
  
  ###
  
  boot_lines <- do.call(rbind, lapply(1:nrow(y.x), function(i) {
    
    dat <- y.x[i,!is.na(y.x[i,])]
    if (!("quadratic_term" %in% names(dat))) {
      data.frame(
        AGE = x_vals,
        y = y.x$intercept[i] + y.x$linear_term[i] * x_vals,
        Gender = y.x$Gender[i],
        Line = i
      )
    } else if (("quadratic_term" %in% names(dat)) && !("cubic_term" %in% names(dat))){
      data.frame(
        AGE = x_vals,
        y = y.x$intercept[i] + y.x$linear_term[i] * x_vals + y.x$quadratic_term[i] * x_vals^2,
        Gender = y.x$Gender[i],
        Line = i
      )
    } else if ("cubic_term" %in% names(dat)){
      data.frame(
        AGE = x_vals,
        y = y.x$intercept[i] + y.x$linear_term[i] * x_vals + y.x$quadratic_term[i] * x_vals^2 + y.x$cubic_term[i] * x_vals^3,
        Gender = y.x$Gender[i],
        Line = i
      )
    }
  }))
  
  peaks <- do.call(rbind, lapply(1:nrow(y.x), function(i) {
    
    dat <- y.x[i,!is.na(y.x[i,])]
    if (("quadratic_term" %in% names(dat)) && !("cubic_term" %in% names(dat))){
      critical.points <- critical.points.quadratic.cubic(
        y.x$intercept[i],
        y.x$linear_term[i],
        y.x$quadratic_term[i]
      )
      
      data.frame(
        x = critical.points$x_critical,
        y = critical.points$y_critical,
        Gender = y.x$Gender[i],
        Line = i
      )
    } else if ("cubic_term" %in% names(dat)){
      # print(i)
      # print(y.x$intercept[i])
      # print(y.x$slopes[i])
      # print(y.x$quadratic_term[i])
      # print(y.x$cubic.term[i])
      
      critical.points <- critical.points.quadratic.cubic(
        y.x$intercept[i],
        y.x$linear_term[i],
        y.x$quadratic_term[i],
        y.x$cubic_term[i]
      )
      
      data.frame(
        x = critical.points$x_max,
        y = critical.points$y_max,
        Gender = y.x$Gender[i],
        Line = i
      )
    }
  }))
  
  
  return(
    list(
      boot_lines = boot_lines,
      peaks = peaks,
      yx = y.x
    )
  )
  
}


bootstrap_regressions <- function(
    ATLAS,
    x,
    y,
    # title,
    # test.label.pos,
    # colorsleft,
    # colorsright,
    nboots = 1000,
    models = c( 
      1, # Left-Female 
      1, # Left-Male
      1, # Right-Female
      1  # Right-Male
    )){
  females <- subset(ATLAS, Gender == "Female")
  left.females <- subset(females, Hemisphere=="Left")
  right.females <- subset(females, Hemisphere=="Right")
  males <- subset(ATLAS, Gender == "Male")
  left.males <- subset(males, Hemisphere=="Left")
  right.males <- subset(males, Hemisphere=="Right")
  
  #-----------------------------------------------------------------------------
  # Bootstrap - Y ~ X
  #-----------------------------------------------------------------------------
  set.seed(100)
  left.females.boots <- boots_slope(
    left.females[,x],
    left.females[,y],
    nboots,
    model = models[1]
  )
  set.seed(100)
  right.females.boots <- boots_slope(
    right.females[,x],
    right.females[,y],
    nboots,
    model = models[3]
  )
  set.seed(100)
  left.males.boots <- boots_slope(
    left.males[,x],
    left.males[,y],
    nboots,
    model = models[2]
  )
  set.seed(100)
  right.males.boots <- boots_slope(
    right.males[,x],
    right.males[,y],
    nboots,
    model = models[4]
  )
  
  females.hemispheres <- dplyr::bind_rows(list(Left=left.females.boots,
                                               Right=right.females.boots),
                                          .id = 'Hemisphere')
  females.hemispheres$Gender <- 'Female'
  
  males.hemispheres <- dplyr::bind_rows(list(Left=left.males.boots,
                                             Right=right.males.boots), 
                                        .id = 'Hemisphere')
  males.hemispheres$Gender <- 'Male'
  
  y.x <- rbind.fill(females.hemispheres,males.hemispheres)
  
  # y.x$hemgen <- paste0(y.x$Hemisphere," - ", 
  #                      y.x$Gender,"  ")
  # y.x$hemgen <- as.factor(y.x$hemgen)
  
  x_vals <- sort(unique(ATLAS[,x]))
  
  ###
  
  boot_lines <- do.call(rbind, lapply(1:nrow(y.x), function(i) {
    
    dat <- y.x[i,!is.na(y.x[i,])]
    if (!("quadratic_term" %in% names(dat))) {
      data.frame(
        AGE = x_vals,
        y = y.x$intercept[i] + y.x$linear_term[i] * x_vals,
        Hemisphere = y.x$Hemisphere[i],
        Gender = y.x$Gender[i],
        Line = i
      )
    } else if (("quadratic_term" %in% names(dat)) && !("cubic_term" %in% names(dat))){
      data.frame(
        AGE = x_vals,
        y = y.x$intercept[i] + y.x$linear_term[i] * x_vals + y.x$quadratic_term[i] * x_vals^2,
        Hemisphere = y.x$Hemisphere[i],
        Gender = y.x$Gender[i],
        Line = i
      )
    } else if ("cubic_term" %in% names(dat)){
      data.frame(
        AGE = x_vals,
        y = y.x$intercept[i] + y.x$linear_term[i] * x_vals + y.x$quadratic_term[i] * x_vals^2 + y.x$cubic_term[i] * x_vals^3,
        Hemisphere = y.x$Hemisphere[i],
        Gender = y.x$Gender[i],
        Line = i
      )
    }
  }))
  return(
    list(
      boot_lines = boot_lines, 
      yx = y.x
    )
  )
  
}

plt.bootstrap_regressions <- function(
    data,
    x,
    y,
    color = NULL,
    group = "Line",
    splitby,
    accuracy.decimals = 0.01,
    font.size = 24,
    title,
    xlab = NULL,
    ylab = NULL,
    legend = FALSE,
    hide.x.axis = FALSE
    ){
  
  if (!is.null(color)){
    data[,color] <- as.factor(data[,color])
    color.levels <- levels(data[,color])
    
    custom.colors <- c()
    for (i in color.levels){ 
      if (grepl("Left",i,fixed=TRUE)){ 
        custom.colors <- c(custom.colors,"#E41A1C")
      } else { 
        custom.colors <- c(custom.colors,"#377EB8")
      }
    }
    
    p <- ggplot(
      data = data,
      aes_string(
        x = x, 
        y = y, 
        group = group, 
        color = color
      )
    ) +
      geom_line(alpha = 0.05, linewidth = 0.1) + # Transparent lines in the plot
      scale_colour_manual("", values = custom.colors) # Custom colors
    
  } else {
    p <- ggplot(
      data = data,
      aes_string(
        x = x, 
        y = y, 
        group = group
      )
    ) +
      geom_line(alpha = 0.05, linewidth = 0.1, color = "blue")
  }
  
  p <- p +
    scale_y_continuous(labels = label_number(accuracy = accuracy.decimals)) +
    labs(
      x = xlab,
      y = ylab
    ) +
    guides(
      color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)) # Fully opaque legend lines
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = font.size, face = "bold"),
      text = element_text(size = font.size, family = "Arial"),
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
      aspect.ratio=1
    )
  
  if (!missing(title)){
    p <- p + ggtitle(title)
  }
  
  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  
  if (hide.x.axis) {
    p <- p + theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(colour = "white"),
      axis.title.x = element_text(colour = "white")
    )
  }
  
  if (!missing(splitby)){
    p <- p + facet_wrap(as.formula(paste0("~",splitby)))
  }
  
  # p <- wrap_elements(full = p)
}

plt.bootstrap_regressions.V2 <- function(
    data,
    peaks = NULL,
    x,
    y,
    color = NULL,
    group = "Line",
    splitby,
    accuracy.decimals = 0.01,
    font.size = 24,
    title,
    xlab = NULL,
    ylab = NULL,
    legend = FALSE,
    hide.x.axis = FALSE
){
  
  if (!is.null(color)){
    data[,color] <- as.factor(data[,color])
    color.levels <- levels(data[,color])
    
    custom.colors <- c()
    for (i in color.levels){ 
      if (grepl("Left",i,fixed=TRUE)){ 
        custom.colors <- c(custom.colors,"#E41A1C")
      } else { 
        custom.colors <- c(custom.colors,"#377EB8")
      }
    }
    
    p <- ggplot(
      data = data,
      aes_string(
        x = x, 
        y = y, 
        group = group, 
        color = color
      )
    ) +
      geom_line(alpha = 0.05, linewidth = 0.1) + # Transparent lines in the plot
      scale_colour_manual("", values = custom.colors) # Custom colors
    
  } else {
    p <- ggplot(
      data = data,
      aes_string(
        x = x, 
        y = y, 
        group = group
      )
    ) +
      geom_line(alpha = 0.05, linewidth = 0.1, color = "blue")
  }
  
  p <- p +
    scale_y_continuous(labels = label_number(accuracy = accuracy.decimals)) +
    labs(
      x = xlab,
      y = ylab
    ) +
    guides(
      color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)) # Fully opaque legend lines
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = font.size, face = "bold"),
      text = element_text(size = font.size, family = "Arial"),
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
      aspect.ratio=1
    )
  
  if(!is.null(peaks)){
    # just in case delete any point below the min age (6.37)
    peaks <- peaks[!(peaks$x < 6.37),]
    # those points beyond the maximum age (81.59)
    peaks <- peaks[!(peaks$x > 81.59),]
    
    x_intercepts <- peaks %>% 
      group_by(Gender) %>% 
      summarise(x_mean = mean(x))
    x_intercepts <- as.data.frame(
      x_intercepts
    )
    
    p <- p +
      geom_point(inherit.aes = FALSE,
                 aes(x=x,y=y),
                 data=peaks,
                 size=0.5,
                 alpha=0.15,
                 color = "green") +
      geom_vline(
        inherit.aes = FALSE,
        aes(xintercept = x_mean),
        data = x_intercepts,
        linetype = "dashed",
        color = "grey22",
        size = 0.5
      )
  }
  
  if (!missing(title)){
    p <- p + ggtitle(title)
  }
  
  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  
  if (hide.x.axis) {
    p <- p + theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(colour = "white"),
      axis.title.x = element_text(colour = "white")
    )
  }
  
  if (!missing(splitby)){
    p <- p + facet_wrap(as.formula(paste0("~",splitby)))
  }
  
  # p <- wrap_elements(full = p)
}


generate.regression.formula <- function(
    y,
    x,
    order,
    raw = TRUE
){
  model <- as.formula(
    paste(
      y,
      paste0(
        "poly(",
        x,
        ",",
        order,
        ",raw = ",raw,")"
      ),
      sep = " ~ "
    )
  )
  return(model)
}

get.residuals.fitted <- function(
    data,
    model,
    residuals.colname = "residuals",
    standardized = FALSE
    ) {
  
  if(standardized){
    # linear model standardized residuals
    data[,paste(residuals.colname,"standardized",sep = "_")] <- rstandard(model)
  } else { # get the raw residuals
    data[,paste(residuals.colname,"raw",sep = "_")] <- residuals(model)
  }
  data$fitted <- fitted(model)
  return(data)
}

plt.residuals <- function(
    ATLAS,
    y,
    x,
    models = c( 
      1, # Left-Female 
      1, # Left-Male
      1, # Right-Female
      1  # Right-Male
    ),
    standardized = TRUE,
    detailed.title = FALSE,
    sub.titles = TRUE,
    hide.ylabels = FALSE,
    titles = NULL
    ){
  
  if (is.null(titles)){
    var.title <- y
    if (y == "Whole_thalamus") {
      var.title <- "Whole thalamus"
    }
  } else {
    var.title.left.females <- titles[1]
    var.title.left.males <- titles[2]
    var.title.right.females <- titles[3]
    var.title.right.males <- titles[4]
  }
  
  # Extract dataframes for each Hemisphere-Gender combination
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  left.females <- subset(ATLAS, Hemisphere=="Left" & Gender == "Female")
  right.females <- subset(ATLAS, Hemisphere=="Right" & Gender == "Female")
  left.males <- subset(ATLAS, Hemisphere=="Left" & Gender == "Male")
  right.males <- subset(ATLAS, Hemisphere=="Right" & Gender == "Male")
  
  # Get the models for each Hemisphere-Gender combination
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  model.left.females <- glm(
    generate.regression.formula(y,x,models[1]),
    data = left.females
  )
  model.right.females <- glm(
    generate.regression.formula(y,x,models[3]),
    data = right.females
  )
  model.left.males <- glm(
    generate.regression.formula(y,x,models[2]),
    data = left.males
  )
  model.right.males <- glm(
    generate.regression.formula(y,x,models[4]),
    data = right.males
  )
  
  # Get the residuals for each Hemisphere-Gender combination
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  residuals.left.females <- get.residuals.fitted(
    left.females,
    model.left.females,
    standardized = TRUE
  )
  
  residuals.right.females <- get.residuals.fitted(
    right.females,
    model.right.females,
    standardized = TRUE
  )
  
  residuals.left.males <- get.residuals.fitted(
    left.males,
    model.left.males,
    standardized = TRUE
  )
  
  residuals.right.males <- get.residuals.fitted(
    right.males,
    model.right.males,
    standardized = TRUE
  )
  
  # Get the diagnostic plots for each Hemisphere-Gender combination
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(standardized){
    residuals.colname <- "residuals_standardized"
    residuals.type = "Std. Residuals"
  } else{
    residuals.colname <- "residuals_raw"
    residuals.type = "Std. Residuals"
  }
  
  if (detailed.title & is.null(titles)) {
    var.title.left.females <- paste(var.title, "Left-Female", sep = " | ")
    var.title.right.females <- paste(var.title, "Right-Female", sep = " | ")
    var.title.left.males <- paste(var.title, "Left-Male", sep = " | ")
    var.title.right.males <- paste(var.title, "Right-Male", sep = " | ")
  } else if (detailed.title & !is.null(titles)){
    var.title.left.females <- paste(var.title.left.females, "Left-Female", sep = " | ")
    var.title.right.females <- paste(var.title.right.females, "Right-Female", sep = " | ")
    var.title.left.males <- paste(var.title.left.males, "Left-Male", sep = " | ")
    var.title.right.males <- paste(var.title.right.males, "Right-Male", sep = " | ")
  } else if (is.null(titles)){
    var.title.left.females <- var.title
    var.title.right.females <- var.title
    var.title.left.males <- var.title
    var.title.right.males <- var.title
  }
  
  plt.residuals.left.females <- plt.diagnostic(
    residuals.left.females,
    residuals = residuals.colname,
    residuals.type = residuals.type,
    fitted = "fitted",
    title = var.title.left.females,
    sub.titles = sub.titles,
    hide.ylabels = hide.ylabels
  )
  
  plt.residuals.right.females <- plt.diagnostic(
    residuals.right.females,
    residuals = residuals.colname,
    residuals.type = residuals.type,
    fitted = "fitted",
    title = var.title.right.females,
    sub.titles = sub.titles,
    hide.ylabels = hide.ylabels
  )
  
  plt.residuals.left.males <- plt.diagnostic(
    residuals.left.males,
    residuals = residuals.colname,
    residuals.type = residuals.type,
    fitted = "fitted",
    title = var.title.left.males,
    sub.titles = sub.titles,
    hide.ylabels = hide.ylabels
  )
  
  plt.residuals.right.males <- plt.diagnostic(
    residuals.right.males,
    residuals = residuals.colname,
    residuals.type = residuals.type,
    fitted = "fitted",
    title = var.title.right.males,
    sub.titles = sub.titles,
    hide.ylabels = hide.ylabels
  )
  
  p <- 
    plt.residuals.left.females |
    plt.residuals.right.females |
    plt.residuals.left.males |
    plt.residuals.right.males
  
  return(
    list(
      p = p,
      left.females = plt.residuals.left.females,
      right.females = plt.residuals.right.females,
      left.males = plt.residuals.left.males,
      right.males = plt.residuals.right.males
    )
  )
}

plt.residuals.gender <- function(
    ATLAS,
    y,
    x,
    models = c( 
      1, # Female 
      1  # Male
    ),
    standardized = TRUE,
    detailed.title = FALSE,
    sub.titles = TRUE,
    hide.ylabels = FALSE,
    titles = NULL
){
  
  # var.title <- y
  # if (y == "Whole_thalamus") {
  #   var.title <- "Whole thalamus"
  # }
  
  if (is.null(titles)){
    var.title <- y
    if (y == "Whole_thalamus") {
      var.title <- "Whole thalamus"
    }
  } else {
    var.title.females <- titles[1]
    var.title.males <- titles[2]
  }
  
  # Extract dataframes for each Gender
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  females <- subset(ATLAS, Gender == "Female")
  males <- subset(ATLAS, Gender == "Male")
  
  # Get the models for each Gender
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  model.females <- glm(
    generate.regression.formula(y,x,models[1]),
    data = females
  )
  model.males <- glm(
    generate.regression.formula(y,x,models[2]),
    data = males
  )
  # Get the residuals for each Gender
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  residuals.females <- get.residuals.fitted(
    females,
    model.females,
    standardized = TRUE
  )
  residuals.males <- get.residuals.fitted(
    males,
    model.males,
    standardized = TRUE
  )
  
  # Get the diagnostic plots for each Hemisphere-Gender combination
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(standardized){
    residuals.colname <- "residuals_standardized"
    residuals.type = "Std. Residuals"
  } else{
    residuals.colname <- "residuals_raw"
    residuals.type = "Std. Residuals"
  }
  
  if (detailed.title & is.null(titles)) {
    var.title.females <- paste(var.title, "Female", sep = " | ")
    var.title.males <- paste(var.title, "Male", sep = " | ")
  } else if (detailed.title & !is.null(titles)){
    var.title.females <- paste(var.title.females, "Female", sep = " | ")
    var.title.males <- paste(var.title.males, "Male", sep = " | ")
  } else if (is.null(titles)){
    var.title.females <- var.title
    var.title.males <- var.title
  }
  
  plt.residuals.females <- plt.diagnostic(
    residuals.females,
    residuals = residuals.colname,
    residuals.type = residuals.type,
    fitted = "fitted",
    title = var.title.females,
    sub.titles = sub.titles,
    hide.ylabels = hide.ylabels
  )
  
  plt.residuals.males <- plt.diagnostic(
    residuals.males,
    residuals = residuals.colname,
    residuals.type = residuals.type,
    fitted = "fitted",
    title = var.title.males,
    sub.titles = sub.titles,
    hide.ylabels = hide.ylabels
  )
  
  p <- 
    plt.residuals.females |
    plt.residuals.males
  
  return(
    list(
      p = p,
      females = plt.residuals.females,
      males = plt.residuals.males
    )
  )
}

anova.results <- function(
    data,
    dependent.variable = "Whole_thalamus",
    fixed_effects = "AGE * Hemisphere * Gender",
    random_effects = NULL # "(1 | ID)"
    ){
  
  if (is.null(random_effects)) {
    eq <- paste(
      dependent.variable,
      fixed_effects,
      sep="~"
    )
  } else {
    eq <- as.formula(
      paste(
        paste(
          dependent.variable,
          fixed_effects,
          sep="~"
        ), 
        random_effects, 
        sep = " + "
      )
    )
  }
  
  print(eq)
  
  # MODELS
  model.lmer <- lmer(
    eq,
    data = data
  )
  
  # lme <- lme(
  #   eq,
  #   random = random_effects, # ~1+Hemisphere | ID
  #   method = "REML",
  #   data = ATLAS_corrected_ICV
  # )
  # aov.lme <- anova(lme, type = "marginal")
  # aov.lme.table <- data.frame(aov.lme)
  # aov.lme.table <- cbind(
  #   effects = row.names(aov.lme.table),
  #   aov.lme.table
  # )
  # row.names(aov.lme.table) <- NULL 
  # lme2 <- lme(
  #   model, 
  #   random = ~ 1 |ID/Hemisphere, 
  #   method = "REML", 
  #   data = ATLAS_corrected_ICV
  # )
  # 
  # lme3 <- lme(
  #   model, 
  #   random = ~ 1 + Hemisphere | ID, 
  #   method = "REML", 
  #   data = ATLAS_corrected_ICV
  # )
  # Anova(lme2, type="III")
  # Anova(lme3, type="III")
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # lm <- lm(
  #   as.formula(fixed_effects),
  #   data = ATLAS_corrected_ICV
  # )
  # aov.lm <- Anova(lm, type="III")
  # aov.lm.table <- data.frame(aov.lm)
  # aov.lm.table <- cbind(
  #   effects = row.names(aov.lm.table),
  #   aov.lm.table
  # )
  # row.names(aov.lm.table) <- NULL
  
  # ANOVAS
  # LMER
  aov.lmer <- anova(model.lmer, type="III")
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
      p_value_stars(aov.lmer.table[r,"Pr..F."])
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
    p_value_stars
  )
  
  names(aov.lmer.table2)[names(aov.lmer.table2) == "effects"] <- "Effects"
  names(aov.lmer.table2)[names(aov.lmer.table2) == "F.value"] <- "F-value"
  names(aov.lmer.table2)[names(aov.lmer.table2) == "Pr..F."] <- "P-value"
  
  print(aov.lmer.table2)
  
  aov.lmer.table2 <- flextable(data = aov.lmer.table2)
  aov.lmer.table2 <- aov.lmer.table2 %>%
    width(j = 2,
          width = 1.75) %>%
    width(j = 3,
          width = 1)
  
  # # LME
  # aov.lme <- anova(lme, type = "marginal")
  # aov.lme.table <- data.frame(aov.lme)
  # aov.lme.table <- cbind(
  #   effects = row.names(aov.lme.table),
  #   aov.lme.table
  # )
  # row.names(aov.lme.table) <- NULL
  # pval.lme.agebyhem <- aov.lme.table[aov.lme.table$effects == "AGE:Hemisphere","p.value"]
  # 
  # # LM
  # aov.lm <- Anova(lm, type="III")
  # aov.lm.table <- data.frame(aov.lm)
  # aov.lm.table <- cbind(
  #   effects = row.names(aov.lm.table),
  #   aov.lm.table
  # )
  # row.names(aov.lm.table) <- NULL
  # pval.lm.agebyhem <- aov.lm.table[aov.lm.table$effects == "AGE:Hemisphere","Pr..F."]
  
  return(
    list(
      anova.table = aov.lmer.table,
      pval.agebyhem = pval.lmer.agebyhem,
      anova.stats = t,
      anova.stats.table = aov.lmer.table2
    )
  )
  
}

cohensd.hem.gen <- function(
  data,
  dependent.variable
  ){
  
  cohensd.hemispheres <- rstatix::cohens_d(
    data = data, 
    as.formula(
      paste(
        dependent.variable,
        'Hemisphere',
        sep="~"
      )
    ), 
    paired = TRUE
  )
  
  cohensd.genders <- rstatix::cohens_d(
    data = data, 
    as.formula(
      paste(
        dependent.variable,
        'Gender',
        sep="~"
      )
    ), 
    paired = FALSE
  )
  
  return(
    list(
      cohensd.hemispheres = cohensd.hemispheres,
      cohensd.genders = cohensd.genders
    )
  )
}

posthoc.hem.gen <- function(
    data,
    dependent.variable,
    parametric = FALSE
    ){
  
  data <- data[,c("ID", dependent.variable, "Hemisphere", "Gender")]
  
  paired_data <- data %>% 
    pivot_wider(names_from = Hemisphere, values_from = dependent.variable)
  
  unpaired_data <- subset(data, Hemisphere == "Left") # take only the half to compare
  unpaired_data <- subset(unpaired_data, select = -Hemisphere)
  
  if (parametric){
    # PARAMETRIC - T-TEST
    ##############################################################################
    # paired t-test
    # t_test_result <- t.test(
    #   paired_data$Left, 
    #   paired_data$Right, 
    #   paired = TRUE
    # )
    # print(t_test_result)
  } else {
    # NON PARAMETRIC - WILCOXON
    ##############################################################################
    # paired Wilcoxon Signed-Rank Test
    wilcox_test.hemisphere <- wilcox.test(
      paired_data$Left, 
      paired_data$Right, 
      paired = TRUE
    )
    # Extract Z value from the test output (can be approximated from test statistic)
    Z_signed_rank <- qnorm(wilcox_test.hemisphere$p.value / 2)  # Approximate Z-value
    # Calculate effect size r
    r_signed_rank <- Z_signed_rank / sqrt(length(paired_data$Left))  # Effect size r
    
    n_pairs <- length(paired_data$Left)
    eta_squared_signed_rank <- wilcox_test.hemisphere$statistic / (n_pairs * (n_pairs - 1))
    
    r_signed_rank2 <- rstatix::wilcox_effsize(
      data,
      as.formula(paste(dependent.variable,"Hemisphere", sep = "~")),
      paired = TRUE
    )
    
    W <- wilcox_test.hemisphere$statistic  # Extract W value
    n <- length(paired_data$Left)  # Number of paired observations
    mu_W <- n * (n + 1) / 4
    sigma_W <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
    Z_manual <- (W - mu_W) / sigma_W
    
    # print(wilcox_test.hemisphere)
    # cat(sprintf("r Signed-Rank: %7.3f \n", round(r_signed_rank, digits = 3)))
    # cat(sprintf("eta_squared Signed-rank: %7.3f \n", round(eta_squared_signed_rank, digits = 3)))
    # print(r_signed_rank2)
    
    posthoc.hem <- paste0(
      "W = ",
      wilcox_test.hemisphere$statistic, 
      ", ",
      "Z = ",
      round(Z_signed_rank, digits = 2),
      ", ",
      "Z_manual = ",
      round(Z_manual, digits = 2),
      ", ",
      p_value_stars(wilcox_test.hemisphere$p.value),
      ", r = ", 
      round(r_signed_rank2$effsize, digits = 2)
    )
    
    
    # non-paired Wilcoxon-Mann-Whitney (WMW) U Test
    mann_whitney.gender <- wilcox.test(
      as.formula(paste(dependent.variable,"Gender", sep = "~")), 
      data = unpaired_data, 
      alternative = "two.sided"
    )
    
    # Z value from the test output (will be provided in the output)
    Z_mann_whitney <- qnorm(mann_whitney.gender$p.value / 2)  # Standardized Z-value from p-value
    # Calculate effect size r
    nfemales <- nrow(subset(unpaired_data, Gender == "Female"))
    nmales <- nrow(subset(unpaired_data, Gender == "Male"))
    N <- nfemales + nmales
    r_mann_whitney <- Z_mann_whitney / sqrt(N)  # Effect size r
    # Calculate eta-squared (η²)
    eta_squared_mann_whitney <- mann_whitney.gender$statistic / (nfemales * nmales)
    
    r_mann_whitney2 <- rstatix::wilcox_effsize(
      unpaired_data,
      as.formula(paste(dependent.variable,"Gender", sep = "~")),
      alternative = "two.sided",
      paired = FALSE
    )
    
    U <- mann_whitney.gender$statistic
    n1 <- sum(unpaired_data$Gender == "Male")
    n2 <- sum(unpaired_data$Gender == "Female")
    mean_U <- (n1 * n2) / 2
    sd_U <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)
    Z_manual <- (U - mean_U) / sd_U
    
    
    # print(mann_whitney.gender)
    # cat(sprintf("r Mann-Whitney: %7.3f \n", round(r_mann_whitney, digits = 3)))
    # cat(sprintf("eta_squared Mann-Whitney: %7.3f \n", round(eta_squared_mann_whitney, digits = 3)))
    # print(r_mann_whitney2)
    
    posthoc.gen <- paste0(
      "W = ",
      mann_whitney.gender$statistic, 
      ", ",
      "Z = ",
      round(Z_mann_whitney, digits = 2),
      ", ",
      "Z_manual = ",
      round(Z_manual, digits = 2),
      ", ",
      p_value_stars(mann_whitney.gender$p.value),
      ", r = ", 
      round(r_mann_whitney2$effsize, digits = 2)
    )
  }
  
  return(
    list(
      posthoc.hem = posthoc.hem,
      posthoc.gen = posthoc.gen
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
      sprintf("P=.%03d ns", round(p * 1000))
      # paste0(
      #   "P = ", 
      #   round(p,digits = 3),
      #   " ns"
      # )
    )  # Not significant
  }
}

full_varcorrection <- function(ATLAS, y){
  ATLAS[ATLAS$Hemisphere == "Left" &
          ATLAS$Gender == "Female",paste0("corrected_",y)] <- 
    var_correction(
      ATLAS[ATLAS$Hemisphere == "Left" &
                            ATLAS$Gender == "Female",y], 
      ATLAS[ATLAS$Hemisphere == "Left" &
                            ATLAS$Gender == "Female","ICV"]
    )
  
  ATLAS[ATLAS$Hemisphere == "Right" &
                        ATLAS$Gender == "Female",paste0("corrected_",y)] <- 
    var_correction(
      ATLAS[ATLAS$Hemisphere == "Right" &
                            ATLAS$Gender == "Female",y], 
      ATLAS[ATLAS$Hemisphere == "Right" &
                            ATLAS$Gender == "Female","ICV"]
    )
  
  ATLAS[ATLAS$Hemisphere == "Left" &
                        ATLAS$Gender == "Male",paste0("corrected_",y)] <- 
    var_correction(
      ATLAS[ATLAS$Hemisphere == "Left" &
                            ATLAS$Gender == "Male",y], 
      ATLAS[ATLAS$Hemisphere == "Left" &
                            ATLAS$Gender == "Male","ICV"]
    )
  
  ATLAS[ATLAS$Hemisphere == "Right" &
                        ATLAS$Gender == "Male",paste0("corrected_",y)] <- 
    var_correction(
      ATLAS[ATLAS$Hemisphere == "Right" &
                            ATLAS$Gender == "Male",y], 
      ATLAS[ATLAS$Hemisphere == "Right" &
              ATLAS$Gender == "Male","ICV"]
    )
  
  return(ATLAS)
}

print_equation <- function(ATLAS,sex,hem,y,x="AGE",order,digits = 7) {
  fit <- lm(
    data = subset(
      ATLAS,
      Hemisphere == hem & Gender== sex
    ),
    formula = as.formula(paste0(y,'~poly(',x,',',order,',raw = TRUE)'))
  ) 
  
  coefs <- coef(fit)
  terms <- names(coefs)
  eq <- paste0(y,", ",sex,",",hem,": y = ", round(coefs[1], digits))
  for (i in 2:length(coefs)) {
    sign <- ifelse(coefs[i] >= 0, " + ", " - ")
    eq <- paste0(eq, sign, abs(round(coefs[i], digits)), "*", terms[i])
  }
  eq
}
