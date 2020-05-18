
estatura <- function(x, method, lateral){
  
  
  ### TrotterGleser ###
  
  if(method == "TrotterGleser" && lateral == "I"){
    
    
    data <- subset(x, Lado == "I")
    
    H_max_LMH <- (2.89 * (data$LMH/10) + 77.47 + 4.54)
    H_min_LMH <- (2.89 * (data$LMH/10) + 77.47 - 4.54)
    
    H_max_LMR <- (3.73 * (data$LMR/10) + 80.62 + 4.59)
    H_min_LMR <- (3.73 * (data$LMR/10) + 80.62 - 4.59)
    
    H_max_LMC <- (3.64 * (data$LMC/10) + 76.14 + 4.57)
    H_min_LMC <- (3.64 * (data$LMC/10) + 76.14 - 4.57)
    
    H_max_LMF <- (2.30 * (data$LMF/10) + 65.82 + 3.97)
    H_min_LMF <- (2.30 * (data$LMF/10) + 65.82 - 3.97)
    
    H_max_LT <- (2.43 * (data$LT/10) + 80.98 + 3.95)
    H_min_LT <- (2.43 * (data$LT/10) + 80.98 - 3.95)
    
    H_max_LMP <- (2.59 * (data$LT/10) + 75.37 + 3.83)
    H_min_LMP <- (2.59 * (data$LT/10) + 75.37 - 3.83)
    
    IND <- data[,1]
    
    datos <- cbind(IND,H_max_LMH, H_min_LMH, H_max_LMR, H_min_LMR, H_max_LMC, H_min_LMC, H_max_LMF, H_min_LMF, H_max_LT, H_min_LT,  H_max_LMP, H_min_LMP)
    
  }
  
  if(method == "TrotterGleser" && lateral == "D"){
    
    data <- subset(x, Lado == "D")
    
    H_max_LMH <- (2.88 * (data$LMH/10) + 77.70 + 4.61)
    H_min_LMH <- (2.88 * (data$LMH/10) + 77.70 - 4.61)
    
    H_max_LMR <- (3.77 * (data$LMR/10) + 79.13 + 4.66)
    H_min_LMR <- (3.77 * (data$LMR/10) + 79.13 - 4.66)
    
    H_max_LMC <- (3.59 * (data$LMC/10) + 76.95 + 4.71)
    H_min_LMC <- (3.59 * (data$LMC/10) + 76.95 - 4.71)
    
    H_max_LMF <- (2.25 * (data$LMF/10) + 68.40 + 4.04)
    H_min_LMF <- (2.25 * (data$LMF/10) + 68.40 - 4.04)
    
    H_max_LT <- (2.40 * (data$LT/10) + 82.24 + 3.97)
    H_min_LT <- (2.40 * (data$LT/10) + 82.24 - 3.97)
    
    H_max_LMP <- (2.57 * (data$LT/10) + 76.13 + 3.86)
    H_min_LMP <- (2.57 * (data$LT/10) + 76.13 - 3.86)
    
    IND <- data[,1]
    
    datos <- cbind(IND,H_max_LMH, H_min_LMH, H_max_LMR, H_min_LMR, H_max_LMC, H_min_LMC, H_max_LMF, H_min_LMF, H_max_LT, H_min_LT,  H_max_LMP, H_min_LMP)
    
  }
  
  ### Pearson ###
  
  if(method == "Pearson" && lateral == "I"){
    
    
    data <- subset(x, Lado == "I")
    
    H_max_LMF <- 81.306 + 1.880 * data$LMF / 10
    H_max_LMH <- 70.641 + 2.894 * data$LMH / 10
    H_max_LT <- 78.664 + 2.376 * data$LT / 10
    H_max_LMR <- 85.925 + 3.217 * data$LMR / 10
    
    IND <- data[,1]
    
    datos <- cbind(IND, H_max_LMF, H_max_LMH, H_max_LT, H_max_LMR)
    
  }
  
  
  if(method == "Pearson" && lateral == "D"){
    
    
    data <- subset(x, Lado == "D")
    
    H_max_LMF <- 81.306 + 1.880 * data$LMF / 10
    H_max_LMH <- 70.641 + 2.894 * data$LMH / 10
    H_max_LT <- 78.664 + 2.376 * data$LT / 10
    H_max_LMR <- 85.925 + 3.217 * data$LMR / 10
    
    IND <- data[,1]
    
    datos <- cbind(IND, H_max_LMF, H_max_LMH, H_max_LT, H_max_LMR)
    
  }
  
  
  ### Telkka ###
  
  if(method == "Telkka" && lateral == "I"){
    
    
    data <- subset(x, Lado == "I")
    
    H_max_LMH <- (169.4 + 2.8 * ((data$LMH/10) - 32.9) + 5.0)
    H_min_LMH <- (169.4 + 2.8 * ((data$LMH/10) - 32.9) - 5.0)
    H_max_LFR <- (169.4 + 3.4 * ((data$LFR/10) - 22.7) + 5.0)
    H_min_LFR <- (169.4 + 3.4 * ((data$LFR/10) - 22.7) - 5.0)
    H_max_LFC <- (169.4 + 3.2 * ((data$LFC/10) - 23.1) + 5.2)
    H_min_LFC <- (169.4 + 3.2 * ((data$LFC/10) - 23.1) - 5.2)
    H_max_LMF <- (169.4 + 2.1 * ((data$LMF/10) - 45.5) + 4.9)
    H_min_LMF <- (169.4 + 2.1 * ((data$LMF/10) - 45.5) - 4.9)
    H_max_LT <- (169.4 + 2.1 * ((data$LT/10) - 36.2) + 4.6)
    H_min_LT <- (169.4 + 2.1 * ((data$LT/10) - 36.2) - 4.6)
    H_max_LMP <- (169.4 + 2.5 * ((data$LMP/10) - 36.1) + 4.4)
    H_min_LMP <- (169.4 + 2.5 * ((data$LMP/10) - 36.1) - 4.4)
    
    IND <- data[,1]
    
    datos <- cbind(IND, H_max_LMH, H_min_LMH, H_max_LFR, H_min_LFR, H_max_LFC, H_min_LFC, H_max_LMF, H_min_LMF, H_max_LT, H_min_LT, H_max_LMP, H_min_LMP)
    
  }
  
  
  if(method == "Telkka" && lateral == "D"){
    
    
    data <- subset(x, Lado == "D")
    
    H_max_LMH <- (169.4 + 2.8 * ((data$LMH/10) - 32.9) + 5.0)
    H_min_LMH <- (169.4 + 2.8 * ((data$LMH/10) - 32.9) - 5.0)
    H_max_LFR <- (169.4 + 3.4 * ((data$LFR/10) - 22.7) + 5.0)
    H_min_LFR <- (169.4 + 3.4 * ((data$LFR/10) - 22.7) - 5.0)
    H_max_LFC <- (169.4 + 3.2 * ((data$LFC/10) - 23.1) + 5.2)
    H_min_LFC <- (169.4 + 3.2 * ((data$LFC/10) - 23.1) - 5.2)
    H_max_LMF <- (169.4 + 2.1 * ((data$LMF/10) - 45.5) + 4.9)
    H_min_LMF <- (169.4 + 2.1 * ((data$LMF/10) - 45.5) - 4.9)
    H_max_LT <- (169.4 + 2.1 * ((data$LT/10) - 36.2) + 4.6)
    H_min_LT <- (169.4 + 2.1 * ((data$LT/10) - 36.2) - 4.6)
    H_max_LMP <- (169.4 + 2.5 * ((data$LMP/10) - 36.1) + 4.4)
    H_min_LMP <- (169.4 + 2.5 * ((data$LMP/10) - 36.1) - 4.4)
    
    IND <- data[,1]
    
    datos <- cbind(IND, H_max_LMH, H_min_LMH, H_max_LFR, H_min_LFR, H_max_LFC, H_min_LFC, H_max_LMF, H_min_LMF, H_max_LT, H_min_LT, H_max_LMP, H_min_LMP)
    
  }
  
  ### Mendoza ###
  
  if(method == "Mendoza" && lateral == "I"){
    
    
    data <- subset(x, Lado == "I")
    
    H_max_LMH <- (59.41 + 0.3269 * data$LMH) + 8.44
    H_min_LMH <- (59.41 + 0.3269 * data$LMH) - 8.44
    H_max_LMF <- (46.89 + 0.2657 * data$LMF) + 6.96
    H_min_LMF <- (46.89 + 0.2657 * data$LMF) - 6.96
    H_max_LFF <- (47.18 + 0.2663 * data$LFF) + 6.90
    H_min_LFF <- (47.18 + 0.2663 * data$LFF) - 6.90
    
    IND <- data[,1]
    
    datos <- cbind(IND, H_max_LMH, H_min_LMH, H_max_LMF, H_min_LMF, H_max_LFF, H_min_LFF)
    
  }
  
  
  if(method == "Mendoza" && lateral == "D"){
    
    
    data <- subset(x, Lado == "D")
    
    H_max_LMH <- (59.41 + 0.3269 * data$LMH) + 8.44
    H_min_LMH <- (59.41 + 0.3269 * data$LMH) - 8.44
    H_max_LMF <- (46.89 + 0.2657 * data$LMF) + 6.96
    H_min_LMF <- (46.89 + 0.2657 * data$LMF) - 6.96
    H_max_LFF <- (47.18 + 0.2663 * data$LFF) + 6.90
    H_min_LFF <- (47.18 + 0.2663 * data$LFF) - 6.90
    
    IND <- data[,1]
    
    datos <- cbind(IND, H_max_LMH, H_min_LMH, H_max_LMF, H_min_LMF, H_max_LFF, H_min_LFF)
    
    
  }
  
  datos
  
}