# 10/8/2018

# Below are the two functions, one is for model fitting: fit_logistic. And another is called join_selected: to combine the selected datasets.
# The fit_logistic generates two files: One file is the list pvalues as well as beta from single variable logistic fitting "filename_betas.csv", another is the list of variables from the multivariate logistic fitting cysles with "filename_model_selection.csv". 
# join_selected generates a single combined file called "IgAG_combined.csv".


setwd(choose.dir())

library(tools)
library(magrittr)

fit_logistic <- function(fileName){
  dat1 <- read.csv(fileName, header = T)
  dat1$ID <- gsub("-", "_", dat1$ID)
  dat1$Group = as.factor(dat1$Group - 1)
  
  # glm fitting
  betas <- c()
  for (i in (1: (length(colnames(dat1)) - 2))){
    m1 <- glm(dat1$Group ~ dat1[, i+2], family = binomial)
    s1 <- summary(m1)
    betas <- rbind(betas, c(colnames(dat1)[i+2], s1$coef[2], s1$coef[8]))
  }
  
  betas <- as.data.frame(betas)
  colnames(betas) <- c('variable', 'beta', 'pvalue')
  write.csv(betas, paste0(file_path_sans_ext(fileName), "_betas.csv"))
  
  # model selection
  betas <- betas %>% mutate(variable = as.character(variable), beta = as.numeric(as.character(beta)), 
                  pvalue = as.numeric(as.character(pvalue)))
  
  betas_selected <- betas[betas$pvalue < 0.05,]
  dat2 <- dat1[, c('ID', 'Group', betas_selected$variable)]
  
  d1 <- dat2[,-1]
  xnames <- setdiff(colnames(d1), 'Group')
  model_select <- c()
  min_AICs <- c()
  
  for (k in (1: (length(xnames) - 2))){
    looped <- setNames(data.frame(matrix(NA, ncol = 4)), c('cycle', 'loop','variable', 'AIC'))
    AICs <- c()
    m = 1
    
    for (j in xnames){
      d2 <- d1
      keeps <- c("Group", setdiff(xnames, j))
      d3 <- d2[, keeps]
      m2 <- glm(Group ~ ., data = d3, family = binomial)
      AICs <- c(AICs, AIC(m2))
      
      looped[j,1] <- paste0('cycle_', k)
      looped[j,2] <- paste0('loop_', m)
      looped[j,3] <- j
      looped[j,4] <- AIC(m2)
      m = m + 1
    }
    model_select <- rbind(model_select, na.omit(looped))
    
    # Find the min AIC corresponding variable and kick it out to update the dataset for the next run.
    drop_ind <-  which.min(AICs)
    d1 <- d1[, -(drop_ind + 1)]
    xnames <- xnames[-drop_ind]
  }
  write.csv(model_select, paste0(file_path_sans_ext(fileName), "_model_selection.csv"))
}

#####################################################

join_selected <- function(fileName1, fileName2){
  dat1 <- read.csv(fileName1, header = T)
  dat1$ID <- gsub("-", "_", dat1$ID)
  dat1$Group = as.factor(dat1$Group - 1)
  
  dat2 <- read.csv(fileName2, header = T)
  dat2$ID <- gsub("-", "_", dat2$ID)
  dat2$Group = as.factor(dat2$Group - 1)
  
  #glm
  betas1 <- c()
  for (i in (1: (length(colnames(dat1)) - 2))){
    m1 <- glm(dat1$Group ~ dat1[, i+2], family = binomial)
    s1 <- summary(m1)
    betas1 <- rbind(betas1, c(colnames(dat1)[i+2], s1$coef[2], s1$coef[8]))
  }
  
  betas2 <- c()
  for (i in (1: (length(colnames(dat2)) - 2))){
    m2 <- glm(dat2$Group ~ dat2[, i+2], family = binomial)
    s2 <- summary(m2)
    betas2 <- rbind(betas2, c(colnames(dat2)[i+2], s2$coef[2], s2$coef[8]))
  }
  
  betas1 <- as.data.frame(betas1)
  colnames(betas1) <- c('variable', 'beta', 'pvalue')
  betas1 <- betas1 %>% mutate(variable = as.character(variable), beta = as.numeric(as.character(beta)), 
                              pvalue = as.numeric(as.character(pvalue)))
 
  betas1_selected <- betas1[betas1$pvalue < 0.05,]
  
  betas2 <- as.data.frame(betas2)
  colnames(betas2) <- c('variable', 'beta', 'pvalue')
  betas2 <- betas2 %>% mutate(variable = as.character(variable), beta = as.numeric(as.character(beta)), 
                              pvalue = as.numeric(as.character(pvalue)))
  
  betas2_selected <- betas2[betas2$pvalue < 0.05,]
  
  xnames1 <- betas1_selected$variable
  xnames2 <- betas2_selected$variable
  
  dat3 <- merge(dat1, dat2, by = c("ID", "Group"))
  dat4 <- dat3[, c("ID","Group", xnames1, xnames2)]
  return (dat4)
}

write.csv(join_selected("file1.csv", "file2.csv"), "your_combined.csv")



############################    
fileName = "your file.csv"
fit_logistic(fileName)

############################
fileName = "your_combined.csv"
fit_logistic(fileName)



