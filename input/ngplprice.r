#########
######### NEMS Production Code for propane and ethane
######### price projections
######### Authors: Giovanni Petris and Janice Lent
######### Last modified September, 2021
#########
######### NOTE:  We assume that the variable "RunName" has been
######### defined in the command that called R and that the
######### working directory has been set to the directory 
######### where data files for the run will be stored.
#########

library(dlm)
## library(dynlm)

######
###### Constants
######
## now is the last year of historical data available
now <- 2022

## PFloor is the minimum value of the propane price projections
## for the first projection year
## If no floor is needed, set PFloor equal to 0.
PFloor <- 0

##### Explanatory note:  An integer n may be expressed as nL 
#####  (e.g., 4L), to save memory.

#####  Constants below may have been changed for sensitivity 
#####  analysis.  Original values are in parentheses.

### The constants delprop1 and delprop2 set the delta values for
### propane for two of the component models.  Default values are
### delprop1 = 0.50 and delprop2 = 0.95
delprop1 <- 0.01 * 0.01
delprop2 <- 0.02 * 0.01
delprop3 <- 0.03 * 0.01

M <- 8L # length of moving averages for WTI, HH prices 

### EFrac is the "ethane fraction," the factor to apply to the 
###  propane delta values to compute ethane delta values 
EFrac <- 0.7 

### Sets RunName
#RunName <- 'NGPL_Price01_2022'

### Start a pdf file to contain the time series plots
pdf(file=paste(RunName, '_', M, '_d_', delprop1, '_', delprop2, '_', delprop3, '_e_', EFrac, '.pdf', sep = ""))

dim_k <- 4L # number of component models
v <- 5L # length of stage I projection period (5)
NIndep <- 4L # number of independent variables in random walk dlm
WVarFrac <- 0.5 # factor to apply to the lm variances to compute W (0.5)

#####  Initial component weights are based on prediction error #####  from the previous L periods.
L <- 3L # burn-in for component weighting

### 'wgt' are the model weights, with 'wgt_bar' being
### the desired weights in long term forecast (asymptotically)
### The vector 'eta' ('eta_bar') is the logistic transform of
### 'wgt' ('wgt_bar', respectively)
### The real 'phi' is the common AR coefficient in the 
### dynamics of the 'eta's.

wgt_bar <- matrix(c(0.04, 0.90, 0.04, 0.02), nrow = 2, ncol = dim_k, byrow = TRUE) # long-term forecasting weigths, for both NGLs
eta_bar <- log(wgt_bar[, -dim_k] / wgt_bar[, dim_k]) # long-term log odds

### The AR coefficient is set based on 27 forecasting steps

phi <- 0.7 # AR coefficient
aa <- 0.5 # coefficient of forecast-error-based adjustments

###### End Constants

######
###### Utility functions
######
## The functions setFG and setF expand the X matrix in the
## dlm object into a collection of separate FF and GG matrices, ## one for each time period.  The expansion is dictated by the ## elements of the JFF and GFF matrices.  The X matrix contains ## the time varying components of the observation and system
## matrices, as discussed on page 45 of Petris et al. (2009).

setFG <- function(mod) {
    ## Input:
    ##   'mod': an object of class 'dlm', with time-varying
    ##    observation and system matrices.
    ## Output:
    ##   the same as the input object 'mod', with additional
    ##   components 'FFa' and 'GGa' containing, in the form of 
    ##   3-dim arrays, the sequences of observation and system 
    ##   matrices built from the 'X' component of the
    ##   input. The last index of the arrays is the time index.

    ##  Create a local copy of the dlm object.
    mod <- mod 

    ##  Create a matrix of logical variables indicating which 
    ##  elements of mod$JFF (mod$JGG) are positive.  The values 
    ##  of the positive elements of mod$JFF (mod$JGG) identify 
    ##  the columns of X that contain the values of the time-
    ##  varying elements of FF (GG): JFF[i,j] = k > 0 implies 
    ##  that, for time s, FF[i,j] = X[s,k].  Each row in the 
    ##  matrix X corresponds to one time period.
    nz <- mod$JFF != 0

    ##  Create a matrix that indicate the positions of the
    ##  non-zero elements of mod$JFF (in the first two columns) 
    ##  and provides the non-zero elements of mod$JFF in the
    ##  third column.
    mod$JFF <- cbind(row(mod$JFF)[nz], col(mod$JFF)[nz], mod$JFF[nz])

    ##  Repeat the previous two steps for JGG.
    nz <- mod$JGG != 0
    mod$JGG <- cbind(row(mod$JGG)[nz], col(mod$JGG)[nz], mod$JGG[nz])

    ##  Assign a variable ll indicating the number of time
    ##  periods.
    ll <- nrow(mod$X)

    ##  Create 3-dimensional arrays to hold the set of GG and FF 
    ##  matrices, one matrix for each time period
    FFa <- array(dim = c(dim(mod$FF), ll))
    GGa <- array(dim = c(dim(mod$GG), ll))

    ##  Create local copies of the FF and GG matrices.
    FFmat <- mod$FF
    GGmat <- mod$GG

    ##  Populate the 3-dimensional arrays.  Each row of X
    ##  contains the time-varying elements for 1 time period.  
    ##  The loop runs through the time periods from 1 to ll.

    for (tt in seq.int(ll)) {

        ##  insert the time varying elements for one time
        ##  period tt
        FFmat[mod$JFF[, -3, drop=FALSE]] <- mod$X[tt, mod$JFF[,3]]

        ##  populate the array FFa with a matrix for time
        ##  period tt
        FFa[,, tt] <- FFmat

        ##  repeat the process for GGa
        GGmat[mod$JGG[, -3, drop=FALSE]] <- mod$X[tt, mod$JGG[,3]]
        GGa[,, tt] <- GGmat
    }

    ##  incorporate the new arrays into the dlm object mod
    mod$FFa <- FFa
    mod$GGa <- GGa

    ##  return the expanded dlm object
    return(mod)
}
    

    ##  The function setF is the same as the function setFG 
    ##  except that it only expands the observation matrix FF

setF <- function(mod) {
    ## Input:
    ##   'mod': an object of class 'dlm', with time-varying 
    ##    observation matrix.
    ## Output:
    ##   the same as the input object 'mod', with additional 
    ##   component 'FFa' containing, in the form of 3-dim 
    ##   arrays, the sequence of observation matrices
    ##   built from the 'X' component of the input. The last 
    ##   index of the array is the time index.
    mod <- mod
    nz <- mod$JFF != 0
    mod$JFF <- cbind(row(mod$JFF)[nz], col(mod$JFF)[nz], mod$JFF[nz])
    ll <- nrow(mod$X)
    FFa <- array(dim = c(dim(mod$FF), ll))
    FFmat <- mod$FF
    for (tt in seq.int(ll)) {
        FFmat[mod$JFF[, -3, drop=FALSE]] <- mod$X[tt, mod$JFF[,3]]
        FFa[,, tt] <- FFmat
    }
    mod$FFa <- FFa
    return(mod)
}
###### End Utility functions

######
###### Output variables

######  The NowForecast vector will hold the forecasts from now.
nowForecast <- vector("list", 1)

#####  Create similar vectors to hold data, forecasts, and weights.
allData <- vector("list", 1)
allForecast <- vector("list", 1)
allWeight <- vector("list", 1)

###### End Output variables

###### Data variables below are as follows
##          "Year"                               # A
##          "Total NGPL production"              # B
##          "Ethane NGPL production"             # C
##          "Propane NGPL production"            # D
##          "Ethane annual cents/gal nominal"    # E
##          "Ethane annual cents/gal real"       # F
##          "Propane annual cents/gal nominal"   # G
##          "Propane annual cents/gal real"      # H
##          "Organic shipment demand"            # I
##          "Resin shipment demand"              # J
##          "Total chemical demand"              # K
##          "WTI"                                # L
##          "HH"                                 # M
##          "Deflator to 2011$"                  # N
##          "Brent"                              # 0
##          "Industrial Ethane Consumption (Quadrillion Btu)"                  # P
##          "Industrial Propane Consumption (Quadrillion Btu)"                 # Q

mod_list <- vector("list", dim_k) # list of component models

## read the data and fit a linear model to data up to 'now'
    dat <- as.matrix(read.table(paste(RunName, "_RInputData.csv", sep = ""),
                            header = FALSE, skip = 2, sep = ",",
                            colClasses = c(rep("numeric", 15)))[, 1:17]) 
    #dat1 <- as.matrix(read.table(paste(RunName, "_IndCons.csv", sep = ""),
    #                        header = FALSE, skip = 1, sep = ",",
    #                        colClasses = c(rep("numeric", 2)))[,2:3])
    #dat <- merge(dat, dat1, by = "row.names", all = TRUE)
    colnames(dat) <- LETTERS[1:17]

## create a subset of data prior to peak ethane
    indcons <- data.frame(dat[,1],dat[,16],dat[,17])
    colnames(indcons) <- LETTERS[1:3]
    
## set year at which excess ethane runs out and project propane's trend forward
    sta_year <- 2010
    sub_year <- 2030
    indcons <- subset(indcons, A<=sub_year)
    indcons <- subset(indcons, A>=sta_year)
    propane_trend <- lm(C ~ A, data=indcons)
    

#####################################################################    
## FOR AEO 2022 ONLY, swap estimated 2021 prices for projected prices
#####################################################################
    
#    Propane2021 <- 93.55*0.840863901
#    Ethane2021 <- 26.48*0.840863901
#    dat[19, 6] <- Ethane2021
#    dat[19, 8] <- Propane2021

####################################################################
## End swapping out projected prices
####################################################################
    
    dat[, c('F', 'H', 'O', 'M')] <- log(dat[, c('F', 'H', 'O', 'M')]) # log prices of Ethane and Propane and Brent and HH 
    joint_lm <- lm(cbind(F, H) ~ -1 + O + M + D + I, data = as.data.frame(dat), subset = A <= now)
    
  ## set up a random walk dynamic regression DLM, based on
  ## linear regression estimates. The random walk dlm is the
  ## last element of mod_list.

    mod_list[[dim_k]] <- dlm(list(FF = matrix(0, nrow = 2, ncol = NIndep*2),
                                  JFF = rbind(c(1 : NIndep, rep(0, NIndep)), c(rep(0, NIndep), 1 : NIndep)),
                                  V = crossprod(residuals(joint_lm)) / summary(joint_lm)[[1]]$df[2],
                                  GG = diag(nr = NIndep*2, nc = NIndep*2),
                                  W = diag(x = WVarFrac * diag(vcov(joint_lm))),
                                  m0 = as.vector(coefficients(joint_lm)),
                                  C0 = vcov(joint_lm),
                                  X = tail(dat[, c("O", "M",  "D", "I")], n = -M)))  
    
    ## The matrix X above consists of the listed columns of 
    ## the dat matrix with the first M rows removed.

    ## set up the expert-based DLMs
    running_ave <- filter(dat[, c("O", "M")], filter = rep(1 / M, M), sides = 1) 
    tmp <- tail.matrix(running_ave, -1) / head.matrix(running_ave, -1)

    ## add a row of NAs on top of the tmp matrix
    tmp <- rbind(rep(NA, 2), tmp); dimnames(tmp) <- NULL
    relative_var <- tail(tmp[, 1] / tmp[, 2], n = -M)
    rm(tmp)
    delta_propane <- cbind(delprop1, delprop2, delprop3) 

    delta_ethane <- delta_propane * EFrac
    for (k in seq.int(dim_k-1)) {
        tmp_mod <- mod_list[[dim_k]]
        tmp_mod$JFF[] <- 0
        tmp_mod$JFF[matrix(c(rep(c(1, 2), each = 2), c(1, 2, 1 + NIndep, 2 + NIndep)), ncol = 2)] <- rep(c(1, 2), 2)
        tmp_mod$JGG <- matrix(0, ncol(tmp_mod$GG), ncol(tmp_mod$GG))
        tmp_mod$JGG[matrix(c(1, 2, 1 + NIndep, 2 + NIndep, rep(c(1, 1 + NIndep), each = 2)), ncol = 2)] <- 3 : 6
        tmp_mod$X <- cbind(tmp_mod$X[, c("O", "M")],
                           relative_var^delta_ethane[, k], 1 - relative_var^delta_ethane[, k],
                           relative_var^delta_propane[, k], 1 - relative_var^delta_propane[, k])
        mod_list[[k]] <- tmp_mod
    }
    rm(tmp_mod)
    
    ## run Kalman filter on each model, saving one-step forecast
    ## errors y contains the E and P prices with the first M 
    ## rows removed and with the first row number equal to 1 
    ## (rather than 1+M). With these matrix definitions,
    ##  GG_t * theta_{t-1} = theta_t

    y <- tail(dat[, c("F", "H")], n = -M, keepnums = FALSE)
    modKF <- vector("list", dim_k)
    forecast_err <- array(dim = c(nrow(dat) - M, 2, dim_k))
    for (j in seq.int(dim_k)) {
        modKF[[j]] <- dlmFilter(y, mod_list[[j]])
        forecast_err[,, j] <- abs(y - modKF[[j]]$f)
    }
    
## compute the model forecasting weights based on one-step
## forecast errors
## compute the adjustment term in the updating of the logit
## weights ('eta')
## in the code below, the sweep function SUBTRACTS the forecsst 
## errors of model k from the forecast errors of each model then
## deletes the kth matrix, which consists of zeros

wgt_adjust <- -aa * sweep(forecast_err, MARGIN = c(1, 2),
                      STATS = forecast_err[,, dim_k])[,, -dim_k] 

## compute the logit weights for the burn-in period of
## length 'L'
    eta0 <- wgt_adjust[1,,]
    tt <- 2L
    while (tt <= L) {
        eta0 <- phi * eta0 + wgt_adjust[tt,,]
        tt <- tt + 1L
    }

## for each Omega, compute the logit weights for all lead times
    eta <- array(0, dim = c(Omega = nrow(y) - L,
                lead = nrow(y) - L, NGL = 2, model = dim_k - 1))
    eta[1, 1,,] <- eta0
    for (ll in 2 : dim(eta)[2])
       eta[1, ll,,] <- phi * (eta[1, ll - 1,,] - eta_bar) + eta_bar
    for (tt in 2 : dim(eta)[1]) {
        ## one-step-ahead logit weights first
        eta[tt, 1,,] <- phi * (eta[tt - 1, 1,,] - eta_bar) +
            eta_bar + wgt_adjust[tt,,]
        ## then all the remaining logit weights
        for (ll in 2 : dim(eta)[2])
            eta[tt, ll,,] <- phi * (eta[tt, ll - 1,,] - eta_bar) + eta_bar
    }

    ## transform weights from logit to standard scale
    tmp <- array(c(exp(eta), rep(1, length.out = prod(dim(eta)[-4]))),
                 dim = c(dim(eta)[-4], dim_k))
    wgt <- sweep(tmp, MARGIN = c(1, 2, 3), FUN = "/",
                 STATS = apply(tmp, MARGIN = c(1, 2, 3), FUN = sum))
    
    ## compute, for each hypothetical 'Omega', the forecasts for the next times;
    ##   need to do it separately for the expert-based models and for the dynamic
    ##   regression model
    forecast0 <- array(dim = dim(wgt))
    forecast <- array(dim = dim(forecast0)[-4])
    

    ## note: for both 'forecast0' and 'wgt' we have
    ## dim 1: time of last observation
    ## dim 2: lead time
    ## dim 3: NGL
    ## dim 4: component model
    for (j in seq.int(dim_k - 1)) {
        ## (1) set up the time-varying GG and FF matrices for all times, as arrays
        mod_list[[j]] <- setFG(mod_list[[j]])

        ## (2) evaluate the forecasts up to 'v' steps ahead
        for (Omega in (L + 1) : (nrow(modKF[[j]]$m) - 1)) {
            a <- modKF[[j]]$m[Omega, ]
            for (i in Omega : nrow(modKF[[j]]$f)) {
                a <- mod_list[[j]]$GGa[,, i] %*% a
                forecast0[Omega - L, i - Omega + 1,, j] <- mod_list[[j]]$FFa[,, i] %*% a
            }
        }
    }
    ## the regression model only has a time-varying 'FF'
    ## (1) set up the time-varying FF matrices for all times, as arrays
    mod_list[[dim_k]] <- setF(mod_list[[dim_k]])
    ## (2) evaluate the forecasts up to 'v' steps ahead
    for (Omega in (L + 1) : (nrow(modKF[[dim_k]]$m) - 1)) {
        a <- modKF[[dim_k]]$m[Omega, ]
        for (i in Omega : nrow(modKF[[dim_k]]$f)) {
            forecast0[Omega - L, i - Omega + 1,, dim_k] <- mod_list[[dim_k]]$FFa[,, i] %*% a
        }
    }
    ## combine the model-specific forecasts
    forecast <- apply(forecast0 * wgt, c(1, 2, 3), sum)

    ## compare forecasts and pseudo-historical data
    y0 <- array(dim = dim(forecast))
    for (ii in seq.int(2))
        for (iii in seq.int(dim(y0)[1])) {
            ind <- nrow(y) - L - iii + 1
            y0[iii, seq.int(ind), ii] <- tail(y[, ii], n = ind)
        }

##  the variable StartYear holds the year of the first 
##  price projections.  We must have
##  StartYear = max(now + 1, dat[1, "A"] + M + L)
    
   StartYear = now + 1
   StartRow <- now - 2012
   Forecast <- exp(forecast)
   Forecast <- Forecast[StartRow:37,1:(37-(StartRow-1)),]
   forecast0_1 <- forecast0[StartRow:37,1:(37-(StartRow-1)),,]
   NowComp1 <- exp(forecast0_1[1,,,1])
   NowComp2 <- exp(forecast0_1[1,,,2])
   NowComp3 <- exp(forecast0_1[1,,,3])
   NowComp4 <- exp(forecast0_1[1,,,4])
   
   ## get last historical year
   indata <- as.data.frame(dat)
   lasthistory <- indata[which(indata$A == now),]
   lasthistory <- subset(lasthistory, select = c(F,H))
   lasthistory$Year <- lasthistory$A
   lasthistory$Ethane <- exp(lasthistory$F)
   lasthistory$Propane <- exp(lasthistory$H)
   
   EthaneSeries1 <- c(lasthistory$Ethane, NowComp1[,1])
   EthaneSeries2 <- c(lasthistory$Ethane, NowComp2[,1])
   EthaneSeries3 <- c(lasthistory$Ethane, NowComp3[,1])
   EthaneSeries4 <- c(lasthistory$Ethane, NowComp4[,1])
   PropaneSeries1 <- c(lasthistory$Propane, NowComp1[,2])
   PropaneSeries2 <- c(lasthistory$Propane, NowComp2[,2])
   PropaneSeries3 <- c(lasthistory$Propane, NowComp3[,2])
   PropaneSeries4 <- c(lasthistory$Propane, NowComp4[,2])
   
   
   ethaneComp1.ts <- ts(EthaneSeries1, start = c(now,1), frequency = 1)
   ethaneComp2.ts <- ts(EthaneSeries2, start = c(now,1), frequency = 1)
   ethaneComp3.ts <- ts(EthaneSeries3, start = c(now,1), frequency = 1)
   ethaneComp4.ts <- ts(EthaneSeries4, start = c(now,1), frequency = 1)
   
   propaneComp1.ts <- ts(PropaneSeries1, start = c(now,1), frequency = 1)
   propaneComp2.ts <- ts(PropaneSeries2, start = c(now,1), frequency = 1)
   propaneComp3.ts <- ts(PropaneSeries3, start = c(now,1), frequency = 1)
   propaneComp4.ts <- ts(PropaneSeries4, start = c(now,1), frequency = 1)
   
   NowForecast <- Forecast[1,,]
   EthaneProject <- c(lasthistory$Ethane, NowForecast[,1])
   PropaneProject <- c(lasthistory$Propane, NowForecast[,2])
   
#### Adjust ethane projection for 2030 scarcity
   dat <- cbind(dat,dat[,17] - (summary(propane_trend)$coefficients[1]+summary(propane_trend)$coefficients[2]*dat[,1]))
   dat[,18] = ifelse(dat[,1]<sub_year,0,dat[,18])
   dat[,18] = ifelse(dat[,18]<0,0,dat[,18])
   
   dat <- cbind(dat,dat[,18]/(dat[,18]+dat[,16]))
   adj_matrix <- subset(dat, dat[,1]>=now)
   
   EthaneProject <- adj_matrix[,19]*PropaneProject+(1-adj_matrix[,19])*EthaneProject
   
   ethaneForecast.ts <- ts(EthaneProject, start = c(now,1), frequency = 1)
   propaneForecast.ts <- ts(PropaneProject, start = c(now,1), frequency = 1)

ts.plot(ethaneForecast.ts, ethaneComp1.ts, ethaneComp2.ts, ethaneComp3.ts, ethaneComp4.ts, main = paste("Ethane Price Projections and Components,", RunName, "Case"), sub = paste("Delta Values: ", delprop1*EFrac, ", ", delprop2*EFrac, ", ", delprop3*EFrac, ";  M = ", M, sep=''), ylab = "2011 Cents per Gallon", xlab = "Year", gpars = list(col = c('red', 'blue', 'darkgreen', 'brown', 'purple'), lty = c(1, 2, 2, 2, 2)))
legend("bottomright", c("Projected", "Comp. 1", "Comp. 2", "Comp. 3", "Comp. 4"), lty = c(1, 2, 2, 2, 2), col = c('red', 'blue', 'darkgreen', 'brown', 'purple'))

ts.plot(propaneForecast.ts, propaneComp1.ts, propaneComp2.ts, propaneComp3.ts, propaneComp4.ts, main = paste("Propane Price Projections and Components,", RunName, "Case"), sub = paste("Delta Values: ", delprop1, ", ", delprop2, ", ", delprop3, ";  M = ", M, sep=''), ylab = "2011 Cents per Gallon", xlab = "Year", gpars = list(col = c('red', 'blue', 'darkgreen', 'brown', 'purple'), lty = c(1, 2, 2, 2, 2)))
legend("bottomright", c("Projected", "Comp. 1", "Comp. 2", "Comp. 3", "Comp. 4"), lty = c(1, 2, 2, 2, 2), col = c('red', 'blue', 'darkgreen', 'brown', 'purple'))



########################################################
########################################################
###   BEGIN FITTING NEW PROPANE MODEL  #################
########################################################
########################################################

## we first fit the model without the prices in log scale
## read in data; don't take logs of the price variables
dat_1 <- as.matrix(read.table(paste(RunName, "_RInputData.csv", sep = ""),
                            header = FALSE, skip = 2, sep = ",",
                            colClasses = c(rep("numeric", 15)))[, 1:15]) 

#####################################################################    
## FOR AEO 2021 ONLY, swap estimated 2020 prices for projected prices
#####################################################################

#Propane2021 <- 93.55*0.840863901
#Ethane2021 <- 26.48*0.840863901
#dat_1[19, 6] <- Ethane2021
#dat_1[19, 8] <- Propane2021

####################################################################
## End swapping out projected prices
####################################################################

colnames(dat_1) <- LETTERS[1:15]
## for the default model, don't take logs
## dat[, c('F', 'H', 'O', 'M')] <- log(dat[, c('F', 'H', 'O', 'M')]) # log prices of Ethane and Propane and Brent and HH 

projdata_1 <- as.data.frame(dat_1)
newprojdata_1 <- subset(projdata_1, select = c('A', 'O', 'D', 'I', 'H'))

newprojdata_1$Year <- newprojdata_1$A
newprojdata_1$Brent <- newprojdata_1$O
newprojdata_1$PropProduction <- newprojdata_1$D
newprojdata_1$OrgDemand <- newprojdata_1$I
newprojdata_1$PropPrice <- newprojdata_1$H

newprojdata_1$A <- NULL
newprojdata_1$O <- NULL
newprojdata_1$D <- NULL
newprojdata_1$I <- NULL
newprojdata_1$H <- NULL

## run the 3-covariate model for propane
prop_lm_1 <- lm(PropPrice ~ -1 + Brent + PropProduction + OrgDemand, data = newprojdata_1, subset = Year <= now)

## isolate the projection years
projonly_1 <- newprojdata_1[which(newprojdata_1$Year > now),]

## reduce the effect of the production coefficient
prop_lm_1$coefficients[2] <- prop_lm_1$coefficients[2] * 0.45

## compute propane projections from covariates
## here we've added an intercept of 0
  projonly_1$PropProject <- projonly_1$Brent * prop_lm_1$coefficients[1] +
  projonly_1$PropProduction * prop_lm_1$coefficients[2] +
  projonly_1$OrgDemand * prop_lm_1$coefficients[3] - 27

## compute a constant to raise the projections to the PFloor constant
PConstant <- max(PFloor - projonly_1$PropProject[1], 0)
projonly_1$PropProject <- projonly_1$PropProject + PConstant


## get last historical year
indata <- as.data.frame(dat)
lasthistory <- indata[which(indata$A == now),]
lasthistory <- subset(lasthistory, select = c(F,H))
lasthistory$Year <- lasthistory$A
lasthistory$Ethane2 <- lasthistory$F
lasthistory$Propane2 <- lasthistory$H
lasthistory$Brent2 <- lasthistory$O

NewPropForecast_0 <- c(lasthistory$Propane2, projonly_1$PropProject)

## create a time series for plotting
NewPropaneForecast_1.ts <- ts(NewPropForecast_0, start = c(StartYear - 1,1), frequency = 1)

###############################################################
## next we fit the model with the price variables in log scale
###############################################################

projdata <- as.data.frame(dat)
newprojdata <- subset(projdata, select = c('A', 'O', 'D', 'I', 'H'))

newprojdata$Year <- newprojdata$A
newprojdata$Brent <- newprojdata$O
newprojdata$PropProduction <- newprojdata$D
newprojdata$OrgDemand <- newprojdata$I
newprojdata$PropPrice <- newprojdata$H

newprojdata$A <- NULL
newprojdata$O <- NULL
newprojdata$D <- NULL
newprojdata$I <- NULL
newprojdata$H <- NULL

## run the 3-covariate model for propane
prop_lm <- lm(PropPrice ~ -1 + Brent + PropProduction + OrgDemand, data = newprojdata, subset = Year <= now)

## isolate the projections years
projonly <- newprojdata[which(newprojdata$Year > now),]

## compute propane projections from covariates
  projonly$PropProject <- projonly$Brent * prop_lm$coefficients[1] +
  projonly$PropProduction * prop_lm$coefficients[2] +
  projonly$OrgDemand * prop_lm$coefficients[3]
  
## convert from log scale to regular scale
  projonly$PropProject <- exp(projonly$PropProject)

## compute a constant to raise the projections to the PFloor constant
PConstant <- max(PFloor - projonly$PropProject[1], 0)
projonly$PropProject <- projonly$PropProject + PConstant

NewPropaneForecast.ts <- ts(projonly$PropProject, start = c(StartYear,1), frequency = 1)

NewBrentForecast_0 <- c(lasthistory$Brent2, projonly_1$Brent)
Brent.ts <- ts(NewBrentForecast_0, start = c(StartYear - 1,1), frequency = 1)

ts.plot(NewPropaneForecast_1.ts, Brent.ts, main = paste("New Propane and Brent Price Projections, different units"), sub = paste("Delta Values: ", delprop1, ", ", delprop2, ", ", delprop3, ";  M = ", M, sep=''), ylab = "2011 Cents per Gallon or Dollars per Barrel", xlab = "Year", gpars = list(col = c('blue', 'brown'), lty = c(1, 1)))
legend("bottomright", c("Propane, No HH, cents per gallon", "Brent, dollars per barrel"), lty = c(1, 1), col = c('blue', 'brown'))

########################################################
########################################################
###   END FITTING NEW PROPANE MODEL  ###################
########################################################
########################################################   

Forecast0 <- exp(forecast0_1)

  ## add dimension names to the Forecast array
    dimnames(NowForecast) <- list("Year" = (now + 1):2050, HGL = c("Ethane", "Propane"))  

    ## save output from the current run
    allForecast <- forecast
    allWeight <- wgt
    nowForecast <- NowForecast
    
    ## replace historical data
    indata <- as.data.frame(dat)
    history <- indata[which(indata$A <= now),]
    history <- subset(history, select = c(A,F,H))
    history$Year <- history$A
    history$A <- NULL
    history <- history[which(history$Year >= 2014),]
    history$Ethane <- exp(history$F)
    history$F <- NULL
    history$Propane <- exp(history$H)
    history$H <- NULL

    
    
######
###### Save the relevant calculated quantities to file
######
save(allForecast, allWeight, file = paste(RunName, ".RData", sep=""))
## use 'load(paste(RunName,".RData"))' to read saved data into R

## Write the MAPE, MPE, and nowForecast lists out to csv files
    
nowForecast <- as.data.frame(nowForecast) 
nowForecast$Year <- StartYear:2050
## nowForecast <- nowForecast[which(nowForecast$Year > now),]

## replace the old propane price projections with the new ones

## nowForecast$Propane <- if(min(NewPropaneForecast_1.ts) > 0)
##                        projonly_1$PropProject else projonly$PropProject

## combine historical and projected values
newForecast <- rbind(history, nowForecast)
newForecast$Year <- NULL
newForecast2 <- as.matrix(newForecast, row.names = 2014:2050,
                          col.names = c('Ethane', 'Propane'))

row.names(newForecast2) <- 2014:2050

## Adjust ethane
adj_matrix <- subset(dat, dat[,1]>=2014)
newForecast2[,1] <- adj_matrix[,19]*newForecast2[,2]+(1-adj_matrix[,19])*newForecast2[,1]

write.csv(newForecast2, file=paste(RunName, '_ROutputData.csv', sep = ""))

dev.off()

pdf(file=paste(RunName, '_', M, '_d_', delprop1, '_', delprop2, '_', delprop3, '_e_', EFrac, '2.pdf', sep = ""))

ts.plot(ethaneForecast.ts, main = paste("Ethane Price Projections ,", RunName, "Case"), sub = paste("Delta Values: ", delprop1*EFrac, ", ", delprop2*EFrac, ", ", delprop3*EFrac, ";  M = ", M, sep=''), ylab = "2011 Cents per Gallon", xlab = "Year", gpars = list(col = c('red'), lty = c(1)))
legend("bottomright", c("Projected"), lty = c(1), col = c('red'))

ts.plot(NewPropaneForecast_1.ts, Brent.ts, main = paste("New Propane and Brent Price Projections, different units"), sub = paste("Delta Values: ", delprop1, ", ", delprop2, ", ", delprop3, ";  M = ", M, sep=''), ylab = "2011 Cents per Gallon or Dollars per Barrel", xlab = "Year", gpars = list(col = c('blue', 'brown'), lty = c(1, 1)))
legend("bottomright", c("Propane, No HH, cents per gallon", "Brent, dollars per barrel"), lty = c(1, 1), col = c('blue', 'brown'))

dev.off()



