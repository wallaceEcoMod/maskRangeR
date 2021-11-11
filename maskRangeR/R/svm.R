#' Classify species ranges based on occurrence coordinates and SDM scores.
#'
#' \code{rangeSVM()} returns a tuned support vector machine (SVM) model that predicts 
#' species identity based on predictors that are solely spatial, based on occurrence coordinates,
#' or a combination of spatial and environmental, based on both occurrence coordinates and 
#' environmental suitability values. Suitability values can be predicted with species distribution
#' models (SDMs; a.k.a. ecological niche models).
#'
#' @param xy1 Matrix or data frame of occurrence coordinates for species 1.
#' @param xy2 Matrix or data frame of occurrence coordinates for species 2.
#' @param ... Other matrices or data frames of occurrence coordinates for additional species.
#' @param sdm Raster or RasterStack representing environmental suitability (can be predictions from SDMs). 
#' These must have the same extent as both species' occurrence points. Default is NULL.
#' @param nrep Numeric for number of SVM tuning iterations. Default is 100.
#' @param weight Boolean. If TRUE, species with fewer occurrence records are weighted higher in the SVM. Default is FALSE.
#' @param mc.cores Number of cores to use for parallel processing. Default is 1.
#' @return The tuned SVM model.
#' @details The tuning operation uses \code{tune.svm()} from the e1071 package, which performs 10-fold
#' cross validation and selects the best model based on classification error. Ranges of the cost and gamma
#' parameters are explored in the tuning exercise. The tuning function is iterated \code{nrep} times, and the
#' parameter combination used most frequently across all iterations is used to build a final SVM model.
#' 
#' When \code{sdm = NULL}, the SVM is purely spatial, based only on the occurrence coordinates of
#' each species. Otherwise, the SVM is fit with both a spatial predictor and any additional ones added as
#' rasters. These extra predictors can be based on predictions from a species distribution model 
#' (SDM; a.k.a. ecological niche model), and in this case would represent environmental or climatic
#' suitability, depending on the variables used in the SDM.
#' 
#' @examples
#' \donttest{
#' r1.sdm <- raster::raster(raster::extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
#' raster::values(r1.sdm) <- (1:raster::ncell(r1.sdm))^2
#' r2.sdm <- raster::raster(raster::extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
#' raster::values(r2.sdm) <- (raster::ncell(r2.sdm):1)^2
#' r3.sdm <- raster::raster(raster::extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
#' r3.sdm [1] <- 10
#' r3.sdm <- raster::distance(r3.sdm)
#' sp1.xy <- data.frame(dismo::randomPoints(r1.sdm, 15, prob = TRUE))
#' colnames(sp1.xy) <- c("longitude", "latitude")
#' sp2.xy <- data.frame(dismo::randomPoints(r2.sdm, 15, prob = TRUE))
#' colnames(sp2.xy) <- c("longitude", "latitude")
#' sp3.xy <- data.frame(dismo::randomPoints(r3.sdm, 15, prob = TRUE))
#' colnames(sp3.xy) <- c("longitude", "latitude")
#' # Spatial SVMs (this can take about a minute to run)
#' svm.SP <- rangeSVM(sp1.xy, sp2.xy, sp3.xy, nrep=5) # more reps are recommended
#' }
#' 
#' @export

rangeSVM <- function(xy1, xy2, ..., 
                     sdm = NULL, nrep = 100, weight = FALSE, mc.cores=1) {
  
  # bind both coordinate matrices
  xy <- as.data.frame(rbind(xy1, xy2))
  otherSp <- list(...)
  nsp <- 2 + length(otherSp)
  sp.num <- c(nrow(xy1), nrow(xy2), sapply(otherSp, nrow))
  xy.otherSp <- do.call("rbind", otherSp)
  xy <- rbind(xy, xy.otherSp)
  # make sure the column names are x and y
  names(xy) <- c("x", "y")
  
  # if using sdm prediction rasters as extra predictor variable, add the values to the matrix 
  if(!is.null(sdm)) {
    sdm.vals <- raster::extract(sdm, xy)
    xy <- cbind(xy, sdm = sdm.vals)
  }
  # add a species signifier field
  xy$sp <- factor(rep(1:nsp, times=sp.num))
  
  # define class weights
  if(weight == TRUE) {
    cw <- 1/table(xy$sp)
  } else {
    cw <- table(xy$sp)/table(xy$sp)
  }
  
  # define a range for parameters C and gamma, which control complexity of fit
  # ranges based on suggestions from https://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
  C_range <- 2^seq(-5, 15, 2)
  gamma_range <- 2^seq(-15, 3, 2)
  
  # perform 10-fold cross validation for ranges of C and gamma
  # methodology based on:
  # @article{karatzoglou2005support,
  # title={Support vector machines in R},
  # author={Karatzoglou, Alexandros and Meyer, David and Hornik, Kurt},
  # year={2005},
  # publisher={Department of Statistics and Mathematics, WU Vienna University of Economics and Business}
  #params_best <- list()
  #performance_best <- list()
  # for(i in 1:nrep) {
  #   m.tune <- e1071::tune.svm(sp ~ ., data = xy, gamma = gamma_range, 
  #                             cost = C_range, class.weights = cw)
  #   # get optimal parameter values
  #   params_best[[i]] <- m.tune$best.parameters
  #   performance_best[[i]] <- m.tune$best.performance
  #   message(paste("Run", i, "complete."))
  # }
  # CM: unfortunately mclapply doesn't seem to print the status
  
  internalFunc=function(i){
    m.tune <- e1071::tune.svm(sp ~ ., data = xy, gamma = gamma_range, 
                              cost = C_range, class.weights = cw)  
    message(paste("Run", i, "complete."))
    # get optimal parameter values
    list( m.tune$best.parameters, m.tune$best.performance)
  }
  if(mc.cores>1){
    out=parallel::mclapply(1:nrep,function(i) {
                              internalFunc(i) 
                            },mc.cores=mc.cores)
  } else {
    out=lapply(1:nrep,function(i) {internalFunc(i)})
  }
  params_best=lapply(out,function(x) x[[1]])
  performance_best=lapply(out,function(x) x[[2]])
  
  # # function to extract the value of either parameter that was best most often
  performance_best <- do.call(rbind, performance_best)
  params_best_df <- cbind(do.call(rbind, params_best)[,1:2], performance_best)
  cost=C_range
  params_best_count <- dplyr::count(params_best_df, gamma, cost)
  
  # Add error message if there is no most frequent combination of parameters
  if(sum(params_best_count$n > 1) == 0){
    stop("Tuning did not produce a most frequent combination of SVM parameters. Please increase nrep and try again.")
  }
  
  params_mostFreq <- params_best_count[params_best_count$n == max(params_best_count$n),1:2]
  
  if(nrow(params_mostFreq) > 1){
    mean_performance <- numeric(nrow(params_mostFreq))
    for (i in 1:nrow(params_mostFreq)) {
      gamma_i <- as.numeric(params_mostFreq[i, 1])
      cost_i <- as.numeric(params_mostFreq[i, 2])
      mostFreq_i <- params_best_df[params_best_df$gamma == gamma_i & params_best_df$cost == cost_i, ]
      mean_performance[i] <- mean(mostFreq_i$performance_best)
    }
    param_combo_best <- params_mostFreq[which.min(mean_performance), ]
  } else {
    param_combo_best <- params_mostFreq
  }
  
  # run final model
  m <- e1071::svm(sp ~ ., data = xy, gamma = param_combo_best$gamma, 
                  cost = param_combo_best$cost, class.weights = cw)
  
  return(m)
}

#' Generate a raster based on predictions of SVM model with values corresponding to the species.
#'
#' \code{rangeSVM_predict()} returns a raster representing the ranges of the species
#' predicted by the fitted SVM tuned with \code{rangeSVM()}.
#'
#' @param svm Model object for the SVM, returned by \code{rangeSVM()}.
#' @param r Raster with the extent desired for the prediction. The values for cells used for predictions must 
#' have non-NA values, but the particular values do not matter.
#' @param sdm Raster or RasterStack representing environmental suitability (can be predictions from SDMs).
#' These rasters must match the predictor variables used in the SVM. Default is NULL.
#' @return The  Raster representing the SVM predictions.
#' @details The values of the output raster are 1, 2, ..., corresponding to xy1, xy2, and any additional species used in \code{rangeSVM()}.
#' These values represent the identities of the species.
#' 
#' @examples
#' \donttest{
#' r1.sdm <- raster::raster(raster::extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
#' raster::values(r1.sdm) <- (1:raster::ncell(r1.sdm))^2
#' r2.sdm <- raster::raster(raster::extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
#' raster::values(r2.sdm) <- (raster::ncell(r2.sdm):1)^2
#' r3.sdm <- raster::raster(raster::extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
#' r3.sdm [1] <- 10
#' r3.sdm <- raster::distance(r3.sdm)
#' sp1.xy <- data.frame(dismo::randomPoints(r1.sdm, 15, prob = TRUE))
#' colnames(sp1.xy) <- c("longitude", "latitude")
#' sp2.xy <- data.frame(dismo::randomPoints(r2.sdm, 15, prob = TRUE))
#' colnames(sp2.xy) <- c("longitude", "latitude")
#' sp3.xy <- data.frame(dismo::randomPoints(r3.sdm, 15, prob = TRUE))
#' colnames(sp3.xy) <- c("longitude", "latitude")
#' # Spatial SVMs (this can take about a minute to run)
#' svm.SP <- rangeSVM(sp1.xy, sp2.xy, sp3.xy, nrep=5)
#' # Use SVM to create a raster of predicted regions
#' rand_svm.SP <- rangeSVM_predict(svm = svm.SP, r = r1.sdm)
#' }
#' @export

rangeSVM_predict <- function(svm, r, sdm = NULL) {
  # extract centroid coordinates from shared extent raster cells
  r.pts <- raster::rasterToPoints(r, spatial = TRUE)
  r.xy <- sp::coordinates(r.pts)
  # rename column names to match response of svm
  colnames(r.xy) <- c("x", "y")
  # if sdm prediction raster input, add the values to the matrix 
  if(!is.null(sdm)) {
    sdm.vals <- raster::extract(sdm, r.xy)
    r.xy <- cbind(r.xy, sdm = sdm.vals)
    sdm.names <- seq(3, 2+ncol(sdm.vals))
    colnames(r.xy)[sdm.names] <- names(svm$x.scale$`scaled:center`[sdm.names])
  }
  
  # predict species identity of all coordinates with svm
  sp12.svm <- raster::predict(svm, r.xy)
  # convert factor response to integer
  sp12.svm <- as.numeric(as.character(sp12.svm))
  # convert back to raster
  r.pts$pred <- sp12.svm
  sp12.svm.ras <- raster::rasterize(r.pts, r, "pred")
  return(sp12.svm.ras)
}
