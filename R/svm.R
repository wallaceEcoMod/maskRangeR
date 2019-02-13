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
#' @param weight Boolean. If TRUE, the species with less occurrence records is weighted higher in the SVM.
#' Default is FALSE.
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
#' 
#' @examples
#' \dontrun{
#' # tune SVMs on coordinates only (spatial)
#' rangeSVM(xy1, xy2)
#' 
#' # tune SVMs on coordinates and SDM prediction rasters (spatial + environmental)
#' rangeSVM(xy1, xy2, raster::stack(sdm1, sdm2))
#' }
#' @export
#' 
#' 

# to clarify:
# 1. specifics on optimality criterion (weights false positives and false negatives equally?)
# 2. nothing to break tie (how do we choose when there are ties?)
# 3. could easily allow users to change k
# 4. sensitivity to relative number of points near border (and assumes that sampling bias was addressed?)
# 5. which params are varied and what they mean
# 6. convey to Cory to make generalizable to more than 2 species

rangeSVM <- function(xy1, xy2, ..., sdm = NULL, nrep = 100, weight = FALSE) {
  
  
  
  # define class weights
  # if(weight == TRUE) {
  #   if(nrow(xy1) != nrow(xy2)) {
  #     if(nrow(xy1) > nrow(xy2)) {
  #       cw <- c("0" = 1, "1" = nrow(xy1)/nrow(xy2))
  #     } else {
  #       cw <- c("0" = nrow(xy2)/nrow(xy1), "1" = 1)
  #     }
  #   }
  # } else {
  #   cw <- c("0" = 1, "1" = 1)
  # }
  
  
  # bind both coordinate matrices
  xy <- rbind(xy1, xy2)
  otherSp <- list(...)
  nsp <- 2 + length(otherSp)
  sp.num <- c(nrow(xy1), nrow(xy2), sapply(otherSp, nrow))
  xy.otherSp <- do.call("rbind", otherSp)
  xy <- rbind(xy, xy.otherSp)
  # if using sdm prediction rasters as extra predictor variable, add the values to the matrix 
  if(!is.null(sdm)) {
    sdm.vals <- raster::extract(sdm, xy)
    xy <- cbind(xy, sdm = sdm.vals)
  }
  # add a species signifier field
  xy$sp <- factor(rep(1:nsp, times=sp.num))
  
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
  params_best <- list()
  # performance_best <- list()
  gamma_best <- numeric(nrep)
  C_best <- numeric(nrep)
  
  for(i in 1:nrep) {
    m.tune <- e1071::tune.svm(sp ~ ., data = xy, gamma = gamma_range, cost = C_range)  
    # print(sum(as.numeric(as.character(predict(m.tune$best.model, aus.xy)))))
    # print(sum(as.numeric(as.character(predict(m.tune$best.model, tel.xy)))))
    # get optimal parameter values
    params_best[[i]] <- m.tune$best.parameters
    # performance_best[[i]] <- m.tune$best.performance
    # gamma_best[i] <- m.tune$best.parameters[[1]]
    # C_best[i] <- m.tune$best.parameters[[2]]
    message(paste("Run", i, "complete."))
  }
  # # function to extract the value of either parameter that was best most often
  # getMax <- function(x) as.numeric(names(which(table(x) == max(table(x)))))
  # # get the optimal parameter values based on iterations of svm
  # gamma_opt <- getMax(gamma_best)
  # C_opt <- getMax(C_best)
  params_best_df <- do.call(rbind, params_best)
  params_best_df$params <- paste0(params_best_df$gamma, params_best_df$cost)
  mostFreq <- names(which(table(params_best_df$params) == max(table(params_best_df$params))))
  params_best_df_mostFreq <- params_best_df[params_best_df$params == mostFreq, 1:2]
  
  # Add error message if there is no most frequent combination of parameters
  if(nrow(params_best_df_mostFreq) == 0){
    stop("Tuning did not produce a most frequent combination of SVM parameters. Please increase nrep and try again.")
  }
  
  # run final model
  if(weight == TRUE) {
    m <- e1071::svm(sp ~ ., data = xy, gamma = params_best_df_mostFreq$gamma[1], 
                    cost = params_best_df_mostFreq$cost[1], class.weights = "inverse")
  } else {
    m <- e1071::svm(sp ~ ., data = xy, gamma = params_best_df_mostFreq$gamma[1], 
                    cost = params_best_df_mostFreq$cost[1])
  }
  
  
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
#' \dontrun{
#' # raster prediction for xy SVM (spatial)
#' svm.xy <- rangeSVM(xy1, xy2)
#' svm.xy.r <- rangeSVM_predict(svm.xy, r)
#' plot(svm.xy.r)
#' 
#' # raster prediction for xyEnv SVM (spatial + SDM predictions)
#' svm.xyEnv <- rangeSVM(xy1, xy2, raster::stack(sdm1, sdm2))
#' svm.xyEnv.r <- rangeSVM_predict(svm.xyEnv, r, raster::stack(sdm1, sdm2))
#' plot(svm.xyEnv.r)
#' }
#' @export

rangeSVM_predict <- function(svm, r, sdm = NULL) {
  # extract centroid coordinates from shared extent raster cells
  r.pts <- raster::rasterToPoints(r, spatial = TRUE)
  r.xy <- sp::coordinates(r.pts)
  # if sdm prediction raster input, add the values to the matrix 
  if(!is.null(sdm)) {
    sdm.vals <- raster::extract(sdm, r.xy)
    r.xy <- cbind(r.xy, sdm = sdm.vals)
  }
  # rename column names to match response of svm
  colnames(r.xy) <- names(svm$x.scale$`scaled:center`)
  # predict species identity of all coordinates with svm
  sp12.svm <- raster::predict(svm, r.xy)
  # convert factor response to integer
  sp12.svm <- as.numeric(as.character(sp12.svm))
  # convert back to raster
  r.pts$pred <- sp12.svm
  sp12.svm.ras <- raster::rasterize(r.pts, r, "pred")
  return(sp12.svm.ras)
}
