rmse_location_yourcats <- function(yc){
    n <- length(array.yourcast(yc))
    groupN <- ncol(array.yourcast(yc)[[1]][,,1])
    in_years <- as.character(yc$aux$sample.frame[1]:yc$aux$sample.frame[2])
    out_years <- as.character(yc$aux$sample.frame[3]:yc$aux$sample.frame[4])
    n_in <- groupN * length(in_years)
    n_out <- groupN * length(out_years)
    array_yc <- array.yourcast(yc)
    
    rmse <- lapply(1:n, function(i) 
        rowSums((array_yc[[i]][,,"yhat"] - array_yc[[i]][,,"y"])**2))
    rmse <- as.data.frame(t(sapply(rmse, function(x) 
        c(sum(x[in_years]/n_in), sum(x[out_years]/n_out)))))
    names(rmse) <- c("in", "out")
    ############################################################################
    #               NEAL YOU IDIOT FIX THIS SHITTY SHITTY CODE                 #
    ############################################################################
    rmse$iso <- yc$aux$G.names$iso3 # <------------------------------------
    rmse
}