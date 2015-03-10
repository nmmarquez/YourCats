rmse_location_yourcats <- function(yc){
    n <- length(array.yourcast(yc))
    groupN <- ncol(array.yourcast(yc)[[1]][,,1])
    in_years <- as.character(yc$aux$sample.frame[1]:yc$aux$sample.frame[2])
    out_years <- as.character(yc$aux$sample.frame[3]:yc$aux$sample.frame[4])
    n_in <- groupN * length(in_years)
    n_out <- groupN * length(out_years)
    array_yc <- array.yourcast(yc)
    y_in <- lapply(array_yc, function(x)c(x[in_years,,1]))
    y_out <- lapply(array_yc, function(x)c(x[out_years,,1]))
    y_hat_in <- lapply(array_yc, function(x)c(x[in_years,,2]))
    y_hat_out <- lapply(array_yc, function(x)c(x[out_years,,2]))
    rmse_func <- function(y, yhat){
        (sum((yhat - y)**2)/ length(y))**.5
    }
    rmse_in <- sapply(1:length(y_in), function(i) 
        rmse_func(y_in[[i]], y_hat_in[[i]]))
    rmse_out <- sapply(1:length(y_out), function(i) 
        rmse_func(y_out[[i]], y_hat_out[[i]]))
    rmse <- data.frame(rmse_in, rmse_out, location_id=names(array_yc),
                       stringsAsFactors=FALSE)
    ############################################################################
    #               NEAL YOU IDIOT FIX THIS SHITTY SHITTY CODE                 #
    ############################################################################
    rmse <- merge(rmse, yc$aux$G.names) # <---------------
    rmse
}