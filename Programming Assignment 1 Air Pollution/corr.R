corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        ## Lista todos los archivos en el path relativo directory
        files_list <- list.files(path = directory, full.names = TRUE);
        
        ## Inicializa el vector correlationList
        correlationList <- c();
        
        i <- 1;
        
        ## Recorre todos los archivos
        while (i <= length(files_list)) {
                ## Data frame con el número de casos completos
                completeCases <- complete(directory, i);
                
                ## Si el número de casos completos es mayor que el límite
                if (completeCases$nobs > threshold) {
                        ## Lee el archivo 
                        data <- read.csv(files_list[i]);
                        ## Correlacion entre sulfato y nitrato
                        correlationList <- c(correlationList, cor(data$sulfate, data$nitrate, use = "complete.obs"));
                }
                ## Incrementa i
                i <- i + 1;
        }
        ## Devuelve la correlationList
        correlationList;
}