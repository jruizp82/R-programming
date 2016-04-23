pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files        
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        
        ## Lista todos los archivos en el path relativo directory
        files_list <- list.files(path = directory, full.names = TRUE)
        
        ## Inicializa el data frame
        data <- data.frame()
        
        ## Con el bucle se va rellenando el dataframe combinando las filas de los archivos indicados en el id
        for (i in id) {
                data <- rbind(data, read.csv(files_list[i]))
        }
        
        if (pollutant == 'sulfate') {
                ## Hace la media de los datos de la columna sulfate ignorando los na 
                mean(data$sulfate, na.rm = TRUE)
        }
        else if (pollutant == 'nitrate') {
                ## Hace la media de los datos de la columna nitrate ignorando los na 
                mean(data$nitrate, na.rm = TRUE)
        }
        
}