complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        ## Lista todos los archivos en el path relativo directory
        files_list <- list.files(path = directory, full.names = TRUE)
        
        ## Inicializa el data frame para casos completos
        completeCases <- data.frame();
        
        ## Bucle para recorrer los archivos
        
        for (i in id) {
                data <- read.csv(files_list[i]);
                ## Vector con los casos completos del archivo i
                c <- complete.cases(data);
                ## Vector que almacena todos los casos completos
                naRm <- data[c, ];
                ## Data frame que se va rellenando con el id y numero de filas
                ## de casos completos
                completeCases <- rbind(completeCases, c(i, nrow(naRm)));
        }
        
        ## Pone los nombres a las columnas
        names(completeCases) <- c("id", "nobs");
        ## Devuelve el data frame
        completeCases;
}