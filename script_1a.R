# Remove the variable datos if exists
remove(datos)
datos = read.csv2('speed-dating-clean.csv', sep=",", dec=".", na.strings = "null", as.is = TRUE)

# Auxiliar function to compute the mode of a vector (there is no built-in function for this)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Store the names of the colums of our data
cols <- colnames(datos)

# Print the number of variables
s = paste("There are ", length(cols) ," columns.")
print(s)

# Create directory
dir.create(file.path(getwd(), "Histograms"), showWarnings = FALSE)

# Iterate over the number of colums
for (i in 1:length(cols)){
  
  # Extract the name of the column
  col_name = cols[i]
  mode_col = mode(datos[[col_name]][1])
  column = datos[[col_name]]
  
  # Case that the type of the variable is numeric
  if(mode_col == "numeric"){
    
    # Print the statistics
    print(paste(col_name, " mean =", mean(column, na.rm = TRUE),"std =", sd(column, na.rm = TRUE)))
    
    table = table(column)
    
    name = gsub("\\.", "_", col_name)
    
    jpeg(paste(getwd(),"/Histograms/", name, ".jpeg", sep=""))
    
    barplot(table, las = 1, space = 0, col = "red", xlab = "", ylab = "Count of individuals", 
            main = paste("Histogram for", col_name))
    
    dev.off()
  
  # Case that the type of the variable is logical
  }else if(mode_col == "logical"){
   
    mode_value = get_mode(column)
    
    print(paste(col_name, " mode is", mode_value))
    
    name = gsub("\\.", "_", col_name)
    
    v = hist(as.numeric(datos[[col_name]]), breaks = 2, plot=FALSE)
    
    jpeg(paste(getwd(),"/Histograms/", name, ".jpeg", sep=""))
    
    plot(v, xaxt = "n", xlab = "Possible values", ylab = "Countof individuals", 
         main = paste("Histogram", col_name), ylim = c(0, length(column)), col = "red")
    axis(1, v$mids, labels = c("FALSE", "TRUE"), tick = FALSE, padj= -1.5)
    
    dev.off()
    
  # Case that the type of the variable is neither numerical nor logical (must be character)
  }else{
    
    mode_value = get_mode(column)
    print(paste(col_name, " mode is", mode_value))
  }

}
