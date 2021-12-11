
# generic download handler

downloadMe <- function(data, name) {
  
  downloadHandler(
    filename = function() {
      paste0(name, ".csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
}