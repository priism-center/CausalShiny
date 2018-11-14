# functions

idcheck <- function(matr) {
  idlist <- c()
  
  for (i in 1:ncol(matr)) {
    if (length(unique(matr[, i])) == nrow(matr)) {
      idlist <- c(idlist, names(matr)[i])
    }
  }
  
  invisible(idlist)
}

compbins <- function(x, n) {
  list(
    start = min(x),
    end = max(x),
    size = (max(x) - min(x))/n
  )
}



# Variable Confirmation
#output$variableconfirm <- renderText({
#  req(filtered)
#####
#  idcheck <- function(matr) {
#    idlist <- c()
#    
#    for (i in 1:ncol(matr)) {
#      if (length(unique(matr[, i])) == nrow(matr)) {
#        idlist <- c(idlist, names(matr)[i])
#      }
#    }
#    
#    invisible(idlist)
#  }
#  #####
#  validate(
#    idcheck(filtered())
#  )
#  paste("Variables are checked")
#})