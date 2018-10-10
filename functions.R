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

# dataset validation
datacheck <- function(data, confound, trt, resp) {
  if (data == "") {
    "Please Upload a dataset"
  }
  
  else {
    
    if (confound == "" || trt == "" || resp == "") {
      "Please identify X, Y, Z"
    }
    
    else {
      
      if (length(unique(data[ ,which(names(data) == trt)])) > 2) {
        "Please check treatment variable selection, and/or missing values"
      }
      
      else {
        NULL
      }
    }
  }
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