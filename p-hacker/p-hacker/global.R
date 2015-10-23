##
#
# const
#
##


DV_PREFIX <- 'DV' # used to name DV
DV_ALL    <- 'DV_all' # name of average DV


##
#
# functions
#
##


# simple wrapper: formats a number in f.2 format
f2 <- function(x, digits=2, prepoint=0, skipZero=FALSE) {	
	if (skipZero == TRUE) {zero <- "."} else {zero <- "0."}
	
	if (length(dim(x)) == 2) {
		apply(x, 2, function(x2) {gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x2) , fixed=TRUE)})
	} else {
		gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x) , fixed=TRUE)
	}
}


# nicely formats a p-value
p.format <- function(x, digits=3) {
	if(is.na(x)) return("NA")
	if(x >= .1^digits) return(paste0("p = ", f2(x, digits, skipZero=TRUE)))
  return(paste0("p < ", f2(.1^digits, digits, skipZero=TRUE)))
}


# returns color for different p-value
p.color <- function(p) {
	if(p <= .05) return("#5AEB31")
	if(p <= .10) return("#EBF2D6")
	if(p <= .20) return("#F4F4F4")
	return("none")
}


# returns significance of a p-value as sequence of stars
p.stars <- function(p) {
  if(p <= .0001) return('****')
  if(p <= .001)  return('***')
  if(p <= .01)   return('**')
  if(p <= .05)   return('*')
  return ('ns')
}


# returns a named vector containing various informations about ANOVA
p.summary <- function(x) {
  sum <- summary(x)[[1]]
  p   <- sum$Pr[1]
  return(c(
    f=paste0( "F(1, ", x$df.residual, ") = ", round(sum$F[1], 2) ),
    p=p.format(p),
    color=p.color(p),
    stars=p.stars(p)
  ))
}


##
# Get rows of dataframe which are selected by a boolean column of another dataframe
#
# df      Dataframe which rows are selected
# df2     Dataframe which contains a boolean column used as a selection mask
# column  Name of the column of df2 to be used as a selection mask
# invert  Optional. If set TRUE boolean column will be negated prior to selection. Default is FALSE.
##
getSelectedRows <- function( df, df2, column, invert=FALSE ) {
  if ( invert ) {
    return( df[ which( !df2[,column] ), ] )
  }
  return( df[ which( df2[,column] ), ] )
}


##
# Get a number of rows from a dataframe
#
# df    Dataframe
# n     Number of rows to grap
##
getNRows <- function( df, n ) {
  return(df[1:n,])
}