source ("http://tinyurl.com/rescale-R")
rescale <- function( x, na.rm=TRUE, plot=FALSE, ...){
  rng <-range(x, na.rm=na.rm)
  
  answer <- (x - rng[1]) / (rng[2] - rng[1])
  if(plot) {
    plot(answer, ...)
  }
  return(answer)
  rescale( c(1,10,"string") )
  rescale2 <- function(x, na.rm=TRUE, plot=FALSE, ...) {
    if( !is.numeric(x) ) {
      stop("Input x should be numeric", call.=FALSE)
    }
    rng <-range(x, na.rm=na.rm)
    
    answer <- (x - rng[1]) / (rng[2] - rng[1])
    if(plot) {
      plot(answer, ...)
    }
    return(answer)
  }
  rescale2 <- function(x, na.rm=TRUE, plot=FALSE, ...) {
    if( !is.numeric(x) ) {
      stop("Input x should be numeric", call.=FALSE)
    }
    rng <-range(x, na.rm=na.rm)
    
    answer <- (x - rng[1]) / (rng[2] - rng[1])
    if(plot) {
      plot(answer, ...)
    }
    return(answer)
  }
  baz <- foo(df, v=0)
  df2 < replace_missing(df, value=0)
  x <- c( 1, 2, NA, 3, NA)
  y <- c(NA, 3, NA, 3, 4)
  [1] FALSE FALSE TRUE FALSE TRUE
  sum( is.na(x) )
  1> 2
  # Putting together!
  sum( is.na(x) & is.na(y) )
  1= 1
  x <- c( 1, 2, NA, 3, NA)
  y <- c(NA, 3, NA, 3, 4)
  sum( is.na(x) & is.na(y) )
  both_na <- function(x, y) {
    sum( is.na(x) & is.na(y) )
  }
  x <- c(NA, NA, NA)
  y1 <- c( 1, NA, NA)
  y2 <- c( 1, NA, NA, NA)
  both_na(x, y1)
  1==1
  # What will this return?
  both_na(x, y2)
  both_na2 <- function(x, y) {
    if(length(x) != length(y)) {
      stop("Input x and y should be the same length")
    }
    sum( is.na(x) & is.na(y) )
  }
  both_na3 <- function(x, y) 
    if(length(x) != length(y)) {
      stop("Input x and y should be vectors of the same length")
    }
    na.in.both <- ( is.na(x) & is.na(y) )
    na.number <- sum(na.in.both)
    na.which <- which(na.in.both)
    message("Found ", na.number, " NA's at position(s):",
            paste(na.which, collapse=", ") )
    
    return( list(number=na.number, which=na.which) )
    df1 <- data.frame(IDs=c("gene1", "gene2", "gene3"),
                      exp=c(2,1,1),
                      stringsAsFactors=FALSE)
    df2 <- data.frame(IDs=c("gene2", "gene4", "gene3", "gene5"),
                      exp=c(-2, NA, 1, 2),
                      stringsAsFactors=FALSE)
    df1 <- data.frame(IDs=c("gene1", "gene2", "gene3"),
                      exp=c(2,1,1),
                      stringsAsFactors=FALSE)
    df2 <- data.frame(IDs=c("gene2", "gene4", "gene3", "gene5"),
                      exp=c(-2, NA, 1, 2),
                      stringsAsFactors=FALSE)
    df1 <- data.frame(IDs=c("gene1", "gene2", "gene3"),
                      exp=c(2,1,1),
                      stringsAsFactors=FALSE)
    df2 <- data.frame(IDs=c("gene2", "gene4", "gene3", "gene5"),
                      exp=c(-2, NA, 1, 2),
                      stringsAsFactors=FALSE)
    # Simplify further to single vectors
    x <- df1$IDs
    y <- df2$IDs
    # Start with a simple version of the problem
    df1 <- data.frame(IDs=c("gene1", "gene2", "gene3"),
                      exp=c(2,1,1),
                      stringsAsFactors=FALSE)
    df2 <- data.frame(IDs=c("gene2", "gene4", "gene3", "gene5"),
                      exp=c(-2, NA, 1, 2),
                      stringsAsFactors=FALSE)
    # Simplify further to single vectors
    x <- df1$IDs
    y <- df2$IDs
    # Now what do we do?
    x <- df1$IDs
    y <- df2$IDs
    # Search for existing functionality to get us started...
    ??intersect
    intersect(x, y)
    #> [1] "gene2" "gene3"
    x <- df1$IDs
    y <- df2$IDs
    # Search for existing functionality to get us started...
    ??intersect
    intersect(x, y)
    #> [1] "gene2" "gene3"
    # Back to search results...
    ?intersect
    x %in% y
    #> [1] FALSE TRUE TRUE
    x %in% y
    #> [1] FALSE TRUE TRUE
    x[x %in% y]
    #> [1] "gene2" "gene3"
    x %in% y
    #> [1] FALSE TRUE TRUE
    x[x %in% y]
    #> [1] "gene2" "gene3"
    y[ y %in% x ]
    #> [1] "gene2" "gene3"
    cbind( x[ x %in% y ], y[ y %in% x ] )
    #> [,1] [,2]
    #> [1,] "gene2" "gene2"
    #> [2,] "gene3" "gene3"
    x %in% y
    #> [1] FALSE TRUE TRUE
    x[x %in% y]
    #> [1] "gene2" "gene3"
    y[ y %in% x ]
    #> [1] "gene2" "gene3"
    cbind( x[ x %in% y ], y[ y %in% x ] )
    #> [,1] [,2]
    #> [1,] "gene2" "gene2"
    #> [2,] "gene3" "gene3"
    cbind( x[ x %in% y ], y[ y %in% x ] )
    # Make this snippet into a first function
    gene_intersect <- function(x, y) {
      cbind( x[ x %in% y ], y[ y %in% x ] )
    }
    # Looks good so far but we need to work with data frames
    gene_intersect(x, y)
    #> [,1] [,2]
    #> [1,] "gene2" "gene2"
    #> [2,] "gene3" "gene3"
    cbind( x[ x %in% y ], y[ y %in% x ] )
    # Make this snippet into a first function
    gene_intersect <- function(x, y) {
      cbind( x[ x %in% y ], y[ y %in% x ] )
    }
    # Looks good so far but we need to work with data frames
    gene_intersect(x, y)
    #> [,1] [,2]
    #> [1,] "gene2" "gene2"
    #> [2,] "gene3" "gene3"
    # Lets edit to take input data frames
    gene_intersect2 <- function(df1, df2) {
      cbind( df1[ df1$IDs %in% df2$IDs, ],
             df2[ df2$IDs %in% df1$IDs, "exp"] )
    }
    # Correct but yucky format for 2nd colnames
    gene_intersect2(df1, df2)
    #> IDs exp df2[df2$IDs %in% df1$IDs, "exp"]
    #> 2 gene2 1 -2
    #> 3 gene3 1 
    # Experiment first to make sure things are as we expect
    gene.colname="IDs"
    df1[,gene.colname]
    #> [1] "gene1" "gene2" "gene3"
    gene_intersect3 <- function(df1, df2, gene.colname="IDs") {
      
      cbind( df1[ df1[,gene.colname] %in%
                    df2[,gene.colname], ],
             exp2=df2[ df2[,gene.colname] %in%
                         df1[,gene.colname], "exp"] )
      
    }
    # Works but the function is not kind on the reader
    gene_intersect3(df1, df2)
    #> IDs exp exp2
    #> 2 gene2 1 -2
    #> 3 gene3 1 1
    gene_intersect4 <- function(df1, df2, gene.colname="IDs") {
      
      df1.name <- df1[,gene.colname]
      df2.name <- df2[,gene.colname]
      
      df1.inds <- df1.name %in% df2.name
      df2.inds <- df2.name %in% df1.name
      
      cbind( df1[ df1.inds, ],
             exp2=df2[ df2.inds, "exp"] )
      df1.name <- df1[,gene.colname]
      df2.name <- df2[,gene.colname]
      
      df1.inds <- df1.name %in% df2.name
      df2.inds <- df2.name %in% df1.name
      
      cbind( df1[ df1.inds, ],
             exp2=df2[ df2.inds, "exp"] )
      gene_intersect4(df1, df2)
      #> IDs exp exp2
      #> 2 gene2 1 -2
      #> 3 gene3 1 1
      df1 <- data.frame(IDs=c("gene1", "gene2", "gene3"),
                        exp=c(2,1,1),
                        stringsAsFactors=FALSE)
      df3 <- data.frame(IDs=c("gene2", "gene2", "gene5", "gene5"),
                        exp=c(-2, NA, 1, 2),
                        stringsAsFactors=FALSE)
      # Works but could do with more spit and polish!
      gene_intersect4(df1, df3)
      
  
        
      #> IDs exp exp2
      #> 1 gene2 1 -2
      #> 2 gene2 1 NA
      #> Warning in data.frame(..., check.names = FALSE): row
   
      merge (df1, df2, by="Ids")
      #> IDs exp.x exp.y
      #> 1 gene2 1 -2
      #> 2 gene3 1 1
      
      
      # Grade Function
      
   x <-
grade <- function(x) {
  c(100,100,100,100, 100, 100, 90)
}
      (sum(x)-min(x))/7
      
    y <- c(100, 90, 90, 90, 90, 90, 97, 80)
    
   library("shiny")
    