pie_with_errorbars<-function (x,sd_x=NULL, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
col = NULL, border = NULL, lty = NULL, main = NULL, initiate_plot=TRUE,col_errorbar=NULL,...) 
{
	if (is.null(col_errorbar)) {col_errorbar = rep("black",length(x)) }
	if (is.null(sd_x)) {sd_x = rep(0,length(x))}
    if (!is.numeric(x) || any(is.na(x) | x < 0)) 
	stop("'x' values must be positive.")
    if (is.null(labels)) 
	labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    sd_x <- sd_x / sum(x)
	x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    if(initiate_plot)
	{
	plot.new()
    }
	pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) 
	xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    
	if(initiate_plot)
	{
	plot.window(xlim, ylim, "", asp = 1)
    }
	
	if (is.null(col)) 
	col <- if (is.null(density)) 
	c("white", "lightblue", "mistyrose", "lightcyan", 
	  "lavender", "cornsilk")
	else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise) 
	-2 * pi
    else 2 * pi
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
				border = border[i], col = col[i], lty = lty[i])
				
		
        P <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
            text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
				 adj = ifelse(P$x < 0, 1, 0), ...)
        }
    }
	for (i in 1L:nx) {
	
		if(sd_x[i]>0)
		{
			n <- max(2, floor(edges * dx[i]))
			P <- t2xy(seq.int(x[i+1],x[i+1]-sd_x[i],length.out = n))
			P$x = P$x*0.75
			P$y = P$y*0.75
			lines(P$x,P$y,col=col_errorbar[i])
			lines(P$x[length(P$x)]*c(0.95,1.05),P$y[length(P$y)]*c(0.95,1.05),col=col_errorbar[i])
		}
		
		
	}
    title(main = main, ...)
    invisible(NULL)
}