#
# This is a Shiny web application.
#
library("shiny")
library("shinydashboard")
library("shinyjs")
library("shinyDTC")

distance <- function(x, y, method="euclidean") {
  n <- nrow(x)
  k <- nrow(y)
  as.matrix(dist(rbind(y, x), method=method))[k+1:n,1:k]
}

gettext <- function(txt) { txt } # Currently only support for English

# data preparation
file <- getOption("shinyDTC.data")
x <- try({ readRDS(file) }, silent = TRUE)
if (inherits(x, "try-error") || !is.data.frame(x) || (nrow(x)==0) || (ncol(x)!=2)) x <- faithful[sample(nrow(faithful), 25),]
#stopifnot("'x' should be a data frame"=is.data.frame(x))
#x <- x[, sapply(x, is.numeric)]
#stopifnot("'x' should have two colums"=(ncol(x)==2))
x  <- scale(x[complete.cases(x),]) # delete all NA's and standardize
mx <- max(abs(x))
#
schoices <- gettext(c("K-Means", "K-Median", "K-Means++", "K-Medoid"))
choices  <- structure(as.list(seq_along(schoices)), names=schoices)

ui <- dashboardPage(
  dashboardHeader(title="MM*Stat",
                  tags$li(
                    class = "dropdown",
                    tags$div(
                      style = "margin-right: 15px; margin-top: 10px;",
                      actionButton("quit", "Quit")
                    ))),
  dashboardSidebar(
    useShinyjs(),
    radioButtons("method", gettext("Method"), choices),
    numericInput("ncluster", gettext("Cluster"), 2, min=2, max=10, step=1),
    timerInput("gonzales"),
    collapsed = FALSE
  ),
  dashboardBody(
    plotOutput("plot", height = "80vh")
  )
)

appenv <- new.env()

server <- function(input, output, session) {

  tr <- timer("gonzales", input, session)

  observeEvent({ input$method ; input$ncluster } , { resetTimer("gonzales") })
  observeEvent(input$quit, { stopApp() })

  output$plot <- renderPlot({
    t   <- tr()
    n   <- nrow(x)
    col <- rep("black", n)
    ncluster <- isolate(input$ncluster)
    method   <- as.integer(isolate(input$method))
    smethod  <- names(choices)[method]
    colk     <- rainbow(ncluster)
    par(mar=c(5.1, 4.1, 2.1, 2.1))
    if (method==1) { # K-Means
      if (t==0) {
        repeat {
          appenv$center <- matrix(runif(2*ncluster, -mx, mx), ncol=2)
          appenv$dist   <- NULL
          d <- distance(x, appenv$center)
          cluster <- apply(d, 1, which.min)
          if (all(table(cluster)>1)) break
        }
      } else {
        for (i in 1:ncluster) appenv$center[i,] <- apply(x[appenv$cluster==i,,drop=FALSE], 2, mean)
      }
      d <- distance(x, appenv$center)
      appenv$cluster <- apply(d, 1, which.min)
      appenv$dist    <- c(appenv$dist, sum(apply(d, 1, min)))
    }
    if (method==2) { # K-Median
      if (t==0) {
        repeat {
          appenv$center <- matrix(runif(2*ncluster, -mx, mx), ncol=2)
          appenv$dist   <- appenv$sdist <- NULL
          d <- distance(x, appenv$center, method = "manhattan")
          cluster <- apply(d, 1, which.min)
          if (all(table(cluster)>1)) break
        }
      } else {
        for (i in 1:ncluster) appenv$center[i,] <- apply(x[appenv$cluster==i,,drop=FALSE], 2, median)
      }
      d <- distance(x, appenv$center)
      appenv$cluster <- apply(d, 1, which.min)
      appenv$dist    <- c(appenv$dist, sum(apply(d, 1, min)))
    }
    if (method==3) { # K-Means++
      if (t<ncluster) {
        if (t==0) {
          appenv$center <- matrix(NA, ncol=2, nrow=ncluster)
          appenv$dist   <- NULL
          i             <- sample(n, 1)
        } else {
          d <- distance(x, appenv$center)
          d <- apply(d, 1, min, na.rm=TRUE)
          i <- sample(length(d), 1, prob=d^2)  # exponential growth
        }
        appenv$center[t+1,] <- x[i,]
      } else{
        for (i in 1:ncluster) appenv$center[i,] <- apply(x[appenv$cluster==i,,drop=FALSE], 2, mean)
      }
      d              <- distance(x, appenv$center)
      appenv$cluster <- apply(d, 1, which.min)
      dc             <- sum(apply(d, 1, min, na.rm=TRUE))
      appenv$dist    <- c(appenv$dist, dc)
    }
    if (method==4) { # K-Medoid
      if (t==0) { # init
        icenter <- sample(n, ncluster)
        appenv$dist <- NULL
      } else { # swap
        icenter <- appenv$icenter
#        d       <- apply(distance(x, x[icenter,]), 1, min, na.rm=TRUE)
        icenter[sample(ncluster, 1)] <- sample(1:n, 1)
      }
      d       <- distance(x, x[icenter,])
      dc      <- sum(apply(d, 1, min))
      if (all(appenv$dist>dc)) { # if better keep the configuration
        appenv$icenter <- icenter
        appenv$center  <- x[appenv$icenter,]
        appenv$cluster <- apply(d, 1, which.min)
        appenv$dist    <- c(appenv$dist, dc)
      }
      layout(matrix(c(1,3,2,3), ncol=2), heights=c(2,1))
      plot(x, pch=19, col=colk[appenv$cluster], asp=TRUE, xlim=-c(-mx, mx), ylim=c(-mx, mx), sub=paste(smethod, "(best)"))
      points(appenv$center, pch="+", cex=2, col=colk)
      cluster <- apply(d, 1, which.min)
      plot(x, pch=19, col=colk[cluster], asp=TRUE, xlim=-c(-mx, mx), ylim=c(-mx, mx), sub=paste(smethod, "(candidate)"))
      points(x[icenter,], pch="+", cex=2, col=colk)
      plot(c(appenv$dist, dc), type="b", xlab="t", ylab="", pch=19, sub="Sum of distances to cluster centers")
    } else {
      layout(1:2, heights=c(2,1))
      plot(x, pch=19, col=colk[appenv$cluster], asp=TRUE, xlim=-c(-mx, mx), ylim=c(-mx, mx), sub=smethod)
      points(appenv$center, pch="+", cex=2, col=colk)
      plot(appenv$dist, type="b", xlab="t", ylab="", pch=19, sub="Sum of distances to cluster centers")
    }
  })
}

shinyApp(ui, server)
