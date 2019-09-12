require(shiny)
require(shinythemes)

is.valid <- function(x) {
    !(is.null(x) || is.na(x) || x == "")
}
options(shiny.autoreload.interval = 1000)
shinyServer(function(input, output, session) {
    
    update    <- reactiveValues(Z = F, "T" = F)

    observeEvent(input$I_Z_stat, { # stat -> quantile
        cat("\ninput s changed\n")
        if (update$Z) { update$Z <- F ; cat(" it's updating\n") } else {
            if (is.valid(input$I_Z_stat)) {
                cat(" it's not updateing\n")
                update$Z <- T
                cat(" mark as updating")
                updateNumericInput(session, "I_Z_quantile",
                                   value = pnorm(input$I_Z_stat))
                cat(" change q\n")
            }
        }
    })
    observeEvent(input$I_Z_quantile, { # quantile -> stat
        cat("\ninput q changed\n")
        if (update$Z) { update$Z <- F ; cat(" it's updating\n") } else {
            if (is.valid(input$I_Z_quantile)) {
                cat(" it's not updateing\n")
                update$Z <- T
                cat(" mark as updating")
                updateNumericInput(session, "I_Z_stat",
                                   value = qnorm(input$I_Z_quantile))
                cat(" change s\n")
            }
        }
    })
    
    
    output$O_Z_distribution <- renderPlot({
        z <- input$I_Z_stat
        if (is.valid(z)) {
            marg <- max(4, abs(z) + 1)
            x <- seq(-marg, marg, by = 0.01) ; y <- dnorm(x)
            xz <- x[x <= z]                  ; yz <- y[x <= z]
            xz <- c(xz, xz[length(xz)])      ; yz <- c(yz, 0)  
            plot(x, y, type = "l")
            polygon(xz, yz, col = "grey70")
            }
    })
})