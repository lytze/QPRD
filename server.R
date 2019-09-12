require(shiny)
require(shinythemes)

is.valid <- function(x) {
    !(is.null(x) || is.na(x) || x == "")
}
options(shiny.autoreload.interval = 1000)
shinyServer(function(input, output, session) {
    
    update    <- reactiveValues(Z = F, "T" = F)
    
    # Normal
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
            x <- seq(-marg, marg, by = 0.01)   ; y <- dnorm(x)
            xz <- x[x <= z]                    ; yz <- y[x <= z]
            xz <- c(xz[1], xz, xz[length(xz)]) ; yz <- c(0, yz, 0)  
            plot(x, y, type = "l")
            polygon(xz, yz, col = "grey70")
            }
    })
    
    # T
    observeEvent(input$I_T_stat, { # stat -> quantile
        cat("\ninput s changed\n")
        if (update$`T`) { update$`T` <- F ; cat(" it's updating\n") } else {
            if (is.valid(input$I_T_stat)) {
                cat(" it's not updateing\n")
                update$`T` <- T
                cat(" mark as updating")
                updateNumericInput(session, "I_T_quantile",
                                   value = pt(input$I_T_stat, df = input$I_T_df))
                cat(" change q\n")
            }
        }
    })
    observeEvent(input$I_T_quantile, { # quantile -> stat
        cat("\ninput q changed\n")
        if (update$`T`) { update$`T` <- F ; cat(" it's updating\n") } else {
            if (is.valid(input$I_T_quantile)) {
                cat(" it's not updateing\n")
                update$`T` <- T
                cat(" mark as updating")
                updateNumericInput(session, "I_T_stat",
                                   value = qt(input$I_T_quantile, df = input$I_T_df))
                cat(" change s\n")
            }
        }
    })
    observeEvent(input$I_T_df, { # df
        cat("\ninput q changed\n")
        if (update$`T`) { update$`T` <- F ; cat(" it's updating\n") } else {
            if (is.valid(input$I_T_df)) {
                cat(" it's not updateing\n")
                update$`T` <- T
                cat(" mark as updating")
                updateNumericInput(session, "I_T_quantile",
                                   value = pt(input$I_T_stat, df = input$I_T_df))
                cat(" change s\n")
            }
        }
    })
    output$O_T_distribution <- renderPlot({
        t <- input$I_T_stat
        df <- input$I_T_df
        if (is.valid(t) && is.valid(df)) {
            marg <- max(4, abs(t) + 1)
            x <- seq(-marg, marg, by = 0.01)   ; y <- dt(x, df)
            xt <- x[x <= t]                    ; yt <- y[x <= t]
            xt <- c(xt[1], xt, xt[length(xt)]) ; yt <- c(0, yt, 0)  
            plot(x, y, type = "l")
            polygon(xt, yt, col = "grey70")
        }
    })
})