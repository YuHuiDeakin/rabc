#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(umap)
library(tidyverse)

df_time <- read.csv("df_time.csv", header = T, stringsAsFactors = F)
df_time <- df_time[, -1]
df_freq <- read.csv("df_freq.csv", header = T, stringsAsFactors = F)
df_freq <- df_freq[, -1]
label_vec <- read.csv("label_vec.csv", header = T, stringsAsFactors = F)
label_vec <- label_vec$x

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "UMAP visualization",
  tabPanel("UMAP calculation and tuning", icon = icon("dove", "font-awesome"),
           #sidebarLayout(
           sidebarPanel(
             radioButtons(inputId = "featuretype", label = "Features to input",
                          choices = c("Time and frequency domain" = "TimeFreq",
                                      "Time domain" = "Time",
                                      "Frequency domain" = "Freq"),
                          selected = "TimeFreq"),
             hr(),
             p(strong("UMAP hyperparameter tuning")),
             p("Number of neighbours"),
             sliderInput(inputId = "n_neighbors", label = NULL,
                         min = 1, max = 100, value = 15, step = 1),
             p("Minimum distance"),
             sliderInput(inputId = "min_dist", label = NULL,
                         min = 0, max = 1, value = 0.1, step = 0.05),
             p("Distance metric"),
             selectInput(inputId = "metric", label = NULL,
                         choices = c("euclidean" = "euclidean",
                                     "manhattan" = "manhattan",
                                     "cosine" = "cosine",
                                     "pearson" = "pearson"),
                         selected = "euclidean")
           ),
           mainPanel(plotOutput("UMAP", height = "auto"))
  ),
  tabPanel("Feature visualization through UMAP", icon = icon("dove", "font-awesome"),
           sidebarPanel(
             selectInput(inputId = "whichfeature", label = "Choose a feature",
                         choices = c("x_mean" = "x_mean",
                                     "y_mean" = "y_mean",
                                     "z_mean" = "z_mean",
                                     "x_variance" = "x_variance",
                                     "y_variance" = "y_variance",
                                     "z_variance" = "z_variance",
                                     "x_sd" = "x_sd",
                                     "y_sd" = "y_sd",
                                     "z_sd" = "z_sd",
                                     "x_max" = "x_max",
                                     "y_max" = "y_max",
                                     "z_max" = "z_max",
                                     "x_min" = "x_min",
                                     "y_min" = "y_min",
                                     "z_min" = "z_min",
                                     "x_range" = "x_range",
                                     "y_range" = "y_range",
                                     "z_range" = "z_range",
                                     "ODBA" = "ODBA",
                                     "x_freqmain" = "x_freqmain",
                                     "y_freqmain" = "y_freqmain",
                                     "z_freqmain" = "z_freqmain",
                                     "x_freqamp" = "x_freqamp",
                                     "y_freqamp" = "y_freqamp",
                                     "z_freqamp" = "z_freqamp",
                                     "x_entropy" = "x_entropy",
                                     "y_entropy" = "y_entropy",
                                     "z_entropy" = "z_entropy"),
                         selected = "x_mean")
           ),
           mainPanel(plotOutput("UMAP_feature", height = "auto"))
  ),
  tabPanel("Selected features", icon = icon("dove", lib = "font-awesome"),
           sidebarPanel(
             checkboxGroupInput(inputId = "customfeatures",
                                label = "choose features",
                                choices = c("x_mean" = "x_mean",
                                            "y_mean" = "y_mean",
                                            "z_mean" = "z_mean",
                                            "x_variance" = "x_variance",
                                            "y_variance" = "y_variance",
                                            "z_variance" = "z_variance",
                                            "x_sd" = "x_sd",
                                            "y_sd" = "y_sd",
                                            "z_sd" = "z_sd",
                                            "x_max" = "x_max",
                                            "y_max" = "y_max",
                                            "z_max" = "z_max",
                                            "x_min" = "x_min",
                                            "y_min" = "y_min",
                                            "z_min" = "z_min",
                                            "x_range" = "x_range",
                                            "y_range" = "y_range",
                                            "z_range" = "z_range",
                                            "ODBA" = "ODBA",
                                            "x_freqmain" = "x_freqmain",
                                            "y_freqmain" = "y_freqmain",
                                            "z_freqmain" = "z_freqmain",
                                            "x_freqamp" = "x_freqamp",
                                            "y_freqamp" = "y_freqamp",
                                            "z_freqamp" = "z_freqamp",
                                            "x_entropy" = "x_entropy",
                                            "y_entropy" = "y_entropy",
                                            "z_entropy" = "z_entropy")),
             actionButton(inputId = "gogogo", label = "Submit")
           ),
           mainPanel(
             textOutput("txt"),
             plotOutput("UMAP_custom", height = "auto")
           )
  )
)


server <- function (input, output, session) {
  paras1 <- reactive({
    as.double(input$n_neighbors)
  })
  paras2 <- reactive({
    as.double(input$min_dist)
  })
  paras3 <- reactive({
    as.character(input$metric)
  })

  para_feature <- reactive({
    input$whichfeature
  })


  output$UMAP <- renderPlot({

    if (input$featuretype == "TimeFreq") {
      df_out <<- dplyr::bind_cols(df_time, df_freq, data.frame(label_vec))
    } else if (input$featuretype == "Time") {
      df_out <- dplyr::bind_cols(df_time, data.frame(label_vec))
    } else if (input$featuretype == "Freq") {
      df_out <- dplyr::bind_cols(df_freq, data.frame(label_vec))
    }

    umap_out <- umap::umap(as.matrix(df_out[,-dim(df_out)[2]]), n_neighbors = paras1(),
                           min_dist = paras2(), metric = paras3())
    umap_df <<- dplyr::bind_cols(as.data.frame(umap_out$layout), labels = df_out$label_vec)

    ggplot2::ggplot(umap_df, aes(x = V1, y = V2, color = labels)) +
      ggplot2::geom_point(alpha = 0.8) +
      xlab("UMAP_dimension_1") +
      ylab("UMAP_dimension_2") +
      ggplot2::scale_color_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 15, face = "bold"))
  }, height = function(){
    session$clientData$output_UMAP_width * 0.7
  })

  output$UMAP_feature <- renderPlot({
    umap_df1 <- dplyr::bind_cols(umap_df, feature = df_out[,para_feature()])

    values_0to1 <- (umap_df1$feature - min(umap_df1$feature)) / (
      max(umap_df1$feature) - min(umap_df1$feature))
    values_col <- unclass(summary(values_0to1))[-4]

    ggplot2::ggplot(umap_df1, aes(x = V1, y = V2, color = feature,
                                  shape = labels)) +
      ggplot2::geom_point() +
      xlab("UMAP_dimension_1") +
      ylab("UMAP_dimension_2") +
      ggplot2::scale_color_gradientn(colours = rainbow(8)[5:1],
                                     values = values_col) +
      theme(axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 15, face = "bold")) +
      guides(
        color = guide_colorbar(order = 1, nbin = 60, draw.ulim = T, draw.llim = T),
        shape = guide_legend(order = 0)
      )
  }, height = function(){
    session$clientData$output_UMAP_feature_width * 0.7
  })

  output$txt <- renderText({
    features <- paste(input$customfeatures, collapse = ", ")
    customfeatures <<- input$customfeatures
    paste("You chose", features)
  })

  observeEvent(input$gogogo, {
    #req(input$customfeatures)
    output$UMAP_custom <- renderPlot({
      umap_new <- umap::umap(as.matrix(df_out[,customfeatures]), metric = "manhattan")
      umap_newdf <- dplyr::bind_cols(as.data.frame(umap_new$layout),
                                     labels = df_out$label_vec)
      ggplot2::ggplot(umap_newdf, aes(x = V1, y = V2, color = labels)) +
        ggplot2::geom_point(alpha = 0.8) +
        xlab("UMAP_dimension_1") +
        ylab("UMAP_dimension_2") +
        ggplot2::scale_color_brewer(palette = "Set1") +
        theme(axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 14, face = "bold"),
              axis.title.y = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 12, face = "bold"),
              legend.title = element_text(size = 15, face = "bold"))
    }, height = function(){
      session$clientData$output_UMAP_custom_width * 0.7
    })
  })
}
# Run the application
shinyApp(ui = ui, server = server)

