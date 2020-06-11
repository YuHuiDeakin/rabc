ui <- shiny::navbarPage(
  title = "UMAP visualization",
  shiny::tabPanel("UMAP calculation and tuning", icon = shiny::icon("dove", "font-awesome"),
           #sidebarLayout(
           shiny::sidebarPanel(
             shiny::radioButtons(inputId = "featuretype", label = "Features to input",
                          choices = c("Time and frequency domain" = "TimeFreq",
                                      "Time domain" = "Time",
                                      "Frequency domain" = "Freq"),
                          selected = "TimeFreq"),
             shiny::hr(),
             shiny::p(shiny::strong("UMAP hyperparameter tuning")),
             shiny::p("Number of neighbours"),
             shiny::sliderInput(inputId = "n_neighbors", label = NULL,
                         min = 1, max = 100, value = 15, step = 1),
             shiny::p("Minimum distance"),
             shiny::sliderInput(inputId = "min_dist", label = NULL,
                         min = 0, max = 1, value = 0.1, step = 0.05),
             shiny::p("Distance metric"),
             shiny::selectInput(inputId = "metric", label = NULL,
                         choices = c("euclidean" = "euclidean",
                                     "manhattan" = "manhattan",
                                     "cosine" = "cosine",
                                     "pearson" = "pearson"),
                         selected = "euclidean")
           ),
           shiny::mainPanel(shiny::plotOutput("UMAP", height = "auto"))
  ),
  shiny::tabPanel("Feature visualization through UMAP", icon = shiny::icon("dove", "font-awesome"),
                  shiny::sidebarPanel(
                    shiny::selectInput(inputId = "whichfeature", label = "Choose a feature",
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
           shiny::mainPanel(shiny::plotOutput("UMAP_feature", height = "auto"))
  ),
  shiny::tabPanel("Selected features", icon = shiny::icon("dove", lib = "font-awesome"),
                  shiny::sidebarPanel(
                    shiny::checkboxGroupInput(inputId = "customfeatures",
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
                    shiny::actionButton(inputId = "gogogo", label = "Submit")
           ),
           shiny::mainPanel(
             shiny::textOutput("txt"),
             shiny::plotOutput("UMAP_custom", height = "auto")
           )
  )
)


server <- function(input, output, session) {
  paras1 <- shiny::reactive({
    as.double(input$n_neighbors)
  })
  paras2 <- shiny::reactive({
    as.double(input$min_dist)
  })
  paras3 <- shiny::reactive({
    as.character(input$metric)
  })

  para_feature <- shiny::reactive({
    input$whichfeature
  })


  output$UMAP <- shiny::renderPlot({

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

    ggplot2::ggplot(umap_df, ggplot2::aes(x = V1, y = V2, color = labels)) +
      ggplot2::geom_point(alpha = 0.8) +
      ggplot2::xlab("UMAP_dimension_1") +
      ggplot2::ylab("UMAP_dimension_2") +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
            axis.text.y = ggplot2::element_text(size = 12, face = "bold"),
            axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
            axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
            legend.text = ggplot2::element_text(size = 12, face = "bold"),
            legend.title = ggplot2::element_text(size = 15, face = "bold"))
  }, height = function(){
    session$clientData$output_UMAP_width * 0.7
  })

  output$UMAP_feature <- shiny::renderPlot({
    umap_df1 <- dplyr::bind_cols(umap_df, feature = df_out[,para_feature()])

    values_0to1 <- (umap_df1$feature - min(umap_df1$feature)) / (
      max(umap_df1$feature) - min(umap_df1$feature))
    values_col <- unclass(summary(values_0to1))[-4]

    ggplot2::ggplot(umap_df1, ggplot2::aes(x = V1, y = V2, color = feature,
                                  shape = labels)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_shape_manual(values = 65:(64 + length(unique(umap_df1$labels)))) +
      ggplot2::xlab("UMAP_dimension_1") +
      ggplot2::ylab("UMAP_dimension_2") +
      ggplot2::scale_color_gradientn(colours = rainbow(8)[5:1],
                                     values = values_col) +
      ggplot2:: theme(axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
            axis.text.y = ggplot2::element_text(size = 12, face = "bold"),
            axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
            axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
            legend.text = ggplot2::element_text(size = 12, face = "bold"),
            legend.title = ggplot2::element_text(size = 15, face = "bold")) +
      ggplot2::guides(
        color = guide_colorbar(order = 1, nbin = 60, draw.ulim = T, draw.llim = T),
        shape = guide_legend(order = 0)
      )
  }, height = function(){
    session$clientData$output_UMAP_feature_width * 0.7
  })

  output$txt <- shiny::renderText({
    features <- paste(input$customfeatures, collapse = ", ")
    customfeatures <<- input$customfeatures
    paste("You chose", features)
  })

  shiny::observeEvent(input$gogogo, {
    #req(input$customfeatures)
    output$UMAP_custom <- shiny::renderPlot({
      umap_new <- umap::umap(as.matrix(df_out[,customfeatures]), metric = "manhattan")
      umap_newdf <- dplyr::bind_cols(as.data.frame(umap_new$layout),
                                     labels = df_out$label_vec)
      ggplot2::ggplot(umap_newdf, ggplot2::aes(x = V1, y = V2, color = labels)) +
        ggplot2::geom_point(alpha = 0.8) +
        ggplot2::xlab("UMAP_dimension_1") +
        ggplot2::ylab("UMAP_dimension_2") +
        ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
              axis.text.y = ggplot2::element_text(size = 12, face = "bold"),
              axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
              axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
              legend.text = ggplot2::element_text(size = 12, face = "bold"),
              legend.title = ggplot2::element_text(size = 15, face = "bold"))
    }, height = function(){
      session$clientData$output_UMAP_custom_width * 0.7
    })
  })
}

#'UMAP visualization
#'
#'UMAP - Uniform Manifold Approximation and Projection. UMAP is a tool to embed
#'high dimensional data into low dimensions and preserves the global structure
#'of the data. In this package, UMAP is used to transform features (time or/and
#'frequency domain) into two dimensions.
#'
#'@details
#'There are three panels in this Shiny App, representing three functions.
#'Panel 1: "UMAP calculation and tuning" - evaluating whether ACC features represent
#'          behaviours.
#'Panel 2: "Feature visualization through UMAP" - how feature values vary across the
#'          two-dimensional UMAP plot.
#'Panel 3: "Custom features" - evaluating the performance of selected features in
#'          differentiation of ACC data.
#'
#'Before calling \code{plot_UMAP}, one should make sure that variables "df_time" (
#'time domain feature set, a data.frame or tibble), "df_freq" (frequency domain
#'feature set, a data.frame or tibble) and "label_vec" (corresponding behaviours,
#'a character vector) exist.
#'
#'@examples
#'plot_UMAP()
plot_UMAP <- function() shiny::shinyApp(ui = ui, server = server)
