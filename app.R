
library(writexl)
library(dplyr)
library(stringr)
library(readxl)
library(lme4)
library(bslib)
library(lmerTest)
library(shiny)
library(lmerTest)
library(ggplot2)
library(rsconnect)

rsconnect::writeManifest()


PISA_dataset_mean1.2 <- read_excel("PISA_multilevel_dataset.xlsx")

# SHINY USER INTERFACE
ui <- page_sidebar(
  title = "Multilevel Modeling Dashboard: Academic Achievements among 15-Year-Olds",
  
  sidebar = sidebar(
    title = "Model Configurations",
    
    # Select Dependent Variable
    selectInput("dep_var", "Dependent Variable (Y):", 
                choices = names(PISA_dataset_mean1.2[c(13:16)]), selected = "score_sci_avg"),
    
    # Select Multiple Independent Variables (Fixed Effects)
    checkboxGroupInput("ind_vars", "Fixed Effects (X Variables):",
                       choices = names(PISA_dataset_mean1.2[-c(1:4,13:16)]), selected = c("HOMEPOS", "COGABIL","BELONG")),
    
    # Select Grouping/Clustering Variable (Random Intercept)
    selectInput("group_var", "Random Intercept Group (Cluster--Country):", 
                choices = c("CNT")),
    # Choose which X variable to use for the X-axis in the visualization
    uiOutput("plot_x_ui"),
    wellPanel(h1(
        p("Source: PISA 2025", style = "font-size: 14px;"),
        p("PI: Hyungoo Lee", style = "font-size: 12px;"))
    )
  ),
  
  # Dashboard Layout tabs
  navset_card_tab(
    nav_panel("Model Summary", 
              verbatimTextOutput("model_summary")),
    nav_panel("Visualization", 
              plotOutput("mlm_plot", height = "550px")),
    nav_panel("Variables",
              wellPanel(
                h4("Dependent Variable Definitions"),
                p(strong("score_sci_avg:"), " Average science achievement score (Numeric variable)."),
                p(strong("score_math_avg:"), " Average math achievement score (Numeric variable)."),
                p(strong("score_env_avg:"), " Average environmental awareness score (Numeric variable)."),
                p(strong("score_read_avg:"), " Average reading score (Numeric variable)."),
                h4("Independent Variable Definitions"),
                p(strong("MALE:"), " Student's gender (Categorical variable)."),
                p(strong("HOMEPOS:"), " Home possessions (Numeric variable)."),
                p(strong("HISEI:"), " Highest parental occupational status (Numeric variable)."),
                p(strong("FAMSUP:"), " Student's perception of family support (Numeric variable)."),
                p(strong("COGABIL:"), " Cognitive adaptability (Numeric variable)."),
                p(strong("SELFREG:"), " Self-regulation (Numeric variable)."),
                p(strong("BELONG:"), " Sense of belonging (Numeric variable)."),
                p(strong("DISCLISCI:"), " Disciplinary climate at school (Numeric variable).")
              )),
    nav_panel("Random Effects (Group-level)", 
              plotOutput("plot_resid"))
  )
)

# SHINY SERVER LOGIC
server <- function(input, output, session) {
  
  # Dynamic UI to pick the primary X-axis variable for graphing based on selection
  output$plot_x_ui <- renderUI({
    req(input$ind_vars)
    selectInput("plot_x", "Primary X-Axis for Graph:", choices = input$ind_vars)
  })
  
  # Reactive Multilevel Model Fitting
  fit_model <- reactive({
    req(input$dep_var, input$ind_vars, input$group_var)
    
    # Build formula string: Y ~ X1 + X2 + (1 | Group)
    fixed_part <- paste(input$ind_vars, collapse = " + ")
    formula_str <- paste0(input$dep_var, " ~ ", fixed_part, " + (1 | ", input$group_var, ")")
    
    # Run linear mixed effects model
    lmer(as.formula(formula_str), data = PISA_dataset_mean1.2)
  })
  
  # Output 1: Statistical Summary Table
  output$model_summary <- renderPrint({
    summary(fit_model())
  })
  
  # Output 2: Interactive Fitted Lines Visualizations by Group
  output$mlm_plot <- renderPlot({
    req(input$plot_x)
    model <- tryCatch(fit_model(), error = function(e) NULL)
    req(model)
    
    # Extract predictions from the model to capture fixed + random effect shifts
    plot_df <- PISA_dataset_mean1.2
    plot_df$Predicted <- predict(model)
    
    # Create the plot mapping the raw points alongside group-specific regression slopes
    ggplot(plot_df, aes_string(x = input$plot_x, y = input$dep_var, color = input$group_var)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = Predicted), size = 1) +
      labs(
        title = paste("Multilevel Model Fits Across Clusters"),
        subtitle = paste("Fitted lines represent unique random intercepts per", input$group_var),
        x = input$plot_x,
        y = input$dep_var
      ) +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  output$plot_resid <- renderPlot({
    model <- tryCatch(fit_model(), error = function(e) NULL)
    req(model)
    p <- as.data.frame(lme4::ranef(model))
    
    ggplot(p, aes(x = condval, y = reorder(grp, condval))) +
      geom_errorbarh(aes(xmin = condval - 1.96 * condsd, 
                         xmax = condval + 1.96 * condsd), height = 0, color = "gray40") +
      geom_point(color = "blue", size = 2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      facet_wrap(~ term, scales = "free_x") +
      labs(x = "Random Effect Value (Conditional Mode)", 
           y = "Group Level", 
           title = "Caterpillar Plot of Random Effects") +
      theme_bw()
    # 3. Convert to a data frame for ggplot2 using as.data.frame()
  })
}

# RUN APPLICATION
shinyApp(ui = ui, server = server)
