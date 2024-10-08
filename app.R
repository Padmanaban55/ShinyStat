
# Load necessary libraries
library(shiny)
library(shinyjs)
library(bslib)  
library(readr)
library(readxl)
library(haven)
library(dplyr)
library(gtsummary)
library(Hmisc)
library(tibble)
library(purrr)
library(broom)
library(DT)

# Define UI for the application
ui <- fluidPage(
  useShinyjs(),
  
  theme = bs_theme(
    bg = "#f8f9fa",  
    fg = "#343a40",  
    primary = "#007bff",  
    secondary = "#6c757d",  
    base_font = font_google("Roboto")  
  ),
  
  titlePanel(
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      h2("Customizable Basic Statistics", style = "color: #007bff; font-weight: bold;"),
      div(
        style = "display: flex; gap: 10px;",
        actionButton("generate_gtsummary", "Generate gtsummary Table", class = "btn btn-primary"),
        actionButton("generate_correlation", "Generate Correlation Matrix", class = "btn btn-primary"),
        actionButton("run_regression", "Run Regression", class = "btn btn-primary")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #ffffff; padding: 20px; border-radius: 10px;",
      
      fileInput("file", "Upload your data file (CSV, Excel, or SPSS)", 
                accept = c(".csv", ".xls", ".xlsx", ".sav")),
      
            # New checkbox to display data preview
      checkboxInput("show_preview", "Show Data Preview", value = FALSE),
      
      h4("gtsummary Table Options", style = "color: #007bff; margin-top: 20px;"),
      uiOutput("group_var_select"),
      uiOutput("var_select"),
      fluidRow(
        column(6,
               actionButton("select_all_vars", "Select All", class = "btn btn-secondary btn-sm", 
                            style = "width: 100%; margin-top: 5px;")
        ),
        column(6,
               actionButton("unselect_all_vars", "Unselect All", class = "btn btn-secondary btn-sm", 
                            style = "width: 100%; margin-top: 5px;")
        )
      ),
      
      radioButtons("summary_stat", "Summary Statistics for Continuous Variables:",
                   choices = list("Mean and Standard Deviation" = "mean_sd",
                                  "Median and IQR" = "median_iqr"),
                   selected = "mean_sd"),
      
      # New UI element for paired tests
      radioButtons("test_type", "Test Type:",
                   choices = list(
                     "Independent" = "independent",
                     "Paired" = "paired"
                   ),
                   selected = "independent"),
      
      # Conditional UI for ID variable when paired tests are selected
      conditionalPanel(
        condition = "input.test_type == 'paired'",
        uiOutput("id_var_select")
      ),
      
       
      # Updated test choices to include paired options
      radioButtons("test_choice", "Statistical Test for Comparisons:",
                   choices = list(
                     "t-test" = "t.test", 
                     "Paired t-test" = "paired.t.test",
                     "Wilcoxon test" = "wilcox.test", 
                     "Wilcoxon signed rank test" = "wilcox.test.paired",
                     "ANOVA" = "aov", 
                     "Kruskal-Wallis test" = "kruskal.test"
                   ),
                   selected = "t.test"),
      
      checkboxInput("add_pvalue", "Add p-value to the summary table", value = TRUE),
      
      actionButton("toggle_override", "Show/Hide Override Options", class = "btn btn-secondary", style = "margin-bottom: 10px;"),
      
            hidden(
        div(id = "override_section",  
            tags$div(
              style = "padding: 10px; background-color: #e9ecef; border-radius: 5px;",
              checkboxGroupInput("continuous_override", "Select Continuous Variables (Override):", choices = NULL),
              checkboxGroupInput("categorical_override", "Select Categorical Variables (Override):", choices = NULL)
            )
        )
      ),
      
      actionButton("toggle_labels", "Show/Hide Label Options", 
                   class = "btn btn-secondary", style = "margin-top: 10px;"),
      
      hidden(
        div(id = "label_options_section",
            uiOutput("label_inputs")
        )
      ),
      
      h4("Correlation Matrix Options", style = "color: #007bff; margin-top: 20px;"),
      checkboxInput("correlation_matrix", "Generate Correlation Matrix", value = FALSE),
      uiOutput("correlation_var_select"),
      
      radioButtons("correlation_type", "Correlation Method:",
                   choices = list("Pearson" = "pearson", 
                                  "Spearman" = "spearman"),
                   selected = "pearson"),
      
      checkboxInput("cor_by_group", "Generate Correlation Matrix by Grouping Variable", value = FALSE),
      
      
      h4("Regression Options", style = "color: #007bff; margin-top: 20px;"),
      checkboxInput("regression_toggle", "Enable Multiple Regression", value = FALSE),
      # Conditional panel that shows additional regression options only when the toggle is on
      conditionalPanel(
        condition = "input.regression_toggle == true",
        selectInput("regression_type", "Regression Type:",
                    choices = list("Linear" = "linear", "Logistic" = "logistic"),
                    selected = "linear"),
        selectInput("dependent_var", "Select Dependent Variable:", choices = NULL),
        checkboxGroupInput("independent_vars", "Select Independent Variables:", choices = NULL),
        checkboxInput("run_by_group", "Run Regression by Grouping Variable", value = FALSE)
        ),
      
      
      actionButton("reset_btn", "Reset All", class = "btn btn-warning", style = "margin-top: 20px;")
    ),
    
    mainPanel(
      style = "background-color: #ffffff; padding: 20px; border-radius: 10px;",
      tabsetPanel(
        type = "tabs",
        # New tab for data preview
        tabPanel("Data Preview", DTOutput("data_preview")),
        tabPanel("Summary Table", tableOutput("summary_table"), downloadButton("download_summary", "Download Summary Table")),
        tabPanel("Correlation Matrix", tableOutput("cor_matrix"), downloadButton("download_correlation", "Download Correlation Matrix")),
        tabPanel("Regression Results", DTOutput("regression_results"), downloadButton("download_regression", "Download Regression Results"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    
    if (file_ext == "csv") {
      return(read_csv(input$file$datapath))
    } else if (file_ext == "xls" || file_ext == "xlsx") {
      return(read_excel(input$file$datapath))
    } else if (file_ext == "sav") {
      return(haven::read_sav(input$file$datapath))
    } else {
      stop("Unsupported file format.")
    }
  })
  
  # Data preview output
  output$data_preview <- renderDT({
    req(input$show_preview)
    req(dataset())
    datatable(head(dataset(), 10), options = list(scrollX = TRUE))
  })
  
  output$group_var_select <- renderUI({
    req(dataset())
    selectInput("group_var", "Choose Grouping Variable (optional):", 
                choices = c("None", names(dataset())), selected = "None")
  })
  
  # New UI element for ID variable
  output$id_var_select <- renderUI({
    req(dataset())
    selectInput("id_var", "Choose ID Variable for Pairing:", 
                choices = c("None", names(dataset())), selected = "None")
  })
  
  output$var_select <- renderUI({
    req(dataset())
    tagList(
      checkboxGroupInput("vars", "Select Variables to Include in Table:", 
                         choices = names(dataset()), 
                         selected = names(dataset()))
    )
  })
  
  observeEvent(input$select_all_vars, {
    req(dataset())
    updateCheckboxGroupInput(session, "vars", 
                             selected = names(dataset()))
  })
  
  observeEvent(input$unselect_all_vars, {
    req(dataset())
    updateCheckboxGroupInput(session, "vars", 
                             selected = character(0))
  })
  
  
  output$correlation_var_select <- renderUI({
    req(input$correlation_matrix)
    req(dataset())
    checkboxGroupInput("cor_vars", "Select Variables for Correlation Matrix:", 
                       choices = names(dataset()), 
                       selected = "None")
  })
  
  observe({
    req(dataset())
    var_names <- names(dataset())
    updateCheckboxGroupInput(session, "continuous_override", choices = var_names)
    updateCheckboxGroupInput(session, "categorical_override", choices = var_names)
    updateSelectInput(session, "dependent_var", choices = var_names)
    updateCheckboxGroupInput(session, "independent_vars", choices = var_names)
  })
  
  observeEvent(input$toggle_override, {
    shinyjs::toggle("override_section")
  })
  
  #####
  # Create the label inputs UI
  output$label_inputs <- renderUI({
    req(dataset())
    vars <- names(dataset())
    
    # Create a list of text inputs, one for each variable
    label_inputs <- lapply(vars, function(var) {
      textInput(
        inputId = paste0("label_", gsub("[^[:alnum:]]", "_", var)),  # sanitize variable names
        label = paste("Label for", var),
        value = var
      )
    })
    
    # Return the list of inputs wrapped in a div
    div(
      style = "padding: 10px; background-color: #e9ecef; border-radius: 5px;",
      label_inputs
    )
  })
  
  # Add observer for toggle_labels button
  observeEvent(input$toggle_labels, {
    shinyjs::toggle("label_options_section")
  })
  
##################################################################################################  
  #gt summary
 summary_table <- reactive({
    req(input$generate_gtsummary)
    
    tryCatch({
      data <- dataset()
      print("Dataset loaded")
      # Apply labels if they exist
      for (var in names(data)) {
        label_input_id <- paste0("label_", gsub("[^[:alnum:]]", "_", var))
        if (!is.null(input[[label_input_id]])) {
          attr(data[[var]], "label") <- input[[label_input_id]]
        }
      }
      
      
      continuous_vars <- input$continuous_override
      categorical_vars <- input$categorical_override
      summary_vars <- input$vars
      
      if (length(summary_vars) == 0) {
        return(data.frame(Message = "Please select at least one variable to include in the table."))
      }
      
      stat_summary <- switch(input$summary_stat,
                             "mean_sd" = list(all_continuous() ~ "{mean} ± {sd}"),
                             "median_iqr" = list(all_continuous() ~ "{median} ({p25}, {p75})"))
      
      valid_vars <- intersect(summary_vars, names(data))
      
      if (length(valid_vars) == 0) {
        return(data.frame(Message = "None of the selected variables exist in the dataset."))
      }
      
      if (input$group_var != "None" && input$group_var %in% names(data)) {
        # Before creating the table, check group variable
        group_data <- data[[input$group_var]]
        group_levels <- unique(group_data[!is.na(group_data)])
        
        print(paste("Group variable:", input$group_var))
        print(paste("Number of group levels:", length(group_levels)))
        print(paste("Group levels:", paste(group_levels, collapse = ", ")))
        
        if (length(group_levels) < 2) {
          return(data.frame(Message = "The grouping variable must have at least two levels for comparison."))
        }
        
        # Create the summary table
        table <- data %>%
          select(all_of(c(input$group_var, valid_vars))) %>%
          tbl_summary(
            by = input$group_var,
            type = list(
              all_of(continuous_vars) ~ "continuous", 
              all_of(categorical_vars) ~ "categorical"
            ),
            statistic = stat_summary,
            missing = "no"
          )
        
        if (input$add_pvalue) {
          print("Attempting to add p-values...")
          
          # Define test function based on number of group levels
          if (length(group_levels) == 2) {
            # For two groups
            test_list <- list(
              all_continuous() ~ "t.test",
              all_categorical() ~ "fisher.test"
            )
            print("Using t.test for continuous and fisher.test for categorical variables")
          } else {
            # For more than two groups
            test_list <- list(
              all_continuous() ~ "aov",
              all_categorical() ~ "chisq.test"
            )
            print("Using ANOVA for continuous and chi-square test for categorical variables")
          }
          
          # Try to add p-values with detailed error handling
          table <- tryCatch({
            table %>% 
              add_p(
                test = test_list,
                pvalue_fun = ~ style_pvalue(.x, digits = 3)
              )
          }, error = function(e) {
            print(paste("Error in add_p:", e$message))
            # Try a more basic approach if the first attempt fails
            tryCatch({
              table %>% 
                add_p(
                  test = all_continuous() ~ "t.test",
                  pvalue_fun = ~ style_pvalue(.x, digits = 3)
                )
            }, error = function(e2) {
              print(paste("Second error in add_p:", e2$message))
              return(table)  # Return table without p-values if both attempts fail
            })
          })
        }
      } else {
        table <- data %>%
          select(all_of(valid_vars)) %>%
          tbl_summary(
            type = list(
              all_of(continuous_vars) ~ "continuous", 
              all_of(categorical_vars) ~ "categorical"
            ),
            statistic = stat_summary,
            missing = "no"
          )
      }
      
      print("Table created successfully")
      return(table)
    }, error = function(e) {
      print(paste("Error occurred:", e$message))
      return(data.frame(Error = paste("An error occurred:", e$message)))
    })
  })
  
  # Output for summary table display
  output$summary_table <- renderTable({
    summary_table()
  })
  
  
  ######################
  ######### Correlation
  cor_matrix <- reactive({
    req(input$generate_correlation)
    req(input$correlation_matrix)
    data <- dataset()
    
    req(length(input$cor_vars) > 0)
    
    cor_data <- data %>%
      select(all_of(input$cor_vars)) %>%
      na.omit()
    
    if (nrow(cor_data) == 0) {
      return(NULL)
    }
    
    cor_matrix_with_p <- function(df, method) {
      cor_res <- rcorr(as.matrix(df), type = method)
      cor_coef <- round(cor_res$r, 3)
      p_values <- round(cor_res$P, 3)
      
      cor_combined <- matrix(paste(cor_coef, "\n(", p_values, ")", sep = ""),
                             nrow = nrow(cor_coef), ncol = ncol(cor_coef))
      colnames(cor_combined) <- colnames(df)
      rownames(cor_combined) <- colnames(df)
      
      cor_tibble <- as_tibble(cor_combined, .name_repair = "minimal") %>%
        rownames_to_column(var = "Variable")
      
      cor_tibble <- cor_tibble %>%
        select(Variable, everything())
      
      return(cor_tibble)
    }
    
    if (input$cor_by_group && input$group_var != "None") {
      grouped_data <- data %>%
        select(all_of(input$cor_vars), !!sym(input$group_var)) %>%
        na.omit()
      
      cor_matrix_list <- grouped_data %>%
        group_split(!!sym(input$group_var)) %>%
        map_df(~ {
          cor_matrix <- cor_matrix_with_p(select(., all_of(input$cor_vars)), method = input$correlation_type)
          group_name <- unique(.[[input$group_var]])
          cor_matrix <- mutate(cor_matrix, Group = group_name)
          return(cor_matrix)
        })
      
      return(cor_matrix_list)
      
    } else {
      return(cor_matrix_with_p(cor_data, method = input$correlation_type))
    }
  })
  
  # Output for correlation matrix display
  output$cor_matrix <- renderTable({
    cor_matrix()
  })
  
  ######################Regression
  regression_results <- reactive({
    req(input$run_regression)
    
    tryCatch({
      data <- dataset()
      
      # Validate inputs
      if (is.null(input$dependent_var) || length(input$independent_vars) == 0) {
        return(datatable(data.frame(Message = "Please select dependent and independent variables.")))
      }
      
      # Handle variable names with spaces
      dep_var <- gsub(" ", ".", input$dependent_var)  # Replace spaces with dots in column names
      ind_vars <- gsub(" ", ".", input$independent_vars)
      
      # Create a copy of the data with renamed columns
      data_renamed <- data
      names(data_renamed)[names(data_renamed) == input$dependent_var] <- dep_var
      for (var in input$independent_vars) {
        names(data_renamed)[names(data_renamed) == var] <- gsub(" ", ".", var)
      }
      
      # Create formula using renamed variables
      formula_str <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
      print(paste("Regression formula:", formula_str))
      
      # Check if the user wants to run the regression by group
      if (input$run_by_group && input$group_var != "None") {
        # Ensure the group variable is present in the dataset
        if (!(input$group_var %in% names(data))) {
          return(datatable(data.frame(Message = "The selected group variable is not in the dataset.")))
        }
        
        # Split the data by the grouping variable
        grouped_data <- split(data_renamed, data_renamed[[input$group_var]])
        
        # Iterate over each group and run the regression separately
        group_results <- purrr::map_df(grouped_data, function(group_data) {
          group_name <- unique(group_data[[input$group_var]])
          
          if (input$regression_type == "linear") {
            model <- lm(as.formula(formula_str), data = group_data)
            
            # Get summary statistics
            summary_stats <- summary(model)
            r_squared <- round(summary_stats$r.squared, 4)
            adj_r_squared <- round(summary_stats$adj.r.squared, 4)
            f_statistic <- round(summary_stats$fstatistic[1], 4)
            
            # Create results table
            results <- tidy(model) %>%
              mutate(
                term = case_when(
                  term == "(Intercept)" ~ term,
                  TRUE ~ input$independent_vars[match(term, ind_vars)]
                )
              )
            
            # Add model statistics
            model_stats <- data.frame(
              term = c("R-squared", "Adjusted R-squared", "F-statistic"),
              estimate = c(r_squared, adj_r_squared, f_statistic),
              std.error = NA,
              statistic = NA,
              p.value = NA
            )
            
            results <- bind_rows(results, model_stats)
            
          } else {  # logistic regression
            model <- glm(as.formula(formula_str), data = group_data, family = binomial())
            
            # Get summary statistics
            summary_stats <- summary(model)
            null_deviance <- round(summary_stats$null.deviance, 4)
            residual_deviance <- round(summary_stats$deviance, 4)
            aic <- round(summary_stats$aic, 4)
            
            # Create results table
            results <- tidy(model) %>%
              mutate(
                term = case_when(
                  term == "(Intercept)" ~ term,
                  TRUE ~ input$independent_vars[match(term, ind_vars)]
                )
              )
            
            # Add model statistics
            model_stats <- data.frame(
              term = c("Null deviance", "Residual deviance", "AIC"),
              estimate = c(null_deviance, residual_deviance, aic),
              std.error = NA,
              statistic = NA,
              p.value = NA
            )
            
            results <- bind_rows(results, model_stats)
          }
          
          # Add group information
          results <- results %>%
            mutate(Group = group_name) %>%
            relocate(Group, .before = term)
          
          return(results)
        })
        
        # Format p-values
        group_results <- group_results %>%
          mutate(
            estimate = round(estimate, 4),
            std.error = round(std.error, 4),
            statistic = round(statistic, 4),
            p.value = ifelse(!is.na(p.value), 
                             ifelse(p.value < 0.001, "<0.001", round(p.value, 4)), 
                             NA)
          )
        
        return(datatable(group_results))
        
      } else {
        # Regular regression for the entire dataset
        if (input$regression_type == "linear") {
          model <- lm(as.formula(formula_str), data = data_renamed)
          
          # Get summary statistics
          summary_stats <- summary(model)
          r_squared <- round(summary_stats$r.squared, 4)
          adj_r_squared <- round(summary_stats$adj.r.squared, 4)
          f_statistic <- round(summary_stats$fstatistic[1], 4)
          
          # Create results table
          results <- tidy(model) %>%
            mutate(
              term = case_when(
                term == "(Intercept)" ~ term,
                TRUE ~ input$independent_vars[match(term, ind_vars)]
              )
            )
          
          # Add model statistics
          model_stats <- data.frame(
            term = c("R-squared", "Adjusted R-squared", "F-statistic"),
            estimate = c(r_squared, adj_r_squared, f_statistic),
            std.error = NA,
            statistic = NA,
            p.value = NA
          )
          
          results <- bind_rows(results, model_stats)
          
        } else {  # logistic regression
          model <- glm(as.formula(formula_str), data = data_renamed, family = binomial())
          
          # Get summary statistics
          summary_stats <- summary(model)
          null_deviance <- round(summary_stats$null.deviance, 4)
          residual_deviance <- round(summary_stats$deviance, 4)
          aic <- round(summary_stats$aic, 4)
          
          # Create results table
          results <- tidy(model) %>%
            mutate(
              term = case_when(
                term == "(Intercept)" ~ term,
                TRUE ~ input$independent_vars[match(term, ind_vars)]
              )
            )
          
          # Add model statistics
          model_stats <- data.frame(
            term = c("Null deviance", "Residual deviance", "AIC"),
            estimate = c(null_deviance, residual_deviance, aic),
            std.error = NA,
            statistic = NA,
            p.value = NA
          )
          
          results <- bind_rows(results, model_stats)
        }
        
        # Format p-values
        results <- results %>%
          mutate(
            estimate = round(estimate, 4),
            std.error = round(std.error, 4),
            statistic = round(statistic, 4),
            p.value = ifelse(!is.na(p.value), 
                             ifelse(p.value < 0.001, "<0.001", round(p.value, 4)), 
                             NA)
          )
        
        return(datatable(results))
      }
      
    }, error = function(e) {
      print(paste("Error in regression:", e$message))
      return(datatable(data.frame(Error = paste("An error occurred:", e$message))))
    })
  })
  
  # Output for regression results display
  output$regression_results <- renderDT({
    as.data.frame(regression_results())  # Display as data table
  })
 #####Download
  
  # Download handlers for summary table, correlation matrix, and regression results
  output$download_summary <- downloadHandler(
    filename = function() { paste("summary_table", Sys.Date(), ".html", sep = "") },
    content = function(file) {
      summary_table_html <- summary_table() %>%
        as_gt() %>%
        gt::as_raw_html()  # Convert to raw HTML content
      
      # Write the HTML content to a file
      write(summary_table_html, file)
    }
  )
  
  
  
  
  output$download_correlation <- downloadHandler(
    filename = function() { paste("correlation_matrix", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      cor_matrix_data <- cor_matrix()
      if (!is.null(cor_matrix_data)) {
        write.csv(cor_matrix_data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_regression <- downloadHandler(
    filename = function() { paste("regression_results", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      regression_results_data <- regression_results()
      if (!is.null(regression_results_data)) {
        write.csv(as.data.frame(regression_results_data), file, row.names = FALSE)
      }
    }
  )

##############################  
  
  # Reset button 
  
  # Update the reset button observer in your server function
  observeEvent(input$reset_btn, {
    # Reset file input
    #reset("file") #Muting file reset
    
    # Use invalidateLater to ensure this runs after the file input is reset
    invalidateLater(100)
    
    # Reset all other inputs
    updateCheckboxInput(session, "show_preview", value = FALSE)
    updateCheckboxInput(session, "add_pvalue", value = TRUE)
    updateCheckboxInput(session, "correlation_matrix", value = FALSE)
    updateCheckboxInput(session, "regression_toggle", value = FALSE)
    updateCheckboxInput(session, "cor_by_group", value = FALSE)
    
    updateRadioButtons(session, "test_type", selected = "independent")
    updateRadioButtons(session, "summary_stat", selected = "mean_sd")
    updateRadioButtons(session, "test_choice", selected = "t.test")
    updateRadioButtons(session, "correlation_type", selected = "pearson")
    updateRadioButtons(session, "regression_type", selected = "linear")
    
    # Reset select inputs
    updateSelectInput(session, "group_var", choices = c("None"), selected = "None")
    updateSelectInput(session, "id_var", choices = c("None"), selected = "None")

    updateSelectInput(session, "dependent_var", choices = NULL)
    
   
  
    
    # Reset checkbox groups
    updateCheckboxGroupInput(session, "vars", choices = NULL, selected = NULL)
    updateCheckboxGroupInput(session, "continuous_override", choices = NULL, selected = NULL)
    updateCheckboxGroupInput(session, "categorical_override", choices = NULL, selected = NULL)
    updateCheckboxGroupInput(session, "cor_vars", choices = NULL, selected = NULL)
    updateCheckboxGroupInput(session, "independent_vars", choices = NULL, selected = NULL)

    
    # Hide sections that might be shown
    shinyjs::hide("override_section")
    shinyjs::hide("label_options_section")
    
    # Clear outputs
    output$data_preview <- renderDT(NULL)
    output$summary_table <- renderTable(NULL)
    output$cor_matrix <- renderTable(NULL)
    output$regression_results <- renderDT(NULL)
    
    

    
    # Show notification
    showNotification("All selections and outputs have been reset", type = "message")
  })
  
  # Add this reactive to handle file resets more effectively
  data <- reactive({
    input$reset_btn  # Add dependency on reset button
    req(input$file)
    
    file_ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      if (file_ext == "csv") {
        read_csv(input$file$datapath)
      } else if (file_ext == "xls" || file_ext == "xlsx") {
        read_excel(input$file$datapath)
      } else if (file_ext == "sav") {
        haven::read_sav(input$file$datapath)
      } else {
        stop("Unsupported file format.")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })
  
  # Update observers that depend on the dataset
  observe({
    req(data())
    var_names <- names(data())
    
    updateSelectInput(session, "group_var", 
                      choices = c("None", var_names), 
                      selected = "None")
    
    updateSelectInput(session, "id_var", 
                      choices = c("None", var_names), 
                      selected = "None")
    
    updateCheckboxGroupInput(session, "vars", 
                             choices = var_names, 
                             selected = "None")
    
    updateCheckboxGroupInput(session, "continuous_override", 
                             choices = var_names)
    
    updateCheckboxGroupInput(session, "categorical_override", 
                             choices = var_names)
    
    updateSelectInput(session, "dependent_var", 
                      choices = var_names)
    
    updateCheckboxGroupInput(session, "independent_vars", 
                             choices = var_names)
    
    updateCheckboxGroupInput(session, "cor_vars", 
                             choices = var_names)
   
  })
}
# Run the application 
shinyApp(ui = ui, server = server)