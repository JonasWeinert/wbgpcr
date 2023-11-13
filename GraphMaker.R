# Setup dependencies
packages <- c("shiny", "bslib", "shinyWidgets", "ggplot2", "dplyr", "tidyr", "haven", "gridExtra")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(haven)  # For read_dta

# Helper functions are the same as in your code

#Colours
background_color <- rgb(76/255, 77/255, 76/255)
text_color <- "white"
bar_colors <- c("grey70", "#0170C0", "#14A14E", "#F8D241")


# Determine the significance stars based on p-value
get_significance_stars <- function(p_value) {
  ifelse(p_value < 0.01, "***",
         ifelse(p_value < 0.05, "**",
                ifelse(p_value < 0.10, "*", "")))
}

# Create a function to gather columns for each group
gather_group <- function(data, group) {
  avg_col_name <- paste0("avg", group)
  ci95_col_name <- paste0("ci95_", group)
  p_col_name <- paste0("p", group)
  
  data %>%
    mutate(lb = !!sym(avg_col_name) - !!sym(ci95_col_name),
           ub = !!sym(avg_col_name) + !!sym(ci95_col_name),
           stars = sapply(!!sym(p_col_name), get_significance_stars),
           group = group) %>%
    select(var_name, avg = !!sym(avg_col_name), lb, ub, stars, group)
}

# Custom labeller function to add line breaks based on underscores
label_wrap_custom <- function(variable, value) {
  sapply(value, function(x) gsub("\n", "\n", x))
  sapply(value, function(x) gsub("_", "\n", x))
}


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  tags$style(HTML("
        hr {
            border-top: 2px solid #333;
            margin-top: 20px;
            margin-bottom: 20px;
        }
        #.sidebar .form-group {
          #  margin-bottom: 5 px;
        }
        .sticky-sidebar {
        position: -webkit-sticky;
        position: sticky;
        top: 0;
        max-height: 100vh;
        overflow-y: auto;
        }
        .titlePanel {
            position: sticky;
            top: 0;
            z-index: 101;
            text-align: center;
            padding: 20px 0;
            background-color: #f7f7f7;
            border-bottom: 1px solid #eee;
        }        
        .creatorBox {
            background-color: #f7f7f7;
            padding: 10px;
            margin-top: 20px;
            text-align: center;
            border-radius: 5px;
        }
            #backToTopBtn {
        display: none;
        position: fixed;
        bottom: 20px;
        right: 30px;
        z-index: 99;
        border: none;
        outline: none;
        background-color: #333;
        color: white;
        cursor: pointer;
        padding: 10px 20px;
        border-radius: 4px;
    }
    #backToTopBtn:hover {
        background-color: #555;
    }
    .sticky-button-container {
        position: sticky;
        bottom: 0;
        z-index: 100;
        background-color: white; /* This ensures the button has a background and content scrolls behind it */
        padding: 10px 0;
        width: 100%;
        border-top: 1px solid #ddd; /* Optional: adds a separating line */
    }
    .scrollable-content {
        overflow-y: auto;
        max-height: calc(100vh - 60px); /* Adjust based on button height */
        padding-right: 10px; /* Optional: provides some spacing */
    }

    ")),
  tags$script(HTML("
      $(document).on('click', '#plot_button', function() {
          $('#main_tabs li a[data-value=\"Output\"]').tab('show');
      });
  ")),
  
  titlePanel("Sahel Graph Generator"),
  sidebarLayout(
    sidebarPanel(
      class = "sticky-sidebar",
      div(class = "scrollable-content",
          fileInput("datafile", "Upload your datasets (.dta format)", multiple = TRUE),
          textInput("plotTitle", "Plot Title", "Regression Output by Treatment Group (Difference to Control in %)"),
          hr(),
          selectizeInput("vars", "Select Variables", choices = NULL, multiple = TRUE),
          uiOutput("dynamic_labels_ui"),
          selectizeInput("treatmentGroups", "Select Treatment Groups", choices = c("Control", "Capital", "Psychosocial", "Full"), multiple = TRUE, selected = c("Control", "Capital", "Psychosocial", "Full")),
          code("Use '_' (underscore) to make a linebreak in the label"),
          hr(),
          textInput("yLabel", "Y Axis Label", "")
      ),
      div(class = "sticky-button-container",
          actionButton("plot_button", "Generate Plot", class = "btn-primary btn-block")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",  # Assigning an ID
        tabPanel("Variable Overview",
                 tableOutput("var_overview"),
                 tags$button("Scroll back to the top", id = "backToTopBtn"),
                 tags$script(HTML("
        // Show button when user scrolls down 20px
        window.onscroll = function() {scrollFunction()};
        function scrollFunction() {
            if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
                document.getElementById('backToTopBtn').style.display = 'block';
            } else {
                document.getElementById('backToTopBtn').style.display = 'none';
            }
        }
        // When user clicks on the button, scroll to top of document
        document.getElementById('backToTopBtn').addEventListener('click', function() {
            document.body.scrollTop = 0;
            document.documentElement.scrollTop = 0;
        });
    "))
                 
        ),
        tabPanel("Output",
                 plotOutput("plot"),
                 conditionalPanel(
                   condition = "input.vars.length > 0", # Check if at least one variable is selected
                   downloadButton("downloadPNG", "Save as PNG", class = "btn-success btn-block")
                 ),
                 div(class="creatorBox", "Found a bug? Want a new feature?", br(), tags$a(href="mailto:jweinert@worldbank.org", "jweinert@worldbank.org"))
        )
      )
    )
  )
)



server <- function(input, output, session) {
  var_to_table_mapping <- reactiveVal()  # Stores mapping of var_name to table_name
  labels <- reactiveValues()
  data_reactive <- reactive({
    req(input$datafile)
    data_list <- lapply(input$datafile$datapath, read_dta)
    data_combined <- do.call(rbind, data_list)
    return(data_combined)
  })
  
  ###
  var_data <- reactive({
    req(data_reactive())
    unique_data <- data_reactive()[, c("var_name", "var_label", "table_name", "country", "phase")]
    unique_data <- unique(unique_data)
    return(unique_data)
  })
  
  
  var_data <- reactive({
    req(data_reactive())
    data_reactive()[, c("var_name", "var_label", "table_name", "country", "phase")]
  })
  
  
  # Render the table for the "Variable Overview" tab
  output$var_overview <- renderTable({
    data <- data_reactive()
    #data$table_name <- paste0(data$country, ": ", data$table_name, ": ", data$phase)
    data_subset <- data[, c("var_name", "var_label", "table_name", "country", "phase")]
    return(data_subset)
  })
  
  observe({
    data <- data_reactive()  # Get the data
    ##
    data$table_name <- paste0(data$country, ": ", data$table_name, ": ", data$phase)
    choices_list <- setNames(data$var_name, paste0("[", data$var_name, "] (", data$table_name, ")"))
    
    # Save the mapping
    mapping <- setNames(as.list(data$table_name), data$var_name)
    var_to_table_mapping(mapping)
    
    updateSelectizeInput(session, "vars", choices = choices_list, selected = NULL)
  })
  
  data <- na.omit(data)
  attr(data, "na.action") <- NULL
  
  
  
  # Render dynamic UI elements for custom labels
  # Render dynamic UI elements for custom labels and transformations
  output$dynamic_labels_ui <- renderUI({
    req(input$vars)
    vars_selected <- input$vars
    boxes <- lapply(vars_selected, function(var) {
      list(
        h5(var),
        selectInput(paste0("transformation_", var), "Transformation", choices = c("No Transformation", "Multiply", "Divide"), selected = "No Transformation"),
        conditionalPanel(
          condition = sprintf("(input.transformation_%s == 'Multiply') || (input.transformation_%s == 'Divide')", var, var),
          numericInput(paste0("factor_", var), "Factor", value = 1)
        ),
        checkboxInput(paste0("check_", var), "Change label", TRUE),
        conditionalPanel(
          condition = sprintf("input.check_%s == true", var),
          textInput(paste0("label_", var), "New label", value = unique(var_data()$var_label[var_data()$var_name == var])[1])
        )
      )
    })
    do.call(tagList, boxes)
  })
  
  
  
  
  output$plot <- renderPlot({
    req(input$plot_button)
    data <- data_reactive()
    data$table_name <- paste0(data$country, ": ", data$table_name, ": ", data$phase)
    
    # Get table names for the selected var_names directly from the mapping
    table_names_selected <- unlist(var_to_table_mapping()[input$vars])
    
    # Filter based on var_name and table_name
    data <- data %>% filter(var_name %in% input$vars & table_name %in% table_names_selected)
    
    # Map the input selections to the numeric group identifiers
    selected_groups <- match(input$treatmentGroups, c("Control", "Capital", "Psychosocial", "Full")) - 1
    
    # Data transformation
    data_long <- bind_rows(lapply(selected_groups, gather_group, data = data))
    
    data_long <- data_long %>%
      group_by(var_name) %>%
      mutate(control_avg = sum(avg[group == 0]),
             perc_diff = (avg - control_avg) / control_avg *100) %>%
      ungroup()
    
    
    # Apply transformations on data_long
    for (var in input$vars) {
      transformation <- input[[paste0("transformation_", var)]]
      factor <- input[[paste0("factor_", var)]]
      rows_to_transform <- which(data_long$var_name == var)
      
      if (!is.null(factor) && !is.na(factor) && factor != 0) {
        if (transformation == "Multiply") {
          data_long$avg[rows_to_transform] <- data_long$avg[rows_to_transform] * factor
          data_long$lb[rows_to_transform] <- data_long$lb[rows_to_transform] * factor
          data_long$ub[rows_to_transform] <- data_long$ub[rows_to_transform] * factor
          #labels[[paste0("label_", var)]] <- paste0(input[[paste0("label_", var)]], " (multiplied by ", factor, ")")
        } else if (transformation == "Divide") {
          data_long$avg[rows_to_transform] <- data_long$avg[rows_to_transform] / factor
          data_long$lb[rows_to_transform] <- data_long$lb[rows_to_transform] / factor
          data_long$ub[rows_to_transform] <- data_long$ub[rows_to_transform] / factor
          #labels[[paste0("label_", var)]] <- paste0(input[[paste0("label_", var)]], " (divided by ", factor, ")")
        }
      }
    }
    
    # Adjust the var_name column based on user input
    data_long$var_name <- sapply(data_long$var_name, function(var) {
      if (input[[paste0("check_", var)]]) {
        if (!is.null(labels[[paste0("label_", var)]])) {
          labels[[paste0("label_", var)]]
        } else {
          input[[paste0("label_", var)]]
        }
      } else {
        var
      }
    })
    
    
    
    
    
    #calc ymin
    ymin <- 3
    ifelse(data_long$lb < 0, ymin <- min(data_long$lb)+(.1*min(data_long$lb)), ymin <- min(data_long$lb)-(.1*min(data_long$lb)))
    
    # Named vector of colors for each group with numeric identifiers as names
    group_colors <- setNames(bar_colors, c("0", "1", "2", "3"))
    
    # Map the input selections to the numeric group identifiers
    selected_groups <- match(input$treatmentGroups, c("Control", "Capital", "Psychosocial", "Full")) - 1
    
    # Filter the group_colors vector to only include the selected groups
    selected_colors <- group_colors[as.character(selected_groups)]
    
    # Create a named vector for the labels where the names are the numeric identifiers
    group_labels <- setNames(c("Control", "Capital", "Psychosocial", "Full"), c("0", "1", "2", "3"))
    
    # Extract the labels for the selected groups
    selected_labels <- group_labels[as.character(selected_groups)]
    
    
    # Plotting
    plot <- ggplot(data_long, aes(x = as.factor(group), y = avg, fill = as.factor(group))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_errorbar(data = subset(data_long, group != 0), aes(ymin = lb, ymax = ub), position = position_dodge(0.7), width = 0.2, color = text_color) +
      geom_text(aes(label = ifelse(group != 0 & stars != "", paste0(round(perc_diff, 2), "%\n", stars), NA),
                    y = ifelse(ub > avg, ub, avg)),
                position = position_dodge(width = 0.7), vjust = -0.2, size = 4, color = text_color) +
      #scale_fill_manual(values = bar_colors, labels = c("Control", "Capital", "Psychosocial", "Full")) +
      scale_fill_manual(values = selected_colors, labels = selected_labels) +
      facet_grid(. ~ var_name, scales = "free_x", space = "free_x", switch = "x", labeller = label_wrap_custom) +
      theme_minimal(base_family = "Arial", base_size = 12) +
      theme(
        plot.background = element_rect(fill = background_color),
        panel.background = element_rect(fill = background_color, color = NA),
        plot.title = element_text(color = text_color, size = 16, hjust = 0.5),
        axis.title = element_text(color = text_color, size = 14),
        axis.text = element_text(color = text_color, size = 12),
        strip.text.x = element_text(size = 14),
        strip.text = element_text(color = text_color, size = 12),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.text = element_text(color = text_color),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = text_color, linetype = "dotted", size = 0.1),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(color = text_color, size = 0.5),
        axis.line.y = element_line(color = text_color, size = 0.5),
        panel.spacing = unit(0, "lines"),
        plot.title.position = "plot",
        plot.margin = margin(1, 1, 1, 1, "cm")
      ) +
      labs(title = input$plotTitle,
           x = "", y = input$yLabel, fill = "") +
      coord_cartesian(ylim = c(ymin, max(data_long$ub) + 1)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
    
    
    print(plot)
    
  })
  
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste("plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 7)
    }
  )
}




shinyApp(ui, server)
