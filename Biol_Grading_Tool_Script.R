# Check if librarian is installed, if not install it - local only
if (!require("librarian", quietly = TRUE)) {
  install.packages("librarian")
}

# Load librarian
library(librarian)

# Use shelf to load packages
shelf(shiny, tidyverse, viridis, patchwork)

# load packages - for publishing to web instance
# library(shiny)
# library(tidyverse)
# library(viridis)
# library(patchwork)

ui <- fluidPage(
  titlePanel("Canvas Gradebook Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Canvas Gradebook CSV",
                accept = c(".csv")),
      hr(),
      uiOutput("section_toggle"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Class Averages",
                 radioButtons("class_plot_type", "Select Plot:",
                              choices = c("Boxplots by Assignment" = "box",
                                          "Line Graph of Class Averages" = "line"),
                              selected = "box"),
                 plotOutput("class_plot", height = "600px")
        ),
        tabPanel("Individual Student",
                 selectInput("student_select", "Select Student:",
                             choices = NULL),
                 radioButtons("student_plot_type", "Select Plot:",
                              choices = c("Boxplots with Student Scores" = "box",
                                          "Student Trend with Class Average" = "line"),
                              selected = "box"),
                 plotOutput("student_plot", height = "600px")
        ),
        tabPanel("Lab Practicals",
                 fluidRow(
                   column(4, uiOutput("lab_color_ui")),
                   column(4, uiOutput("lab_groupvar_ui"))
                 ),
                 uiOutput("lab_practical_plots_ui")
        ),
        tabPanel("Export",
                 downloadButton("download_data", "Download Cleaned Gradebook"),
                 br(), br(),
                 tableOutput("export_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  processed_data <- reactiveVal(NULL)
  points_possible <- reactiveVal(NULL)
  
  # ── File upload & processing ─────────────────────────────────────────────────
  observeEvent(input$file, {
    req(input$file)
    
    raw_data <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    points_row_idx <- which(raw_data$Student == "Points Possible")
    
    if (length(points_row_idx) > 0) {
      points_df <- raw_data[points_row_idx, ]
      points_possible(points_df)
      raw_data <- raw_data[-c(1:points_row_idx), ]
    }
    
    raw_data <- raw_data %>%
      filter(!str_detect(Student, regex("test student", ignore_case = TRUE)))
    
    standard_cols <- c("Student", "ID", "SIS User ID", "SIS Login ID", "Section")
    all_cols <- names(raw_data)
    
    exclude_patterns <- c("Current Points", "Final Points", "Current Score",
                          "Unposted Current Score", "Final Score", "Unposted Final Score")
    
    assignment_cols <- all_cols[!all_cols %in% standard_cols]
    assignment_cols <- assignment_cols[!str_detect(assignment_cols,
                                                   paste(exclude_patterns, collapse = "|"))]
    
    clean_data <- raw_data %>%
      select(Student, `SIS User ID`, Section, all_of(assignment_cols))
    
    clean_data <- clean_data %>%
      separate(Student, into = c("Last_Name", "First_Name"), sep = ", ", extra = "merge") %>%
      mutate(First_Name = str_remove(First_Name, " .*$"))
    
    names(clean_data) <- str_remove(names(clean_data), " \\(\\d+\\)$")
    assignment_cols   <- str_remove(assignment_cols,   " \\(\\d+\\)$")
    
    clean_data <- clean_data %>%
      mutate(across(all_of(assignment_cols), ~as.numeric(.)))
    
    processed_data(clean_data)
    
    student_choices <- clean_data %>%
      arrange(Last_Name, First_Name) %>%
      mutate(display_name = paste(Last_Name, First_Name, sep = ", ")) %>%
      pull(display_name)
    
    updateSelectInput(session, "student_select", choices = c("", student_choices))
  })
  
  # ── Section toggle ────────────────────────────────────────────────────────────
  output$section_toggle <- renderUI({
    req(processed_data())
    if ("Section" %in% names(processed_data())) {
      radioButtons("section_view", "Section View:",
                   choices = c("Combined" = "combined",
                               "Separate by Section" = "separate"),
                   selected = "combined")
    }
  })
  
  # ── Long-format plot data ─────────────────────────────────────────────────────
  plot_data <- reactive({
    req(processed_data(), points_possible())
    
    data   <- processed_data()
    points <- points_possible()
    
    exclude_patterns <- c("Current Points", "Final Points", "Current Score",
                          "Unposted Current Score", "Final Score", "Unposted Final Score")
    assignment_cols <- setdiff(names(data), c("Last_Name", "First_Name", "SIS User ID", "Section"))
    assignment_cols <- assignment_cols[!str_detect(assignment_cols,
                                                   paste(exclude_patterns, collapse = "|"))]
    
    long_data <- data %>%
      pivot_longer(cols = all_of(assignment_cols),
                   names_to = "Assignment",
                   values_to = "Score")
    
    points_assignment_cols <- names(points)[str_detect(names(points), "\\(\\d+\\)$")]
    
    points_long <- points %>%
      select(all_of(points_assignment_cols)) %>%
      pivot_longer(cols = everything(),
                   names_to = "Assignment_Raw",
                   values_to = "Points_Possible") %>%
      mutate(Assignment    = str_remove(Assignment_Raw, " \\(\\d+\\)$"),
             Points_Possible = as.numeric(Points_Possible)) %>%
      select(Assignment, Points_Possible)
    
    long_data <- long_data %>%
      left_join(points_long, by = "Assignment") %>%
      mutate(Percentage = (Score / Points_Possible) * 100,
             Assignment = factor(Assignment, levels = unique(Assignment)))
    
    long_data
  })
  
  # ── Lab practical data ────────────────────────────────────────────────────────
  lab_data <- reactive({
    req(plot_data())
    plot_data() %>%
      filter(str_detect(Assignment, regex("practical", ignore_case = TRUE)))
  })
  
  lab_assignments <- reactive({
    req(lab_data())
    levels(droplevels(lab_data()$Assignment))
  })
  
  # ── Lab tab controls ──────────────────────────────────────────────────────────
  
  # Color-by selector: Section (if present) or none
  output$lab_color_ui <- renderUI({
    req(processed_data())
    has_section <- "Section" %in% names(processed_data())
    choices <- if (has_section) c("Section" = "Section") else c("None" = "none")
    radioButtons("lab_color_by", "Color groups by:",
                 choices = choices,
                 selected = names(choices)[1])
  })
  
  # Optional external grouping CSV upload
  output$lab_groupvar_ui <- renderUI({
    tagList(
      fileInput("lab_group_csv", "Optional: Upload grouping CSV",
                accept = ".csv",
                placeholder = "CSV with student ID + group column"),
      uiOutput("lab_group_col_ui")
    )
  })
  
  lab_group_data <- reactive({
    req(input$lab_group_csv)
    read_csv(input$lab_group_csv$datapath, show_col_types = FALSE)
  })
  
  output$lab_group_col_ui <- renderUI({
    req(lab_group_data())
    cols <- names(lab_group_data())
    tagList(
      selectInput("lab_group_id_col",  "Student ID column:",       choices = cols),
      selectInput("lab_group_var_col", "Grouping variable column:", choices = cols)
    )
  })
  
  # Merge external group variable into lab data if provided
  lab_data_merged <- reactive({
    req(lab_data())
    data <- lab_data()
    
    if (!is.null(input$lab_group_csv) &&
        !is.null(input$lab_group_id_col) &&
        !is.null(input$lab_group_var_col)) {
      
      group_df <- lab_group_data() %>%
        select(
          `SIS User ID` = all_of(input$lab_group_id_col),
          Group         = all_of(input$lab_group_var_col)
        ) %>%
        mutate(`SIS User ID` = as.character(`SIS User ID`))
      
      data <- data %>%
        mutate(`SIS User ID` = as.character(`SIS User ID`)) %>%
        left_join(group_df, by = "SIS User ID")
    }
    
    data
  })
  
  # Resolve which variable drives color/fill
  active_color_var <- reactive({
    req(input$lab_color_by)
    data <- lab_data_merged()
    
    # External group takes priority if uploaded and merged
    if (!is.null(input$lab_group_csv) &&
        !is.null(input$lab_group_var_col) &&
        "Group" %in% names(data)) {
      return("Group")
    }
    
    if (input$lab_color_by == "Section" && "Section" %in% names(data)) {
      return("Section")
    }
    
    return(NULL)
  })
  
  # ── Helper: build one density | boxplot | density panel ──────────────────────
  make_lab_plot <- function(data, assignment_name, color_var, section_levels) {
    
    df <- data %>%
      filter(Assignment == assignment_name, !is.na(Percentage))
    
    if (nrow(df) == 0) return(NULL)
    
    y_min <- 0
    y_max <- 100
    
    # Build color palette
    if (!is.null(color_var) && color_var %in% names(df)) {
      groups       <- sort(unique(df[[color_var]]))
      n_groups     <- length(groups)
      group_colors <- setNames(viridis(n_groups, option = "viridis"), groups)
      fill_scale   <- scale_fill_manual(values = group_colors, name = color_var)
    } else {
      fill_scale   <- scale_fill_manual(values = c("All" = viridis(1, begin = 0.4)))
    }
    
    # Decide left/right density split:
    #   - If coloring by Section and ≥2 sections → left = section 1, right = section 2
    #   - Otherwise → both sides show the full distribution (useful for other groupings)
    if (!is.null(color_var) && color_var == "Section" && length(section_levels) >= 2) {
      df_left      <- df %>% filter(Section == section_levels[1])
      df_right     <- df %>% filter(Section == section_levels[2])
      left_label   <- section_levels[1]
      right_label  <- section_levels[2]
    } else {
      df_left      <- df
      df_right     <- df
      left_label   <- "Score Density"
      right_label  <- "Score Density"
    }
    
    # ── Left density (flipped so it grows leftward) ──
    if (!is.null(color_var) && color_var %in% names(df_left)) {
      p_left <- ggplot(df_left, aes(y = Percentage, fill = .data[[color_var]]))
    } else {
      p_left <- ggplot(df_left, aes(y = Percentage, fill = "All"))
    }
    p_left <- p_left +
      geom_density(alpha = 0.7, color = "black") +
      scale_x_reverse() +
      coord_cartesian(ylim = c(y_min, y_max)) +
      fill_scale +
      theme_minimal() +
      theme(
        axis.title.x     = element_text(size = 9),
        axis.title.y     = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        legend.position  = "none",
        panel.grid.minor = element_blank()
      ) +
      labs(x = paste0(left_label, "\nScore Density"))
    
    # ── Center boxplot ──
    if (!is.null(color_var) && color_var %in% names(df)) {
      p_box <- ggplot(df, aes(x = .data[[color_var]], y = Percentage,
                              fill = .data[[color_var]])) +
        geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
        fill_scale +
        theme_minimal() +
        theme(
          axis.title.y     = element_text(size = 10),
          axis.text.x      = element_text(size = 9),
          legend.position  = "bottom",
          legend.title     = element_text(size = 9),
          legend.text      = element_text(size = 8),
          panel.grid.minor = element_blank()
        ) +
        labs(title = assignment_name,
             x     = color_var,
             y     = "Grade (%)") +
        ylim(y_min, y_max)
    } else {
      p_box <- ggplot(df, aes(x = "All", y = Percentage)) +
        geom_boxplot(fill = viridis(1, begin = 0.4), alpha = 0.8,
                     outlier.shape = 21, outlier.size = 2) +
        theme_minimal() +
        theme(
          axis.title.y     = element_text(size = 10),
          axis.text.x      = element_blank(),
          axis.ticks.x     = element_blank(),
          legend.position  = "none",
          panel.grid.minor = element_blank()
        ) +
        labs(title = assignment_name, x = NULL, y = "Grade (%)") +
        ylim(y_min, y_max)
    }
    
    # ── Right density ──
    if (!is.null(color_var) && color_var %in% names(df_right)) {
      p_right <- ggplot(df_right, aes(y = Percentage, fill = .data[[color_var]]))
    } else {
      p_right <- ggplot(df_right, aes(y = Percentage, fill = "All"))
    }
    p_right <- p_right +
      geom_density(alpha = 0.7, color = "black") +
      coord_cartesian(ylim = c(y_min, y_max)) +
      fill_scale +
      theme_minimal() +
      theme(
        axis.title.x     = element_text(size = 9),
        axis.title.y     = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        legend.position  = "none",
        panel.grid.minor = element_blank()
      ) +
      labs(x = paste0(right_label, "\nScore Density"))
    
    # Stitch with patchwork
    p_left + p_box + p_right +
      plot_layout(widths = c(1, 1.5, 1))
  }
  
  # ── Dynamic UI: one plotOutput per lab practical ──────────────────────────────
  output$lab_practical_plots_ui <- renderUI({
    req(lab_assignments())
    assignments <- lab_assignments()
    
    if (length(assignments) == 0) {
      return(p("No assignments matching 'lab' or 'practical' found in the gradebook."))
    }
    
    plot_output_list <- lapply(seq_along(assignments), function(i) {
      tagList(
        hr(),
        plotOutput(paste0("lab_plot_", i), height = "400px")
      )
    })
    
    do.call(tagList, plot_output_list)
  })
  
  # Render each plot
  observe({
    req(lab_assignments(), lab_data_merged())
    
    assignments    <- lab_assignments()
    data           <- lab_data_merged()
    color_var      <- active_color_var()
    section_levels <- if ("Section" %in% names(data)) sort(unique(data$Section)) else NULL
    
    lapply(seq_along(assignments), function(i) {
      local({
        idx         <- i
        assign_name <- assignments[idx]
        plot_id     <- paste0("lab_plot_", idx)
        
        output[[plot_id]] <- renderPlot({
          make_lab_plot(data, assign_name, color_var, section_levels)
        })
      })
    })
  })
  
  # ── Class averages plot ───────────────────────────────────────────────────────
  output$class_plot <- renderPlot({
    req(plot_data())
    
    data         <- plot_data()
    section_view <- if (is.null(input$section_view)) "combined" else input$section_view
    
    if (input$class_plot_type == "box") {
      p <- ggplot(data, aes(x = Assignment, y = Percentage, fill = Assignment)) +
        geom_boxplot() +
        scale_fill_viridis(discrete = TRUE, option = "turbo") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              legend.position = "none") +
        labs(title = "Score Distribution by Assignment",
             x = "Assignment", y = "Percentage Score") +
        ylim(0, 100)
      
      if (section_view == "separate" && "Section" %in% names(data)) {
        p <- p + facet_wrap(~ Section, ncol = 1)
      }
      
    } else {
      if (section_view == "separate" && "Section" %in% names(data)) {
        avg_data <- data %>%
          group_by(Assignment, Section) %>%
          summarise(Mean_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop") %>%
          mutate(Assignment_Num = as.numeric(Assignment))
        
        p <- ggplot(avg_data, aes(x = Assignment_Num, y = Mean_Percentage,
                                  color = Section, group = Section)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_color_viridis(discrete = TRUE)
      } else {
        avg_data <- data %>%
          group_by(Assignment) %>%
          summarise(Mean_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop") %>%
          mutate(Assignment_Num = as.numeric(Assignment))
        
        p <- ggplot(avg_data, aes(x = Assignment_Num, y = Mean_Percentage)) +
          geom_line(color = viridis(1), size = 1.2) +
          geom_point(color = viridis(1), size = 3)
      }
      
      p <- p +
        theme_minimal() +
        theme(legend.position = "top") +
        labs(title = "Class Average Performance Over Time",
             x = "Assignment Number", y = "Mean Percentage Score") +
        ylim(0, 100)
    }
    
    p
  })
  
  # ── Individual student plot ───────────────────────────────────────────────────
  output$student_plot <- renderPlot({
    req(input$student_select, input$student_select != "", plot_data())
    
    name_parts <- str_split(input$student_select, ", ")[[1]]
    last_name  <- name_parts[1]
    first_name <- name_parts[2]
    
    data         <- plot_data()
    student_data <- data %>% filter(Last_Name == last_name, First_Name == first_name)
    
    if (input$student_plot_type == "box") {
      p <- ggplot(data, aes(x = Assignment, y = Percentage)) +
        geom_boxplot(aes(fill = Assignment), alpha = 0.7) +
        geom_point(data = student_data, aes(x = Assignment, y = Percentage),
                   color = "black", size = 4, shape = 18) +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              legend.position = "none") +
        labs(title = paste("Performance:", input$student_select),
             subtitle = "Student scores shown as diamonds",
             x = "Assignment", y = "Percentage Score") +
        ylim(0, 100)
      
    } else {
      class_avg <- data %>%
        group_by(Assignment) %>%
        summarise(Mean_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop") %>%
        mutate(Assignment_Num = as.numeric(Assignment))
      
      student_trend <- student_data %>%
        mutate(Assignment_Num = as.numeric(Assignment))
      
      p <- ggplot() +
        geom_line(data = class_avg,
                  aes(x = Assignment_Num, y = Mean_Percentage),
                  color = "gray50", size = 1, linetype = "dashed") +
        geom_point(data = class_avg,
                   aes(x = Assignment_Num, y = Mean_Percentage),
                   color = "gray50", size = 2) +
        geom_line(data = student_trend,
                  aes(x = Assignment_Num, y = Percentage),
                  color = viridis(1, begin = 0.7), size = 1.2) +
        geom_point(data = student_trend,
                   aes(x = Assignment_Num, y = Percentage),
                   color = viridis(1, begin = 0.7), size = 3) +
        theme_minimal() +
        labs(title    = paste("Trend Analysis:", input$student_select),
             subtitle = "Dashed line = Class Average",
             x = "Assignment Number", y = "Percentage Score") +
        ylim(0, 100)
    }
    
    p
  })
  
  # ── Export ────────────────────────────────────────────────────────────────────
  final_grades <- reactive({
    req(processed_data(), points_possible())
    
    data   <- processed_data()
    points <- points_possible()
    
    exclude_patterns <- c("Current Points", "Final Points", "Current Score",
                          "Unposted Current Score", "Final Score", "Unposted Final Score")
    assignment_cols <- setdiff(names(data), c("Last_Name", "First_Name", "SIS User ID", "Section"))
    assignment_cols <- assignment_cols[!str_detect(assignment_cols,
                                                   paste(exclude_patterns, collapse = "|"))]
    
    points_assignment_cols <- names(points)[str_detect(names(points), "\\(\\d+\\)$")]
    
    points_vec <- points %>%
      select(all_of(points_assignment_cols)) %>%
      unlist() %>%
      as.numeric()
    
    clean_names <- str_remove(points_assignment_cols, " \\(\\d+\\)$")
    names(points_vec) <- clean_names
    points_vec <- points_vec[names(points_vec) %in% assignment_cols]
    
    final_data <- data %>%
      rowwise() %>%
      mutate(
        Total_Points_Earned   = sum(c_across(all_of(assignment_cols)), na.rm = TRUE),
        Total_Points_Possible = sum(points_vec[assignment_cols], na.rm = TRUE),
        Final_Score_Pct = (Total_Points_Earned / Total_Points_Possible) * 100,
        Letter_Grade = case_when(
          is.na(Final_Score_Pct) | is.infinite(Final_Score_Pct) ~ "N/A",
          Final_Score_Pct >= 90 ~ "A",
          Final_Score_Pct >= 87 ~ "B+",
          Final_Score_Pct >= 80 ~ "B",
          Final_Score_Pct >= 77 ~ "C+",
          Final_Score_Pct >= 70 ~ "C",
          Final_Score_Pct >= 67 ~ "D+",
          Final_Score_Pct >= 60 ~ "D",
          TRUE ~ "F"
        ),
        Points_Earned = round(Total_Points_Earned, 1)
      ) %>%
      ungroup()
    
    # Build dynamic column name from the total points possible (constant across rows)
    total_pts       <- round(sum(points_vec, na.rm = TRUE), 1)
    points_col_name <- paste0("Points (out of ", total_pts, ")")
    
    final_data %>%
      rename(!!points_col_name := Points_Earned) %>%
      select(Last_Name, First_Name, `SIS User ID`,
             `Final Score (%)` = Final_Score_Pct,
             all_of(points_col_name), Letter_Grade) %>%
      arrange(Last_Name, First_Name) %>%
      mutate(`Final Score (%)` = round(`Final Score (%)`, 2))
  })
  
  output$export_preview <- renderTable({
    req(final_grades())
    head(final_grades(), 10)
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("cleaned_gradebook_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(final_grades(), file)
  )
}

shinyApp(ui = ui, server = server)