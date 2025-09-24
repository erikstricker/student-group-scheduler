# ==============================================================================
# STUDENT GROUP SCHEDULER SHINY APP (V5)
# A single-file, visually appealing Shiny app for generating group schedules.
# Now with intelligent algorithm switching for perfect unique pairings when g=2.
# ==============================================================================

# -- 1. LOAD REQUIRED LIBRARIES --
library(shiny)
library(shinythemes)
library(dplyr)
library(stringr)
library(DT)
library(utils) # for combn

# ==============================================================================
# -- 2. HELPER FUNCTION --
# The logic now branches: uses a perfect round-robin for g=2 (no constraints)
# and the flexible random shuffle for all other cases.
# ==============================================================================

#' Generate a daily group schedule with balanced groups and constraints.
#'
#' @param student_names_vec A character vector of unique student names.
#' @param g The maximum desired number of students per group.
#' @param num_days The number of days to generate the schedule for.
#' @param forbidden_pairs A list of character vectors, where each vector is a pair of names that should not be grouped.
#' @return A data frame containing the formatted group schedule.
generate_group_schedule <- function(student_names_vec, g, num_days, forbidden_pairs = list()) {
  
  # --- Intelligent Algorithm Switching ---
  # If group size is 2 AND there are no forbidden pairs, use the Round-Robin algorithm
  # for mathematically perfect, unique pairings. Otherwise, use the flexible shuffle method.
  if (g == 2 && length(forbidden_pairs) == 0) {
    # --- Round-Robin Algorithm for perfect unique pairings ---
    students_to_schedule <- student_names_vec
    s_eff <- length(students_to_schedule)
    
    # Add a dummy student if the number is odd to make the algorithm work
    if (s_eff %% 2 != 0) {
      students_to_schedule <- c(students_to_schedule, "__DUMMY__")
    }
    s_adj <- length(students_to_schedule)
    
    num_rounds <- s_adj - 1
    days_to_generate <- min(num_days, num_rounds)
    if (days_to_generate < 1) return(data.frame())
    
    all_schedules <- list()
    
    fixed_student <- students_to_schedule[s_adj]
    rotating_students <- students_to_schedule[1:(s_adj - 1)]
    
    for (day in 1:days_to_generate) {
      day_pairs <- list()
      day_pairs[[1]] <- c(fixed_student, rotating_students[1])
      
      if (s_adj > 2) {
        for (i in 1:((s_adj / 2) - 1)) {
          pair <- c(rotating_students[1 + i], rotating_students[length(rotating_students) - i + 1])
          day_pairs[[i + 1]] <- pair
        }
      }
      all_schedules[[day]] <- day_pairs
      
      # Rotate the list for the next day
      rotating_students <- c(rotating_students[length(rotating_students)], rotating_students[-length(rotating_students)])
    }
    
    # --- Formatting for Round-Robin ---
    num_groups <- s_adj / 2
    schedule_df <- data.frame(matrix(NA, nrow = num_groups, ncol = days_to_generate))
    colnames(schedule_df) <- paste0("Day_", 1:days_to_generate)
    rownames(schedule_df) <- paste0("Group_", 1:num_groups)
    
    for (day in 1:days_to_generate) {
      for (group_num in 1:num_groups) {
        group <- all_schedules[[day]][[group_num]]
        group_real_students <- group[group != "__DUMMY__"] # Filter out dummy
        
        if (length(group_real_students) > 0) {
          schedule_df[group_num, day] <- str_c("(", str_c(sort(group_real_students), collapse = ", "), ")")
        }
      }
    }
    return(schedule_df)
    
  } else {
    # --- Balanced Random Shuffle Algorithm (for g > 2 or g=2 with constraints) ---
    s <- length(student_names_vec)
    forbidden_pairs_sorted <- lapply(forbidden_pairs, sort)
    
    num_groups <- ceiling(s / g)
    base_size <- floor(s / num_groups)
    extra_students <- s %% num_groups
    group_sizes <- rep(base_size, num_groups)
    if (extra_students > 0) {
      group_sizes[1:extra_students] <- base_size + 1
    }
    
    all_schedules <- list()
    
    for (day in 1:num_days) {
      valid_day_schedule_found <- FALSE
      retry_count <- 0
      
      while (!valid_day_schedule_found && retry_count < 200) {
        current_student_list <- sample(student_names_vec)
        shuffled_group_sizes <- sample(group_sizes)
        
        end_indices <- cumsum(shuffled_group_sizes)
        start_indices <- c(1, end_indices[-length(end_indices)] + 1)
        
        day_groups_proposal <- list()
        for (i in 1:num_groups) {
          day_groups_proposal[[i]] <- current_student_list[start_indices[i]:end_indices[i]]
        }
        
        is_proposal_valid <- TRUE
        for (group in day_groups_proposal) {
          if (length(group) < 2) next
          possible_pairs <- combn(group, 2, simplify = FALSE)
          possible_pairs_sorted <- lapply(possible_pairs, sort)
          
          for (pair in possible_pairs_sorted) {
            if (any(sapply(forbidden_pairs_sorted, function(forbidden) all(pair == forbidden)))) {
              is_proposal_valid <- FALSE
              break
            }
          }
          if (!is_proposal_valid) break
        }
        
        if (is_proposal_valid) {
          valid_day_schedule_found <- TRUE
          all_schedules[[day]] <- day_groups_proposal
        }
        retry_count <- retry_count + 1
      }
      
      if (!valid_day_schedule_found) {
        stop(paste("Error: Could not find a valid arrangement for Day", day, "that respects the constraints. Please try reducing constraints or regenerating."))
      }
    }
    
    schedule_df <- data.frame(matrix(NA, nrow = num_groups, ncol = num_days))
    colnames(schedule_df) <- paste0("Day_", 1:num_days)
    rownames(schedule_df) <- paste0("Group_", 1:num_groups)
    
    for (day in 1:num_days) {
      for (group_num in 1:num_groups) {
        group <- all_schedules[[day]][[group_num]]
        schedule_df[group_num, day] <- str_c("(", str_c(sort(group), collapse = ", "), ")")
      }
    }
    return(schedule_df)
  }
}

# ==============================================================================
# -- 3. USER INTERFACE (UI) --
# ==============================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel(
    div(
      icon("calendar-alt", class = "fa-2x", style = "vertical-align: middle; color: #18BC9C;"),
      span("Student Group Scheduler", style = "vertical-align: middle; font-weight: bold;")
    ),
    windowTitle = "Group Scheduler"
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #F8F9FA; border-radius: 8px;",
      h3("Settings", class = "text-primary"),
      hr(),
      
      sliderInput("num_students", "Adjust Number of Students:", min = 2, max = 200, value = 21),
      textAreaInput("student_names", "Enter Student Names (one per line):", rows = 10),
      
      numericInput("g", "Maximum Students per Group:", value = 2, min = 2, max = 50, step = 1),
      helpText("Groups will be balanced. For g=2, a perfect unique pairing schedule is generated."),
      
      textAreaInput("forbidden_pairs", "Forbidden Pairings (one pair per line, separated by ;):", 
                    placeholder = "e.g., Student_1;Student_5\nStudent_3;Student_12", rows = 4),
      
      sliderInput("num_days", "Number of Days to Schedule:", min = 1, max = 30, value = 5),
      
      actionButton("generate", "Generate Schedule", icon = icon("cogs"), class = "btn-success btn-lg btn-block"),
      
      hr(),
      p("Built with", a("R Shiny.", href = "https://shiny.posit.co/", target = "_blank"), style = "font-size: 0.9em; text-align: center;"),
      p("Created with Gemini 2.5 Pro.", style = "font-size: 0.8em; text-align: center; color: #777;")
    ),
    
    mainPanel(
      # Welcome Panel
      conditionalPanel(
        condition = "input.generate == 0",
        div(
          class = "jumbotron",
          h2("Welcome to the Group Scheduler!", align = "center"),
          p("Use the settings on the left to enter your student names and desired group structure, then click 'Generate Schedule' to create your plan.", align = "center"),
          hr(),
          p(tags$ul(
            tags$li(strong("Student Names:"), " Enter your full list of students, one per line."),
            tags$li(strong("Maximum Students per Group:"), " Define the largest size for any group."),
            tags$li(strong("Forbidden Pairings:"), " List any two students who should never be in the same group, separated by a comma."),
            tags$li(strong("Number of Days:"), " Select how many days the schedule should cover.")
          )),
          style = "background-color: #ECF0F1; border-radius: 8px;"
        )
      ),
      # Schedule Panel
      DT::dataTableOutput("schedule_table")
    )
  )
)

# ==============================================================================
# -- 4. SERVER LOGIC --
# ==============================================================================
server <- function(input, output, session) {
  
  schedule_data <- reactiveVal(NULL)
  
  observeEvent(input$num_students, {
    student_names <- paste0("Student_", 1:input$num_students, collapse = "\n")
    updateTextAreaInput(session, "student_names", value = student_names)
  }, ignoreInit = FALSE)
  
  # This observes clicks from the generate button
  observeEvent(input$generate, {
    
    student_names_vec <- str_split(input$student_names, "\n")[[1]] %>% str_trim() %>% .[. != ""]
    s <- length(student_names_vec)
    
    forbidden_pairs_list <- list()
    if (nchar(input$forbidden_pairs) > 0) {
      forbidden_raw <- str_split(input$forbidden_pairs, "\n")[[1]] %>% .[. != ""]
      forbidden_pairs_list <- lapply(forbidden_raw, function(line) {
        str_split(line, ";")[[1]] %>% str_trim()
      })
    }
    
    validate(
      need(s >= 2, "Error: Please enter at least two students."),
      need(length(unique(student_names_vec)) == s, "Error: All student names must be unique."),
      need(input$g > 1, "Error: Group size must be at least 2."),
      need(s >= input$g, "Error: Total students must be >= max group size.")
    )
    
    if(length(forbidden_pairs_list) > 0) {
      all_are_pairs <- all(sapply(forbidden_pairs_list, length) == 2)
      validate(need(all_are_pairs, "Error: Each forbidden pairing must contain exactly two names separated by a semicolon."))
      
      all_forbidden_names <- unlist(forbidden_pairs_list)
      names_are_valid <- all(all_forbidden_names %in% student_names_vec)
      validate(need(names_are_valid, "Error: A name in the forbidden list does not exist in the main student list."))
    }
    
    new_schedule <- generate_group_schedule(
      student_names_vec = student_names_vec,
      g = input$g,
      num_days = input$num_days,
      forbidden_pairs = forbidden_pairs_list
    )
    schedule_data(new_schedule)
  })
  
  output$schedule_table <- DT::renderDataTable({
    req(schedule_data())
    DT::datatable(
      schedule_data(),
      class = 'cell-border stripe hover',
      rownames = TRUE,
      filter = 'none',
      options = list(
        paging = FALSE,
        scrollY = "600px", scrollCollapse = TRUE,
        autoWidth = TRUE, scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons'
    )
  })
}

# ==============================================================================
# -- 5. RUN THE APPLICATION --
# ==============================================================================
shinyApp(ui = ui, server = server)


