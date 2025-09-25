# ==============================================================================
# STUDENT GROUP SCHEDULER SHINY APP (V8)
# A single-file, visually appealing Shiny app for generating group schedules.
# Now with a greedy algorithm for g=3 to maximize pairing variety.
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
# The logic now has four branches:
# 1. Round-robin for g=2 (perfect pairs)
# 2. Greedy heuristic for g=3 (minimized pair repeats)
# 3. Round-robin -> combination for g=4 (structured foursomes)
# 4. Random shuffle for any case with forbidden pairs.
# ==============================================================================

#' Generate a daily group schedule with balanced groups and constraints.
#'
#' @param student_names_vec A character vector of unique student names.
#' @param g The desired number of students per group (2, 3, or 4).
#' @param num_days The number of days to generate the schedule for.
#' @param forbidden_pairs A list of character vectors with forbidden pairs.
#' @return A data frame containing the formatted group schedule.
generate_group_schedule <- function(student_names_vec, g, num_days, forbidden_pairs = list()) {
  
  g <- as.numeric(g) # Ensure g is numeric for comparisons
  
  # --- Intelligent Algorithm Switching ---
  if (g == 2 && length(forbidden_pairs) == 0) {
    # --- Round-Robin Algorithm for perfect unique pairings ---
    students_to_schedule <- student_names_vec
    if (length(students_to_schedule) %% 2 != 0) {
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
          day_pairs[[i + 1]] <- c(rotating_students[1 + i], rotating_students[length(rotating_students) - i + 1])
        }
      }
      all_schedules[[day]] <- day_pairs
      rotating_students <- c(rotating_students[length(rotating_students)], rotating_students[-length(rotating_students)])
    }
    
    num_groups <- s_adj / 2
    schedule_df <- data.frame(matrix(NA, nrow = num_groups, ncol = days_to_generate))
    colnames(schedule_df) <- paste0("Day_", 1:days_to_generate)
    rownames(schedule_df) <- paste0("Group_", 1:num_groups)
    
    for (day in 1:days_to_generate) {
      for (group_num in 1:num_groups) {
        group <- all_schedules[[day]][[group_num]]
        group_real <- group[group != "__DUMMY__"]
        if (length(group_real) > 0) {
          schedule_df[group_num, day] <- str_c("(", str_c(sort(group_real), collapse = ", "), ")")
        }
      }
    }
    return(schedule_df)
    
  } else if (g == 4 && length(forbidden_pairs) == 0) {
    # --- New Round-Robin -> Combination Algorithm for groups of 4 ---
    students_to_schedule <- student_names_vec
    if (length(students_to_schedule) %% 2 != 0) {
      students_to_schedule <- c(students_to_schedule, "__DUMMY__")
    }
    s_adj <- length(students_to_schedule)
    
    num_rounds <- s_adj - 1
    days_to_generate <- min(num_days, num_rounds)
    if (days_to_generate < 1) return(data.frame())
    
    all_pair_schedules <- list()
    fixed_student <- students_to_schedule[s_adj]
    rotating_students <- students_to_schedule[1:(s_adj - 1)]
    
    for (day in 1:days_to_generate) {
      day_pairs <- list()
      day_pairs[[1]] <- c(fixed_student, rotating_students[1])
      if (s_adj > 2) {
        for (i in 1:((s_adj / 2) - 1)) {
          day_pairs[[i + 1]] <- c(rotating_students[1 + i], rotating_students[length(rotating_students) - i + 1])
        }
      }
      all_pair_schedules[[day]] <- day_pairs
      rotating_students <- c(rotating_students[length(rotating_students)], rotating_students[-length(rotating_students)])
    }
    
    num_final_groups <- ceiling(s_adj / 4)
    schedule_df <- data.frame(matrix(NA, nrow = num_final_groups, ncol = days_to_generate))
    colnames(schedule_df) <- paste0("Day_", 1:days_to_generate)
    rownames(schedule_df) <- paste0("Group_", 1:num_final_groups)
    
    for (day in 1:days_to_generate) {
      day_pairs <- all_pair_schedules[[day]]
      num_pairs <- length(day_pairs)
      for (i in 1:num_final_groups) {
        p1_idx <- i
        p2_idx <- i + floor(num_pairs / 2)
        if (p2_idx > num_pairs) {
          group <- day_pairs[[p1_idx]]
        } else {
          group <- c(day_pairs[[p1_idx]], day_pairs[[p2_idx]])
        }
        group_real <- group[group != "__DUMMY__"]
        if (length(group_real) > 0) {
          schedule_df[i, day] <- str_c("(", str_c(sort(group_real), collapse = ", "), ")")
        }
      }
    }
    return(schedule_df)
    
  } else if (g == 3 && length(forbidden_pairs) == 0) {
    # --- New Greedy Heuristic Algorithm for groups of 3 to minimize pair repeats ---
    s <- length(student_names_vec)
    pair_counts <- matrix(0, nrow = s, ncol = s, dimnames = list(student_names_vec, student_names_vec))
    all_schedules <- list()
    
    for (day in 1:num_days) {
      available_students <- student_names_vec
      day_groups <- list()
      
      while(length(available_students) >= 3) {
        s1 <- available_students[1]
        potential_s2 <- available_students[available_students != s1]
        if (length(potential_s2) == 0) break
        s1_counts <- pair_counts[s1, potential_s2]
        best_s2_candidates <- potential_s2[s1_counts == min(s1_counts)]
        s2 <- sample(best_s2_candidates, 1)
        
        potential_s3 <- available_students[!available_students %in% c(s1, s2)]
        if (length(potential_s3) == 0) break
        s3_scores <- pair_counts[s1, potential_s3] + pair_counts[s2, potential_s3]
        best_s3_candidates <- potential_s3[s3_scores == min(s3_scores)]
        s3 <- sample(best_s3_candidates, 1)
        
        new_group <- c(s1, s2, s3)
        day_groups[[length(day_groups) + 1]] <- new_group
        
        pair_counts[s1, s2] <- pair_counts[s2, s1] <- pair_counts[s1, s2] + 1
        pair_counts[s1, s3] <- pair_counts[s3, s1] <- pair_counts[s1, s3] + 1
        pair_counts[s2, s3] <- pair_counts[s3, s2] <- pair_counts[s2, s3] + 1
        
        available_students <- available_students[!available_students %in% new_group]
      }
      
      if (length(available_students) > 0) {
        day_groups[[length(day_groups) + 1]] <- available_students
      }
      all_schedules[[day]] <- day_groups
    }
    
    max_num_groups <- max(sapply(all_schedules, length))
    schedule_df <- data.frame(matrix(NA, nrow = max_num_groups, ncol = num_days))
    colnames(schedule_df) <- paste0("Day_", 1:num_days)
    rownames(schedule_df) <- paste0("Group_", 1:max_num_groups)
    
    for (day in 1:num_days) {
      day_groups <- all_schedules[[day]]
      for (group_num in 1:length(day_groups)) {
        group <- day_groups[[group_num]]
        schedule_df[group_num, day] <- str_c("(", str_c(sort(group), collapse = ", "), ")")
      }
    }
    return(schedule_df)
    
  } else {
    # --- Fallback to Balanced Random Shuffle (for any case with constraints) ---
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
    div(icon("calendar-alt", class = "fa-2x", style = "vertical-align: middle; color: #18BC9C;"),
        span("Student Group Scheduler", style = "vertical-align: middle; font-weight: bold;")),
    windowTitle = "Group Scheduler"
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #F8F9FA; border-radius: 8px;",
      h3("Settings", class = "text-primary"),
      hr(),
      
      sliderInput("num_students", "Adjust Number of Students:", min = 2, max = 200, value = 21),
      textAreaInput("student_names", "Enter Student Names (one per line):", rows = 10),
      
      selectInput("g", "Students per Group:", choices = c("2", "3", "4"), selected = "2"),
      helpText("Sizes 2 & 4 use structured algorithms. Size 3 uses a greedy algorithm to minimize pair repeats."),
      
      textAreaInput("forbidden_pairs", "Forbidden Pairings (one pair per line, separated by ;):", 
                    placeholder = "e.g., Student_1;Student_5\nStudent_3;Student_12", rows = 4),
      
      sliderInput("num_days", "Number of Days to Schedule:", min = 1, max = 30, value = 5),
      
      actionButton("generate", "Generate Schedule", icon = icon("cogs"), class = "btn-success btn-lg btn-block"),
      
      hr(),
      p("Built with", a("R Shiny.", href = "https://shiny.posit.co/", target = "_blank"), style = "font-size: 0.9em; text-align: center;"),
      p("Created with Gemini 2.5 Pro.", style = "font-size: 0.8em; text-align: center; color: #777;")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.generate == 0",
        div(
          class = "jumbotron",
          h2("Welcome to the Group Scheduler!", align = "center"),
          p("Use the settings on the left to enter your student names and desired group structure, then click 'Generate Schedule' to create your plan.", align = "center"),
          hr(),
          p(tags$ul(
            tags$li(strong("Student Names:"), " Enter your full list of students, one per line."),
            tags$li(strong("Students per Group:"), " Select the desired group size."),
            tags$li(strong("Forbidden Pairings:"), " List any two students who should never be in the same group, separated by a semicolon."),
            tags$li(strong("Number of Days:"), " Select how many days the schedule should cover.")
          )),
          style = "background-color: #ECF0F1; border-radius: 8px;"
        )
      ),
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
      need(as.numeric(input$g) > 1, "Error: Group size must be at least 2."),
      need(s >= as.numeric(input$g), "Error: Total students must be >= group size.")
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
      options = list(paging = FALSE, scrollY = "600px", scrollCollapse = TRUE,
                     autoWidth = TRUE, scrollX = TRUE, dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
      extensions = 'Buttons'
    )
  })
}

# ==============================================================================
# -- 5. RUN THE APPLICATION --
# ==============================================================================
shinyApp(ui = ui, server = server)

