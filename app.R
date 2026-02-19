library(shiny)
library(bslib)
library(pool)
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(shinyjs)
library(rhandsontable)
library(jsonlite)
library(glue)
library(openai)
library(tesseract)
library(pdftools)
library(shinycssloaders) # For mobile UI feedback

# 1. Load the raw data
lab_targets_raw <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
lab_targets_raw$sort_id <- 1:nrow(lab_targets_raw)

# 2. Reconstruct the exact list structure for downstream code
# This keeps the "Category Name" = c("Test1", "Test2") format
lab_config <- split(lab_targets_raw$test_name, lab_targets_raw$category)

# 3. Clean up the names (removes the index names added by split)
lab_config <- lapply(lab_config, as.character)

# Also keep a flat list for the AI
all_test_names <- lab_targets_raw$test_name

source("mod_lab_ingestion.R")

# Define the null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b
# ==============================================================================
# 1. DATABASE & CONFIGURATION
# ==============================================================================
#if (file.exists("creds.R")) source("creds.R")

ADMIN_USER <- Sys.getenv("APP_USER", unset = "admin")
ADMIN_PASS <- Sys.getenv("APP_PASS", unset = "Nephrology@1")

# ==============================================================================
# 1. DATABASE & CONFIGURATION
# ==============================================================================
pool <- tryCatch({
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = Sys.getenv("DO_DB_NAME"), 
    host     = Sys.getenv("DO_DB_HOST"),
    user     = Sys.getenv("DO_DB_USER"),
    password = Sys.getenv("DO_DB_PASSWORD"),
    port     = as.integer(Sys.getenv("DO_DB_PORT", unset = "25060")),
    sslrootcert = Sys.getenv("DO_SSLROOTCERT", unset = "ca-certificate.crt"),
    sslmode  = "require"
  )
}, error = function(e) {
  # This logs the error to DO Runtime Logs but lets the app live
  message("DB connection failed: ", e$message)
  NULL 
})

onStop(function() {
  poolClose(pool)
})


if (is.null(pool)) {
  message("No DB connection available.")
  return(NULL)
}

# ------------------------------------------------------------------------------
# Safe pooled connection wrapper
# ------------------------------------------------------------------------------
.with_conn <- function(pool, code) {
  if (is.null(pool)) stop("Database pool is NULL")
  
  if (inherits(pool, "Pool")) {
    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)
  } else {
    con <- pool
  }
  
  force(code)
}



all_test_names <- unname(unlist(lab_config))
category_names <- names(lab_config) 

# ==============================================================================
# 2. CORE HELPERS
# ==============================================================================
clean_phone <- function(phone_str) {
  if (is.null(phone_str) || is.na(phone_str)) return("")
  nums <- str_replace_all(phone_str, "[^0-9]", "")
  if (nchar(nums) == 12 && str_starts(nums, "91")) nums <- str_sub(nums, 3)
  if (nchar(nums) > 10) nums <- str_sub(nums, -10)
  return(nums)
}

parse_patient_text <- function(txt) {
  res <- list()
  res$hospital_number <- str_extract(txt, "(?i)UHID\\s*:\\s*([A-Z0-9.]+)") %>% str_remove("(?i)UHID\\s*:\\s*")
  name_match <- str_extract(txt, "(?i)(Mr\\.|Ms\\.|Mrs\\.|Master)\\s+([^.\\n]+)")
  if (!is.na(name_match)) {
    full_name <- str_remove(name_match, "(?i)(Mr\\.|Ms\\.|Mrs\\.|Master)\\s+") %>% trimws()
    parts <- str_split(full_name, "\\s+")[[1]]
    res$first_name <- parts[1]
    res$last_name <- if(length(parts) > 1) paste(parts[2:length(parts)], collapse=" ") else ""
  }
  if (str_detect(txt, "(?i)Male")) res$gender <- "male"
  else if (str_detect(txt, "(?i)Female")) res$gender <- "female"
  dob_match <- str_extract(txt, "\\d{1,2}-[A-Za-z]{3}-\\d{4}")
  if (!is.na(dob_match)) res$dob <- as.Date(dob_match, format = "%d-%b-%Y")
  res$phone <- str_extract(txt, "(?i)Phone\\s*:\\s*([0-9-]+)") %>% str_remove("(?i)Phone\\s*:\\s*")
  addr_match <- str_extract(txt, "(?i)Address\\s*:\\s*(.*?)(\\n|Phone|$)") %>% str_remove("(?i)Address\\s*:\\s*") %>% trimws()
  res$address1 <- addr_match
  return(res)
}

safe_update_input <- function(session, id, value) {
  lname <- tolower(id)
  is_empty <- is.null(value) || is.na(value) || (is.character(value) && !nzchar(trimws(value)))
  if (grepl("date|dob|birth", lname)) {
    updateDateInput(session, id, value = if (is_empty) NA else format(as.Date(value), "%Y-%m-%d"))
  } else if (grepl("gender", lname)) {
    updateRadioButtons(session, id, selected = if (is_empty) character(0) else tolower(as.character(value)))
  } else {
    updateTextInput(session, id, value = if (is_empty) "" else toupper(as.character(value)))
  }
}

# ==============================================================================
# 3. UI DEFINITION
#
# 1. Source the module file at the very top of app.R
source("mod_lab_ingestion.R")

ui <- uiOutput("main_app_container")


# ui <- page_navbar(
#   title = span(style = "font-weight: 700; color: white;", "Renal IQ - Clinical Portal"),
#   theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#26A69A"),
#   id = "main_nav",
#   
#   header = tagList(
#     useShinyjs(),
#     tags$head(tags$style(HTML("
#       .navbar-nav { width: 100%; display: flex; justify-content: flex-start; }
#       .navbar-nav .nav-link { color: black !important; font-weight: 600; font-size: 1.05rem; }
#       .navbar-nav .nav-link.active { color: white !important; background: #26A69A; border-radius: 4px; }
#       .patient-context-line { padding: 10px 20px; background: #ffffff; border-bottom: 2px solid #26A69A; }
#       /* Style for the sidebar visit tiles */
#       .visit-tile-btn { text-align: left !important; margin-bottom: 5px; }
#       /* Slick Table Customization */
# table.dataTable thead th {
#   background-color: #26A69A !important;
#   color: white !important;
#   font-weight: 500;
#   border: none !important;
#   text-transform: uppercase;
#   font-size: 0.75rem;
# }
# 
# .dataTables_scrollBody {
#   border-bottom: 1px solid #eee !important;
# }
# 
# /* Reduced cell padding for density */
# table.dataTable tbody td {
#   padding: 8px 12px !important;
#   font-family: 'Inter', sans-serif;
#   border-bottom: 1px solid #f0f0f0 !important;
# }
#     "))),
#     uiOutput("auth_screen_overlay"),
#     uiOutput("header_patient_context")
#   ),
#   
#   nav_panel("1. Registration", uiOutput("reg_ui")),
#   
#   # --- CONNECTED: Clinical Notes now uses the server-side UI ---
#   nav_panel("2. Clinical Notes", uiOutput("notes_ui")),
#   
#   nav_panel("3. Lab Entry",
#             card(
#               card_header(
#                 div(class = "d-flex justify-content-between align-items-center",
#                     span("Clinical Lab Flowsheet"),
#                     div(
#                       actionButton("open_add_lab", "Add New Date", class = "btn-success me-2", icon = icon("plus")),
#                       actionButton("rename_col_btn", "Edit Date Header", class = "btn-outline-dark me-2"),
#                       actionButton("save_flowsheet", "Save All Changes", class = "btn-primary", icon = icon("save"))
#                     ))
#               ),
#               card_body(
#                 rHandsontableOutput("history_table") %>% withSpinner(color="#26A69A")
#               )
#             )
#   ),
#   
#   nav_panel("4. Mobile Rx", uiOutput("rx_ui")),
#   
#   nav_panel("5. Lab Extraction",
#             card(
#               card_header("AI Lab Ingestion"),
#               fileInput(NS("lab_ai_module", "file_input"), "Upload PDF/Image"),
#               DTOutput(NS("lab_ai_module", "results_table"))
#             )
#   )
# )

# ==============================================================================
# 4. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # 1. AUTHENTICATION STATE
  logged_in <- reactiveVal(FALSE)
  login_error <- reactiveVal("")
  
  # 2. MAIN UI TOGGLE
  output$main_app_container <- renderUI({
    if (!logged_in()) {
      # LOGIN SCREEN VIEW
      page_fillable(
        theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#26A69A"),
        div(style = "display: flex; justify-content: center; align-items: center; height: 100vh; background-color: #f4f7f6;",
            card(width = "400px", class = "shadow-lg",
                 card_header(class = "bg-primary text-white text-center", h3("Renal IQ", class="mb-0")),
                 card_body(
                   textInput("u_id", "Username"),
                   passwordInput("u_pw", "Password"),
                   actionButton("login_btn", "Secure Login", class = "btn-primary w-100 btn-lg"),
                   uiOutput("login_err_msg")
                 )
            )
        )
      )
    } else {
      # AUTHENTICATED APP VIEW (Your Original Navbar)
      page_navbar(
        title = span(style = "font-weight: 700; color: white;", "Renal IQ - Clinical Portal"),
        theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#26A69A"),
        id = "main_nav",
        
        header = tagList(
          useShinyjs(),
          tags$head(tags$style(HTML("
            .navbar-nav { width: 100%; display: flex; justify-content: flex-start; }
            .navbar-nav .nav-link { color: black !important; font-weight: 600; font-size: 1.05rem; }
            .navbar-nav .nav-link.active { color: white !important; background: #26A69A; border-radius: 4px; }
            .patient-context-line { padding: 10px 20px; background: #ffffff; border-bottom: 2px solid #26A69A; }
          "))),
          uiOutput("header_patient_context") # Shows patient name & Logout button
        ),
        
        nav_panel("1. Registration", uiOutput("reg_ui")),
        nav_panel("2. Clinical Notes", uiOutput("notes_ui")),
        nav_panel("3. Lab Entry", 
                  card(
                    card_header(
                      div(class = "d-flex justify-content-between align-items-center",
                          span("Clinical Lab Flowsheet"),
                          div(
                            actionButton("open_add_lab", "Add New Date", class = "btn-success me-2", icon = icon("plus")),
                            actionButton("rename_col_btn", "Edit Date Header", class = "btn-outline-dark me-2"),
                            actionButton("save_flowsheet", "Save All Changes", class = "btn-primary", icon = icon("save"))
                          ))
                    ),
                    card_body(rHandsontableOutput("history_table") %>% withSpinner(color="#26A69A"))
                  )
        ),
        nav_panel("4. Mobile Rx", uiOutput("rx_ui")),
        nav_panel("5. Lab Extraction", 
                  card(
                    card_header("AI Lab Ingestion"),
                    fileInput(NS("lab_ai_module", "file_input"), "Upload PDF/Image"),
                    DTOutput(NS("lab_ai_module", "results_table"))
                  )
        )
      )
    }
  })
  
  # 3. LOGIN BUTTON LOGIC
  observeEvent(input$login_btn, {
    # Replace with your actual credentials or DB check
    if (input$u_id == "admin" && input$u_pw == "password123") {
      logged_in(TRUE)
    } else {
      login_error("Invalid username or password.")
    }
  })
  
  output$login_err_msg <- renderUI({
    req(login_error() != "")
    p(login_error(), style = "color: red; margin-top: 10px; text-align: center;")
  })
  
  # 4. PATIENT CONTEXT & LOGOUT
  output$header_patient_context <- renderUI({
    req(logged_in())
    # Content only shows if a patient is actually selected in the system
    pt_name <- if(!is.null(current_pt())) paste("Active Patient:", current_pt()$name) else "No Patient Selected"
    
    div(class = "patient-context-line d-flex justify-content-between align-items-center",
        span(style = "font-weight: 700; color: #26A69A;", pt_name),
        actionButton("logout_btn", "Logout", class = "btn-outline-danger btn-sm")
    )
  })
  
  observeEvent(input$logout_btn, {
    logged_in(FALSE)
    current_pt(NULL) # Clear patient session on logout
  })

  
  # Inside server <- function(input, output, session) { ... }
  lab_ingestion_server("lab_ai_module", pool, current_pt)
  
  output$global_controls <- renderUI({
    req(current_pt()) # Only show if a patient is selected
    div(style = "padding: 10px; background: #f8f9fa; border-bottom: 1px solid #ddd;",
        div(class = "container-fluid d-flex justify-content-between align-items-center",
            span(strong("Active Patient: "), current_pt()$name),
            actionButton("switch_patient_btn", "Switch Patient / Close File", 
                         class = "btn-outline-danger btn-sm", 
                         icon = icon("user-slash"))
        )
    )
  })
  
  
  
  # Add this inside your server function
  observeEvent(input$login_btn, {
    # Log the first 10 characters of the URL to verify it's being read
    url <- Sys.getenv("DATABASE_URL")
    message("DEBUG: URL starts with: ", substr(url, 1, 15))
    message("DEBUG: Pool status: ", !is.null(pool))
  })
  
  observeEvent(input$test_db, {
    res <- tryCatch({
      # Attempt a raw connection
      con <- dbConnect(
        RPostgres::Postgres(),
        dbname = Sys.getenv("DATABASE_URL")
      )
      dbDisconnect(con)
      "Connection Success!"
    }, error = function(e) e$message)
    
    showModal(modalDialog(title = "DB Diagnostics", res))
  })
  
  observe({
    print(paste("DEBUG: Is pool NULL?", is.null(pool)))
    if (!is.null(pool)) {
      print(paste("DEBUG: Is pool valid?", pool::dbIsValid(pool)))
    }
  })
  
  note_state <- reactiveValues(
    active_visit_id = NULL,
    visit_list = data.frame()
  )
  pt_state <- reactiveValues(creating_new = FALSE)
  
  observe({
    message("DEBUG: Host is ", Sys.getenv("DO_DB_HOST"))
    message("DEBUG: URL Length is ", nchar(Sys.getenv("DATABASE_URL")))
    message("DEBUG: Pass Length is ", nchar(Sys.getenv("DO_DB_PASSWORD")))
  })
  
  observe({
    message("DATABASE_URL exists: ", nzchar(Sys.getenv("DATABASE_URL")))
  })
  
  # --- State Management ---
  logged_in <- reactiveVal(FALSE)
  current_pt <- reactiveVal(NULL) 
  rx_master <- reactiveVal(data.frame())
  rx_meds <- reactiveValues(df = data.frame(brand_name=character(), generic=character(), dose=character(), freq=character(), route=character(), duration=character()))
  rx_meta <- reactiveValues(is_history = FALSE, history_date = NULL)
  
  required_cols <- c("brand_name", "generic", "dose", "freq", "route", "duration")
  
  # Define this function before calling it in the observer
  load_lab_results <- function(pt_id) {
    if (is.null(pool)) return(NULL)
    tryCatch({
      # Table is 'labs', column is 'test_date'
      res <- .with_conn(pool,
                        dbGetQuery(con,
                                   "SELECT * FROM labs WHERE patient_id::text = $1 ORDER BY test_date DESC",
                                   list(as.character(pt_id))
                        )
      )
      
      if(nrow(res) > 0) {
        showNotification(paste("Found", nrow(res), "lab records."), type = "message")
      }
    }, error = function(e) {
      message("Lab Error: ", e$message)
    })
  }
  output$manual_lab_inputs <- renderUI({
    lapply(all_test_names, function(t) {
      numericInput(paste0("lab_", make.names(t)), t, value = NA)
    })
  })
  # Trigger the Popup Modal
  observeEvent(input$open_add_lab, {
    # Load the lab targets for grouping
    lab_targets_raw <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
    
    showModal(modalDialog(
      title = "Lab Entry & History Lookup",
      size = "l",
      # We use a standard dateInput. 
      # If it still looks like text, it's usually because 'shiny' didn't load its dependencies properly.
      dateInput("modal_lab_date", "Step 1: Select Date", 
                value = Sys.Date()), 
      
      hr(),
      h6("Step 2: Enter or Edit Values (Existing data will load automatically)", 
         class = "text-muted mb-3"),
      
      div(style = "max-height: 50vh; overflow-y: auto; padding: 10px; border: 1px solid #eee; border-radius: 5px;",
          lapply(unique(lab_targets_raw$category), function(cat) {
            tagList(
              div(style = "background:#f1f1f1; padding:5px 10px; font-weight:bold; margin-top:10px;", cat),
              layout_column_wrap(
                width = 1/3,
                lapply(lab_targets_raw$test_name[lab_targets_raw$category == cat], function(t) {
                  numericInput(paste0("lab_", make.names(t)), t, value = NA)
                })
              )
            )
          })
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_labs_modal", "Save All to Flowsheet", class="btn-success")
      )
    ))
  })
  
  # Close modal after saving
  observeEvent(input$save_labs, {
    # ... (Your existing DB save logic) ...
    removeModal()
    showNotification("Record added successfully", type = "message")
  })
  
  # --- Authentication Router ---
  output$auth_logic <- renderUI({
    if (!logged_in()) {
      div(style="max-width:400px; margin:100px auto;", card(card_header("Staff Login"), card_body(
        textInput("u_id", "Username"), passwordInput("u_pw", "Password"), 
        actionButton("login_btn", "Login", class="w-100 btn-primary"), uiOutput("login_err")
      )))
    } else {
      tagList(
        uiOutput("pt_banner"),
        navset_pill(
          id = "app_tabs",
          nav_panel("1. Registration", uiOutput("reg_ui")),
          nav_panel("2. Clinical Notes", uiOutput("notes_ui")),
          nav_panel("3. Lab Entry", uiOutput("lab_ui")),
          nav_panel("4. Mobile Rx", uiOutput("rx_ui"))
        )
      )
    }
  })
  
  load_rx_master <- function() { 
    if (is.null(pool)) return(NULL) # Guard clause
    tryCatch({
      data <- dbGetQuery(pool, "SELECT * FROM drug_master")
      rx_master(data)
    }, error = function(e) {
      message("Error loading drug master: ", e$message)
    })
  }
  
  observeEvent(input$login_btn, {
    if (input$u_id == ADMIN_USER && input$u_pw == ADMIN_PASS) {
      logged_in(TRUE)
      load_rx_master()
    } else {
      output$login_err <- renderUI({ p("Invalid Credentials", class="text-danger mt-2") })
    }
  })
  
  # --- Shared Patient Banner ---
  # Updated Banner for clarity
  output$pt_banner <- renderUI({
    req(current_pt())
    pt <- current_pt()
    div(class="sticky-banner d-flex justify-content-between align-items-center",
        div(
          strong(paste(pt$first_name, pt$last_name)), 
          span(class="mx-2", "|"), 
          span("UHID:", pt$hospital_number)
        ),
        actionButton("switch_patient_btn", "Switch Patient", 
                     icon = icon("user-friends"), 
                     class = "btn-warning w-100 mt-3")
    )
  })
  
  output$manual_lab_inputs <- renderUI({
    lapply(all_test_names, function(t) {
      numericInput(paste0("lab_", make.names(t)), t, value = NA)
    })
  })  
  
# --- Module: Registration UI --
  
  # Define the search reactive
  
  
  output$reg_ui <- renderUI({
    div(class = "container-fluid p-1", # Tightened padding
        # Primary Action
        actionButton("go_new_pt", "Create New Patient", 
                     class="btn-success w-100 mb-2", icon = icon("user-plus")),
        
        # Search Card
        card(
          card_header(class="bg-primary text-white d-flex justify-content-between", 
                      "Patient Search", 
                      if(!is.null(current_pt()) || pt_state$creating_new) 
                        actionButton("hide_search", "Minimize", class="btn-sm btn-light")),
          
          textInput("reg_q", NULL, placeholder="Search Name/Phone/UHID..."),
          
          # We wrap the table in a div to control its height
          div(style = "max-height: 300px; overflow-y: auto;",
              DTOutput("reg_table"))
        ),
        
        # Profile Panel appears IMMEDIATELY here
        uiOutput("profile_panel_ui")
    )
  })
  
  output$reg_title <- renderText({ if(is.null(current_pt())) "New Registration" else paste("Editing:", current_pt()$id) })
  observeEvent(input$switch_patient_btn, {
    current_pt(NULL)
    updateTextInput(session, "patient_search", value = "")
    nav_select("main_nav", "Portal")
    showNotification("Patient session closed.", type = "message")
  })
  
  # --- Server Logic: Profile Panel ---
  # --- Server Logic: Profile Panel ---
  output$profile_panel_ui <- renderUI({
    # 1. Safety Check: Only proceed if we are creating new OR a patient is selected
    # This prevents the "invalid 'x' type in 'x || y'" error
    is_creating <- isTRUE(pt_state$creating_new)
    has_selection <- !is.null(current_pt())
    
    # If neither condition is met, return nothing (removes the gap)
    if (!is_creating && !has_selection) return(NULL)
    
    # 2. Determine initial values based on state
    # If editing, pull from current_pt(); if new, use empty strings
    pt <- if (has_selection) current_pt() else list()
    
    # 3. Build the UI Card
    div(class = "mt-2", # Minimal top margin to touch the search card above
        card(
          card_header(class = "bg-dark text-white d-flex justify-content-between align-items-center", 
                      textOutput("reg_title"),
                      actionButton("close_profile", icon("times"), class = "btn-sm btn-outline-light")),
          
          card_body(
            # UHID / Hospital Number
            textInput("hospital_number", "UHID (Hospital Number)", 
                      value = pt$hospital_number %||% ""),
            
            # Name Row
            
             
              textInput("first_name", "First Name *", value = pt$first_name %||% ""),
              textInput("last_name", "Last Name *", value = pt$last_name %||% ""),
            
            
            # DOB and Gender
              dateInput("dob", "DOB *", 
                        value = if(!is.null(pt$dob)) as.Date(pt$dob) else NA),
              radioButtons("gender", "Gender", 
                           choices = c("male", "female"), 
                           selected = pt$gender %||% "male", 
                           inline = TRUE),
            
            
            # Contact & Clinical Info
            textInput("phone", "Phone *", value = pt$phone %||% ""),
            
            textAreaInput("address1", "Address", 
                          value = pt$address1 %||% pt$address %||% "", 
                          rows = 2),
            
            textAreaInput("allergies", "Allergies", 
                            value = pt$allergies %||% "NIL", 
                            rows = 1),
              textAreaInput("comments", "Clinical Comments", 
                            value = pt$comments %||% "", 
                            rows = 1)
            ),
           card_footer(
            div(class = "d-grid gap-2", # Full-width buttons for mobile-friendly tapping
                actionButton("save_pt", "Save Patient Record", 
                             class = "btn-success btn-lg"), 
                actionButton("close_profile", "Cancel & Close", 
                             class = "btn-light"))
          )
        )
    )
  })
  
  # --- Event Handlers ---
  
  # Fixed "Create New Patient" logic
  observeEvent(input$go_new_pt, {
    pt_state$creating_new <- TRUE
    current_pt(NULL) # Clear selection
    
    # Reset all form fields
    updateTextInput(session, "hospital_number", value = "")
    updateTextInput(session, "first_name", value = "")
    updateTextInput(session, "last_name", value = "")
    updateDateInput(session, "dob", value = NA)
    updateTextInput(session, "phone", value = "")
    updateTextAreaInput(session, "address1", value = "")
    updateTextAreaInput(session, "comments", value = "")
    
    # Scroll to profile on mobile
    runjs("window.scrollTo(0, document.body.scrollHeight);")
  })
  
  # Handle Close/Cancel
  observeEvent(input$cancel_profile, {
    pt_state$creating_new <- FALSE
    current_pt(NULL)
  })
  
  
  # Replace both reg_search and search_results with this:
  search_results <- reactive({
    req(input$reg_q)
    
    # Guard: If pool didn't initialize, don't crash the app
    if (is.null(pool)) {
      showNotification("Database not connected.", type = "error")
      return(NULL)
    }
    
    # Clean the input for phone number matching or partial text search
    query_val <- paste0("%", trimws(input$reg_q), "%")
    clean_q <- clean_phone(input$reg_q)
    
    tryCatch({
      # Dynamically find the hospital number column name
      actual_cols <- DBI::dbListFields(pool, "registrations")
      true_hosp_col <- actual_cols[grep("hospital_number", actual_cols, ignore.case = TRUE)][1]
      if(is.na(true_hosp_col)) true_hosp_col <- "hospital_number"
      
      # Combined SQL logic: search by Name, Phone, or UHID
      sql <- glue::glue_sql("
      SELECT * FROM registrations
      WHERE first_name ILIKE {query_val}
      OR last_name ILIKE {query_val}
      OR phone ILIKE {query_val}
      OR CAST({`true_hosp_col`} AS TEXT) ILIKE {query_val}
      OR RIGHT(REGEXP_REPLACE(phone, '[^0-9]', '', 'g'), 10) = {clean_q}
      LIMIT 15
    ", .con = pool)
      
      DBI::dbGetQuery(pool, sql)
      
    }, error = function(e) {
      message("Search error: ", e$message)
      return(NULL)
    })
  })
  
  
  # Update Title Based on Context
  output$reg_title <- renderText({
    if (pt_state$creating_new) "New Patient Registration"
    else paste("Editing Profile:", current_pt()$first_name, current_pt()$last_name)
  })
  
  observe({
    req(logged_in())
    invalidateLater(900000) # 15 minutes
    # Add logic to log out or show a warning
  })
  
  output$paste_container <- renderUI({
    if(!is.null(current_pt())) return(NULL)
    div(class="bg-light p-2 mb-3", textAreaInput("quick_paste", "HIS Quick Paste", rows=3), actionButton("parse_btn", "Parse Text", class="btn-info btn-sm text-white"))
  })
  
  observeEvent(input$parse_btn, {
    req(input$quick_paste); p <- parse_patient_text(input$quick_paste)
    lapply(names(p), function(n) safe_update_input(session, n, p[[n]]))
  })
  
  reg_search <- reactive({
    req(input$reg_q)
    search_term <- as.character(input$reg_q)
    clean_q <- clean_phone(search_term)
    
    with_conn(pool,
              dbGetQuery(con,
                         glue_sql("
    SELECT 
      id, 
      first_name, 
      last_name, -- This matches your 'head(x)'
      dob, 
      gender, 
      phone, 
      hospital_number,
      comments
    FROM registrations 
    WHERE first_name ILIKE {paste0('%', search_term, '%')} 
       OR last_name ILIKE {paste0('%', search_term, '%')}
       OR hospital_number ILIKE {paste0('%', search_term, '%')} 
       OR RIGHT(REGEXP_REPLACE(phone, '[^0-9]', '', 'g'), 10) = {clean_q} 
    LIMIT 15", .con = pool)))
  })
  
  output$reg_table <- renderDT({
    df <- search_results()
    req(df, nrow(df) > 0)
    
    datatable(
      df,
      selection = "single",
      rownames = FALSE,
      options = list(
        dom = 'tp',
        scrollX = TRUE,
        # Hide technical columns (IDs, TSV, etc.) to keep mobile UI clean
        columnDefs = list(list(visible = FALSE, targets = c(0, 8, 13, 14, 15))) 
      )
    )
  })
  
  # REPLACE your reg_search and search_results with this single version
  
  load_rx_history <- function(pt_id) {
    # 1. Connection Guard: Stop if the database pool is missing
    if (is.null(pool)) {
      message("Rx History Load: No DB connection.")
      return(NULL)
    }
    
    req(pt_id)
    
    tryCatch({
      # 2. Query the latest prescription for this patient
      # We cast id to text to ensure the $1 placeholder matches the Postgres column type
      res <- dbGetQuery(pool, 
                        "SELECT meds_json, visit_date FROM prescriptions 
       WHERE patient_id::text = $1 
       ORDER BY visit_date DESC LIMIT 1", 
                        list(as.character(pt_id)))
      
      # 3. Handle the result
      if(nrow(res) > 0 && !is.na(res$meds_json[1]) && res$meds_json[1] != "") {
        
        # Update metadata for the UI badge
        rx_meta$is_history <- TRUE
        rx_meta$history_date <- as.character(res$visit_date[1])
        
        # 4. Parse JSON safely
        # use simplifyVector=TRUE to get a data frame directly
        raw_data <- fromJSON(res$meds_json[1])
        
        # If raw_data is a list (JSON array), convert to data frame
        if(!is.data.frame(raw_data)) {
          raw_data <- as.data.frame(raw_data)
        }
        
        # 5. Column Reconciliation (Fixing the 'brand' vs 'brand_name' issue)
        if("brand" %in% names(raw_data)) {
          names(raw_data)[names(raw_data) == "brand"] <- "brand_name"
        }
        
        # 6. Ensure all required columns exist so the UI doesn't break
        required_cols <- c("brand_name", "generic", "dose", "freq", "route", "duration")
        for(col in required_cols) {
          if(!(col %in% names(raw_data))) raw_data[[col]] <- ""
        }
        
        # Update the reactive dataframe with the history
        rx_meds$df <- raw_data[, required_cols, drop = FALSE]
        message("Rx History Loaded: ", nrow(rx_meds$df), " items from ", rx_meta$history_date)
        
      } else {
        # No history found: Reset to a clean slate
        rx_meta$is_history <- FALSE
        rx_meta$history_date <- NULL
        rx_meds$df <- data.frame(brand_name=character(), generic=character(), 
                                 dose=character(), freq=character(), 
                                 route=character(), duration=character(), 
                                 stringsAsFactors = FALSE)
        message("No Rx history found for Patient ID: ", pt_id)
      }
      
    }, error = function(e) {
      message("CRITICAL Rx History Error: ", e$message)
      showNotification("Failed to retrieve previous medicines", type = "error")
    })
  }
  
  
  
  observeEvent(input$reg_table_rows_selected, {
    s <- input$reg_table_rows_selected
    req(s)
    
    # 1. Get data from the search results
    # Use isolates or ensure this data exists to prevent crashes
    selected_data <- search_results()[s, ]
    
    # 2. Reset the creating_new flag 
    # This tells the UI we are EDITING/VIEWING, not creating fresh
    pt_state$creating_new <- FALSE  
    
    # 3. Load Rx History 
    # We use as.character to avoid issues with integer/numeric types in Postgres
    pt_id <- selected_data$id
    if (!is.null(pt_id) && !is.na(pt_id)) {
      load_rx_history(pt_id)
    } else {
      # Fallback if ID is missing: clear the meds list
      rx_meds$df <- data.frame(brand_name=character(), generic=character(), 
                               dose=character(), freq=character(), 
                               route=character(), duration=character())
      showNotification("ID missing - History not loaded", type = "warning")
    }
    
    # 4. Set current_pt (do this AFTER loading history to avoid race conditions)
    current_pt(as.list(selected_data)) 
    
    # 5. Update Form Inputs
    updateTextInput(session, "hospital_number", value = selected_data$hospital_number %||% "")
    updateTextInput(session, "first_name", value = selected_data$first_name %||% "")
    updateTextInput(session, "last_name", value = selected_data$last_name %||% "")
    
    # Date check: ensure dob isn't a weird format
    dob_val <- if(!is.null(selected_data$dob) && !is.na(selected_data$dob)) as.Date(selected_data$dob) else NA
    updateDateInput(session, "dob", value = dob_val)
    
    updateTextInput(session, "phone", value = selected_data$phone %||% "")
    updateTextAreaInput(session, "address1", value = selected_data$address1 %||% "")
    updateTextAreaInput(session, "allergies", value = selected_data$allergies %||% "NIL")
    updateTextAreaInput(session, "comments", value = selected_data$comments %||% "")
    
    # 6. Smooth Scroll to the form
    runjs("setTimeout(function(){ window.scrollTo({ top: document.body.scrollHeight, behavior: 'smooth' }); }, 100);")
  })
  
  observeEvent(input$go_new_pt, { current_pt(NULL); updateTabsetPanel(session, "reg_tabs", "Profile") })
  
  observeEvent(input$save_pt, {
    req(input$first_name, input$phone) # Mobile validation
    
    # Build the list of data
    pt_data <- list(
      hospital_number = input$hospital_number,
      first_name = input$first_name,
      last_name = input$last_name,
      dob = as.character(input$dob),
      gender = input$gender,
      phone = input$phone,
      address1 = input$address1,
      allergies = input$allergies,
      comments = input$comments
    )
    
    if (pt_state$creating_new) {
      # INSERT: Do NOT include 'id' in the insert list
      dbWriteTable(pool, "registrations", as.data.frame(pt_data), append = TRUE, row.names = FALSE)
      showNotification("New Patient Created", type = "success")
    } else {
      # UPDATE
      req(current_pt()$id)
      # Your existing update logic here...
    }
    
    # Reset state and hide form
    pt_state$creating_new <- FALSE
    current_pt(NULL)
  })
  
  
  # --- Module 2: Clinical Notes ---
  
  
  # --- Module: Clinical Notes ---
  
  output$notes_ui <- renderUI({
    req(current_pt())
    
    page_sidebar(
      sidebar = sidebar(
        title = "Visit History",
        width = 300,
        # Action button to clear fields for a new record
        actionButton("new_visit_btn", "Start New Empty Note", 
                     class="btn-success w-100 mb-3", 
                     icon = icon("plus")),
        hr(),
        # Scrollable container for the past visit tiles
        uiOutput("visit_tiles_ui"), 
        hr(),
        uiOutput("delete_visit_ui")
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          div(class="d-flex justify-content-between align-items-center",
              textOutput("note_header"),
              span(class="badge bg-secondary", paste("PID:", current_pt()$id)))
        ),
        
        # Vitals Entry Section - Now 1/5 width to accommodate Follow-up
        layout_column_wrap(
          width = 1/5,
          textInput("v_bp", "BP (mmHg)", placeholder = "120/80"),
          numericInput("v_hr", "Pulse (bpm)", value = NA),
          numericInput("v_weight", "Weight (kg)", value = NA),
          textInput("v_temp", "Temp (F)", placeholder = "98.4"),
          dateInput("v_followup", "Follow-up Date", value = NA) # REINSTATED
        ),
        
        hr(),
        br(),
        # Examination & Plan
        div(style = "flex-grow: 1; display: flex; flex-direction: column;",
            textAreaInput("clinic_notes", "Examination & Plan", 
                          rows = 18, width = "100%", 
                          placeholder = "Enter chief complaints, findings, and assessment...")
        ),
        
        card_footer(
          actionButton("save_note", "Save Visit Record", class="btn-primary btn-lg w-100")
        )
      )
    )
  })
  
  # --- Sidebar Tile Rendering Logic ---
  output$visit_tiles_ui <- renderUI({
    req(current_pt())
    
    df <- dbGetQuery(pool, 
                     "SELECT id, visit_date FROM visitsmodule 
                    WHERE patient_id::text = $1 ORDER BY visit_date DESC", 
                     list(as.character(current_pt()$id)))
    
    if(nrow(df) == 0) return(p("No previous visits.", class="text-muted p-3 text-center"))
    
    tagList(
      lapply(1:nrow(df), function(i) {
        visit_id <- df$id[i]
        v_date <- as.Date(df$visit_date[i], origin = "1970-01-01")
        
        # Visual feedback: Highlight the active tile
        is_active <- identical(as.integer(visit_id), as.integer(note_state$active_visit_id))
        
        # Use custom CSS for a "Tile" look
        div(
          style = "margin-bottom: 8px;",
          actionButton(
            inputId = paste0("tile_trigger_", visit_id),
            label = tagList(
              div(style="display: flex; justify-content: space-between; width: 100%;",
                  span(icon("calendar-day"), style="margin-right: 10px;"),
                  span(format(v_date, "%d %b %Y"), style="font-weight: 600; flex-grow: 1;"),
                  if(is_active) icon("chevron-right") else ""
              )
            ),
            class = if(is_active) "btn btn-primary w-100 shadow-sm" else "btn btn-outline-dark w-100 text-start",
            onclick = sprintf("Shiny.setInputValue('select_visit', %d, {priority:'event'})", visit_id)
          )
        )
      })
    )
  })
  
  # --- State & Data Handling ---
  observeEvent(input$select_visit, {
    vid <- input$select_visit
    req(vid)
    
    # 1. Update global state so the app knows which record we are editing
    note_state$active_visit_id <- vid
    
    # 2. Fetch specific record from visitsmodule table
    res <- dbGetQuery(pool, "SELECT visit_date, visit_json FROM visitsmodule WHERE id = $1", list(vid))
    req(nrow(res) > 0)
    
    # Store the date in state for the header display
    note_state$active_visit_date <- as.character(as.Date(res$visit_date[1], origin = "1970-01-01"))
    
    # 3. Parse JSON Safely
    # Your prior data structure: { "vitals": {...}, "clinic_notes": "...", "followup_date": "..." }
    data <- tryCatch({
      fromJSON(res$visit_json[1])
    }, error = function(e) list(vitals = list(), clinic_notes = "", followup_date = NULL))
    
    # 4. Update UI Inputs with the retrieved data
    updateTextInput(session, "v_bp", value = data$vitals$bp %||% "")
    updateNumericInput(session, "v_hr", value = data$vitals$hr %||% NA)
    updateNumericInput(session, "v_weight", value = data$vitals$weight %||% NA)
    updateTextInput(session, "v_temp", value = data$vitals$temp %||% "")
    updateTextAreaInput(session, "clinic_notes", value = data$clinic_notes %||% "")
    
    # 5. Populate Follow-up Date specifically
    if(!is.null(data$followup_date) && data$followup_date != "") {
      updateDateInput(session, "v_followup", value = as.Date(data$followup_date))
    } else {
      updateDateInput(session, "v_followup", value = NA)
    }
  })
  
  # Sidebar Table with Clickable Dates
  output$visit_history_table <- renderDT({
    req(current_pt())
    
    # 1. Fetch data and Join with targets to identify abnormal values
    # We use the 'labs' table for clinical data and 'lab_targets' for ranges
    query <- "
    SELECT l.test_name, l.test_date, l.num_val, l.value_text, 
           t.low_limit, t.high_limit
    FROM labs l
    LEFT JOIN lab_targets t ON l.test_name = t.test_name
    WHERE l.patient_id::text = $1
    ORDER BY l.test_date DESC
  "
    df <- dbGetQuery(pool, query, list(as.character(current_pt()$id)))
    
    if(nrow(df) == 0) return(NULL)
    
    # 2. Prep data for pivoting
    df_clean <- df %>%
      mutate(
        # Format Date for slick header
        fmt_date = format(as.Date(test_date), "%d %b %y"),
        # Logic for Red text: if outside limits, wrap in a span
        is_abnormal = case_when(
          !is.na(num_val) & !is.na(high_limit) & num_val > high_limit ~ TRUE,
          !is.na(num_val) & !is.na(low_limit) & num_val < low_limit ~ TRUE,
          TRUE ~ FALSE
        ),
        # Display value: Priority to Number, then Text
        display_val = ifelse(!is.na(num_val), as.character(num_val), value_text)
      )
    
    # 3. Pivot Wide: Tests as Rows, Dates as Columns
    df_wide <- df_clean %>%
      select(test_name, fmt_date, display_val) %>%
      tidyr::pivot_wider(names_from = fmt_date, values_from = display_val)
    
    # 4. Create the Slick Table
    datatable(
      df_wide,
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      extensions = 'FixedColumns',
      options = list(
        dom = 't',               # Minimalist: Table only, no search/info
        scrollX = TRUE,          # Horizontal scroll for dates
        scrollY = "500px",       # Vertical scroll for many tests
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 1), # Freeze test names on left
        pageLength = 100,
        columnDefs = list(
          # Justify Test Names to the left, and values to the center
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-center', targets = "_all", width = '80px') # Reduced cell width
        )
      )
    ) %>%
      # Styling: Slick Grey Row Headers
      formatStyle(
        columns = 1,
        fontWeight = '600',
        backgroundColor = '#fcfcfc',
        borderRight = '2px solid #eee'
      ) %>%
      # Logic: Apply Red color to abnormal values globally
      # Note: This logic targets the HTML class we can inject via the query
      formatStyle(
        columns = names(df_wide)[-1],
        valueColumns = names(df_wide)[-1],
        color = styleInterval(c(-Inf, Inf), c('red', 'red')), # Simplified logic or use specific CSS
        fontWeight = '500'
      )
  })
  
  # Delete Button UI with Conditional Rendering
  output$delete_visit_ui <- renderUI({
    req(note_state$active_visit_id)
    actionButton("confirm_delete_btn", "Delete Selected Visit", class="btn-outline-danger w-100 btn-sm")
  })
  
  # --- Event Handlers for Deletion ---
  
  observeEvent(input$confirm_delete_btn, {
    showModal(modalDialog(
      title = "Warning: Permanent Deletion",
      "Are you sure you want to delete this clinical note? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_delete", "Yes, Delete Record", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$execute_delete, {
    req(note_state$active_visit_id)
    
    tryCatch({
      dbExecute(pool, "DELETE FROM visitsmodule WHERE id = $1", list(note_state$active_visit_id))
      showNotification("Record deleted successfully", type = "warning")
      
      # Reset UI state
      note_state$active_visit_id <- NULL
      note_state$active_visit_date <- NULL
      updateTextInput(session, "v_bp", value = "")
      updateNumericInput(session, "v_hr", value = NA)
      updateNumericInput(session, "v_weight", value = NA)
      updateTextInput(session, "v_temp", value = "")
      updateTextAreaInput(session, "clinic_notes", value = "")
      
      removeModal()
    }, error = function(e) {
      showNotification(paste("Error during deletion:", e$message), type = "error")
    })
  })
  observeEvent(input$save_note, {
    req(current_pt())
    
    # Construct the JSON payload to match your previous format
    visit_data_list <- list(
      vitals = list(
        bp = input$v_bp,
        hr = input$v_hr,
        weight = input$v_weight,
        temp = input$v_temp
      ),
      clinic_notes = input$clinic_notes,
      followup_date = as.character(input$v_followup) # Added to JSON
    )
    
    json_payload <- toJSON(visit_data_list, auto_unbox = TRUE)
    
    tryCatch({
      if (is.null(note_state$active_visit_id)) {
        # INSERT New Record
        dbExecute(pool, 
                  "INSERT INTO visitsmodule (patient_id, visit_date, visit_json, created_at) VALUES ($1, $2, $3, $4)",
                  list(as.character(current_pt()$id), Sys.Date(), json_payload, Sys.time())
        )
      } else {
        # UPDATE Existing Record
        dbExecute(pool, 
                  "UPDATE visitsmodule SET visit_json = $1, created_at = $2 WHERE id = $3",
                  list(json_payload, Sys.time(), note_state$active_visit_id)
        )
      }
      showNotification("Record Saved Successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error")
    })
  })
  
  
  
  
  
  # --- Module 2: Labs ---
  # Reactive to fetch and pivot lab data
  lab_history_pivot <- reactive({
    req(current_pt())
    
    # Fetch all labs for current patient
    df <- dbGetQuery(pool, glue_sql("
      SELECT test_name, test_date, num_val, value_text 
      FROM labs 
      WHERE patient_id::text = {as.character(current_pt()$id)}
      ORDER BY test_date DESC
    ", .con = pool))
    
    if(nrow(df) == 0) return(NULL)
    
    # 1. Format the value for display (Priority: Number, then Text)
    # 2. Convert Date to a pretty string for the column header
    df_clean <- df %>%
      mutate(
        display_val = ifelse(!is.na(num_val), as.character(num_val), value_text),
        test_date = format(as.Date(test_date), "%d %b %y")
      ) %>%
      select(test_name, test_date, display_val)
    
    # Pivot from Long to Wide
    df_wide <- df_clean %>%
      tidyr::pivot_wider(
        names_from = test_date, 
        values_from = display_val,
        values_fn = list(display_val = ~ .[1]) # Handles accidental duplicates
      )
    
    return(df_wide)
  })
  
  # Render the DT with Fixed Columns
  # 1. Create a reactive to handle the pivot and data structure
  lab_flowsheet_data <- reactive({
    req(current_pt())
    
    # Fetch data
    df <- dbGetQuery(pool, "
    SELECT test_name, test_date, num_val 
    FROM labs 
    WHERE patient_id::text = $1 
    ORDER BY test_date DESC", list(as.character(current_pt()$id)))
    
    if(nrow(df) == 0) return(NULL)
    
    # Pivot Wide: Dates become columns
    df_wide <- df %>%
      mutate(fmt_date = format(as.Date(test_date), "%d %b %y")) %>%
      select(test_name, fmt_date, num_val) %>%
      tidyr::pivot_wider(names_from = fmt_date, values_from = num_val)
    
    return(df_wide)
  })
  
  # 2. Render the editable table
  
  # Reactive to prepare categorized data
  # 1. Reactive to prepare data in the exact CSV order
  # Prepare categorized data using factors to preserve CSV order
  categorized_lab_data <- reactive({
    req(current_pt())
    
    # Fetch lab results
    labs_df <- dbGetQuery(pool, "SELECT test_name, test_date, num_val FROM labs WHERE patient_id::text = $1", 
                          list(as.character(current_pt()$id)))
    
    # Load reference order from CSV
    lab_targets_raw <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
    lab_targets_raw$sort_id <- 1:nrow(lab_targets_raw)
    
    if(nrow(labs_df) == 0) {
      # If no data, return a template based on CSV
      return(lab_targets_raw %>% select(category, test_name) %>% mutate(placeholder = NA))
    }
    
    # Join, Factorize for order, and Pivot
    df_wide <- labs_df %>%
      inner_join(lab_targets_raw, by = "test_name") %>%
      mutate(
        fmt_date = format(as.Date(test_date), "%d %b %y"),
        category = factor(category, levels = unique(lab_targets_raw$category))
      ) %>%
      select(category, test_name, fmt_date, num_val) %>%
      tidyr::pivot_wider(names_from = fmt_date, values_from = num_val) %>%
      # Ensure test names stay in CSV order after pivot
      left_join(lab_targets_raw %>% select(test_name, sort_id), by = "test_name") %>%
      arrange(sort_id) %>%
      select(-sort_id)
    
    return(df_wide)
  })
  
  output$history_table <- renderRHandsontable({
    df <- categorized_lab_data()
    req(df)
    
    rhandsontable(df, stretchH = "all", rowHeaders = FALSE, height = 600) %>%
      hot_col(col = "category", readOnly = TRUE, width = 140) %>%
      hot_col(col = "test_name", readOnly = TRUE, width = 160) %>%
      hot_table(fixedColumnsLeft = 2, highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_cols(renderer = "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        if (col <= 1) { 
          td.style.background = '#fcfcfc'; 
          td.style.fontWeight = 'bold';
        }
        if (col > 1) { td.style.textAlign = 'center'; }
      }
    ")
  })
  observeEvent(input$open_add_lab, {
    # We read the raw file to get category groupings
    lab_targets_raw <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
    
    showModal(modalDialog(
      title = "Add/Edit Labs for Specific Date",
      size = "l",
      # This ID triggers the 'loader' logic above
      dateInput("modal_lab_date", "Observation Date", value = Sys.Date()), 
      hr(),
      div(style = "max-height: 55vh; overflow-y: auto; padding: 10px;",
          lapply(unique(lab_targets_raw$category), function(cat) {
            tagList(
              h6(cat, style="background:#f8f9fa; padding:8px; border-left: 4px solid #26A69A; font-weight: bold;"),
              layout_column_wrap(
                width = 1/3,
                lapply(lab_targets_raw$test_name[lab_targets_raw$category == cat], function(t) {
                  # These IDs must match the naming convention in the loader logic
                  numericInput(paste0("lab_", make.names(t)), t, value = NA)
                })
              )
            )
          })
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_labs_modal", "Save to Flowsheet", class="btn-success")
      )
    ))
  })
  # Logic to load existing data into the Modal when a date is selected
  observeEvent(input$modal_lab_date, {
    req(current_pt(), input$modal_lab_date)
    
    # 1. Fetch any existing results for this specific date and patient
    existing_labs <- dbGetQuery(pool, 
                                "SELECT test_name, num_val FROM labs WHERE patient_id::text = $1 AND test_date = $2",
                                list(as.character(current_pt()$id), as.character(input$modal_lab_date))
    )
    
    # 2. Iterate through all possible tests from your CSV config
    # 'lab_targets_raw' must be available in the server scope
    lapply(lab_targets_raw$test_name, function(t) {
      input_id <- paste0("lab_", make.names(t))
      
      # Check if this specific test has a value in the DB results
      match_row <- existing_labs[existing_labs$test_name == t, ]
      
      if (nrow(match_row) > 0) {
        # Data exists -> Update with DB value
        updateNumericInput(session, input_id, value = match_row$num_val[1])
      } else {
        # No data -> Clear the field to NA
        updateNumericInput(session, input_id, value = NA)
      }
    })
  })
  observeEvent(input$save_flowsheet, {
    req(input$history_table)
    
    # Convert RHT back to a data frame
    df_edited <- hot_to_r(input$history_table)
    
    # Pivot back to LONG format for DB storage
    df_long <- df_edited %>%
      tidyr::pivot_longer(cols = -test_name, names_to = "test_date", values_to = "num_val") %>%
      filter(!is.na(num_val)) %>%
      mutate(
        patient_id = as.character(current_pt()$id),
        test_date = as.Date(test_date, format = "%d %b %y"),
        created_at = Sys.time()
      )
    
    # DB Update logic (Upsert)
    tryCatch({
      dbWriteTable(pool, "stg_flowsheet", df_long, temporary = TRUE, overwrite = TRUE)
      dbExecute(pool, "
      INSERT INTO labs (patient_id, test_date, test_name, num_val, created_at)
      SELECT patient_id, test_date, test_name, num_val, created_at FROM stg_flowsheet
      ON CONFLICT (patient_id, test_date, test_name) 
      DO UPDATE SET num_val = EXCLUDED.num_val, created_at = EXCLUDED.created_at
    ")
      showNotification("Flowsheet saved successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Save Error:", e$message), type = "error")
    })
  })
  # 1. Update the Rename Logic
  observeEvent(input$confirm_rename, {
    req(input$col_to_rename, input$new_col_date)
    
    old_date_str <- input$col_to_rename # e.g., "18 Feb 26"
    new_date_val <- input$new_col_date   # e.g., 2026-02-19
    
    # Convert the column name (string) back to a real Date object for the SQL query
    old_date_val <- as.Date(old_date_str, format = "%d %b %y")
    
    tryCatch({
      # Update the database
      dbExecute(pool, "
      UPDATE labs 
      SET test_date = $1 
      WHERE patient_id::text = $2 AND test_date = $3",
                list(new_date_val, as.character(current_pt()$id), old_date_val)
      )
      
      removeModal()
      showNotification("Date header updated in database.", type = "message")
      
      # 2. TRIGGER REFRESH: 
      # We clear the active patient and re-set it to force the table to re-query
      temp_pt <- current_pt()
      current_pt(NULL)
      current_pt(temp_pt)
      
    }, error = function(e) {
      showNotification(paste("Update Error:", e$message), type = "error")
    })
  })
  # Popup to rename a date column
  observeEvent(input$rename_col_btn, {
    df <- hot_to_r(input$history_table)
    date_cols <- names(df)[-(1:2)] # Exclude category and test_name
    
    showModal(modalDialog(
      title = "Edit Column Date",
      selectInput("col_to_rename", "Select Column to Change", choices = date_cols),
      dateInput("new_col_date", "New Date", value = Sys.Date()),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_rename", "Update Header", class = "btn-primary")
      )
    ))
  })
  
  # Update the header in the reactive data
  observeEvent(input$confirm_rename, {
    new_date_str <- format(input$new_col_date, "%d %b %y")
    # Logic here would involve updating the database dates for that patient 
    # or modifying the live rhandsontable data
    dbExecute(pool, "
    UPDATE labs SET test_date = $1 
    WHERE patient_id::text = $2 AND test_date = $3",
              list(input$new_col_date, as.character(current_pt()$id), as.Date(input$col_to_rename, "%d %b %y"))
    )
    removeModal()
    showNotification("Date header updated.")
  })
  output$lab_ui <- renderUI({
    req(current_pt())
    card(
      card_header(div(class="d-flex justify-content-between", "Laboratory Entry", 
                      div(actionButton("exp_lab", "Expand All", class="btn-sm"), actionButton("col_lab", "Collapse All", class="btn-sm")))),
      dateInput("lab_date", "Observation Date", value=Sys.Date()),
      accordion(id="lab_acc", multiple=T,
                lapply(category_names, function(cat) {
                  accordion_panel(title=cat, value=cat,
                                  lapply(lab_config[[cat]], function(test) {
                                    div(class="lab-row", span(test), div(style="width:120px;", numericInput(paste0("lab_", make.names(test)), NULL, value=NA)))
                                  })
                  )
                })
      ),
      actionButton("save_labs", "Save Lab Records", class="btn-success w-100 mt-3")
    )
  })
  observeEvent(input$delete_col_btn, {
    req(input$col_to_rename)
    target_date <- as.Date(input$col_to_rename, format = "%d %b %y")
    
    dbExecute(pool, "DELETE FROM labs WHERE patient_id::text = $1 AND test_date = $2",
              list(as.character(current_pt()$id), target_date))
    
    # Refresh UI
    temp_pt <- current_pt()
    current_pt(NULL)
    current_pt(temp_pt)
    showNotification("Column deleted.", type = "warning")
  })
  
  observe({
    req(current_pt(), input$lab_date)
    
    # 1. Clear all inputs first for the new date/patient selection
    for(t in all_test_names) {
      updateNumericInput(session, paste0("lab_", make.names(t)), value = NA)
    }
    
    # 2. Fetch results using updated column names
    tryCatch({
      # Use COALESCE to prioritize the number, but fall back to text if it's a qualitative lab
      existing <- dbGetQuery(pool, 
                             "SELECT test_name, num_val, value_text 
                              FROM labs 
                              WHERE patient_id::text = $1 AND test_date = $2", 
                             list(as.character(current_pt()$id), as.character(input$lab_date))
      )
      
      # 3. Match DB test_names to UI Input IDs
      if(nrow(existing) > 0) {
        for(i in seq_len(nrow(existing))) {
          db_name <- existing$test_name[i]
          
          if (db_name %in% all_test_names) {
            input_id <- paste0("lab_", make.names(db_name))
            
            # Use the numeric value for the numericInput
            # If it's a 'value_text' only result, it will stay NA in the numeric box
            updateNumericInput(session, input_id, value = existing$num_val[i])
          }
        }
      }
    }, error = function(e) {
      message("Lab Loading Error: ", e$message)
    })
  })
  
  observeEvent(input$exp_lab, { accordion_panel_open("lab_acc", category_names) })
  observeEvent(input$col_lab, { accordion_panel_close("lab_acc", category_names) })
  
  observeEvent(input$clear_form, {
    # 1. Reset the internal patient state
    current_pt(NULL)
    
    # 2. Explicitly clear every UI input
    updateTextInput(session, "hospital_number", value = "")
    updateTextInput(session, "first_name", value = "")
    updateTextInput(session, "last_name", value = "")
    updateDateInput(session, "dob", value = NA)
    updateRadioButtons(session, "gender", selected = character(0))
    updateTextInput(session, "phone", value = "")
    updateTextAreaInput(session, "address1", value = "")
    updateTextAreaInput(session, "allergies", value = "NIL")
    updateTextAreaInput(session, "comments", value = "")
    
    # 3. Clear the Rx state so it doesn't stay from the previous patient
    rx_meds$df <- data.frame(brand_name=character(), generic=character(), 
                             dose=character(), freq=character(), 
                             route=character(), duration=character())
    
    showNotification("Form Cleared", type = "message")
  })
  observeEvent(input$go_new_pt, {
    # 1. Set state to NULL for a new record
    current_pt(NULL)
    
    # 2. Clear all fields (identical to clear_form logic)
    updateTextInput(session, "hospital_number", value = "")
    updateTextInput(session, "first_name", value = "")
    updateTextInput(session, "last_name", value = "")
    updateDateInput(session, "dob", value = NA)
    updateRadioButtons(session, "gender", selected = character(0))
    updateTextInput(session, "phone", value = "")
    updateTextAreaInput(session, "address1", value = "")
    updateTextAreaInput(session, "allergies", value = "NIL")
    updateTextAreaInput(session, "comments", value = "")
    
    # 3. Reset Rx data
    rx_meds$df <- data.frame(brand_name=character(), generic=character(), 
                             dose=character(), freq=character(), 
                             route=character(), duration=character())
    
    # 4. Switch the user to the Profile tab
    updateTabsetPanel(session, "reg_tabs", "Profile")
  })
  
  observeEvent(input$save_labs, {
    req(current_pt())
    
    res <- lapply(all_test_names, function(t) {
      val <- input[[paste0("lab_", make.names(t))]]
      if(is.null(val) || is.na(val)) return(NULL)
      
      data.frame(
        patient_id = as.character(current_pt()$id),
        test_date  = as.Date(input$lab_date),
        test_name  = t,
        num_val    = as.numeric(val), # Changed from 'value'
        created_at = Sys.time()
      )
    }) %>% bind_rows()
    
    if(nrow(res) > 0) {
      tryCatch({
        # Note: Added 'num_val' to the conflict update logic
        dbWriteTable(pool, "stg_labs", res, temporary = TRUE, overwrite = TRUE)
        dbExecute(pool, "
        INSERT INTO labs (patient_id, test_date, test_name, num_val, created_at) 
        SELECT patient_id, test_date, test_name, num_val, created_at FROM stg_labs 
        ON CONFLICT (patient_id, test_date, test_name) 
        DO UPDATE SET num_val = EXCLUDED.num_val, created_at = EXCLUDED.created_at
      ")
        showNotification("Data synchronized.", type = "success")
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error")
      })
    }
    
    removeModal()
    showNotification("Record added successfully", type = "message")
  })
  observeEvent(input$save_labs_modal, {
    req(current_pt(), input$modal_lab_date)
    
    # Collect all inputs from the modal
    lab_results <- lapply(lab_targets_raw$test_name, function(t) {
      val <- input[[paste0("lab_", make.names(t))]]
      if (is.null(val) || is.na(val)) return(NULL)
      
      data.frame(
        patient_id = as.character(current_pt()$id),
        test_date  = as.Date(input$modal_lab_date),
        test_name  = t,
        num_val    = as.numeric(val),
        created_at = Sys.time()
      )
    }) %>% bind_rows()
    
    if (nrow(lab_results) > 0) {
      tryCatch({
        dbWriteTable(pool, "stg_labs_modal", lab_results, temporary = TRUE, overwrite = TRUE)
        dbExecute(pool, "
        INSERT INTO labs (patient_id, test_date, test_name, num_val, created_at) 
        SELECT patient_id, test_date, test_name, num_val, created_at FROM stg_labs_modal 
        ON CONFLICT (patient_id, test_date, test_name) 
        DO UPDATE SET num_val = EXCLUDED.num_val, created_at = EXCLUDED.created_at
      ")
        
        removeModal()
        showNotification("Records synchronized.", type = "message")
        
        # Refresh the main flowsheet table
        # Triggering a dummy change to current_pt forces the reactive to re-query
        tmp <- current_pt()
        current_pt(NULL)
        current_pt(tmp)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error")
      })
    }
  })
  
  # Load the full drug library from Postgres
  
  
  # Search Debouncing: Reduces DB load and stops flicker
  # --- Search & Interaction Observers ---
  
  # Debounced search to prevent flickering
  rx_search_val <- reactive({ input$rx_q }) %>% debounce(300)
  
  
  
  output$rx_ui <- renderUI({
    # 1. Ensure a patient is selected before showing anything
    req(current_pt())
    
    # 2. Safety: If the data hasn't initialized yet, show a loader
    if (is.null(rx_meds$df)) {
      return(div(class="text-center p-5", spinner(), p("Initializing Medication Library...")))
    }
    
    # 3. Check if we are viewing a historical prescription
    is_h <- isTRUE(rx_meta$is_history)
    
    card(
      card_header(
        div(class="d-flex justify-content-between align-items-center",
            span(icon("prescription"), " Medication Builder"),
            if(is_h) span(class="badge bg-warning text-dark", 
                          paste("History from:", rx_meta$history_date))
        )
      ),
      
      card_body(
        # --- SEARCH SECTION ---
        div(class = "mb-3",
            textInput("rx_q", "Search Drug Library", 
                      placeholder = "Enter brand name"),
            # This UI output handles the dropdown of search matches
            uiOutput("rx_search_results")
        ),
        
        hr(),
        
        # --- CURRENT PRESCRIPTION LIST ---
        # This displays the medicines added so far
        tags$label(strong("Current Prescription")),
        div(class = "rx-list-container mb-3",
            style = "min-height: 50px; border-radius: 8px;",
            uiOutput("rx_list") 
        ),
        
        # --- ADVANCED TABLE (Optional toggle) ---
        accordion(
          accordion_panel(
            "Edit Detailed Frequencies/Duration",
            icon = icon("table"),
            rHandsontableOutput("rx_hot")
          )
        )
      ),
      
      card_footer(
        div(class="d-grid gap-2",
            actionButton("save_rx", "Finalize & Save Prescription", 
                         class="btn-success btn-lg", icon = icon("save")))
      )
    )
  })
  
  output$rx_search_results <- renderUI({
    term <- rx_search_val()
    req(term); req(nchar(term) >= 2)
    matches <- rx_master() %>% 
      filter(grepl(term, brand_name, ignore.case=T) | grepl(term, generic, ignore.case=T)) %>% 
      head(5)
    
    if(nrow(matches) == 0) return(actionButton("new_drug_modal", paste("Register '", term, "' as New Drug"), class="btn-warning w-100"))
    
    lapply(1:nrow(matches), function(i) {
      d <- matches[i, ]
      div(
        style = "padding: 12px; border-bottom: 1px solid #eee; cursor: pointer; background: white; transition: 0.2s;",
        class = "search-item-hover",
        onclick = sprintf("Shiny.setInputValue('add_rx_id', %d, {priority:'event'})", d$id),
        
        # Brand and Generic Row
        div(
          span(strong(toupper(d$brand_name), style="color: #2c3e50; font-size: 1.05rem;")),
          span(paste0(" (", d$generic, ")"), style="color: #7f8c8d; font-size: 0.9rem;")
        ),
        
        # Metadata Row with Vertical Separators
        div(style = "margin-top: 5px; color: #16a085; font-size: 0.85rem; font-weight: 500;",
            span(paste("Dose:", d$dose %||% "NA")),
            span(" | ", style="color: #bdc3c7; font-weight: 300;"),
            span(paste("Freq:", d$freq %||% "NA")),
            span(" | ", style="color: #bdc3c7; font-weight: 300;"),
            span(paste("Route:", d$route %||% "NA")),
            span(" | ", style="color: #bdc3c7; font-weight: 300;"),
            span(paste("Dur:", d$duration %||% "NA"))
        )
      )
    })
  })
  
  # Add drug from search results to the current list
  observeEvent(input$add_rx_id, {
    req(rx_master())
    new_d <- rx_master() %>% 
      filter(id == input$add_rx_id) %>% 
      select(brand_name, generic, dose, freq, route, duration)
    
    if(new_d$duration == "" || is.na(new_d$duration)) new_d$duration <- "Until next visit"
    
    rx_meds$df <- bind_rows(rx_meds$df, new_d)
    updateTextInput(session, "rx_q", value = "") # Clear search bar
  })
  
  observeEvent(input$new_drug_modal, {
    showModal(modalDialog(title="Add New Drug to Master",
                          textInput("m_brand", "Brand Name", value=input$rx_q), textInput("m_generic", "Generic"),
                          layout_column_wrap(width=1/2, textInput("m_dose", "Dose"), textInput("m_freq", "Freq")),
                          textInput("m_route", "Route", value="ORAL"), 
                          textInput("m_dur", "Duration", value="Until next visit"), # Set default in modal
                          footer=tagList(modalButton("Cancel"), actionButton("save_master", "Save to Library"))
    ))
  })
  
  observeEvent(input$save_master, {
    # If user cleared the default in modal, put it back
    final_dur <- if(input$m_dur == "" | is.na(input$m_dur)) "Until next visit" else input$m_dur
    
    dbExecute(pool, "INSERT INTO drug_master (brand_name, generic, dose, freq, route, duration) VALUES ($1,$2,$3,$4,$5,$6)",
              list(input$m_brand, input$m_generic, input$m_dose, input$m_freq, input$m_route, final_dur))
    load_rx_master(); removeModal(); showNotification("Drug Registered")
  })
  
  output$rx_list <- renderUI({
    df <- rx_meds$df
    
    # 1. Soft Check: If no medicines exist, show a clean placeholder instead of blanking out
    if (is.null(df) || nrow(df) == 0) {
      return(
        div(class="text-center p-4 border rounded-3 bg-light text-muted", 
            style="border: 2px dashed #ddd;",
            icon("pills"), tags$br(),
            "No medicines in current prescription.", tags$br(),
            tags$small("Search above to add medications."))
      )
    }
    
    # 2. Render the list of medicines
    lapply(1:nrow(df), function(i) {
      # Ensure columns exist to avoid 'subscript out of bounds'
      brand   <- df$brand_name[i] %||% "Unknown"
      generic <- df$generic[i] %||% ""
      dose    <- df$dose[i] %||% ""
      freq    <- df$freq[i] %||% ""
      
      # Logic for Duration fallback
      display_dur <- if(is.na(df$duration[i]) || df$duration[i] == "") "Until next visit" else df$duration[i]
      
      # Mobile-friendly list item
      div(class="d-flex justify-content-between align-items-center p-3 mb-2 border rounded shadow-sm bg-white",
          div(style="flex-grow: 1;",
              div(strong(brand, style="color: #2c3e50; font-size: 1.1em;")),
              if(nchar(generic) > 0) div(tags$small(class="text-muted", paste0("(", generic, ")"))),
              div(style="margin-top: 4px;",
                  span(class="badge bg-info text-dark", dose),
                  span(class="badge bg-light text-dark border", freq),
                  span(class="badge bg-secondary", display_dur)
              )
          ),
          # Enhanced delete button for mobile touch targets
          actionButton(paste0("del_rx_", i), 
                       label = NULL, 
                       icon = icon("trash-can"), 
                       class = "btn-outline-danger border-0 ms-2", 
                       style = "padding: 10px;",
                       onclick = sprintf("Shiny.setInputValue('del_rx_idx', %d, {priority: 'event'})", i))
      )
    })
  })
  
  observeEvent(input$del_rx_idx, { 
    rx_meds$df <- rx_meds$df[-as.numeric(input$del_rx_idx), ] 
  })
  
  output$rx_hot <- renderRHandsontable({
    df <- rx_meds$df
    req(nrow(df) > 0)
    rhandsontable(df, stretchH = "all", rowHeaders = FALSE) %>%
      hot_cols(manualColumnResize = TRUE)
  })
  
  observeEvent(input$rx_hot, {
    updated_df <- hot_to_r(input$rx_hot)
    updated_df[] <- lapply(updated_df, as.character)
    
    # Auto-fill "Until next visit" if a user clears a cell in the table
    updated_df$duration <- ifelse(updated_df$duration == "" | is.na(updated_df$duration), "Until next visit", updated_df$duration)
    
    rx_meds$df <- updated_df
  })
  
  observeEvent(input$save_rx, {
    req(current_pt())
    df_to_save <- rx_meds$df
    #names(df_to_save)[names(df_to_save)=="brand_name"] <- "brand"
    json_str <- as.character(toJSON(df_to_save, auto_unbox = TRUE))
    
    tryCatch({
      dbExecute(pool, glue_sql("INSERT INTO prescriptions (patient_id, visit_date, meds_json) VALUES ({current_pt()$id}, {Sys.Date()}, {json_str}) 
                                ON CONFLICT (patient_id, visit_date) DO UPDATE SET meds_json = EXCLUDED.meds_json", .con=pool))
      showNotification("Prescription Saved")
    }, error = function(e) {
      showNotification(paste("Save Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$new_visit_btn, {
    # 1. Reset Internal State
    note_state$active_visit_id <- NULL
    note_state$active_visit_date <- as.character(Sys.Date())
    
    # 2. Clear Clinical Notes Tab Inputs
    updateTextInput(session, "v_bp", value = "")
    updateNumericInput(session, "v_hr", value = NA)
    updateNumericInput(session, "v_weight", value = NA)
    updateTextInput(session, "v_temp", value = "")
    updateTextAreaInput(session, "clinic_notes", value = "")
    
    # NEW: Reset the Follow-up Date picker
    updateDateInput(session, "v_followup", value = NA)
    
    # 3. Clear Lab Flowsheet Modal Inputs
    # We use make.names to match how the IDs were created in the UI
    lapply(lab_targets_raw$test_name, function(t) {
      updateNumericInput(session, paste0("lab_", make.names(t)), value = NA)
    })
    
    # 4. Clear Prescription Reactive Dataframe
    # This ensures the Rx table in Tab 4 starts empty
    if(exists("rx_meds")) {
      rx_meds$df <- data.frame(
        brand_name = character(),
        dose = character(),
        frequency = character(),
        duration = character(),
        stringsAsFactors = FALSE
      )
    }
    
    # 5. UI Feedback & Navigation
    showNotification("New empty record initialized.", type = "message")
    
    # Optional: Jump to the Clinical Notes tab automatically
    # updateTabsetPanel(session, "main_nav", selected = "2. Clinical Notes")
  })
  
  
  observe({
    key_exists <- Sys.getenv("OPENAI_API_KEY") != ""
    print(paste("OpenAI Key detected:", key_exists))
  })
  
  
  
}

shinyApp(ui, server)


