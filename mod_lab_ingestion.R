# 1. Load configuration from CSV (Run at app startup)
# Ensure lab_targets.csv is in your project directory
lab_targets <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
all_test_names <- lab_targets$test_name

# 2. UI Function
# lab_ingestion_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     card(
#       card_header("AI Lab Ingestion - Master Version"),
#       card_body(
#         fileInput(ns("file_input"), "Upload PDF or Image", multiple = TRUE),
#         # Mobile Loading Spinner
#         DTOutput(ns("results_table")) %>% shinycssloaders::withSpinner(color="#26A69A"),
#         uiOutput(ns("action_buttons"))
#       )
#     ),
#     accordion(
#       open = FALSE,
#       accordion_panel(
#         "Loud Debug & Privacy Log",
#         icon = icon("bug"),
#         verbatimTextOutput(ns("debug_console"))
#       )
#     )
#   )
# }

# 3. Server Function
lab_ingestion_server <- function(id, pool, current_pt) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    extracted_data <- reactiveVal(data.frame())
    debug_logs <- reactiveVal("System Ready. Waiting for upload...")
    
    output$debug_console <- renderPrint({ cat(debug_logs()) })
    
    observeEvent(input$file_input, {
      req(input$file_input, current_pt())
      
      debug_logs(paste0("\n--- New Upload: ", Sys.time(), " ---"))
      files <- input$file_input
      all_results <- list()
      
      withProgress(message = 'AI Analysis in Progress...', value = 0, {
        for (i in 1:nrow(files)) {
          path <- files$datapath[i]
          
          tryCatch({
            # STEP 1: TEXT EXTRACTION & SANITIZATION
            raw_text <- if(grepl("pdf", files$type[i], ignore.case = TRUE)) {
              debug_logs(paste0(debug_logs(), "\nReading PDF: ", files$name[i]))
              txt <- paste(pdftools::pdf_text(path), collapse = "\n\n")
              iconv(txt, "UTF-8", "ASCII", sub="") # Clean non-ASCII
            } else {
              debug_logs(paste0(debug_logs(), "\nOCR Image: ", files$name[i]))
              tesseract::ocr(path)
            }
            
            # STEP 2: DIRECT HTTR CALL
            # STEP 2: DIRECT HTTR CALL
            incProgress(0.5, detail = "Consulting AI...")
            
            # STEP 2: REFINED AI REQUEST
            res <- httr::POST(
              url = "https://api.openai.com/v1/chat/completions",
              httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
              httr::content_type_json(),
              body = list(
                model = "gpt-4o-mini",
                messages = list(
                  # Update the system prompt in mod_lab_ingestion.R
                  list(role = "system", content = "You are a medical data parser. 
      Extract data into a JSON array using these EXACT keys:
      - 'test_name': Use provided list names.
      - 'num_val': The number ONLY as a decimal (e.g., 23.0).
      - 'unit': The unit ONLY (e.g., 'mg/dl').
      - 'test_date': YYYY-MM-DD.
      If a test is non-numeric (e.g. 'Normal'), put it in 'value_text' and leave 'num_val' null.
      Return RAW JSON only."),
                  list(role = "user", content = paste("Target List:", paste(all_test_names, collapse=", "), "\n\nText:", raw_text))
                ),
                temperature = 0
              ),
              encode = "json"
            )
            
            # STEP 3: HANDLE API FAILURES
            if (httr::status_code(res) != 200) {
              err <- httr::content(res, "text")
              debug_logs(paste0(debug_logs(), "\nAPI ERROR: ", err))
              next
            }
            
            # STEP 4: PARSE & CLEAN (THE BACKTICK FIX)
            # STEP 4: ZERO-LOGIC PARSING
            ai_out <- httr::content(res)$choices[[1]]$message$content
            clean_json <- gsub("```json|```", "", ai_out) %>% trimws()
            batch <- jsonlite::fromJSON(clean_json)
            
            if (is.data.frame(batch) && nrow(batch) > 0) {
              # Standardize column names to lowercase just in case
              names(batch) <- tolower(names(batch))
              
              # Join with your CSV limits
              batch <- batch %>%
                inner_join(lab_targets, by = "test_name") %>%
                mutate(
                  # AI already gave us num_val, so we just use it!
                  status = case_when(
                    is.na(num_val) ~ "TEXT",
                    !is.na(high_limit) & num_val > high_limit ~ "HIGH",
                    !is.na(low_limit) & num_val < low_limit ~ "LOW",
                    TRUE ~ "NORMAL"
                  )
                )
              
              all_results[[i]] <- batch
            }
          }, error = function(e) {
            debug_logs(paste0(debug_logs(), "\nProcessing Error: ", e$message))
          })
        }
        
        final_df <- bind_rows(all_results)
        extracted_data(final_df)
      })
    })
    
    # Render results with conditional formatting
    output$results_table <- renderDT({
      req(nrow(extracted_data()) > 0)
      datatable(extracted_data(), options = list(dom = 't', pageLength = 100), rownames = FALSE) %>%
        formatStyle('status', target = 'row',
                    backgroundColor = styleEqual(c('HIGH', 'LOW'), c('#ffdad9', '#fff4d1')))
    })
    
    output$action_buttons <- renderUI({
      req(nrow(extracted_data()) > 0)
      div(class = "mt-3 d-flex gap-2",
          actionButton(ns("save"), "Confirm & Save to DB", class = "btn-success w-100"),
          actionButton(ns("clear"), "Discard", class = "btn-outline-danger"))
    })
    
    # DATABASE SAVE WITH DUPLICATE CHECK
    # Step 4: The Ironclad Save
    observeEvent(input$save, {
      req(extracted_data(), current_pt())
      tryCatch({
        # 1. Format ID as character
        pt_id_str <- as.character(current_pt()$id)
        
        # 2. THE COLUMN GATEKEEPER
        save_df <- extracted_data()
        # List all columns your DB table 'labs' now expects
        required_cols <- c("num_val", "unit", "value_text", "test_date")
        for(col in required_cols) {
          if(!(col %in% names(save_df))) {
            save_df[[col]] <- NA  
          }
        }
        
        # 3. Pull existing records - UPDATED to match new schema
        # We pull num_val as well to make the duplicate check more precise
        existing <- dbGetQuery(pool, glue_sql("
      SELECT test_name, test_date, num_val
      FROM labs 
      WHERE patient_id = {pt_id_str}
    ", .con = pool))
        
        # 4. Prepare for upload
        upload_queue <- save_df %>%
          mutate(
            patient_id = pt_id_str, 
            created_at = Sys.time(),
            # Ensure numeric type for DB compatibility
            num_val = suppressWarnings(as.numeric(num_val)),
            # Ensure proper Date objects
            test_date = as.Date(test_date) 
          ) %>%
          select(patient_id, test_name, num_val, unit, value_text, test_date, created_at)
        
        # 5. Robust Anti-duplicate Check
        if (nrow(existing) > 0) {
          existing$test_date <- as.Date(existing$test_date)
          existing$num_val <- as.numeric(existing$num_val)
          
          # We join on name, date, and value to ensure we aren't 
          # double-saving the exact same result
          upload_queue <- anti_join(
            upload_queue, 
            existing, 
            by = c("test_name", "test_date", "num_val")
          )
        }
        
        # 6. Final Write
        if (nrow(upload_queue) > 0) {
          # Use dbWriteTable to append the data
          dbWriteTable(pool, "labs", upload_queue, append = TRUE, row.names = FALSE)
          
          showNotification(
            paste("Success: Saved", nrow(upload_queue), "new records."), 
            type = "message"
          )
          
          # Clear the UI table after success
          extracted_data(data.frame())
        } else {
          showNotification("All records already exist in database.", type = "warning")
        }
        
      }, error = function(e) {
        # This will catch the 'column value does not exist' if any part of 
        # the logic still references the old column name
        debug_logs(paste0(debug_logs(), "\nDB Save Error: ", e$message))
      })
    })
  

    
    observeEvent(input$clear, { extracted_data(data.frame()) })
  })
}