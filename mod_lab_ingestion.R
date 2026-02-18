# mod_lab_ingestion.R

target_tests <- c("Creatinine", "Potassium", "Sodium", "Hb", "TLC", "Platelets")

lab_refs <- list(
  "Creatinine" = c(low = 0.6, high = 1.2, unit = "mg/dL"),
  "Potassium"  = c(low = 3.5, high = 5.1, unit = "mmol/L"),
  "Sodium"     = c(low = 135, high = 145, unit = "mmol/L"),
  "Hb"         = c(low = 12.0, high = 16.0, unit = "g/dL"),
  "TLC"        = c(low = 4000, high = 11000, unit = "cells/uL"),
  "Platelets"  = c(low = 150, high = 450, unit = "x10^3/uL")
)

lab_ingestion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("AI Lab Ingestion - Precision Extraction"),
      card_body(
        uiOutput(ns("doc_identity_hero")),
        fileInput(ns("file_input"), "Upload Lab Reports", 
                  multiple = TRUE, accept = c(".pdf", ".png", ".jpg", ".jpeg")),
        
        accordion(
          accordion_panel(
            "Privacy Log: Sanitized Text Sent to AI",
            icon = icon("user-shield"), 
            verbatimTextOutput(ns("redacted_preview"))
          ),
          open = FALSE
        ),
        hr(),
        uiOutput(ns("commit_wrapper")),
        DTOutput(ns("extracted_results_table")) %>% 
          shinycssloaders::withSpinner(color="#26A69A")
      )
    )
  )
}


lab_ingestion_server <- function(id, pool, current_pt,lab_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    extracted_data <- reactiveVal(data.frame())
    scrubbed_text_log <- reactiveVal("")
    extracted_doc_name <- reactiveVal(NULL)
    
    observeEvent(input$file_input, {
      req(input$file_input, current_pt())
      
      # Reset State
      extracted_data(data.frame())
      extracted_doc_name(NULL)
      scrubbed_text_log("")
      
      files <- input$file_input
      # Extract ALL possible tests from lab_config defined in app.R
      all_possible_tests <- unname(unlist(lab_config))
      whitelist_str <- paste(all_possible_tests, collapse = ", ")
      
      active_fname <- toupper(trimws(current_pt()$first_name))
      active_lname <- toupper(trimws(current_pt()$last_name))
      
      # RESTORED PROGRESS BAR
      withProgress(message = 'Analyzing Lab Reports...', detail = 'Starting OCR...', value = 0, {
        
        all_results <- lapply(1:nrow(files), function(i) {
          incProgress(1/nrow(files), detail = paste("Processing file", i, "of", nrow(files)))
          
          path <- files$datapath[i]
          
          # OCR Layer
          raw_text <- if(grepl("pdf", files$type[i], ignore.case = TRUE)) {
            paste(pdftools::pdf_text(path), collapse = "\n")
          } else { tesseract::ocr(path) }
          
          clean_raw <- toupper(raw_text)
          
          # 1. Identity Extraction (Local)
          name_match <- str_extract(raw_text, "(?i)Name\\s*:\\s*[^\\n\\|\\t]{3,50}")
          if(!is.na(name_match)) {
            val <- trimws(str_remove(name_match, "(?i)Name\\s*:\\s*"))
            val <- str_split(val, "\\s{2,}|Lab No|Age|Gender")[[1]][1]
            extracted_doc_name(val) 
          }
          
          # 2. Batch Guard (Strict only if > 1 file)
          if (nrow(files) > 1) {
            is_match <- grepl(active_fname, clean_raw, fixed = TRUE) || 
              grepl(active_lname, clean_raw, fixed = TRUE)
            if (!is_match) {
              showNotification(paste("Mismatch detected in", files$name[i]), type = "error")
              return(NULL)
            }
          }
          
          # 3. Redaction
          processed_text <- str_replace_all(raw_text, "(?i)(Name\\s*:\\s*).*?(\\s{2,}|\\n|$)", "\\1 [REDACTED] ")
          scrubbed_text_log(processed_text)
          
          # 4. AI Extraction with Full Whitelist
          tryCatch({
            response <- openai::create_chat_completion(
              model = "gpt-4o-mini",
              messages = list(
                list(role = "system", content = paste0(
                  "You are a medical data parser. Extract ONLY these tests: [", whitelist_str, "]. ",
                  "CRITICAL RULES: ",
                  "1. IGNORE tests not in the list. ",
                  "2. PLATELETS: Normalize to 10^3 (e.g., 150000 -> 150). ",
                  "3. TLC/WBC/ANC/ALC: Normalize to absolute units (e.g., 4.5 -> 4500). ",
                  "4. Format test_name EXACTLY as provided in the list. ",
                  "RETURN JSON: { \"results\": [ {\"test_name\": \"...\", \"value\": 1.2, \"test_date\": \"YYYY-MM-DD\"} ] }")),
                list(role = "user", content = processed_text)
              )
            )
            
            txt <- str_remove_all(response$choices$message.content, "^```json|```$")
            parsed <- fromJSON(txt)$results
            return(as.data.frame(parsed))
          }, error = function(e) return(NULL))
        })
        
        combined <- bind_rows(all_results)
        if (nrow(combined) > 0) {
          combined <- combined %>%
            mutate(value = as.numeric(as.character(value))) %>%
            filter(!is.na(value))
          extracted_data(combined)
        }
      })
    })
    
    # Restored UI Rendering with Normalization & Highlighting
    output$extracted_results_table <- renderDT({
      req(extracted_data())
      df <- extracted_data()
      
      # Ensure numeric for comparison
      df$value <- as.numeric(as.character(df$value))
      
      # 1. CALCULATE ABNORMALITY
      df$is_abnormal <- mapply(function(name, val) {
        # Find the reference key that matches our test name (Case-insensitive)
        # This handles "Creatinine" vs "creatinine"
        match_idx <- which(tolower(names(lab_refs)) == tolower(name))
        
        if(length(match_idx) > 0 && !is.na(val)) {
          ref <- lab_refs[[match_idx]]
          low <- as.numeric(ref["low"])
          high <- as.numeric(ref["high"])
          
          # Return TRUE if outside bounds
          return(val < low || val > high)
        }
        return(FALSE) # Default to normal if no ref found
      }, df$test_name, df$value)
      
      # 2. RENDER TABLE
      datatable(df, 
                rownames = FALSE, 
                options = list(
                  dom = 't', 
                  pageLength = 100,
                  columnDefs = list(
                    # Centering: target columns 0, 1, and 2 (Name, Value, Date)
                    list(className = 'dt-center', targets = 0:2),
                    # Keep is_abnormal hidden
                    list(visible = FALSE, targets = 3)
                  )
                ), 
                colnames = c("Test Name", "Result", "Date")) %>%
        formatStyle(
          'value', 'is_abnormal',
          backgroundColor = styleEqual(c(TRUE), c('#fff0f0')),
          color = styleEqual(c(TRUE), c('#d9534f')),
          fontWeight = styleEqual(c(TRUE), c('bold'))
        )
    })
    
    # Dynamic Commit Button
    output$commit_wrapper <- renderUI({
      req(nrow(extracted_data()) > 0)
      display_name <- extracted_doc_name() %||% "Selected Patient"
      actionButton(ns("sync_to_db"), paste("Commit for", display_name), class = "btn-success w-100")
    })
    
    # Database Sync
    observeEvent(input$sync_to_db, {
      req(extracted_data(), current_pt())
      target_id <- as.character(current_pt()$id)
      
      tryCatch({
        # 1. Prepare the data from the AI extraction
        upload_queue <- extracted_data() %>%
          mutate(
            test_date = as.Date(lubridate::parse_date_time(test_date, orders = c("dmy", "ymd", "mdy"))),
            patient_id = target_id
          ) %>% 
          filter(!is.na(test_date))
        
        # 2. FETCH EXISTING DATA to check for duplicates
        # We only pull the keys (date/name) for this specific patient
        existing_query <- sqlInterpolate(pool, 
                                         'SELECT test_name, test_date FROM labs WHERE patient_id = ?id', 
                                         id = target_id)
        existing_records <- dbGetQuery(pool, existing_query)
        
        # 3. FILTER OUT DUPLICATES
        if (nrow(existing_records) > 0) {
          # Ensure dates are in the same format for comparison
          existing_records$test_date <- as.Date(existing_records$test_date)
          
          # anti_join removes rows from upload_queue that exist in existing_records
          upload_queue <- anti_join(upload_queue, existing_records, 
                                    by = c("test_name", "test_date"))
        }
        
        # 4. FINAL COMMIT
        if (nrow(upload_queue) > 0) {
          upload_queue <- upload_queue %>%
            mutate(
              source = "AI Ingestion", 
              unit = "standard", 
              created_at = Sys.time()
            ) %>%
            select(patient_id, test_date, test_name, value, unit, source, created_at)
          
          dbWriteTable(pool, "labs", upload_queue, append = TRUE, row.names = FALSE)
          
          showNotification(paste("Success: Added", nrow(upload_queue), "new records."), type = "message")
          
          # Clear the UI
          extracted_data(data.frame())
          extracted_doc_name(NULL)
        } else {
          showNotification("All records already exist in the database. Nothing to add.", type = "warning")
          extracted_data(data.frame()) 
        }
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error")
      })
    })
  })
}