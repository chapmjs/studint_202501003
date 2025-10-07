# Student Interactions — Minimal Viable Product (R + Shiny + MySQL)
# -------------------------------------------------------------------
# This is a single-file Shiny app (app.R) you can deploy to shinyapps.io.
# It uses: DBI, RMariaDB, pool, bslib, DT, shinyvalidate, shinyjs.
#
# ✅ What it does:
# - Add/edit students
# - Quick-capture interactions (date/time, place, notes)
# - Dashboard: today’s interactions
# - Students directory with search + inline view of a student's interactions
#
# 🔐 Security (MVP):
# - Expect to gate access using shinyapps.io authentication or add shinymanager later.
# - Always use HTTPS in production (shinyapps.io provides this).
#
# 🛠 Environment variables (set in shinyapps.io):
#   DB_HOST, DB_PORT (e.g., 3306), DB_USER, DB_PASS, DB_NAME
#
# -------------------------------------------------------------------

# ---- Packages ----
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DBI)
  library(pool)
  library(RMariaDB)
  library(DT)
  library(shinyvalidate)
  })

# ---- Small UI helpers ----
# Simple datetime input built from two text inputs (keeps deps minimal)
datetimeInput <- function(inputId, label, value = Sys.time()) {
  tagList(
    tags$label(label, class = "form-label"),
    div(class = "d-flex gap-2",
        textInput(paste0(inputId, "_date"), NULL, format(as.Date(value), "%Y-%m-%d"), placeholder = "YYYY-MM-DD"),
        textInput(paste0(inputId, "_time"), NULL, format(as.POSIXct(value), "%H:%M"), placeholder = "HH:MM")
    )
  )
}

# ---- DB Pool ----
get_db_pool <- function() {
  pool::dbPool(
    drv      = RMariaDB::MariaDB(),
    host     = Sys.getenv("DB_HOST"),
    port     = as.integer(Sys.getenv("DB_PORT", "3306")),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    dbname   = Sys.getenv("DB_NAME"),
    bigint   = "numeric",
    timezone = "+00:00"  # store in UTC
  )
}

pool <- get_db_pool()

onStop(function() {
  poolClose(pool)
})

# ---- SQL Helpers ----
# NOTE: Use parameterized queries to avoid SQL injection.

create_student <- function(conn, dat) {
  sql <- "INSERT INTO students
    (first_name, last_name, phone, email, grad_month, grad_year, hometown, major, linkedin_url, instagram_handle)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  dbExecute(conn, sql, params = list(
    dat$first_name, dat$last_name, dat$phone, dat$email,
    dat$grad_month, dat$grad_year, dat$hometown, dat$major,
    dat$linkedin_url, dat$instagram_handle
  ))
}

update_student <- function(conn, id, dat) {
  sql <- "UPDATE students SET
            first_name = ?, last_name = ?, phone = ?, email = ?,
            grad_month = ?, grad_year = ?, hometown = ?, major = ?,
            linkedin_url = ?, instagram_handle = ?
          WHERE id = ?"
  dbExecute(conn, sql, params = list(
    dat$first_name, dat$last_name, dat$phone, dat$email,
    dat$grad_month, dat$grad_year, dat$hometown, dat$major,
    dat$linkedin_url, dat$instagram_handle, as.numeric(id)
  ))
}

fetch_students <- function(conn, q = NULL, major = NULL, grad_year = NULL) {
  base <- "SELECT id, first_name, last_name, major, grad_year, grad_month, hometown, email, phone FROM students"
  where <- c()
  args <- list()
  if (!is.null(q) && nzchar(q)) {
    where <- c(where, "(first_name LIKE ? OR last_name LIKE ? OR email LIKE ?)")
    like <- paste0("%", q, "%")
    args <- c(args, list(like, like, like))
  }
  if (!is.null(major) && nzchar(major)) {
    where <- c(where, "major = ?")
    args <- c(args, list(major))
  }
  if (!is.null(grad_year) && nzchar(grad_year)) {
    where <- c(where, "grad_year = ?")
    args <- c(args, list(as.integer(grad_year)))
  }
  sql <- base
  if (length(where)) sql <- paste(base, "WHERE", paste(where, collapse = " AND "))
  sql <- paste(sql, "ORDER BY last_name, first_name LIMIT 500")
  dbGetQuery(conn, sql, params = args)
}

fetch_student_detail <- function(conn, id) {
  dbGetQuery(conn, "SELECT * FROM students WHERE id = ?", params = list(as.numeric(id)))
}

create_interaction <- function(conn, dat) {
  sql <- "INSERT INTO interactions (student_id, occurred_at, place, notes)
          VALUES (?, ?, ?, ?)"
  dbExecute(conn, sql, params = list(
    as.numeric(dat$student_id), as.character(dat$occurred_at), dat$place, dat$notes
  ))
}

fetch_interactions_by_student <- function(conn, student_id) {
  dbGetQuery(conn, "SELECT id, occurred_at, place, notes
                    FROM interactions
                    WHERE student_id = ?
                    ORDER BY occurred_at DESC
                    LIMIT 200", params = list(as.numeric(student_id)))
}

fetch_todays_interactions <- function(conn, tz = "America/Boise") {
  # Server stores UTC; convert window to UTC
  now_local <- as.POSIXct(Sys.time(), tz = tz)
  start_local <- as.POSIXct(format(now_local, "%Y-%m-%d 00:00:00"), tz = tz)
  end_local   <- start_local + 86400
  start_utc <- as.POSIXct(format(start_local, tz = "UTC", usetz = TRUE), tz = "UTC")
  end_utc   <- as.POSIXct(format(end_local,   tz = "UTC", usetz = TRUE), tz = "UTC")

  dbGetQuery(conn, "SELECT i.id, i.occurred_at, i.place, i.notes,
                           s.first_name, s.last_name
                    FROM interactions i
                    JOIN students s ON s.id = i.student_id
                    WHERE i.occurred_at >= ? AND i.occurred_at < ?
                    ORDER BY i.occurred_at DESC",
             params = list(as.character(start_utc), as.character(end_utc)))
}

# ---- UI ----
app_theme <- bs_theme(
  version = 5,
  base_font = font_google("Inter", local = TRUE),
  bootswatch = "flatly"
)

ui <- navbarPage(
  title = "Student Interactions",
  theme = app_theme,
  id = "main_nav",
  header = tagList(
        tags$style(HTML(".sticky-save{position:sticky;bottom:0;background:#fff;padding:0.5rem;border-top:1px solid #eee;}
 .required::after{content:' *';color:#d33;}"))
  ),

  # Dashboard
  tabPanel("Dashboard",
    fluidPage(
      h3("Today’s Interactions"),
      DTOutput("tbl_today"),
      br(),
      actionButton("go_new_interaction", "New Interaction", class = "btn btn-primary")
    )
  ),

  # Students Directory
  tabPanel("Students",
    fluidPage(
      fluidRow(
        column(4, textInput("q", "Search name/email", placeholder = "Type to search")),
        column(3, textInput("filter_major", "Major", placeholder = "e.g., Supply Chain")),
        column(2, selectInput("filter_grad_year", "Grad Year", c("" = "", 2025:2032))),
        column(3, actionButton("do_search", "Apply Filters", class = "btn btn-primary mt-4"))
      ),
      DTOutput("tbl_students")
    )
  ),

  # New Interaction
  tabPanel("New Interaction",
    fluidPage(
      fluidRow(
        column(12,
          uiOutput("interaction_form")
        )
      )
    )
  ),

  # New Student
  tabPanel("New Student",
    fluidPage(
      fluidRow(
        column(12, uiOutput("student_form"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  ns <- NS("")

  # --- Navigation helpers ---
  observeEvent(input$go_new_interaction, updateNavbarPage(session, "main_nav", "New Interaction"))

  # --- Dashboard: Today’s Interactions ---
  todays <- reactivePoll(
  10000,
  session,
  function() Sys.time(),
  function() fetch_todays_interactions(pool)
)
  )

  output$tbl_today <- renderDT({
    dat <- todays()
    if (nrow(dat)) {
      dat$occurred_at <- format(as.POSIXct(dat$occurred_at, tz = "UTC"), "%Y-%m-%d %H:%M UTC")
      dat$Name <- paste(dat$first_name, dat$last_name)
      dat <- dat[, c("occurred_at", "Name", "place", "notes")]
      names(dat) <- c("When (UTC)", "Student", "Place", "Notes")
    }
    datatable(dat, rownames = FALSE,
              options = list(pageLength = 10, order = list(list(0, 'desc'))))
  })

  # --- Students: Directory + modal detail ---
  students_data <- eventReactive(input$do_search, {
    fetch_students(pool, q = input$q, major = input$filter_major, grad_year = input$filter_grad_year)
  }, ignoreInit = FALSE)

  output$tbl_students <- renderDT({
    dat <- students_data()
    if (!nrow(dat)) return(datatable(dat))
    dat$Name <- paste(dat$first_name, dat$last_name)
    dat$Grad <- ifelse(is.na(dat$grad_year), "", paste0(dat$grad_month, "/", dat$grad_year))
    dat$View <- sprintf('<button class="btn btn-sm btn-outline-primary view-btn" data-id="%s">View</button>', dat$id)
    show <- dat[, c("Name", "major", "Grad", "hometown", "email", "phone", "View")]
    datatable(show, escape = FALSE, selection = "none", rownames = FALSE,
              options = list(pageLength = 25), callback = JS(
                "table.on('click', 'button.view-btn', function(){",
                "  var id = $(this).data('id');",
                "  Shiny.setInputValue('view_student_id', id, {priority: 'event'});",
                "});"
              ))
  })

  observeEvent(input$view_student_id, {
    sid <- input$view_student_id
    s <- fetch_student_detail(pool, sid)
    inter <- fetch_interactions_by_student(pool, sid)

    showModal(modalDialog(size = "l", easyClose = TRUE,
      title = sprintf("%s %s", s$first_name[1], s$last_name[1]),
      tagList(
        fluidRow(
          column(6,
            tags$ul(
              tags$li(strong("Major:"), paste(s$major[1])),
              tags$li(strong("Grad:"), paste0(s$grad_month[1], "/", s$grad_year[1])),
              tags$li(strong("Hometown:"), s$hometown[1])
            )
          ),
          column(6,
            tags$ul(
              tags$li(strong("Email:"), s$email[1]),
              tags$li(strong("Phone:"), s$phone[1]),
              tags$li(strong("LinkedIn:"), a(href = s$linkedin_url[1], target = "_blank", s$linkedin_url[1])),
              tags$li(strong("Instagram:"), s$instagram_handle[1])
            )
          )
        ),
        h4("Recent Interactions"),
        DTOutput("tbl_student_interactions")
      )
    ))

    output$tbl_student_interactions <- renderDT({
      if (!nrow(inter)) return(datatable(inter))
      inter$occurred_at <- format(as.POSIXct(inter$occurred_at, tz = "UTC"), "%Y-%m-%d %H:%M UTC")
      datatable(inter[, c("occurred_at", "place", "notes")], rownames = FALSE,
                options = list(pageLength = 10))
    })
  })

  # --- New Student Form ---
  output$student_form <- renderUI({
    tagList(
      h3("New Student"),
      div(class = "row g-3",
        div(class = "col-md-3",
          textInput("s_first", label = tagList(span("First name", class = "required")), "")
        ),
        div(class = "col-md-3",
          textInput("s_last", label = tagList(span("Last name", class = "required")), "")
        ),
        div(class = "col-md-3", textInput("s_phone", "Phone", "")),
        div(class = "col-md-3", textInput("s_email", "Email", "")),
        div(class = "col-md-2", selectInput("s_grad_month", "Grad Month", choices = setNames(1:12, month.abb))),
        div(class = "col-md-2", selectInput("s_grad_year", "Grad Year", choices = 2025:2032)),
        div(class = "col-md-4", textInput("s_hometown", "Hometown", "")),
        div(class = "col-md-4", textInput("s_major", "Major", "")),
        div(class = "col-md-6", textInput("s_linkedin", "LinkedIn URL", "")),
        div(class = "col-md-6", textInput("s_instagram", "Instagram / Social Handle", ""))
      ),
      div(class = "sticky-save d-grid gap-2",
        actionButton("save_student", "Save Student", class = "btn btn-primary btn-lg")
      )
    )
  })

  iv_student <- InputValidator$new()
  iv_student$add_rule("s_first", sv_required())
  iv_student$add_rule("s_last", sv_required())
  # Email is optional, but if provided must be valid
  iv_student$add_rule("s_email", sv_optional())
  iv_student$add_rule("s_email", sv_email())

  observeEvent(input$save_student, {
    iv_student$enable()
    req(iv_student$is_valid())

    dat <- list(
      first_name = trimws(input$s_first),
      last_name  = trimws(input$s_last),
      phone      = trimws(input$s_phone),
      email      = trimws(input$s_email),
      grad_month = as.integer(input$s_grad_month),
      grad_year  = as.integer(input$s_grad_year),
      hometown   = trimws(input$s_hometown),
      major      = trimws(input$s_major),
      linkedin_url = trimws(input$s_linkedin),
      instagram_handle = trimws(input$s_instagram)
    )

    tryCatch({
      create_student(pool, dat)
      showNotification("Student saved.", type = "message")
      updateTextInput(session, "s_first", value = "")
      updateTextInput(session, "s_last", value = "")
      updateTextInput(session, "s_phone", value = "")
      updateTextInput(session, "s_email", value = "")
      updateTextInput(session, "s_hometown", value = "")
      updateTextInput(session, "s_major", value = "")
      updateTextInput(session, "s_linkedin", value = "")
      updateTextInput(session, "s_instagram", value = "")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # --- New Interaction Form ---
  student_choices <- reactivePoll(
  15000,
  session,
  function() Sys.time(),
  function() {
    df <- dbGetQuery(pool, "SELECT id, first_name, last_name FROM students ORDER BY last_name, first_name LIMIT 1000")
    setNames(df$id, paste(df$first_name, df$last_name))
  }
)
      setNames(df$id, paste(df$first_name, df$last_name))
    }
  )

  output$interaction_form <- renderUI({
    tagList(
      h3("New Interaction"),
      div(class = "row g-3",
        div(class = "col-md-6",
          selectizeInput("i_student_id", label = tagList(span("Student", class = "required")), choices = student_choices(), options = list(placeholder = "Search student..."))
        ),
        div(class = "col-md-3",
          datetimeInput("i_when", "Date & Time", Sys.time())
        ),
        div(class = "col-md-3",
          textInput("i_place", "Place", placeholder = "Hallway, Office, Zoom…")
        ),
        div(class = "col-12",
          textAreaInput("i_notes", label = tagList(span("Notes", class = "required")), placeholder = "What did you discuss?", width = "100%", height = "120px")
        )
      ),
      div(class = "sticky-save d-grid gap-2",
        actionButton("save_interaction", "Save Interaction", class = "btn btn-primary btn-lg")
      )
    )
  })

  iv_inter <- InputValidator$new()
  iv_inter$add_rule("i_student_id", sv_required())
  iv_inter$add_rule("i_notes", sv_required())

  parse_datetime_inputs <- function(prefix, tz = "America/Boise") {
    d <- input[[paste0(prefix, "_date")]]
    t <- input[[paste0(prefix, "_time")]]
    if (!nzchar(d)) d <- format(Sys.Date(), "%Y-%m-%d")
    if (!nzchar(t)) t <- format(Sys.time(), "%H:%M")
    as.POSIXct(paste(d, t), tz = tz)
  }

  observeEvent(input$save_interaction, {
    iv_inter$enable(); req(iv_inter$is_valid())
    # Convert to UTC for storage
    dt_local <- parse_datetime_inputs("i_when")
    occurred_utc <- format(as.POSIXct(dt_local, tz = "UTC"), "%Y-%m-%d %H:%M:%S")

    dat <- list(
      student_id = input$i_student_id,
      occurred_at = occurred_utc,
      place = trimws(input$i_place),
      notes = trimws(input$i_notes)
    )

    tryCatch({
      create_interaction(pool, dat)
      showNotification("Interaction saved.", type = "message")
      updateTextInput(session, "i_place", value = "")
      updateTextInput(session, "i_notes", value = "")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

shinyApp(ui, server)


# ---------------------------
# MySQL Schema (run once)
# ---------------------------
# Save the following as schema.sql and run it on your MySQL instance.
#
# CREATE TABLE IF NOT EXISTS students (
#   id BIGINT AUTO_INCREMENT PRIMARY KEY,
#   first_name VARCHAR(100) NOT NULL,
#   last_name  VARCHAR(100) NOT NULL,
#   phone VARCHAR(25) NULL,
#   email VARCHAR(255) NULL UNIQUE,
#   grad_month TINYINT NULL,
#   grad_year SMALLINT NULL,
#   hometown VARCHAR(255) NULL,
#   major VARCHAR(120) NULL,
#   linkedin_url VARCHAR(255) NULL,
#   instagram_handle VARCHAR(120) NULL,
#   created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
#   updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
#   INDEX idx_students_name (last_name, first_name),
#   INDEX idx_students_major (major),
#   INDEX idx_students_grad (grad_year, grad_month)
# ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
#
# CREATE TABLE IF NOT EXISTS interactions (
#   id BIGINT AUTO_INCREMENT PRIMARY KEY,
#   student_id BIGINT NOT NULL,
#   occurred_at DATETIME NOT NULL,
#   place VARCHAR(120) NULL,
#   notes TEXT NOT NULL,
#   created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
#   INDEX idx_interactions_student_time (student_id, occurred_at),
#   CONSTRAINT fk_interactions_student FOREIGN KEY (student_id) REFERENCES students(id) ON DELETE CASCADE
# ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
#
# -- Optional: seed a test student
# INSERT INTO students (first_name, last_name, email, major, grad_month, grad_year)
# VALUES ('Test','Student','test@example.com','Supply Chain',12,2026);
#
# ---------------------------
# Deployment Notes
# ---------------------------
# 1) Commit app.R to a GitHub repo (for versioning). This is for source control only.
# 2) In shinyapps.io: create a new application and deploy from RStudio (rsconnect) or CLI.
# 3) In shinyapps.io app settings → Environment Variables: set DB_HOST, DB_PORT, DB_USER, DB_PASS, DB_NAME.
# 4) Open security: if needed, add shinymanager for a login gate (post-MVP).
# 5) Verify connectivity: ensure the shinyapps.io egress IPs are allowed in your DB provider’s firewall.
