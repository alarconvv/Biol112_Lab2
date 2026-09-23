# Simulator to replace coins selection for Lab2 - Genetics
#Biol112 fall 2026
#Viviana Romero Alarcon

library(shiny)
library(bslib)
library(DT)

# UI Definition
ui <- page_fluid(
  theme = bs_theme(bootswatch = "zephyr", primary = "#005A9C"),
  titlePanel("Koi Fish Mendelian Genetics Simulator"),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Mating Pair Selection",
      
      selectInput("mother_gen", "Mother Phenotype & Genotype:",
                  choices = c("Red (RR)" = "RR", 
                              "Red & White / Mottled (Rr)" = "Rr", 
                              "White (rr)" = "rr")),
      
      selectInput("father_gen", "Father Phenotype & Genotype:",
                  choices = c("Red (RR)" = "RR", 
                              "Red & White / Mottled (Rr)" = "Rr", 
                              "White (rr)" = "rr")),
      
      hr(),
      actionButton("mate_btn", "Simulate 2 Offspring", class = "btn-primary w-100 btn-lg"),
      hr(),
      actionButton("add_log_btn", "Log Pair Result to Table 1", class = "btn-success w-100"),
      br(), br(),
      actionButton("reset_btn", "Reset Lab Data", class = "btn-outline-danger w-100 btn-sm")
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      # Punnett Square & Active Cross
      card(
        card_header("1. Punnett Square Reference"),
        uiOutput("punnett_square_ui")
      ),
      
      # Simulation Output for Paper Table 1
      card(
        card_header("2. Offspring Results (Copy to Worksheet Table 1)"),
        uiOutput("offspring_results_ui")
      )
    ),
    
    # Class Data Log Table
    card(
      card_header("Logged Mating Pairs (Pairs 1 to 20 Tracker)"),
      DTOutput("log_table"),
      footer = uiOutput("class_totals_ui")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Map genotypes to phenotypes
  geno_to_pheno <- function(g) {
    switch(g,
           "RR" = "Red",
           "Rr" = "Red & White",
           "rr" = "White")
  }
  
  # Reactive current simulation result
  current_result <- reactiveVal(NULL)
  
  # Reactive dataframe to log all 20 pairs
  pair_log <- reactiveVal(data.frame(
    `Pair #` = integer(),
    `Mother Color` = character(),
    `Father Color` = character(),
    `Mother Genotype` = character(),
    `Father Genotype` = character(),
    `# Red (RR)` = integer(),
    `# Mottled (Rr)` = integer(),
    `# White (rr)` = integer(),
    check.names = FALSE
  ))
  
  # Render Punnett Square dynamically based on parent selection
  output$punnett_square_ui <- renderUI({
    m_alleles <- strsplit(input$mother_gen, "")[[1]]
    f_alleles <- strsplit(input$father_gen, "")[[1]]
    
    # Helper to order alleles (R before r)
    format_g <- function(a1, a2) {
      res <- paste0(sort(c(a1, a2), decreasing = FALSE), collapse = "")
      if (res == "rR") return("Rr")
      return(res)
    }
    
    b1 <- format_g(m_alleles[1], f_alleles[1])
    b2 <- format_g(m_alleles[1], f_alleles[2])
    b3 <- format_g(m_alleles[2], f_alleles[1])
    b4 <- format_g(m_alleles[2], f_alleles[2])
    
    HTML(paste0("
      <p><b>Parental Cross:</b> ", input$mother_gen, " × ", input$father_gen, "</p>
      <table class='table table-bordered text-center' style='max-width: 300px; margin: auto;'>
        <thead>
          <tr>
            <th>Mother \\ Father</th>
            <th>", f_alleles[1], "</th>
            <th>", f_alleles[2], "</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <th>", m_alleles[1], "</th>
            <td><b>Box 1:</b><br>", b1, "</td>
            <td><b>Box 2:</b><br>", b2, "</td>
          </tr>
          <tr>
            <th>", m_alleles[2], "</th>
            <td><b>Box 3:</b><br>", b3, "</td>
            <td><b>Box 4:</b><br>", b4, "</td>
          </tr>
        </tbody>
      </table>
    "))
  })
  
  # Simulate 2 Offspring when button clicked
  observeEvent(input$mate_btn, {
    m_alleles <- strsplit(input$mother_gen, "")[[1]]
    f_alleles <- strsplit(input$father_gen, "")[[1]]
    
    sim_one <- function() {
      m_pick <- sample(m_alleles, 1)
      f_pick <- sample(f_alleles, 1)
      res <- paste0(sort(c(m_pick, f_pick), decreasing = FALSE), collapse = "")
      if (res == "rR") return("Rr")
      return(res)
    }
    
    o1 <- sim_one()
    o2 <- sim_one()
    
    counts <- list(
      RR = sum(c(o1, o2) == "RR"),
      Rr = sum(c(o1, o2) == "Rr"),
      rr = sum(c(o1, o2) == "rr")
    )
    
    current_result(list(
      off1 = o1,
      off2 = o2,
      counts = counts
    ))
  })
  
  # Display Offspring Output explicitly matching paper Table 1
  output$offspring_results_ui <- renderUI({
    res <- current_result()
    if (is.null(res)) {
      return(p("Select parent genotypes and click 'Simulate 2 Offspring' to generate outcomes."))
    }
    
    HTML(paste0("
      <div class='alert alert-info'>
        <h5><b>Simulation Outcomes:</b></h5>
        <ul>
          <li><b>Offspring 1:</b> ", res$off1, " (", geno_to_pheno(res$off1), ")</li>
          <li><b>Offspring 2:</b> ", res$off2, " (", geno_to_pheno(res$off2), ")</li>
        </ul>
        <hr>
        <h5><b>Record These Counts in Table 1:</b></h5>
        <table class='table table-sm table-striped'>
          <tr><th># Red (RR)</th><td><b>", res$counts$RR, "</b></td></tr>
          <tr><th># Mottled (Rr)</th><td><b>", res$counts$Rr, "</b></td></tr>
          <tr><th># White (rr)</th><td><b>", res$counts$rr, "</b></td></tr>
        </table>
      </div>
    "))
  })
  
  # Add current result to log table
  observeEvent(input$add_log_btn, {
    res <- current_result()
    if (is.null(res)) return()
    
    df <- pair_log()
    next_pair <- nrow(df) + 1
    
    if (next_pair > 20) {
      showNotification("You have already logged 20 pairs (40 offspring max).", type = "warning")
      return()
    }
    
    new_row <- data.frame(
      `Pair #` = next_pair,
      `Mother Color` = geno_to_pheno(input$mother_gen),
      `Father Color` = geno_to_pheno(input$father_gen),
      `Mother Genotype` = input$mother_gen,
      `Father Genotype` = input$father_gen,
      `# Red (RR)` = res$counts$RR,
      `# Mottled (Rr)` = res$counts$Rr,
      `# White (rr)` = res$counts$rr,
      check.names = FALSE
    )
    
    pair_log(rbind(df, new_row))
  })
  
  # Render DT Table
  output$log_table <- renderDT({
    datatable(pair_log(), options = list(dom = 't', pageLength = 20), rownames = FALSE)
  })
  
  # Render Class Summary Totals for TA Check-off
  output$class_totals_ui <- renderUI({    
    df <- pair_log()      
  if (nrow(df) == 0) {return(HTML("<b>Total Offspring Logged:</b> 0 / 40"))}          
    total_red <- sum(df$`# Red (RR)`)
  total_mottled <- sum(df$`# Mottled (Rr)`)
  total_white <- sum(df$`# White (rr)`)
  total_offspring <- total_red + total_mottled + total_white
  
  HTML(paste0("
      <div style='font-size: 1.1em;'>
        <b>Summary Totals (", total_offspring, " / 40 Offspring):</b> 
        <span class='badge bg-danger ms-2'>Red (RR): ", total_red, "</span>
        <span class='badge bg-primary ms-2'>Mottled (Rr): ", total_mottled, "</span>
        <span class='badge bg-secondary ms-2'>White (rr): ", total_white, "</span>
      </div>
    "))
  })
  
  # Reset Simulation
  observeEvent(input$reset_btn, {
    current_result(NULL)
    pair_log(data.frame(
      `Pair #` = integer(),
      `Mother Color` = character(),
      `Father Color` = character(),
      `Mother Genotype` = character(),
      `Father Genotype` = character(),
      `# Red (RR)` = integer(),
      `# Mottled (Rr)` = integer(),
      `# White (rr)` = integer(),
      check.names = FALSE
    ))
  })
}

shinyApp(ui = ui, server = server)