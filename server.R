server <- function(input, output, session) {
  
  ensure_data_frame <- function(data) {
    if (is.list(data) && !is.data.frame(data)) {
      data <- as.data.frame(data)
    }
    return(data)
  }
  
  asignar_tipo <- function(valor_evento) {
    tipos <- list(
      Falla = c("exact", "d", "dead", "died", "f", "fail", "failed", "failure", "report", "repair", "repaired", "replaced", "replacement", "1"),
      Censura_a_Izquierda = c("l", "l-censored", "left-censored", "left", "leftcensored", "start", "mstart", "3"),
      Censura_a_Derecha = c("a", "alive", "c", "censor", "censored", "end", "mend", "noreport", "r", "r-censored", "right-censored", "removed", "right", "rightcensored", "s", "survived", "survive", "suspend", "suspended", "2"),
      Censura_Por_Intervalo = c("b", "bin", "i", "interval", "i-censored", "intervalcensored", "interval-censored", "4")
    )
    
    valor_evento <- tolower(trimws(valor_evento))
    
    for (tipo in names(tipos)) {
      if (valor_evento %in% tipos[[tipo]]) {
        tipo <- gsub("_", " ", tipo)
        return(tipo)
      }
    }
    
    return("No Especificado")
  }
  
  bases_de_datos <- c(
    "at7987", "bearinga", "bearingcage", "componentd", "deviceh", "devicen", "fan", "lfp1370", 
    "turbine","locomotivecontrol", "rocketmotor", "titanium2", "vehiclemotor", "heatexchanger", 
    "bulb", "appliancea","atrazinejune","customerlife","bkfatigue10","chemicalprocess",
    "engineemissions","lzbearing","pipelinethickness","repairtimes","tractorbreaks","tree25years",
    "shockabsorber","v7tube"
  )
  
  datos <- reactive({
    if (input$data_input == "predefined") {
      req(input$base_datos)
      tryCatch({
        data <- get(input$base_datos)
        data <- ensure_data_frame(data)
        return(data)
      }, error = function(e) {
        return(NULL)
      })
    } else if (input$data_input == "upload") {
      req(input$file1)
      tryCatch({
        if (endsWith(input$file1$name, ".xlsx")) {
          data <- read_excel(input$file1$datapath)
        } else {
          delimiter <- switch(input$delimiter,
                              "," = ",",
                              "\t" = "\t",
                              ";" = ";",
                              " " = " ")
          data <- read.table(input$file1$datapath, header = TRUE, sep = delimiter)
        }
        data <- ensure_data_frame(data)
        return(data)
      }, error = function(e) {
        return(NULL)
      })
    }
  })
  
  datos_filtrados <- reactive({
    req(datos())
    data <- datos()
    data <- ensure_data_frame(data)
    if (input$search_input != "") {
      search_term <- tolower(input$search_input)
      data <- data[apply(data, 1, function(row) any(grepl(search_term, row, ignore.case = TRUE))), , drop = FALSE]
    }
    return(data)
  })
  
  paginated_data <- reactive({
    req(datos_filtrados())
    data <- datos_filtrados()
    data <- ensure_data_frame(data)
    page_number <- input$page_number
    items_per_page <- 10
    start_index <- (page_number - 1) * items_per_page + 1
    end_index <- min(start_index + items_per_page - 1, nrow(data))
    if (start_index <= end_index) {
      return(data[start_index:end_index, , drop = FALSE]) # Asegurar que siempre sea un data.frame
    } else {
      return(data.frame()) # Devolver un data.frame vacío si no hay más datos
    }
  })
  
  total_pages <- reactive({
    req(datos_filtrados())
    data <- datos_filtrados()
    data <- ensure_data_frame(data)
    items_per_page <- 10
    ceiling(nrow(data) / items_per_page)
  })
  
  output$table_output <- render_gt({
    req(paginated_data())
    datos_mostrar <- paginated_data()
    datos_mostrar <- ensure_data_frame(datos_mostrar)
    
    if (input$col_response == "No aplica") {
      datos_mostrar$Tipo_de_Evento <- "No Especificado"
    } else if (!is.null(input$col_response) && input$col_response %in% colnames(datos_mostrar)) {
      datos_mostrar <- datos_mostrar %>%
        mutate(Tipo_de_Evento = sapply(datos_mostrar[[input$col_response]], asignar_tipo))
    }
    
    gt(datos_mostrar) %>%
      cols_align(align = "center") %>%
      tab_options(heading.title.font.size = px(24)) %>%
      tab_header(
        title = glue("Conjunto de datos: {ifelse(input$data_input == 'predefined', input$base_datos, input$file1$name)}"),
        subtitle = ""
      ) %>%
      gt_theme_pff()
  })
  
  output$pagination_ui <- renderUI({
    req(total_pages())
    tagList(
      numericInput("page_number", "Página:", min = 1, max = total_pages(), value = 1, width = 70),
      actionButton("prev_button", "Anterior"),
      actionButton("next_button", "Siguiente")
    )
  })
  
  observeEvent(input$prev_button, {
    new_page <- input$page_number - 1
    if (new_page < 1) {
      new_page <- 1
    }
    updateNumericInput(session, "page_number", value = new_page)
  })
  
  observeEvent(input$next_button, {
    new_page <- input$page_number + 1
    if (new_page > total_pages()) {
      new_page <- total_pages()
    }
    updateNumericInput(session, "page_number", value = new_page)
  })
  
  output$data_selection_ui <- renderUI({
    if (input$data_input == "predefined") {
      fluidRow(
        column(
          width = 12,
          selectInput("base_datos", "Selecciona un conjunto de datos:", choices = bases_de_datos)
        ) 
      )
    } else if (input$data_input == "upload") {
      fluidRow(
        column(
          width = 12,
          fileInput("file1", "Selecciona un archivo"),
          selectInput("delimiter", "Delimitador:",
                      choices = list("Coma" = ",",
                                     "Tabulación" = "\t",
                                     "Punto y Coma" = ";",
                                     "Espacio" = " "))
        )
      )
    }
  })
  
  output$col_response_ui <- renderUI({
    req(datos())
    choices <- colnames(datos())
    choices <- c("No aplica", choices)
    
    fluidRow(
      column(
        width = 12,
        selectInput("col_response", "Selecciona la columna que contiene la variable asociada al evento:",
                    choices = choices)
      )
    )
  })
}

shinyApp(ui = ui, server = server)