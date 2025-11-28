# 1.Paket yang Diperlukan
library(shiny)
library(readr)
library(ggplot2)
library(plotly)
library(DT)

data_raw <- read.csv2("Tugas 3 - weather.csv", skip = 1)

head(data_raw)

# Peta Label
col_labels <- c(
  "MinTemp" = "Suhu Minimum (°C)",
  "MaxTemp" = "Suhu Maksimum (°C)",
  "Rainfall" = "Curah Hujan (mm)",
  "Evaporation" = "Penguapan (mm)",
  "Sunshine" = "Durasi Sinar Matahari (Jam)",
  "WindGustDir" = "Arah Angin Terkuat",
  "WindGustSpeed" = "Kecepatan Angin Terkuat (km/jam)",
  "WindDir9am" = "Arah Angin Pagi (9am)",
  "WindDir3pm" = "Arah Angin Sore (3pm)",
  "WindSpeed9am" = "Kecepatan Angin Pagi (km/jam)",
  "WindSpeed3pm" = "Kecepatan Angin Sore (km/jam)",
  "Humidity9am" = "Kelembaban Pagi (%)",
  "Humidity3pm" = "Kelembaban Sore (%)",
  "Pressure9am" = "Tekanan Udara Pagi (hPa)",
  "Pressure3pm" = "Tekanan Udara Sore (hPa)",
  "Cloud9am" = "Tutupan Awan Pagi (Skala 0-8)",
  "Cloud3pm" = "Tutupan Awan Sore (Skala 0-8)",
  "Temp9am" = "Suhu Pagi (°C)",
  "Temp3pm" = "Suhu Sore (°C)",
  "RainToday" = "Hujan Hari Ini?",
  "RISK_MM" = "Risiko Hujan Besok (mm)",
  "RainTomorrow" = "Hujan Besok?"
)

# 2. Daftar Variabel untuk Pilihan Input
numeric_cols <- names(data_raw)[sapply(data_raw, is.numeric)]
categorical_cols <- names(data_raw)[sapply(data_raw, function(x) is.character(x) | is.factor(x))]
all_cols <- names(data_raw)

# 3. Antarmuka Pengguna (UI)
ui <- fluidPage(
  titlePanel("Aplikasi Weather Track"),
  
  sidebarLayout(
    sidebarPanel(
      # Pilih Jenis Visualisasi
      selectInput("plot_type", "Tampilkan Dengan:",
                  choices = c("Scatter Plot Interaktif" = "scatter",
                              "Line Plot Interaktif" = "line",
                              "Bar Plot Interaktif" = "bar",
                              "Tabel Data Interaktif" = "table")),
      
      # Pilihan untuk Scatter dan Line Plot
      conditionalPanel(
        condition = "input.plot_type == 'scatter' | input.plot_type == 'line'",
        selectInput("x_var", "Variabel X (Sumbu Horizontal):",
                    choices = numeric_cols, selected = "MinTemp"),
        selectInput("y_var", "Variabel Y (Sumbu Vertikal):",
                    choices = numeric_cols, selected = "MaxTemp"),
        selectInput("color_var_scl", "Variabel Warna/Grup (Opsional):",
                    choices = c("Tidak Ada" = "None", all_cols), selected = "RainToday")
      ),
      
      # Pilihan untuk Bar Plot
      conditionalPanel(
        condition = "input.plot_type == 'bar'",
        selectInput("bar_var", "Variabel Bar Plot (Kategorikal):",
                    choices = categorical_cols, selected = "WindGustDir")
      ),
      
      helpText("Dibuat oleh: dezssertsoul.")
    ),
    
    # Output Utama
    mainPanel(
      # Output Plotly
      conditionalPanel(
        condition = "input.plot_type == 'scatter' | input.plot_type == 'line' | input.plot_type == 'bar'",
        plotlyOutput("interactive_plot", height = "500px")
      ),
      # Output Data Table
      conditionalPanel(
        condition = "input.plot_type == 'table'",
        DTOutput("data_table")
      )
    )
  )
)

# 4. Logika Server
server <- function(input, output) {
  
  data_filtered <- reactive({
    req(input$plot_type)
    
    df <- data_raw
    
    if (input$plot_type == "scatter" | input$plot_type == "line") {
      req(input$x_var, input$y_var)
      
      cols_to_keep <- c(input$x_var, input$y_var)
      if (input$color_var_scl != "None") {
        cols_to_keep <- c(cols_to_keep, input$color_var_scl)
      }
      df <- df[, cols_to_keep, drop = FALSE]
      
    } else if (input$plot_type == "bar") {
      req(input$bar_var)
      df <- df[, input$bar_var, drop = FALSE]
    }
    
    df <- na.omit(df)
    
    return(df)
  })
  
  # Render Plot Interaktif
  output$interactive_plot <- renderPlotly({
    df_plot <- data_filtered()
    
    if (input$plot_type == "scatter") {
      # Scatter Plot
      p <- ggplot(df_plot, aes_string(x = input$x_var, y = input$y_var, 
                                      color = if (input$color_var_scl != "None") input$color_var_scl else NULL)) +
        geom_point(alpha = 0.7) +
        labs(title = paste("Scatter Plot:", input$y_var, "vs", input$x_var),
             x = input$x_var, y = input$y_var) +
        theme_minimal()
      
    } else if (input$plot_type == "line") {
      # Line Plot
      df_plot$Index <- 1:nrow(df_plot)
      
      p <- ggplot(df_plot, aes_string(x = "Index", y = input$y_var, 
                                      color = if (input$color_var_scl != "None") input$color_var_scl else NULL)) +
        geom_line() +
        labs(title = paste("Line Plot:", input$y_var, "sepanjang Observasi"),
             x = "Indeks Observasi", y = input$y_var) +
        theme_minimal()
      
    } else if (input$plot_type == "bar") {
      # Bar Plot untuk Frekuensi
      p <- ggplot(df_plot, aes_string(x = input$bar_var)) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Bar Plot Frekuensi:", input$bar_var),
             x = input$bar_var, y = "Frekuensi") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    } else {
      return(NULL)
    }
    
    # Konversi ggplot ke plotly agar interaktif
    ggplotly(p)
  })
  
  # Render Tabel Data Interaktif
  output$data_table <- renderDT({
    datatable(data_raw, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# 7. Jalankan Aplikasi

shinyApp(ui = ui, server = server)
