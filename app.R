# app.R

# 1. Muat (Load) Library yang Dibutuhkan
library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# --- Persiapan Data ---
# Catatan: Asumsi file data berada di direktori kerja yang sama.
# Kita menggunakan 'skip = 1' untuk melewati baris header pertama ("weather")
# yang terdeteksi saat file di-konversi ke CSV.
file_path <- "Tugas 3 - weather.csv"

# Baca data
weather_data <- read.csv(file_path, skip = 1, stringsAsFactors = FALSE)

# Tambahkan kolom ID (Indeks Baris) untuk plotting berurutan
weather_data <- weather_data %>%
  mutate(Index = 1:n())

# Ambil semua nama kolom untuk pilihan variabel, kecuali 'Index'
variable_choices <- names(weather_data)[names(weather_data) != "Index"]

# --------------------------------------------------------------------------------

# 2. User Interface (UI)
ui <- fluidPage(
  
  # Judul Aplikasi
  titlePanel("Visualisasi Data Cuaca Interaktif"),
  
  # Tata letak sidebar
  sidebarLayout(
    
    # Panel Samping (Input)
    sidebarPanel(
      
      h3("Pengaturan Visualisasi"),
      
      # Input 1: Pilihan Variabel
      selectInput(
        inputId = "variable",
        label = "Pilih Variabel yang Ingin Divisualisasikan:",
        choices = variable_choices,
        selected = "MaxTemp" # Default selection
      ),
      
      # Input 2: Pilihan Jenis Plot
      radioButtons(
        inputId = "plot_type",
        label = "Pilih Jenis Visualisasi:",
        choices = c(
          "a. Scatter Plot Interaktif" = "scatter",
          "b. Line Plot Interaktif" = "line",
          "c. Bar Plot Interaktif" = "bar",
          "d. Tabel Data Interaktif" = "table"
        ),
        selected = "scatter"
      ),
      
      # Keterangan Plot Bar
      p(HTML("<em>Catatan: Bar Plot paling cocok untuk variabel Kategori/Diskret (misalnya: WindGustDir, RainToday).</em>"))
      
    ),
    
    # Panel Utama (Output)
    mainPanel(
      
      # Output untuk Plotly (Plot Interaktif)
      conditionalPanel(
        condition = "input.plot_type != 'table'",
        plotlyOutput("interactive_plot")
      ),
      
      # Output untuk DT (Tabel Interaktif)
      conditionalPanel(
        condition = "input.plot_type == 'table'",
        DTOutput("interactive_table")
      )
    )
  )
)

# --------------------------------------------------------------------------------

# 3. Server (Logika Aplikasi)
server <- function(input, output) {
  
  # Logika untuk Plot Interaktif (Scatter, Line, Bar)
  output$interactive_plot <- renderPlotly({
    
    # Ambil data kolom yang dipilih
    selected_col_name <- input$variable
    
    # ------------------
    # Cek Tipe Plot yang Dipilih
    # ------------------
    
    if (input$plot_type %in% c("scatter", "line")) {
      
      # Scatter dan Line Plot: Plot variabel (Y) terhadap Indeks (X)
      # Ini berguna untuk melihat tren berurutan atau pola data harian.
      p <- ggplot(weather_data, aes_string(x = "Index", y = selected_col_name)) +
        labs(
          title = paste("Visualisasi", selected_col_name, "terhadap Indeks Hari"),
          x = "Indeks Hari (Urutan Pengamatan)",
          y = selected_col_name
        ) +
        theme_minimal()
      
      if (input$plot_type == "scatter") {
        p <- p + geom_point(alpha = 0.6)
      } else if (input$plot_type == "line") {
        p <- p + geom_line(group = 1, color = "blue", alpha = 0.7)
      }
      
    } else if (input$plot_type == "bar") {
      
      # Bar Plot: Menghitung frekuensi (count) dari nilai-nilai unik
      
      # Menghitung frekuensi
      df_count <- weather_data %>%
        count(.data[[selected_col_name]], sort = TRUE, name = "Frequency") %>%
        # Ganti nama kolom untuk plotting yang lebih bersih
        rename(Category = 1)
      
      p <- ggplot(df_count, aes(x = Category, y = Frequency, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(
          title = paste("Frekuensi Nilai untuk Variabel:", selected_col_name),
          x = selected_col_name,
          y = "Frekuensi"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotasi label X
    }
    
    # Konversi plot ggplot2 ke plotly agar interaktif
    ggplotly(p)
  })
  
  # Logika untuk Tabel Data Interaktif
  output$interactive_table <- renderDT({
    # Menampilkan seluruh data
    datatable(
      weather_data,
      options = list(
        pageLength = 10, # 10 baris per halaman
        scrollX = TRUE   # Aktifkan scroll horizontal
      ),
      # Hapus kolom 'Index' yang hanya digunakan untuk plotting
      filter = 'top',
      rownames = FALSE
    )
  })
  
}

# 4. Jalankan Aplikasi
shinyApp(ui = ui, server = server)