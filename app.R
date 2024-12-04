library(shiny)
library(bslib)
library(ggplot2)
library(glue)
library(dplyr)
library(tidyverse)
library(scales)
library(highcharter)
library(caret)
library(rpart)
library(rpart.plot)
library(DT)

# Input data
data <- read.csv("https://raw.githubusercontent.com/seanmarshelleproj/dashboard.proj/refs/heads/main/Data/mxmh_survey_results.csv")
# Menghapus data NA
data$Age <- as.numeric(data$Age)
# Menghapus outlier
Q1 <- quantile(data$Age, 0.25, na.rm = TRUE)  
Q3 <- quantile(data$Age, 0.75, na.rm = TRUE)  
IQR <- Q3 - Q1                               
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data <- data[data$Age >= lower_bound & data$Age <= upper_bound, ]
# Mengubah string menjadi kategori
data <- data %>%
  mutate(
    StreamingCategory = case_when(
      Primary.streaming.service %in% c("Spotify", "Apple Music", "YouTube Music", "Pandora") ~ "Major Streaming Services",
      Primary.streaming.service == "Other streaming service" ~ "Other",
      Primary.streaming.service == "I do not use a streaming service." ~ "Not Using Streaming",
      TRUE ~ "Unknown"))
# Menghapus variable yang tidak digunakan
data <- data %>% select(-Permissions)
data<- data %>% select(-Timestamp)
# Mengubah menjadi kategori
data <- data %>%
  mutate(
    Anxiety=as.integer(Anxiety),
    OCD=as.integer(OCD),
    Insomnia=as.integer(Insomnia),
    Depression=as.integer(Depression))
categorical_columns <- c(
  "Frequency..Classical.", "Frequency..Country.", "Frequency..EDM.",
  "Frequency..Folk.", "Frequency..Gospel.", "Frequency..Hip.hop.",
  "Frequency..Jazz.", "Frequency..K.pop.", "Frequency..Latin.",
  "Frequency..Lofi.", "Frequency..Metal.", "Frequency..Pop.",
  "Frequency..R.B.", "Frequency..Rap.", "Frequency..Rock.",
  "Frequency..Video.game.music.", "Music.effects", "Instrumentalist",
  "While.working", "Composer", "Fav.genre", "Exploratory", "Foreign.languages")
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)
music_effects_map <- c("No effect" = 0, "Worsen" = 1, "Improve" = 2)
data$Music.effects <- as.numeric(factor(data$Music.effects, levels = names(music_effects_map), labels = unname(music_effects_map)))
# Kolom frequency
frequency_columns <- c(
  "Frequency..Classical.", "Frequency..Country.", "Frequency..EDM.",
  "Frequency..Folk.", "Frequency..Gospel.", "Frequency..Hip.hop.",
  "Frequency..Jazz.", "Frequency..K.pop.", "Frequency..Latin.",
  "Frequency..Lofi.", "Frequency..Metal.", "Frequency..Pop.",
  "Frequency..R.B.", "Frequency..Rap.", "Frequency..Rock.",
  "Frequency..Video.game.music.")
# Kolom Nmerik
numerical_columns <- names(data)[sapply(data, is.numeric)]
# Tema
custom_theme <- bs_theme(
  bootswatch = "minty",
  primary = "#3498db",  
  secondary = "#2ecc71",
  success = "#28a745",  
  info = "#17a2b8",     
  warning = "#ffc107",  
  danger = "#dc3545",   
  base_font = font_google("Roboto"))

# ui
ui <- page_navbar(theme=custom_theme,
# Halaman utama
    nav_panel( title = tags$div(
    style = "display: flex; align-items: center;", 
    tags$span(class = "fa fa-house", style = "margin-right: 10px;"),
    "Halaman Utama"),
    fluidRow(column(12,div(
          style = "position: relative; height: auto; overflow: hidden;",
          card(
            tags$img(
              src = "https://raw.githubusercontent.com/seanmarshelleproj/dashboard.proj/main/Gambar/picture%201.jpg",
              alt = "Picture1",
              style = "width: 100%; height: auto; display: block;"),
            tags$p(
              "Musik memiliki pengaruh signifikan terhadap kesehatan mental.
              Mendengarkan musik dapat merangsang emosi positif, mengurangi stress, dan 
              dapat meningkatkan suasana hati. Musik juga dapat digunakan dalam terapi untuk
              membantu dalam menurunkan tingkat depresi, gangguan kecemasan, dan taruma.
              Dashboard ini berisi data mengenai pengaruh musik terhadap kesehatan mental seseorang.",
              style = "text-align: justify; font-size: 18px; bold; margin-top: 10px;"),
            tags$img(
              src = "https://raw.githubusercontent.com/seanmarshelleproj/dashboard.proj/refs/heads/main/Gambar/Logo%20IPB%20University_Horizontal.png",
              alt="Picture2",
              style="width: 25%; height: auto; display: block; margin-left: auto; margin-right: auto;"))))),
    fluidRow(column(12,div(
          style = "margin-top: 10px;",
          card(dataTableOutput("table")))))),

# Eksplorasi data 
    nav_panel( title = tags$div(
    style = "display: flex; align-items: center;",  
    tags$span(class = "fa fa-chart-pie", style = "margin-right: 10px;"),
    "Explorasi Data"),
            navset_pill_list(
              # Genre musik
              nav_panel( title = tags$div(
                style = "display: flex; align-items: center; font-weight: bold;", 
                tags$span(class = "fa fa-headphones", style = "margin-right: 10px;"), # Ikon dokter (Font Awesome)
                "Genre Musik"),
                        sliderInput("genre_slider", "Umur",
                                    min = min(data$Age, na.rm = TRUE),
                                    max = max(data$Age, na.rm = TRUE),
                                    value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE))),
                        splitLayout(
                          cellWidths = c("50%", "50%"),
                          checkboxGroupInput("checkbox_group1",
                                             "Genre Musik",
                                             choices = unique(data$Fav.genre[1:ceiling(length(unique(data$Fav.genre))/2)]),
                                             selected = unique(data$Fav.genre)),
                          checkboxGroupInput("checkbox_group2",
                                             "Genre Musik",
                                             choices = unique(data$Fav.genre[(ceiling(length(unique(data$Fav.genre))/2) + 1):length(unique(data$Fav.genre))]),
                                             selected = unique(data$Fav.genre))),
                        highchartOutput("genre_bar_chart")),
              # Layanan streaming 
              nav_panel( title = tags$div(
                 style = "display: flex; align-items: center; font-weight: bold;", 
                 tags$span(class = "fa fa-youtube", style = "margin-right: 10px;"), 
                 "Layanan Streaming"),
                        checkboxGroupInput("checkbox_group",
                                           "Aplikasi streaming",
                                           choices = unique(data$Primary.streaming.service[!is.na(data$Primary.streaming.service) & data$Primary.streaming.service != ""]),
                                           selected = unique(data$Primary.streaming.service[!is.na(data$Primary.streaming.service) & data$Primary.streaming.service != ""])),
                        highchartOutput("pie_chart")),
              # Kesehatan mental
               nav_panel(title = tags$div(
                style = "display: flex; align-items: center; font-weight: bold;", 
                tags$span(class = "fa fa-user-md", style = "margin-right: 10px;"), 
                "Kesehatan Mental"),
                        fluidRow(column(4,selectInput(
                                   "select1", "Masalah kesehatan mental",
                                      choices = c(
                                     "Anxiety" = "Anxiety",
                                     "OCD" = "OCD",
                                     "Depresi" = "Depression",
                                     "Insomnia" = "Insomnia")),
                                 tags$h4("Summary Table"),
                                 tableOutput("table_kesehatan")),
                          column(8,highchartOutput("bar_chart_mental")))),
              # Korelasi
              nav_panel(title = tags$div(
                style = "display: flex; align-items: center; font-weight: bold;", 
                tags$span(class = "fa fa-project-diagram", style = "margin-right: 10px;"), 
                "Korelasi"),
                layout_columns(
                  card("Pilih Prediktor 1",
                       selectInput(
                         inputId = "Predictor1",
                         label = "Pilih 1 Prediktor: ",
                         choices = numerical_columns)
                       ),
                  card("Pilih Prediktor 2",
                       selectInput(
                         inputId = "Predictor2",
                         label = "Pilih 1 Prediktor: ",
                         choices = numerical_columns)
                       ),
                ),
                layout_columns(
                  card("Hasil Korelasi",
                       dataTableOutput("correlation_table")))),
              # Frekuensi
              nav_panel( title = tags$div(
                style = "display: flex; align-items: center; font-weight: bold;", 
                tags$span(class = "fa fa-repeat", style = "margin-right: 10px;"), 
                "Frekuensi"),
                selectInput("select2","Pilih Frekuensi Mendengarkan Genre Lagu: ",
                            choices = frequency_columns),
                checkboxGroupInput("checkbox_group_freq",
                                   "Frekuensi Mendengarkan",
                                   choices = c("Rarely","Sometimes","Never","Very frequently"),
                                   selected = c("Rarely","Sometimes","Never","Very frequently")),
                highchartOutput("pie_chart_frequency")),
            )
  ),
  # Pemodelan
  nav_panel(title = tags$div(
    style = "display: flex; align-items: center;",  
    tags$span(class = "fa fa-tree", style = "margin-right: 10px;"),
    "Decision Tree"),
            layout_columns(
              card(tags$div(
                style = "display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 20px;",
                tags$span(class = "fa fa-chart-bar", style = "margin-right: 10px;"),
                "Pemodelan"),
                   layout_column_wrap(
                     width = 1,
                     card(
                       fluidRow(
                         column(8,
                                selectizeInput(
                                  inputId = "Predictors",
                                  label = tags$div(
                                    style = "display: flex; align-items: center; font-weight: bold;",
                                    tags$span(class = "fa fa-check-square", style = "margin-right: 10px;"),
                                    "Pilih Prediktor"
                                  ),
                                  choices = setdiff(names(data), "Music.effects"),
                                  multiple = TRUE
                                )
                         ),
                         column(4,
                                br(),
                                input_task_button("fit_model", "Fit Model")
                         )
                       ),
                       tableOutput("table metrik dt")
                     )
                   )),
              layout_column_wrap(
                width = 1,
                card(
                  plotOutput("Plot dt"),
                  downloadButton("download_tree_plot","Download Plot", class="btn btn-primary"),
                  plotOutput("Kesalahan Klasifikasi dt")
                )
              )
            )
  ),
  # Tentang
nav_panel(
  title = tags$div(
    style = "display: flex; align-items: center;", 
    tags$span(class = "fa fa-exclamation-circle", style = "margin-right: 10px;"),
    "Tentang"
  ),
  tags$div(
    tags$div(
      style = "display: flex; align-items: center; justify-content: center; margin-bottom: 20px;",
      tags$span(
        class = "fa fa-users", 
        style = "font-size: 24px; margin-right: 10px; color: #007BFF;" 
      ),
      tags$h3(
        "Tim Pengembang",
        style = "margin: 0;" 
      )
    ),
    tags$div(
      style = "display: flex; justify-content: center; gap: 20px;", 
      tags$div(
        style = "flex: 1; max-width: 300px; text-align: center;", 
        card(
          img(
            src = "https://raw.githubusercontent.com/seanmarshelleproj/dashboard.proj/refs/heads/main/Gambar/IMG_20241013_173546.jpeg", 
            style = "width: 150px; height: 180px; border-radius: 10%; margin: 0 auto; display: block;" 
          ),
          tags$div(
            tags$p("Sean Marshelle", style = "font-weight: bold; margin: 10px 0 0;"), 
            tags$p("G1501231012", style = "margin: 0;"), # NIM
            tags$p("seanmarshelle@apps.ipb.ac.id", style = "margin: 0; font-size: 14px;")
          )
        )
      ),
      tags$div(
        style = "flex: 1; max-width: 300px; text-align: center;", 
        card(
          img(
            src = "https://raw.githubusercontent.com/seanmarshelleproj/dashboard.proj/refs/heads/main/Gambar/DSC05437.JPG", 
            style = "width: 150px; height: 180px; border-radius: 10%; margin: 0 auto; display: block;"
          ),
          tags$div(
            tags$p("Siti Mutiah", style = "font-weight: bold; margin: 10px 0 0;"), 
            tags$p("G1501231027", style = "margin: 0;"), # NIM
            tags$p("smutiah842@gmail.com", style = "margin: 0; font-size: 14px;") 
          )
        )
      ),
      tags$div(
        style = "flex: 1; max-width: 300px; text-align: center;", # Styling kartu
        card(
          img(
            src = "https://raw.githubusercontent.com/seanmarshelleproj/dashboard.proj/refs/heads/main/Gambar/1732934370397.jpg",
            style = "width: 150px; height: 180px; border-radius: 10%; margin: 0 auto; display: block;" 
          ),
          tags$div(
            tags$p("Tasya Anisah Rizqi", style = "font-weight: bold; margin: 10px 0 0;"), 
            tags$p("G1501231046", style = "margin: 0;"), # NIM
            tags$p("tasyaanisahrizqi@gmail.com", style = "margin: 0; font-size: 14px;") 
          ))))),
  layout_columns(
    card(
      card_header(
        tags$div(
          style = "display: flex; align-items: center; justify-content: center;",
          tags$span(class = "fa fa-table", style = "margin-right: 10px; font-size: 20px;"),
          "Data")),
      p("Terapi musik adalah penggunaan musik untuk meningkatkan stres, suasana hati,
        dan kesehatan mental pada seseorang. Terapi ini diakui sebagai praktik
        berbasis bukti sebagai katalis untuk hormon bahagia yaitu oksitosin.",
        style = "text-align: justify;"),
      p("Dataset ini bertujuan untuk mengidentifikasi apakah terdapat korelasi
        antara selera musik individu dengan kesehatan mental. Data ini dapat
        berkontribusi penerapan terapi musik dan memberikan pandangan menarik
        tentang pikiran.", style = "text-align: justify;"),
      p("Menginterpretasikan data:", style = "text-align: justify; font-weight: bold;"),
      p("Block 0: Background", style = "text-align: justify; font-weight: bold;"),
      p("Responden menjawab pertanyaan umum yang berfokus pada latar belakang musik
        dan kebiasaan mendengarkan.", style = "text-align: justify;"),
      p("Block 1: Genre Musik", style = "text-align: justify; font-weight: bold;"),
      p("Responden memberikan peringkat seberapa sering mereka mendengarkan
        16 genre musik, dimana mereka dapat memilih:", style = "text-align: justify;"),
      p("Tidak pernah", style = "text-align: justify;"),
      p("Jarang", style = "text-align: justify;"),
      p("Kadang-kadang", style = "text-align: justify;"),
      p("Sangat sering", style = "text-align: justify;"),
      p("Block 2: Kesehatan Mental", style = "text-align: justify; font-weight: bold;"),
      p("Responden memberi peringkat kecemasan, depresi, insomnia, dan OCD pada
        skala 0 hingga 10 dimana:", style = "text-align: justify;"),
      p("0 - Saya tidak mengalami ini", style = "text-align: justify;"),
      p("10 - Saya mengalami secara teratur, terus-menerus atau
        secara ekstrim", style = "text-align: justify;"),
      downloadButton("downloaddata","Download Data")),
    card(
      card_header(
        tags$div(
          style = "display: flex; align-items: center; justify-content: center;",
          tags$span(class = "fa fa-cogs", style = "margin-right: 10px; font-size: 20px;"),
          "Pemodelan Decision Tree"
        )
      ),
      p("Decission Tree merupakan metode pembelajaran mesin yang populer
        dengan kemampuan untuk melakukan analisa dataset dengan menghasilkan
        sebuah set peraturan pemilihan. Peraturan ini diatur dalam sebuah pohon
        dimana setiap variable membuat sebuah node pemilihan. Metode ini sangat cocok
        digunakan pada data kategorik dan kontinu, mudah dipahami dan waktu komputasi
        yang cepat (Mutlu, et al., 2023).", style = "text-align: justify;"),
      tags$img(
        src = "https://raw.githubusercontent.com/seanmarshelleproj/dashboard.proj/refs/heads/main/Gambar/decision_tree_2024-12-04.png",
        alt = "Picture3",
        style = "width: 100%; height: auto; display: block;"),
      p("Daftar Pustaka", style = "text-align: center; font-weight: bold;"),
      p("Mutlu, H. B., Yucel, N., Durmaz, F., Cengil, E., dan Yildirim, M. 2023.
        Prediction of Maternal Health Risk with Traditional Machine Learning Methods.
        NATURENGS: MTU Journal of Engineering and Natural Sciences, 4(1): 16 - 23.", 
        style = "text-align: justify;"),
      ))),
  title = "Dashboard Pengaruh Musik terhadap Kesehatan Mental",
  id = "page"
)

# server
server <- function(input, output){
  
  # tabel data
  output$table <-
    renderDataTable({datatable(data, selection="single")})
 
   # Plot umur dan genre
  filter_genre <- reactive({
    selected_genres <- c(input$checkbox_group1, input$checkbox_group2)
    data %>%
      filter(Age >= input$genre_slider[1], Age <= input$genre_slider[2]) %>%
      filter(Fav.genre %in% selected_genres)
  })
  
  output$genre_bar_chart <- renderHighchart({
    filtered_data <- filter_genre()
    genre_age_data <- filtered_data %>%
      group_by(Fav.genre) %>%
      count() %>%
      arrange(desc(n))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Distribusi Genre Kesukaan Berdasarkan Umur") %>%
      hc_xAxis(categories = genre_age_data$Fav.genre, title = list(text = "Genres")) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_add_series(
        name = "Count",
        data = genre_age_data$n
      ) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
  })
  
  # stream
  filter_streaming <- reactive({
    data %>%
      filter(Primary.streaming.service %in% input$checkbox_group)
  })
  output$pie_chart <- renderHighchart({
    filtered_data <- filter_streaming()
    
    pie_data <- filtered_data %>%
      count(Primary.streaming.service) %>%
      mutate(percent = n / sum(n) * 100)
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Layanan Streaming") %>%
      hc_add_series(
        name = "Percentage",
        data = list_parse2(
          pie_data %>%
            select(name = Primary.streaming.service, y = percent)
        )
      ) %>%
      hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = '{point.name}: {point.y:.1f}%')))
  })
  
  # masalah kesehatan mental
  filter_mental_health <- reactive({
    selected_column <- input$select1
    data %>%
      filter(!is.na(.data[[selected_column]])) %>%  
      group_by(Score = .data[[selected_column]]) %>%  
      summarise(Count = n(), .groups = "drop")  
  })
  output$table_kesehatan <- renderTable({
    filtered_data <- filter_mental_health()
    filtered_data
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
  
  output$bar_chart_mental <- renderHighchart({
    filtered_data <- filter_mental_health() 
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = paste("Distribusi dari", input$select1, "Scores")) %>%
      hc_xAxis(categories = filtered_data$Score, title = list(text = "Score")) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_add_series(
        name = input$select1,
        data = filtered_data$Count,
        type = "column"
      ) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
  })
  output$table_kesehatan <- renderTable({
    filtered_data <- filter_mental_health()
    filtered_data
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
  
  # korelasi
  output$correlation_table <- renderDataTable({
    req(input$Predictor1, input$Predictor2)
    predictor1 <- input$Predictor1
    predictor2 <- input$Predictor2
    correlation <- cor(data[[predictor1]], data[[predictor2]], use = "complete.obs", method = "pearson")
    result <- data.frame(
      Predictor1 = predictor1,
      Predictor2 = predictor2,
      Correlation = round(correlation, 2)
    )
    datatable(result, options = list(pageLength = 5))
  })
  
  # Frekuensi
  output$pie_chart_frequency <- renderHighchart({
    data <- data.frame(
      Genre = rep(c("Frequency..Classical.", "Frequency..Country.", "Frequency..EDM.",
                    "Frequency..Folk.", "Frequency..Gospel.", "Frequency..Hip.hop.",
                    "Frequency..Jazz.", "Frequency..K.pop.", "Frequency..Latin.",
                    "Frequency..Lofi.", "Frequency..Metal.", "Frequency..Pop.",
                    "Frequency..R.B.", "Frequency..Rap.", "Frequency..Rock.",
                    "Frequency..Video.game.music."), each = 4),
      Frequency = rep(c("Rarely", "Sometimes", "Never", "Very frequently"), times = 16),
      Count = sample(1:100, 64, replace = TRUE)
    )
    filtered_data <- data %>%
      dplyr::filter(
        Genre == input$select2,        
        Frequency %in% input$checkbox_group_freq
      )
    chart_data <- filtered_data %>%
      dplyr::group_by(Frequency) %>%
      dplyr::summarise(Total = sum(Count)) %>%
      dplyr::arrange(desc(Total))
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = paste("Distribusi Frekuensi Mendengarkan -", input$select2)) %>%
      hc_series(
        list(
          name = "Frekuensi",
          data = list_parse2(chart_data),
          dataLabels = list(
            enabled = TRUE,
            format = '{point.name}: {point.percentage:.1f}%')))})
  
  # Decision tree server logic
  observeEvent(input$fit_model, {
    tryCatch({
      if (is.null(input$Predictors) || length(input$Predictors) == 0) {
        showNotification("Pilih minimal 1 variable.", type = "error")
        return()}
      formula <- as.formula(paste("Music.effects ~", paste(input$Predictors, collapse = " + ")))
      dt_data <- data %>% select(c(input$Predictors, "Music.effects")) %>% na.omit()
      dt_data$Music.effects <- factor(dt_data$Music.effects)
      set.seed(123) 
      train_index <- createDataPartition(dt_data$Music.effects, p = 0.7, list = FALSE)
      train_data <- dt_data[train_index, ]
      test_data <- dt_data[-train_index, ]
      tree_model <- rpart(formula, data = train_data, method = "class")
      all_predictions <- predict(tree_model, dt_data, type = "class")
      all_predictions <- factor(all_predictions, levels = levels(dt_data$Music.effects))
      all_confusion <- confusionMatrix(all_predictions, dt_data$Music.effects)
      accuracy <- all_confusion$overall["Accuracy"]
      precision_per_class <- all_confusion$byClass[, "Pos Pred Value", drop = FALSE]
      recall_per_class <- all_confusion$byClass[, "Sensitivity", drop = FALSE]
      f1_score_per_class <- 2 * (precision_per_class * recall_per_class) /
        (precision_per_class + recall_per_class)
      f1_score_per_class[is.na(f1_score_per_class)] <- 0
      macro_precision <- mean(precision_per_class, na.rm = TRUE)
      macro_recall <- mean(recall_per_class, na.rm = TRUE)
      macro_f1_score <- mean(f1_score_per_class, na.rm = TRUE)
      metrics <- data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
        Nilai = c(
          paste0(round(accuracy * 100, 2), "%"),
          paste0(round(macro_precision * 100, 2), "%"),
          paste0(round(macro_recall * 100, 2), "%"),
          paste0(round(macro_f1_score * 100, 2), "%")))
      output$`table metrik dt` <- renderTable({
        metrics
      }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
      output$`Plot dt` <- renderPlot({
        rpart.plot(
          tree_model,
          main = "Decision Tree untuk variable respon (Music Effect)",
          sub = "Class labels: 1 = Tidak ada efek, 2 = Memburuk, 3 = Membaik",
          box.palette = "GnBu",
          shadow.col = "gray",
          nn = TRUE  )})
      output$download_tree_plot <- downloadHandler(
        filename = function() {
          paste("decision_tree_", Sys.Date(), ".png", sep = "")},
        content = function(file) {
          png(file, width = 800, height = 600)  
          rpart.plot(
            tree_model,
            main = "Decision Tree for Music Effects",
            sub = "Class labels: 1 = Tidak ada efek, 2 = Memburuk, 3 = Membaik",
            box.palette = "GnBu",
            shadow.col = "gray",
            nn = TRUE  )
          dev.off()})
      output$`Kesalahan Klasifikasi dt` <- renderPlot({
        error_rate <- 1 - accuracy
        error_rate_numeric <- as.numeric(error_rate)
        error_data <- data.frame(
          Data = "All Data",
          ErrorRate = error_rate_numeric)
        ggplot(error_data, aes(x = Data, y = ErrorRate, fill = Data)) +
          geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
          geom_text(aes(label = scales::percent(ErrorRate)), vjust = -0.5, size = 5) +
          labs(
            title = "Kesalahan Klasifikasi pada Data",
            x = "Data",
            y = "Kesalahan Klasifikasi"
          ) +
          scale_y_continuous(limits = c(0, 1), expand = c(0, 0), labels = scales::percent) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))})
      }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")})})
  
  # download data
  output$downloaddata <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui=ui, server=server)