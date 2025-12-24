library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(lubridate)

# Adres do danych (plik CSV na GitHubie)
url <- "https://raw.githubusercontent.com/ksarniak2-uw/zbiorsentymentuapl/main/baza_SENTYMENT_PROSTY.csv"

# Wczytanie zbioru danych (separator średnik)
df_raw <- read_csv2(url)

# Wstępne przetworzenie danych (preprocessing)
df <- df_raw %>%
  # Ujednolicenie nazw zmiennych
  rename(
    data = Data,
    tytul = Tytul,
    zrodlo = Zrodlo,
    sentyment = Sentyment,
    link = Link
  ) %>%
  # Wybór tylko głównych kategorii sentymentu
  filter(sentyment %in% c("POZYTYWNY", "NEGATYWNY", "NEUTRALNY")) %>%
  mutate(
    # Konwersja kolumny data na format Date
    data_dzien = as.Date(data),
    # Wyznaczenie miesiąca do agregacji danych na wykresach
    data_miesiac = floor_date(data_dzien, "month"),
    # Konwersja zmiennych jakościowych na faktory
    sentyment = factor(sentyment),
    zrodlo = factor(zrodlo)
  ) %>%
  # Usunięcie braków danych w dacie
  filter(!is.na(data_dzien))

# Definicja interfejsu użytkownika (UI)
ui <- fluidPage(
  titlePanel("Analiza sentymentu - Projekt"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtry"),
      
      # Suwak do wyboru przedziału czasowego
      sliderInput(
        "zakres_dat",
        "Wybierz okres:",
        min = min(df$data_dzien),
        max = max(df$data_dzien),
        value = c(min(df$data_dzien), max(df$data_dzien)),
        timeFormat = "%Y-%m"
      ),
      
      # Wybór kategorii sentymentu
      checkboxGroupInput(
        "wybor_sentymentu",
        "Sentyment:",
        choices = levels(df$sentyment),
        selected = levels(df$sentyment)
      ),
      
      # Wybór źródła
      selectInput(
        "wybor_zrodla",
        "Źródło:",
        choices = c("Wszystkie", sort(levels(df$zrodlo))),
        selected = "Wszystkie"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        # Zakładka z wykresami ogólnymi
        tabPanel("Wykresy", 
                 br(),
                 plotOutput("wykres_czas"),
                 br(),
                 plotOutput("wykres_sentyment")
        ),
        # Zakładka z analizą dominanty
        tabPanel("Dominacja",
                 br(),
                 plotOutput("wykres_dominacja")
        ),
        # Zakładka z danymi surowymi
        tabPanel("Tabela", 
                 br(),
                 DTOutput("tabela_danych")
        )
      )
    )
  )
)

# Logika serwera (Backend)
server <- function(input, output) {
  
  # Reaktywne filtrowanie danych na podstawie widgetów UI
  dane_filtrowane <- reactive({
    temp <- df %>%
      filter(
        sentyment %in% input$wybor_sentymentu,
        data_dzien >= input$zakres_dat[1],
        data_dzien <= input$zakres_dat[2]
      )
    
    # Dodatkowy warunek dla źródła
    if (input$wybor_zrodla != "Wszystkie") {
      temp <- temp %>% filter(zrodlo == input$wybor_zrodla)
    }
    
    temp
  })
  
  # Paleta kolorów dla wykresów
  kolory <- c("POZYTYWNY" = "forestgreen", 
              "NEGATYWNY" = "firebrick", 
              "NEUTRALNY" = "gray50")
  
  # Wykres 1: Rozkład liczby artykułów w czasie
  output$wykres_czas <- renderPlot({
    dane_filtrowane() %>%
      count(data_miesiac, sentyment) %>%
      ggplot(aes(x = data_miesiac, y = n, fill = sentyment)) +
      geom_col() +
      scale_fill_manual(values = kolory) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "Rok", y = "Liczba artykułów") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Wykres 2: Ogólna struktura sentymentu
  output$wykres_sentyment <- renderPlot({
    ggplot(dane_filtrowane(), aes(x = sentyment, fill = sentyment)) +
      geom_bar() +
      scale_fill_manual(values = kolory) +
      labs(x = "Sentyment", y = "Liczebność") +
      theme_minimal() +
      theme(legend.position = "none") + 
      geom_text(stat='count', aes(label=..count..), vjust=-0.5)
  })
  
  # Wykres 3: Dominujący sentyment w poszczególnych miesiącach
  output$wykres_dominacja <- renderPlot({
    # Wyznaczenie najczęstszego sentymentu dla każdego miesiąca
    wyniki_miesiaca <- dane_filtrowane() %>%
      count(data_miesiac, sentyment) %>%
      group_by(data_miesiac) %>%
      top_n(1, n) %>% # Wybór wartości maksymalnej
      slice(1)
    
    ggplot(wyniki_miesiaca, aes(x = data_miesiac, y = n, fill = sentyment)) +
      geom_col() +
      scale_fill_manual(values = kolory) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "Rok", y = "Liczba (kategoria dominująca)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Tabela wynikowa
  output$tabela_danych <- renderDT({
    dane_filtrowane() %>%
      select(data_dzien, tytul, zrodlo, sentyment) %>%
      arrange(desc(data_dzien))
  }, options = list(pageLength = 10))
}

# Uruchomienie aplikacji
shinyApp(ui, server)