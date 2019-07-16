##################################################
############## INSTALACJA PAKIETÓW ###############
##################################################

packages <- c("tidyverse", "data.table", "lattice", "gridExtra")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

##################################################
############### WCZYTANIE PAKIETÓW ###############
##################################################
library(tidyverse)

##################################################
################### INSTRUKCJA ###################
##################################################

## Przed puszczeniem tego skryptu trzeba upewnić się, że w katalogu, w którym jest plik nazwa_projektu.Rproj
## są też puste katalogi kooperacja i rywalizacja

##################################################
################### OPIS DANYCH ##################
##################################################

## Defualtowo wczytywane są pliki z katalogów Akcelerometr1 i surowe. Z tego pierwszego wczytywane są pliki
## csv, a z drugiego txt. Zanim się puści ten skrypt warto się przyjrzeć jak wyglądają dane. W danych dla
## których ten skrypt został napisany pliki csv miały 12 kolumn: pierwsze 3 to były kąty Eulera z ahrs,
## potem trzy kolumny z przyspieszeniem, trzy z prędkością i trzy z magnetometrem. W tym skrypcie używałem
## tylko kolumn z surowymi danymi z akcelerometru (nie do końca wiedziałem jak była liczona prędkość).
## W przypadku plików txt są w nich jedynie dane z akcelerometru. W przypadku danych z pliku csv to zbierane
## były one z częstotliwością 4Hz, a w przypadku plików txt pliki, które w nazwie miały long z częstotliwością
## 8Hz, a bez z 4Hz. Niezależnie od typu plików używałem płytki MinIMU-9 v3 Gyro, Accelerometer,
## and Compass (L3GD20H and LSM303D Carrier)

##################################################
############### PRZYGOTOWANIE DANYCH #############
##################################################

## Niezależnie od tego czy plik ma rozszerzenie csv czy txt to w nazwie powinien mieć albo ryw albo kop. Na 
## podstawie nazwy są potem pliki przydzielane do warunków. Ogólnie nazwy plików powinny wyglądać w następujący
## sposób: osoba01mkop.*. Ważne jest, żeby osoby będące w tej same parze miały kolejne po sobie nazwy.

##################################################
#################### FUNKCJE #####################
##################################################

read_files <- function(type = 'txt', folder = 'dane'){
  ## Wczytuje surowe dane z akcelerometru i w zależności od częstotliwości przelicza dane surowe na mm/s^2.
  ## Jeśli plik ma w nazwie long, to zakłada częstotliwość 8Hz, a jak nie to 4Hz.
  ## Jako argumenty bierze:
    ## type - rozszerzenie plików, które ma wczytać.
    ## folder - katalog, w którym znajdują się pliki.
  ## Zwraca listę tibble, gdzie każdy element jest tabelą danych z trzema kolumnami: AX, AY i AZ.
  if (type == 'txt') {
    list_of_files_name <- list.files(path = paste0(folder, "/"), pattern = ".txt")
    list_of_files <- list()
    for (name in list_of_files_name){
      normalizator <- if_else(grepl(x = name, pattern = "long"), .244, .122)
      list_of_files[[name %>%
                       str_extract("[:alnum:]+")]] <- read_table(paste0(folder,"/",name),
                                                                 col_names = FALSE,
                                                                 skip = 1) %>%
        mutate(X2 = as.numeric(X2),
               X3 = as.numeric(X3),
               X4 = as.numeric(X4),
               AX = X2 * normalizator,
               AY = X3 * normalizator,
               AZ = X4 * normalizator) %>%
        na.omit() %>%
        select(AX, AY, AZ) %>%
        na.omit()
    }
  } 
  if (type == 'csv') {
    list_of_files_name <- list.files(path = paste0(folder, "/"), pattern = ".csv")
    list_of_files <- list()
    for (name in list_of_files_name) {
      list_of_files[[name %>%
                       str_extract("[:alnum:]+")]] <- data.table::fread(paste0(folder,"/",name)) %>%
        rename(X1 = 1,
               X2 = 2,
               X3 = 3,
               X4 = 4,
               X5 = 5,
               X6 = 6,
               X7 = 7,
               X8 = 8,
               X9 = 9,
               X10 = 10,
               X11 = 11,
               X12 = 12) %>%
        mutate(X4 = X4 %>% 
                 str_extract("[-[:digit:]]+") %>%
                 as.numeric(),
              X5 = as.numeric(X5),
              X6 = as.numeric(X6),
              X7 = as.numeric(X7),
              X8 = as.numeric(X8),
              X9 = as.numeric(X9),
              AX = X4 * .244,
              AY = X5 * .244,
              AZ = X6 * .244) %>%
        select(AX, AY, AZ) %>%
        as_tibble() %>%
        na.omit()
        
    }
  }
  return(list_of_files)
}


compute_mean <- function(list_of_df){
  ## Ta jest funkcja, która uśrednia wychylenia głowy do połowy sekundy
  ## Jako argumenty bierze:
  ## list_of_df - listę tibble
  ## Zwraca listę z tibble, która ma następujące kolumny:
    ## AX - Pierwsze współrzędna z akcelerometru uśredniona
    ## AY - Druga współrzędna z akcelerometru uśredniona
    ## AZ - Trzecia współrzędna z akcelerometru uśredniona
    ## VX - Pierwsza współrzędna prędkości uśredniona
    ## VY - Druga współrzędna prędkości uśredniona
    ## VZ - Trzecie współrzędna prędkości uśredniona
    ## Alength - długość wektora przyspieszenia
    ## Vlength - długość wektora prędkości
  results <- list()
  for (i in 1:length(list_of_df)){
    table <- list_of_df[[i]]
    if (grepl(x = names(list_of_df[i]), pattern = "long")){
      id <-  rep(1:ceiling(nrow(table)/4), 4) %>% sort()
      id <- id[1:nrow(table)]
      results[[names(list_of_df[i])]] <- table %>%
        mutate(id = id,
               VX = (AX - lag(AX)) / (1/8),
               VY = (AY - lag(AY)) / (1/8),
               VZ = (AZ - lag(AZ)) / (1/8)) %>%
        group_by(id) %>%
        summarise(AX = mean(AX, na.rm = TRUE),
                  AY = mean(AY, na.rm = TRUE),
                  AZ = mean(AZ, na.rm = TRUE),
                  VX = mean(VX, na.rm = TRUE),
                  VY = mean(VY, na.rm = TRUE),
                  VZ = mean(VZ, na.rm = TRUE)) %>%
        mutate(Vlength = sqrt(VX^2 + VY^2 + VZ^2),
               Alength = sqrt(AX^2 + AY^2 + AZ^2))
    } else {
      id <-  rep(1:ceiling(nrow(table)/2),2) %>% sort()
      id <- id[1:nrow(table)]
      results[[names(list_of_df[i])]] <- table %>%
        mutate(id = id,
               VX = (AX - lag(AX)) / (1/4),
               VY = (AY - lag(AY)) / (1/4),
               VZ = (AZ - lag(AZ)) / (1/4)) %>%
        group_by(id) %>%
        summarise(AX = mean(AX, na.rm = TRUE),
                  AY = mean(AY, na.rm = TRUE),
                  AZ = mean(AZ, na.rm = TRUE),
                  VX = mean(VX, na.rm = TRUE),
                  VY = mean(VY, na.rm = TRUE),
                  VZ = mean(VZ, na.rm = TRUE)) %>%
        mutate(Vlength = sqrt(VX^2 + VY^2 + VZ^2),
               Alength = sqrt(AX^2 + AY^2 + AZ^2))
    }
  }
  return(results)  
}

read_input <- function(chart, place){
  ## To jest funkcja, która służy do pytania użytkownika o liczbę.
  ## Jako argumenty bierze:
    ## chart - string z nazwą wykresu
    ## place - string z miejscem, o które jest pytanie.
  ## Jeśli dostanie inny znak to zwraca -1 i komunikat, że użytkownik jest kretynem.
  ## Jeśli dostanie liczbę to zwraca liczbę
  
  prompt <- paste0(chart,"."," Podaj miejsce, w którym trzeba odciąć na ", place, ": ")
  result <- readline(prompt = prompt)
  result <- as.numeric(result)
  if (is.na(result)){
    print("Kretynie! Miała być liczba!")
    result <- -1
  }
  return(result)
}

plot_chart <- function(series, chart, xlim_left = 0, xlim_right = 0){
  ## To jest funkcja, która rysuje wykresy i dwie linie, w których trzeba je przyciąć.
  ## Jako argumenty bierze:
    ## series - vector integerów, z którego rysuje wykres.
    ## char - string z nazwą wykresu.
    ## xlim_left - integer określający położenia czerwonej linii
    ## xlim_righ - integer określający położenia niebieskiej linii
  ## Zwraca wykres
  plot(series,
       main = chart,
       ylab = "Wychylenie",
       xlab = "Czas w 1/2 sekundy",
       type = 'l',
       xaxt = 'n')
  axis(1, at = seq(0, length(series), 10), las=2)
  abline(v = xlim_left, col = "red")
  abline(v = xlim_right, col = "blue")
}


crop_charts <- function(condition, type = 'vel'){
  ## To jest funkcja służąca do przycinania wykresów.
  ## Wyświetla dwa wykresy i pyta, w których miejscach należy je przyciąć.
  ## Bierze dwa argumenty:
    ## condition - listę date frame'ów
    ## condition_name - string z nazwą folderu, w którym należy zapisać wyniki
  ## Ta funkcja nic nie zwraca, ale za to zapisuje wyniki do plików csv. Sama rozpoznaje czy ma być to katalog
  ## rywalizacja czy kooperacja.
  
  iterator <- seq(from = 1, to = length(condition), by = 2)
  for (i in iterator){
    if (type == 'vel'){
      osoba1 <- condition[[i]] %>%
        mutate(length = Vlength)
      osoba2 <- condition[[i+1]] %>%
        mutate(length = Vlength)
    } else {
      osoba1 <- condition[[i]] %>%
        mutate(length = Alength)
      osoba2 <- condition[[i+1]] %>%
        mutate(length = Alength) 
    }
    decision <- FALSE
    answer <- "nie"
    xlim_left <- -1
    xlim_right <- -1
    while(decision != TRUE){
      chart <- names(condition[i])
      chart2 <- names(condition[i+1])
      answer2 <- "nie"
      xlim_left2 <- -1
      xlim_right2 <- -1
      if (answer == "nie" | (xlim_left < 0) | (xlim_right < 0)){
        plot_chart(series = osoba1$length, chart = chart)
        plot_chart(series = osoba2$length, chart = chart2)
        xlim_left <- read_input(chart, "początku")
        plot_chart(series = osoba1$length, chart = chart, xlim_left = xlim_left)
        plot_chart(series = osoba2$length, chart = chart2)
        xlim_right <- read_input(chart, "końcu")
        plot_chart(series = osoba1$length, chart = chart, xlim_left = xlim_left, xlim_right = xlim_right)
        plot_chart(series = osoba2$length, chart = chart2)
        answer <- readline(prompt = "Czy na pewno tak chcesz przyciąć pierwszy wykres? (tak lub nie) ")
      }
      
      if (answer == "tak" & (xlim_left > 0) & (xlim_right > 0)){
        xlim_left2 <- read_input(chart2, "początku")
        plot_chart(series = osoba1$length, chart = chart, xlim_left = xlim_left, xlim_right = xlim_right)
        plot_chart(series = osoba2$length, chart = chart2, xlim_left = xlim_left2)
        xlim_right2 <- read_input(chart2, "końcu")
        plot_chart(series = osoba1$length, chart = chart, xlim_left = xlim_left, xlim_right = xlim_right)
        plot_chart(series = osoba2$length, chart = chart2, xlim_left = xlim_left2, xlim_right = xlim_right2)
        answer2 <- readline(prompt = "Czy na pewno tak chcesz przyciąć drugi wykres? (tak lub nie) ")
      }
      
      
      if (answer == "tak" & answer2 == "tak" & (xlim_left2 > 0) & (xlim_right2 > 0)){
        osoba1 <- osoba1 %>%
          slice(-xlim_right:-nrow(.)) %>%
          slice(-1:-xlim_left)
        osoba2 <- osoba2 %>%
          slice(-xlim_right2:-nrow(.)) %>%
          slice(-1:-xlim_left2)
        if (nrow(osoba1) > nrow(osoba2)) {
          osoba1 <- osoba1 %>%
            slice(1:nrow(osoba2))
        } else {
          osoba2 <- osoba2 %>%
            slice(1:nrow(osoba1))
        }
          
        if (grepl(x = chart, pattern = 'kop') & grepl(x = chart2, pattern = 'kop')){
          number <- list.files(path = "kooperacja/", pattern = '.csv') %>%
            str_extract(pattern = "[:digit:]+") %>%
            as.numeric() %>%
            max()
          if (number == -Inf){
            number <- 0
          }
          write.csv(osoba1, paste0("kooperacja",'/','para',number+1,'A','.csv'))
          write.csv(osoba2, paste0("kooperacja",'/','para',number+1,'B','.csv'))
        }
        if (grepl(x = chart, pattern = 'ryw') & grepl(x = chart2, pattern = 'ryw')){
          number <- list.files(path = "rywalizacja/", pattern = '.csv') %>%
            str_extract(pattern = "[:digit:]+") %>%
            as.numeric() %>%
            max()
          if (number == -Inf){
            number <- 0
          }
          write.csv(osoba1, paste0("rywalizacja",'/','para',number+1,'A','.csv'))
          write.csv(osoba2, paste0("rywalizacja",'/','para',number+1,'B','.csv'))
        }
        decision <- TRUE 
      }
    }
  }
}

##################################################
################### WYKONANIE ####################
##################################################

## Wczytanie danych
nowe <- read_files(type = 'txt', folder = 'surowe')
stare <- read_files(type = 'csv', folder = 'Akcelerometr1')

## Policzenie prędkości, oraz długości odpowiednich wektorów.
nowe <- compute_mean(nowe)
stare <- compute_mean(stare)

## Zmiana ustawień by wyświetlały się dwa wykresy jeden pod drugim.
par(mfrow = c(2,1))


