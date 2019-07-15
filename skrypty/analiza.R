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

## Przed puszczeniem tego skryptu trzeba zrobić dwie rzeczy:
  ## 1) Upewnić się, że w katalogach kooperacja i rywalizacja są pliki csv
  ## 2) Upewnić się, że pliki w parach są tej samej długości. Można użyć komendy bashowej wc -l *.csv

##################################################
##################### UŻYCIE #####################
##################################################

## W zasadzie jeśli jest się pewnym, że wszystkie pliki są to można zaznaczyć cały kod i puścić.
## Na samym początku jest zdefiniowanych parę funkcji, które w paru ostatnich linijkach są wykonywane.

##################################################
#################### FUNKCJE #####################
##################################################

read_files <- function(condition){
  ## To jest bardzo prosta funkcja, która wczytuje pliki csv.
  ## Jako argument bierze:
    ## condition - string z nazwą folderu, w którym są pliki csv.
  ## Zwraca listę tibble.
  list_names <- list.files(path = paste0(condition,'/'), pattern = ".csv")
  list_files <- list()
  for (name in list_names){
    list_files[[name %>%
                 str_extract('[[:alnum:]-]+')]] <- read.csv(file = paste0(condition,'/',name))
  }
  return(list_files)
}


class_allocation <- function(table, class = 5, type = 'Vlength'){
  ## To jest funkcja, która służy do binowania skali ciągłej do przedziałowej
  ## za pomocą odchyleń standardowych.
  ## Jako argumenty bierze:
    ## table - ramka danych
    ## class - liczba odchyleń standardowych. Defualtowe 5 stworzy tak naprawdę
    ## 10 klas od -5 do + 5 odchylenia standardowego.
    ## type - nazwa kolumny, którą trzeba podzielić na klasy
  ## Zwraca tibble z dodatkową kolumną class, w której są klasy na które podzielono zmienną type.
  mean <- mean(table[,type])
  sd <- sd(table[,type])
  breaks <- vector(mode = 'numeric')
  for (i in c(-class:class)){
    breaks <- append(x = breaks,
           values = mean + (i * sd))
  }
  table <- table %>%
    mutate(class = cut(x = .[,type],
                       breaks = breaks,
                       labels = FALSE,
                       include.lowest = TRUE),
           class = class %>% as.character() %>% as.numeric(),
           class = case_when((.[,type] > breaks[max(class, na.rm = TRUE)]) ~ max(class, na.rm = TRUE),
                             (.[,type] < breaks[min(class, na.rm = TRUE)]) ~ max(class, na.rm = TRUE),
                             TRUE ~ class))
  return(table)
}

populate_matrix <- function(table1,
                            table2,
                            class = 10,
                            synch = TRUE){
  ## To jest funkcja, która tworzy macierz, w której zapisane jest współwystępowanie klas.
  ## Na podstawie tej macierzy powstają wykresy i liczone są wyniki Chi kwadrat.
  ## Jako argumenty bierze:
    ## table1 - ramka danych z wynikami pierwszej osoby z pary
    ## table2 - ramka danych z wynikami drugiej osoby z pary
    ## class - liczba klas, powinna być dwa razy większa niż w przypadku class_allocation
    ## synch - zmienna logiczna. Defualtowo założona jest synchronizacja. Oznacza to, że 
    ## tylko jedna komórka jest wypełniania (ta na przecięciu dwóch klas). W przypadku braku
    ## synchronizacji wypełniana jest cała kolumna lub wiersz.
  ## Zwraca macierz z frekwencjami poszczególnych klas.
  matrix <- matrix(dat = 0,
                   nrow = class,
                   ncol = class)
  if (synch == TRUE){
    table <- tibble(x = table1$class, y = table2$class) %>%
      group_by(x, y) %>%
      summarise(n = n()) %>%
      mutate(n = n)
    matrix[table$x,table$y] <- table$n
    matrix[table$y,table$x] <- table$n
  } else {
    x <- tibble(x = table1$class) %>%
      group_by(x) %>%
      summarise(n = n()) %>%
      mutate(n = n)
    y <- tibble(y = table2$class) %>%
      group_by(y) %>%
      summarise(n = n()) %>%
      mutate(n = n)
    matrix[x$x,] <- x$n 
    matrix[y$y,] <- matrix[y$y,] + y$n
    matrix[,x$x] <- x$n 
    matrix[,y$y] <- matrix[,y$y] + y$n
  }
  return(matrix)
}

draw_matrices <- function(list, class = 10, time_window = 60, show_full = FALSE){
  ## Funkcja kombajn, która rysuje wykresy.
  ## Jako argumenty bierze:
    ## list - lista tibble z policzonymi klasami
    ## time_window - liczba sekund, które powinno zawierać okno czasowe, z którego zbierane są wyniki
    ## na początku, w środku i na końcu interakcji.
    ## class - liczba klas
    ## show_full - zmienna logiczna. Defualtowo ustawiona w ten sposób, że funkcja pokazuje trzy wykresy z
    ## 3 okien czasowych. Jeśli ustawi się TRUE to pokaże dwa wykresy dla całej długości interakcji.
  ## Zwraca listę z dwoma elementami. 1) wykres, na którym jest 6 levelplotów z wzorcami wychyleń przy założonej synchronizacji i bez w
  ## 3 minutowych oknach czasowych: na początku interakcji, w samym środku interakcji i na samym końcu
  ## interakcji. 2) macierze z prawdopodobieństwem każdej z klas.

  iterator <- seq(from = 1, to = length(list), by = 2)
  matrix_start <- matrix(data = 0,
                         nrow = class,
                         ncol = class)
  matrix_middle <- matrix(data = 0,
                          nrow = class,
                          ncol = class)
  matrix_end <- matrix(data = 0,
                       nrow = class,
                       ncol = class)
  matrix_full <- matrix(data = 0,
                        nrow = class,
                        ncol = class)
  matrix_start_teo <- matrix(data = 0,
                             nrow = class,
                             ncol = class)
  matrix_middle_teo <- matrix(data = 0,
                              nrow = class,
                              ncol = class)
  matrix_end_teo <- matrix(data = 0,
                           nrow = class,
                           ncol = class)
  matrix_full_teo <- matrix(data = 0,
                        nrow = class,
                        ncol = class)
  for (i in iterator){
    table1 <- list[[i]]
    table2 <- list[[i+1]]
    temp_matrix <- populate_matrix(table1 = table1[1:(time_window*2),], table2 = table2[1:(time_window*2),])
    matrix_start <- matrix_start + temp_matrix
    temp_matrix <- populate_matrix(table1 = table1[(ceiling(nrow(table1)/2)-time_window):(ceiling(nrow(table1)/2) + time_window),],
                                   table2 = table2[(ceiling(nrow(table2)/2)-time_window):(ceiling(nrow(table2)/2) + time_window),])
    matrix_middle <- matrix_middle + temp_matrix
    temp_matrix <- populate_matrix(table1 = table1[(nrow(table1)-(time_window*2)):nrow(table1),],
                                   table2 = table2[(nrow(table2)-(time_window*2)):nrow(table2),])
    matrix_end <- matrix_end + temp_matrix
    temp_matrix <- populate_matrix(table1 = table1,
                                   table2 = table2)
    matrix_full <- matrix_full + temp_matrix
    
    temp_matrix <- populate_matrix(table1 = table1[1:(time_window*2),], table2 = table2[1:(time_window*2),], synch = FALSE)
    matrix_start_teo <- matrix_start_teo + temp_matrix
    temp_matrix <- populate_matrix(table1 = table1[(ceiling(nrow(table1)/2)-time_window):(ceiling(nrow(table1)/2) + time_window),],
                                   table2 = table2[(ceiling(nrow(table2)/2)-time_window):(ceiling(nrow(table2)/2) + time_window),],
                                   synch = FALSE)
    matrix_middle_teo <- matrix_middle_teo + temp_matrix
    temp_matrix <- populate_matrix(table1 = table1[(nrow(table1)-(time_window*2)):nrow(table1),],
                                   table2 = table2[(nrow(table2)-(time_window*2)):nrow(table2),],
                                   synch = FALSE)
    matrix_end_teo <- matrix_end_teo + temp_matrix
    temp_matrix_teo <- populate_matrix(table1 = table1,
                                       table2 = table2,
                                       synch = FALSE)
    matrix_full_teo <- matrix_full_teo + temp_matrix_teo
  }
  matrix_start <- .5 * (matrix_start * t(matrix_start))
  matrix_start_prob <- matrix_start / sum(matrix_start)
  matrix_middle <- .5 * (matrix_middle * t(matrix_middle))
  matrix_middle_prob <- matrix_middle / sum(matrix_middle)
  matrix_end <- .5 * (matrix_end * t(matrix_end))
  matrix_end_prob <- matrix_end / sum(matrix_end)
  matrix_full <- .5 * (matrix_full * t(matrix_full))
  matrix_full_prob <- matrix_full / sum(matrix_full)
  start <- lattice::levelplot(matrix_start_prob,
            col.regions = terrain.colors(1000),
            xlab = "Examiner 1",
            ylab = "Examiner 2",
            main = "Empirical distribution start")
  middle <- lattice::levelplot(matrix_middle_prob,
                     col.regions = terrain.colors(1000),
                     xlab = "Examiner 1",
                     ylab = "Examiner 2",
                     main = "Empirical distribution middle")
  end <- lattice::levelplot(matrix_end_prob,
                     col.regions = terrain.colors(1000),
                     xlab = "Examiner 1",
                     ylab = "Examiner 2",
                     main = "Empirical distribution end")
  full <- lattice::levelplot(matrix_full_prob,
                             col.regions = terrain.colors(1000),
                             xlab = "Examiner 1",
                             ylab = "Examiner 2",
                             main = "Empirical distribution full")
  
  matrix_start_teo <- .5 * (matrix_start_teo * t(matrix_start_teo))
  matrix_start_teo_prob <- matrix_start_teo / sum(matrix_start_teo)
  matrix_middle_teo <- .5 * (matrix_middle_teo * t(matrix_middle_teo))
  matrix_middle_teo_prob <- matrix_middle_teo / sum(matrix_middle_teo)
  matrix_end_teo <- .5 * (matrix_end_teo * t(matrix_end_teo))
  matrix_end_teo_prob <- matrix_end_teo / sum(matrix_end_teo)
  matrix_full_teo <- .5 * (matrix_full_teo * t(matrix_full_teo))
  matrix_full_teo_prob <- matrix_full_teo / sum(matrix_full_teo)
  start_teo <- lattice::levelplot(matrix_start_teo_prob,
                              col.regions = terrain.colors(1000),
                              xlab = "Examiner 1",
                              ylab = "Examiner 2",
                              main = "Empirical distribution start teo")
  middle_teo <- lattice::levelplot(matrix_middle_teo_prob,
                               col.regions = terrain.colors(1000),
                               xlab = "Examiner 1",
                               ylab = "Examiner 2",
                               main = "Empirical distribution middle teo")
  end_teo <- lattice::levelplot(matrix_end_teo_prob,
                            col.regions = terrain.colors(1000),
                            xlab = "Examiner 1",
                            ylab = "Examiner 2",
                            main = "Empirical distribution end teo")
  full_teo <- lattice::levelplot(matrix_full_teo_prob,
                             col.regions = terrain.colors(1000),
                             xlab = "Examiner 1",
                             ylab = "Examiner 2",
                             main = "Empirical distribution full teo")
  matrices <- list(matrix_start = matrix_start, 
                   matrix_middle = matrix_middle,
                   matrix_end = matrix_end,
                   matrix_start_teo = matrix_start_teo,
                   matrix_middle_teo = matrix_middle_teo,
                   matrix_end_teo = matrix_end_teo)
  plots <- gridExtra::grid.arrange(start, start_teo,
                                   middle, middle_teo,
                                   end, end_teo,
                                   nrow = 3, ncol = 2)
  if (show_full == TRUE){
    plots <- gridExtra::grid.arrange(full, full_teo, nrow = 2)
    matrices <- list(full = matrix_full,
                     full_teo = matrix_full_teo)
  }
  return(list(matrices = matrices, plots = plots))
  
}

compute_chi_square <- function(list_kooperacja, list_rywalizacja, limit = 4, show_full = FALSE){
  ## Funkcja, która liczy chi kwadrat goodness-of-fit. Innymi słowy na ile jeden rozkład pochodzi z drugiego.
  ## Ponieważ ta funkcja odwołuje się nazw w liście to argumenty muszą być wynikiem funkcji draw_matrices.
  ## Jako argumenty bierze:
    ## list_kooperacja - lista macierzy w warunku kooperacji
    ## list_rywalizacja - lista macierzy w warunku rywalizacji
    ## limit - indeks pierwszej nie pustej klasy. Przez to, że wziąłem odchylenie standardowe jako jednostkę
    ## to czasem zahacza ono o liczby ujemne, które nie mają szansy wystąpić. Dlatego musiałem tutaj to 
    ## sztucznie ograniczyć. Jeśli by klasy w inny sposób przypisywać to limi może być 0.
  ## Zwraca listę z wynikami poszczególnych testów.
  chi_test <- function(matrix_A, matrix_B, ...){
    ## Funkcja do liczenia testów chi kwadrat goodness of fit.
    ## Jako argumenty bierze:
      ## matrix_A - macierz frekwencji
      ## matrix_B - macierz frekwencji, na podstawie, której będzie liczone prawdopodobieństwo.
    ## Zwraca wynik testu
    ncol_A <- ncol(matrix_A)
    nrow_A <- nrow(matrix_A)
    ncol_B <- ncol(matrix_B)
    nrow_B <- nrow(matrix_B)
    matrix_A <- matrix_A[limit:nrow_A,limit:ncol_A]
    matrix_B <- matrix_B[limit:nrow_A,limit:ncol_B]
    chisq.test(x = matrix_A, p = matrix_B/sum(matrix_B))
  }
  if (show_full == FALSE){
    start_middle_koop <- chi_test(matrix_A = list_kooperacja$matrix_start, matrix_B = list_kooperacja$matrix_middle)
    middle_start_koop <- chi_test(matrix_B = list_kooperacja$matrix_start, matrix_A = list_kooperacja$matrix_middle)
    start_end_koop <- chi_test(matrix_A = list_kooperacja$matrix_start, matrix_B = list_kooperacja$matrix_end)
    end_start_koop <- chi_test(matrix_B = list_kooperacja$matrix_start, matrix_A = list_kooperacja$matrix_end)
    middle_end_koop <- chi_test(matrix_A = list_kooperacja$matrix_middle, matrix_B = list_kooperacja$matrix_end)
    end_middle_koop <- chi_test(matrix_B = list_kooperacja$matrix_middle, matrix_A = list_kooperacja$matrix_end)
    
    start_middle_ryw <- chi_test(matrix_A = list_rywalizacja$matrix_start, matrix_B = list_rywalizacja$matrix_middle)
    middle_start_ryw <- chi_test(matrix_B = list_rywalizacja$matrix_start, matrix_A = list_rywalizacja$matrix_middle)
    start_end_ryw <- chi_test(matrix_A = list_rywalizacja$matrix_start, matrix_B = list_rywalizacja$matrix_end)
    end_start_ryw <- chi_test(matrix_B = list_rywalizacja$matrix_start, matrix_A = list_rywalizacja$matrix_end)
    middle_end_ryw <- chi_test(matrix_A = list_rywalizacja$matrix_middle, matrix_B = list_rywalizacja$matrix_end)
    end_middle_ryw <- chi_test(matrix_A = list_rywalizacja$matrix_middle, matrix_B = list_rywalizacja$matrix_end)
    
    start_ryw_koop <- chi_test(matrix_A = list_rywalizacja$matrix_start, matrix_B = list_kooperacja$matrix_start)
    start_koop_ryw <- chi_test(matrix_B = list_rywalizacja$matrix_start, matrix_A = list_kooperacja$matrix_start)
    middle_ryw_koop <- chi_test(matrix_A = list_rywalizacja$matrix_middle, matrix_B = list_kooperacja$matrix_middle)
    middle_koop_ryw <- chi_test(matrix_B = list_rywalizacja$matrix_middle, matrix_A = list_kooperacja$matrix_middle)
    end_ryw_koop <- chi_test(matrix_A = list_rywalizacja$matrix_end, matrix_B = list_kooperacja$matrix_end)
    end_koop_ryw <- chi_test(matrix_B = list_rywalizacja$matrix_end, matrix_A = list_kooperacja$matrix_end)
    
    results <- list("Porównanie początku i środka w warunku kooperacji" = start_middle_koop,
                "Porównanie środka i początku w warunku kooperacji" = middle_start_koop,
                "Porównanie początku i środka w warunku rywalizacji" = start_middle_ryw,
                "Porównanie środka i początku w warunku rywalizacji" = middle_start_ryw,
                "Porównanie początku i końca w warunku kooperacji" = start_end_koop,
                "Porównanie końca i początku w warunku kooperacji" = end_start_koop,
                "Porównanie początku i końca w warunku rywalizacji" = start_end_ryw,
                "Porównanie końca i początku w warunku rywalizacji" = end_start_ryw,
                "Porównanie środka i końca w warunku kooperacji" = middle_end_koop,
                "Porównanie końca i środka w warunku kooperacji" = end_middle_koop,
                "Porównanie środka i końca w warunku rywalizacji" = middle_end_ryw,
                "Porównanie końca i środka w warunku rywalizacji" = end_middle_ryw,
                "Porównanie początku między rywalizacji i kooperacją" = start_ryw_koop,
                "Porównanie początku między kooperacja i rywalizacją" = start_koop_ryw,
                "Porównanie środka między rywalizacją i kooperacją" = middle_ryw_koop,
                "Porównanie środka między kooperacją i rywalizacją" = middle_koop_ryw,
                "Porównanie końca między rywalizacją i kooperacją" = end_ryw_koop,
                "Porównanie końca między kooperacją i rywalizacją" = end_koop_ryw)
  }
  if (show_full == TRUE){
    full_ryw_koop <- chi_test(matrix_A = list_rywalizacja$full, matrix_B = list_kooperacja$full)
    full_koop_ryw <- chi_test(matrix_B = list_rywalizacja$full, matrix_A = list_kooperacja$full)
    results <- list("Porównanie rywalizacji i kooperacji" = full_ryw_koop,
                    "Porównanie kooperacji i rywalizacji" = full_koop_ryw)
  }
  return(results)
}

##################################################
################### WYKONANIE ####################
##################################################

## Wczytanie plików z obu warunków
kooperacja <- read_files('kooperacja')
rywalizacja <- read_files('rywalizacja')

## Przyporządkowanie klas
kooperacja <- lapply(kooperacja, class_allocation)
rywalizacja <- lapply(rywalizacja, class_allocation)

