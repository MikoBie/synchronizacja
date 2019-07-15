##################################################
################## INSTRUKCJA ####################
##################################################

## Pamiętaj o tym, żeby każdą linijkę wykonywać pojedynczo. Poza tym wszystko jest w miarę opisane.

##################################################
#################### UŻYCIE ######################
##################################################

## Przycinanie wykresów działa dokładnie tak samo jak poprzednio. 
## Wykresy przytnij tylko raz. Jak już to zrobisz to możesz sobie zakomentować od tego mniejsza.

source('przycinanie/przycinanie.R')

crop_charts(nowe)
crop_charts(stare)

## Do tego miejsca

## Wczytanie danych i podział na klasy.
source('przycinanie/analiza.R')

## Narysowanie wykresów. Zwróć uwagę na to, że funkcja draw_matrices bierze argumenty time_window i show_full.
## Pierwszy służy do manipulowania długością okna czasowego. Defualtowo jest ustawione 60 sekund. Możesz
## tą wartością pomanipulować i zobaczyć jak wyniki będą się zmieniać jeśli będzie to więcej lub mniej. Show_full
## służy do podjęcia decyzji czy chce się zobaczyć wykres zbiorczy. Ten argument przyjmuje wartości TRUE albo
## FALSE. Defulatowo ustawiony jest jako FALSE
draw_kooperacja <- draw_matrices(kooperacja, time_window = 60, show_full = FALSE)
draw_rywalizacja <- draw_matrices(rywalizacja, time_window = 60, show_full = FALSE)


## Policzenie testu chi kwadrat goodness-of-fit. Zasadniczo jest to dość prosty test, który sprawdza na ile
## częstości w jednym rozkładzie pochodzą z drugiego rozkładu. Trzeba jednak pamiętać o tym, że trzeba policzyć
## go zarówno, żeby sprawdzić czy rozkład A pochodzi z B jak i dla B, że pochodzi z A. Jeśli tak jest to
## dopiero wtedy można mówić o niezależności rozkładów. To jest trochę tak jak liczby naturalne należą do
## zbioru liczb rzeczywistych, ale liczby rzeczywiste nie należą do zbioru liczb naturalnych
tests <- compute_chi_square(list_kooperacja = draw_kooperacja$matrices,
                            list_rywalizacja = draw_rywalizacja$matrices,
                            show_full = FALSE)

tests

## Żeby policzyć test chi kwadrat dla obu warunków bez podziału na te okna czasowe to trzeba najpierw też
## policzyć draw_kooperacja i draw_rywalizacja. 
