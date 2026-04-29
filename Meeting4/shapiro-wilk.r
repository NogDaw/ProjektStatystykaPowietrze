# 1. Wczytanie danych (ścieżka domyślna dla pulpitu w Linuxie)
# Funkcja read.csv2 automatycznie używa średników jako separatorów.
# Parametr na.strings od razu podmienia "empty" i "-200" na poprawne wartości NA.
df <- read.csv2("~/Desktop/AirQualityUCI.csv", na.strings = c("empty", "-200"))

# 2. Przygotowanie danych do testu
# Usuwamy puste wartości (NA) z kolumny RH, żeby test nie zwrócił błędu
rh_clean <- na.omit(df$PT08.S1)

# 3. Losowanie próbki do testu Shapiro-Wilka
# Test Shapiro-Wilka ma limit 5000 obserwacji. Nasz zbiór ma ich ponad 9000, 
# więc losujemy próbkę 5000 elementów. Funkcja set.seed() zapewnia powtarzalność.
set.seed(42)
rh_sample <- sample(rh_clean, 5000)

# 4. Wykonanie testu Shapiro-Wilka i wyświetlenie wyniku
wynik_testu <- shapiro.test(rh_sample)
print(wynik_testu)