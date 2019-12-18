require(dplyr)

# Test 1. Prüfen ob Werk-A statistisch gesehen mehr Produkte als Werk-B produtziert. 
# Produktion (A)
pa <- c(1184,1203,1219,1238,1243,1204,1269,1256,1156,1248)

# Produktion (B)
pb <- c(1136,1178,1212,1193,1226,1154,1230,1222,1161,1148)

# Stat. Zusmmen-Fassung
summary(pa)
summary(pb)

mean(pa) %>% paste(" (Mean PA)") %>% print()
mean(pb) %>% paste(" (Mean PB)") %>% print()

# T-Test (einseitig / one-tail)
# H0 (Null-Hypotese) - "pb >= pa" (greater) - (inverse zur Test-Fragestellung)
# H1 (alternative) - "pb < pa" (less)
test_result_1 <- t.test(pa, pb, alternative = "less")
test_result_1

hist(pa)
hist(pb)

"Kritischer T-Wert für df=18 und Konfidenz-Interal 95% = 1.734" %>% print
"T-Wert (2.2796) > Kritischer T-Wert, d.h. H0 verwerfen" %>% print
"Test-Ergebnis - alternative H1: pb < pa. D.h. Produktion-B ist tatsächlich geringer" %>% print

# Testen 2. Stellt B-Werk im Durchschnitt tatsächlich mehr als 1200 Produkten her?
# H0 (null): mu von pb <= 1200  
# H1 (alternativ): mu von pb > 1200  
test_result_2 <- t.test(pb, mu=1200, alternative = "greater")
test_result_2
"Konfidenz-Interval gültig - H0 bleibt, d.h. B-Werk stellt im Durchschnitt weinger als 1200 Produkte her" %>% print