library("ggplot2")


# Eingangsparameter
# D         ... Fräserdurchmesser [mm]
# a_p       ... Schnitttiefe [mm]
# a_e       ... Schnittbreite [mm]
# z_w       ... Anzahl der Schneiden
# v_c       ... Schnittgeschwindigkeit [m/min]
# f_z       ... Vorschub pro Zahn [mm/min]
# k_c1.1    ... Spezifische Schnittkraft für h = b = 1 (Tabelle) [N/mm^2]
# z         ... Werkstoffkonstante (Tabelle)
# lambda    ... Drallwinkel [Grad]
# gamma_tat ... Tatsächlicher Spanwinkel [Grad]
# gamma_0   ... Basisspanwinkel Stahl=6, Guss=2, Alu=0 [Grad]
schnittdaten <- function(D, a_p, a_e, z_w, v_c, f_z, k_c1.1, z, lambda, gamma_tat, gamma_0, print = TRUE){
    # Gilt nur fürs Walzfräsen!
    if (print) print(sprintf("Fräser: D=%f, z_w=%f, Drallwinkel=%f, Spanwinkel=%f", D, z_w, lambda, gamma_tat))
    if (print) print(sprintf("Fräsung: a_p=%f, a_e=%f", a_p, a_e))
    if (print) print(sprintf("Material: v_c=%f, f_z=%f, k_c1.1=%f, z=%f, Basisspanwinkel=%f", v_c, f_z, k_c1.1, z, gamma_0))

    # Umdrehungszahl
    n_c <- (v_c * 10^3) / ( D * pi ) 

    # Eingriffswinkel (in Grad)
    phi_s <- (acos( 1 - ( (2 * a_e ) / D ) ) * 180) / pi

    # Vorschub
    v_f <- f_z * z_w * n_c

    if (print) print(sprintf("Maschine: n_c = %f [1/min], v_f = %f [mm/min]", n_c, v_f))
    if (print) print("")

    # Spanungsbreite
    b <- a_p / cos((lambda*pi)/180)

    # Mittenspandicke
    h_m <- (360 / ( pi * phi_s )) * (a_e / D) * f_z * sin(((90-lambda)*pi)/180)


    
    # Konstanten:
    # Verschleiß (30%)
    K_ver <- 1.3
    # Spanstauchung
    K_st <- 1.2
    # Spanwinkel
    K_gamma <- 1 - (( gamma_tat - gamma_0 ) / 100)
    # Schnittgeschwindigkeit (hier für Hartmetall, HSS = 1.2)
    K_v <- 1.0

    # Daraus errechnet sich dann die spezifische Schnittkraft
    k_c = ((1^z) / (h_m^z)) * k_c1.1 * K_gamma * K_v * K_st * K_ver 

    if (print) print(sprintf("k_c = %0.2f [N/mm^2]", k_c))

    # Anzahl der im Eingriff befindlichen Schneiden
    z_E <- ( z_w * phi_s )/(360)

    # Mittlere Hauptschnittkraft pro Fräserschneide
    F_cm <- b * h_m * k_c
    if (print) print(sprintf("F_cm = %0.2f [N]", F_cm))

    # Antriebsleistung der Maschine
    # ny = 80%
    P <- (F_cm * v_c * z_E) / (60 * 10^3 * 0.8)
    if (print) print(sprintf("P = %0.4f [kW]", P))
    if (print) print("===")
    return(F_cm)
}


# a_p sollte beim Walzfräsen etwa so groß sein wie D
# 3mm End mill, 3mm sheet metal, a_e 0.25
# Drallwinkel 45 Grad, zwei schneiden

# Stahl: v_c 200/240, f_z 0.1/0.05 (Schruppen/Schlichten)
schnittdaten(3, 3, 0.25, 2, 200, 0.1, 1780, 0.17, 45, 0, 6)
schnittdaten(3, 3, 0.25, 2, 240, 0.05, 1780, 0.17, 45, 0, 6)

# Alu: v_c 300/360, f_z 0.06/0.03
schnittdaten(3, 3, 0.25, 2, 300, 0.06, 700, 0.18, 45, 10, 0)
schnittdaten(3, 3, 0.25, 2, 360, 0.03, 700, 0.18, 45, 10, 0)

# Vergleichbare Werte
D <- 3
z_w <- 2
lambda <- 45
a_p <- 3


p <- ggplot(data.frame(x = c(0.05, 1)), aes(x))

jpeg('rplot.jpg')
print(p + 
      labs(title=sprintf("Walzfräsen mit D=%d, z_w=%d, a_p=%d", D, z_w, a_p)) + 
      xlab("a_e [mm]") + 
      ylab("Mittlere Hauptschnittkraft pro Schneide [N]") + 
      stat_function(fun=schnittdaten, aes(color="S235", alpha="Schruppen"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 200, f_z = 0.1, k_c1.1 = 1780, z = 0.17, lambda = lambda, gamma_tat = 0, gamma_0 = 6, print = FALSE)) + 
      stat_function(fun=schnittdaten, aes(color="S235", alpha="Schlichten"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 200 * 1.2, f_z = 0.1 * 0.5, k_c1.1 = 1780, z = 0.17, lambda = lambda, gamma_tat = 0, gamma_0 = 6, print = FALSE)) + 
      stat_function(fun=schnittdaten, aes(color="Aluminium", alpha="Schruppen"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 300, f_z = 0.06, k_c1.1 = 780, z = 0.18, lambda = lambda, gamma_tat = 10, gamma_0 = 0, print = FALSE)) + 
      stat_function(fun=schnittdaten, aes(color="Aluminium", alpha = "Schlichten"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 300 * 1.2, f_z = 0.06 * 0.5, k_c1.1 = 780, z = 0.18, lambda = lambda, gamma_tat = 10, gamma_0 = 0, print = FALSE)) + 
scale_colour_manual("Material", values = c("red", "blue")) + 
scale_alpha_manual("Vorgang", values = c(0.3, 1))
      ) 
dev.off()
