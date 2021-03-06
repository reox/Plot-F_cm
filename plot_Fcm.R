library("ggplot2")

# Funktionen aus Dietrich, Tschätsch "Praxis der Zerspanungstechnik" 11. Auflage 2014

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
schnittdaten_walzfraesen <- function(D, a_p, a_e, z_w, v_c, f_z, k_c1.1, z, lambda, gamma_tat, gamma_0, print = TRUE){
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
    # Verschleiß (30%) - kann auch höher angenommen werden (1.5)
    K_ver <- 1.3
    # Spanstauchung, = 1.2 für Fräsen
    K_st <- 1.2
    # Spanwinkel
    K_gamma <- 1 - (( gamma_tat - gamma_0 ) / 100)
    # Schnittgeschwindigkeit (hier für Hartmetall, HSS = 1.2)
    # Achtung: für v_c < 100m/min gelten da andere Formen! Für >100m/min eher vernachlässigbar
    K_v <- 1.0

    # Daraus errechnet sich dann die spezifische Schnittkraft [N/mm^2]
    k_c = ((1^z) / (h_m^z)) * k_c1.1 * K_gamma * K_v * K_st * K_ver 

    if (print) print(sprintf("k_c = %0.2f [N/mm^2]", k_c))

    # Anzahl der im Eingriff befindlichen Schneiden
    z_E <- ( z_w * phi_s )/(360)

    # Mittlere Hauptschnittkraft pro Fräserschneide
    F_cm <- b * h_m * k_c
    if (print) print(sprintf("F_cm = %0.2f [N]", F_cm))

    # Antriebsleistung der Maschine
    # ny = 80% (Wirkungsgrad)
    P <- (F_cm * v_c * z_E) / (60 * 10^3 * 0.8)
    if (print) print(sprintf("P = %0.4f [kW]", P))
    if (print) print("===")
    return(F_cm)
}

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
schnittdaten_stirnfraesen <- function(D, a_p, a_e, z_w, v_c, f_z, k_c1.1, z, lambda, gamma_tat, gamma_0, print = TRUE){
    # Gilt nur fürs Walzfräsen!
    if (print) print(sprintf("Fräser: D=%f, z_w=%f, Drallwinkel=%f, Spanwinkel=%f", D, z_w, lambda, gamma_tat))
    if (print) print(sprintf("Fräsung: a_p=%f, a_e=%f", a_p, a_e))
    if (print) print(sprintf("Material: v_c=%f, f_z=%f, k_c1.1=%f, z=%f, Basisspanwinkel=%f", v_c, f_z, k_c1.1, z, gamma_0))

    # Umdrehungszahl
    n_c <- (v_c * 10^3) / ( D * pi ) 

    # Eingriffswinkel (in Grad)
    # Wir gehen hier nur von dem Fall aus, dass phi_a = 0 ist.
    # Siehe Buch Seite 177
    # Dann ist phi_s = phi_e und B = a_e
    B <- a_e
    # Mhhh Ich glaube im Buch ist da eine Falsche Formel abgedruckt...
    # nach nachfrage: ja die Formel ist im Buch falsch!
    # phi_s <- (acos( - ((2*B) / D) ) * 180) / pi
    # wäre die Formel im Buch, allerdings funktioniert die nur für B < D/2 - da ja sonst der acos nicht bestimmt ist.
    # Abgedruckt in dem dazugehörigen Bild ist aber explizit ein Winkel für B > D/2!
    # In der Herleitung werden einfach phi_A und A_1 = 0 gesetzt, daher fallen dann Dinge weg.
    # Das ist auch alles soweit richtig, auch dass diese Formel für Winkel bis B < D/2 stimmt (also 90 Grad)
    # Jedoch wird dann nicht der Rest beachtet...
    #
    # Also entweder ich hab nen Fehler in der 11. Auflage gefunden oder ich rechne falsch :D
    # Diese Formel scheint richtiger zu sein
    phi_s <- ((acos( (-((2*B - D) / D))) * 180) / pi)
    print(sprintf("%f --> %f", B, phi_s))

    # Ideal ist D = 1.4 bis 1.6 * B

    # Vorschub
    v_f <- f_z * z_w * n_c

    if (print) print(sprintf("Maschine: n_c = %f [1/min], v_f = %f [mm/min]", n_c, v_f))
    if (print) print("")

    # Spanungsbreite
    # Der Einstellwinkel chi wird mit 90 Grad gewählt, da wir schaftfräser verwenden --> sin(90) = 1
    # Allgemeine Form b = a_p / sin(chi)
    b <- a_p

    # Mittenspandicke
    # Auch hier wird die Mittenspandicke berechnen. Ebenfalls sin(chi) = sin(90) = 1
    h_m <- (360 / (pi * phi_s)) * f_z * (B / D) * 1

    # Konstanten:
    # Verschleiß (30%) - kann auch höher angenommen werden (1.5)
    K_ver <- 1.3
    # Spanstauchung, = 1.2 für Fräsen
    K_st <- 1.2
    # Spanwinkel
    K_gamma <- 1 - (( gamma_tat - gamma_0 ) / 100)
    # Schnittgeschwindigkeit (hier für Hartmetall, HSS = 1.2)
    # Achtung: für v_c < 100m/min gelten da andere Formen! Für >100m/min eher vernachlässigbar
    K_v <- 1.0

    # Daraus errechnet sich dann die spezifische Schnittkraft [N/mm^2]
    k_c = ((1^z) / (h_m^z)) * k_c1.1 * K_gamma * K_v * K_st * K_ver 

    if (print) print(sprintf("k_c = %0.2f [N/mm^2]", k_c))

    # Anzahl der im Eingriff befindlichen Schneiden
    z_E <- ( z_w * phi_s ) / (360)

    # Mittlere Hauptschnittkraft pro Fräserschneide
    F_cm <- b * h_m * k_c
    if (print) print(sprintf("F_cm = %0.2f [N]", F_cm))

    # Antriebsleistung der Maschine
    # ny = 80% (Wirkungsgrad)
    P <- (F_cm * v_c * z_E) / (60 * 10^3 * 0.8)
    if (print) print(sprintf("P = %0.4f [kW]", P))
    if (print) print("===")
    return(F_cm)
}


# a_p sollte beim Walzfräsen etwa so groß sein wie D
# 3mm End mill, 3mm sheet metal, a_e 0.25
# Drallwinkel 45 Grad, zwei schneiden

# Stahl: v_c 200/240, f_z 0.1/0.05 (Schruppen/Schlichten)
# Üblicher Spanwinkel von 0 Grad, 6 Grad Basis
schnittdaten_walzfraesen(3, 3, 0.25, 2, 200, 0.1, 1780, 0.17, 45, 0, 6)
schnittdaten_walzfraesen(3, 3, 0.25, 2, 240, 0.05, 1780, 0.17, 45, 0, 6)

# Alu: v_c 300/360, f_z 0.06/0.03
# Üblicher Spanwinkel von 10 Grad, 0 Grad Basis (??? Verifikation benötigt! ???)
schnittdaten_walzfraesen(3, 3, 0.25, 2, 300, 0.06, 700, 0.18, 45, 10, 0)
schnittdaten_walzfraesen(3, 3, 0.25, 2, 360, 0.03, 700, 0.18, 45, 10, 0)

# Vergleichbare Werte
D <- 3
z_w <- 2
lambda <- 45
a_p <- 3


p <- ggplot(data.frame(x = c(0.05, 1)), aes(x))

jpeg('walzfraesen.jpg', height = 600, width = 800, units = 'px')
print(p + 
      labs(title=bquote("Walzfräsen mit D="~.(D)~", "~z[w]~"="~.(z_w)~", "~a[p]~"="~.(a_p))) + 
      xlab(expression(a[e]~"[mm]")) + 
      ylab("Mittlere Hauptschnittkraft pro Schneide [N]") + 
      stat_function(fun=schnittdaten_walzfraesen, aes(color="S235", alpha="Schruppen"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 200, f_z = 0.1, k_c1.1 = 1780, z = 0.17, lambda = lambda, gamma_tat = 0, gamma_0 = 6, print = FALSE)) + 
      stat_function(fun=schnittdaten_walzfraesen, aes(color="S235", alpha="Schlichten"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 200 * 1.2, f_z = 0.1 * 0.5, k_c1.1 = 1780, z = 0.17, lambda = lambda, gamma_tat = 0, gamma_0 = 6, print = FALSE)) + 
      stat_function(fun=schnittdaten_walzfraesen, aes(color="Aluminium", alpha="Schruppen"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 300, f_z = 0.06, k_c1.1 = 780, z = 0.18, lambda = lambda, gamma_tat = 10, gamma_0 = 0, print = FALSE)) + 
      stat_function(fun=schnittdaten_walzfraesen, aes(color="Aluminium", alpha = "Schlichten"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 300 * 1.2, f_z = 0.06 * 0.5, k_c1.1 = 780, z = 0.18, lambda = lambda, gamma_tat = 10, gamma_0 = 0, print = FALSE)) + 
scale_colour_manual("Material", values = c("red", "blue")) + 
scale_alpha_manual("Vorgang", values = c(0.3, 1))
      ) 
dev.off()


D <- 3
z_w <- 2
lambda <- 45
a_p <- 3

p <- ggplot(data.frame(x = c(0.05, D)), aes(x))

jpeg('stirnfraesen.jpg', height = 600, width = 800, units = 'px')
# Interessant ist die Aussage des Plots. Die Kraft nimmt gegen den vollen Durchmesser hin ab. 
# Das scheint nicht ganz logisch zu sein. Vielleicht sind da auch gewisse Grenzen dem Modell gesetzt.
# Im Buch wird jedenfalls davon gesprochen, dass das Beste Ergebnis bei B/E = 1/3 ist (Der Fräser ist nur zu 1/3 im Eingriff)
print(p + 
      labs(title=bquote("Stirnfraesen mit D="~.(D)~", "~z[w]~"="~.(z_w)~", "~a[p]~"="~.(a_p))) + 
      xlab(expression(a[e]~"[mm]")) + 
      ylab("Mittlere Hauptschnittkraft pro Schneide [N]") + 
      stat_function(fun=schnittdaten_stirnfraesen, aes(color="S235", alpha="Schruppen"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 200, f_z = 0.1, k_c1.1 = 1780, z = 0.17, lambda = lambda, gamma_tat = 0, gamma_0 = 6, print = FALSE)) + 
      stat_function(fun=schnittdaten_stirnfraesen, aes(color="S235", alpha="Schlichten"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 200 * 1.2, f_z = 0.1 * 0.5, k_c1.1 = 1780, z = 0.17, lambda = lambda, gamma_tat = 0, gamma_0 = 6, print = FALSE)) + 
      stat_function(fun=schnittdaten_stirnfraesen, aes(color="Aluminium", alpha="Schruppen"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 300, f_z = 0.06, k_c1.1 = 780, z = 0.18, lambda = lambda, gamma_tat = 10, gamma_0 = 0, print = FALSE)) + 
      stat_function(fun=schnittdaten_stirnfraesen, aes(color="Aluminium", alpha = "Schlichten"), geom="line", args=list( D = D, a_p = a_p, z_w = z_w, v_c = 300 * 1.2, f_z = 0.06 * 0.5, k_c1.1 = 780, z = 0.18, lambda = lambda, gamma_tat = 10, gamma_0 = 0, print = FALSE)) + 
scale_colour_manual("Material", values = c("red", "blue")) + 
scale_alpha_manual("Vorgang", values = c(0.3, 1))
      ) 
dev.off()
