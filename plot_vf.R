library("ggplot2")

# Funktionen aus Dietrich, Tschätsch "Praxis der Zerspanungstechnik" 11. Auflage 2014

# Return v_f for f_z, z_w, n_c
vfcalc <- function(f_z, z_w, n_c){

    print(z_w)
    print(f_z * z_w * n_c)
    return(f_z * z_w * n_c)

}


# Minimal D=0.5 n_c = 6000 --> 9m/min
# Maximal D=8, n_c = 24000 --> 600m/min
p <- ggplot(data.frame(x = c(0.005, 0.15)), aes(x))
p <- p + scale_y_continuous(limits = c(0, 2500))

#png('schnittgeschwindigkeit.png', height = 800, width = 1000, units = 'px')

ns = seq(6000,24000,6000)

for (n in ns) {
    p <- p + stat_function(fun=vfcalc, aes(color=factor(n)), args=list(n_c = n, z_w = 1))
}

print(
      p + 
      xlab(bquote(f[z]~"[mm]")) +
      ylab(bquote(v[f]~"[mm/min]")) +
      labs(title="Vorschub in Abhängigkeit von Drehzahl, Anzahl der Zähne und Vorschub pro Zahn") +
    scale_colour_manual("Drehzahl", values = c("red", "blue", "green", "black"))
) 
#dev.off()
