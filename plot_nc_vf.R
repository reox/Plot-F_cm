library("ggplot2")

# Funktionen aus Dietrich, Tschätsch "Praxis der Zerspanungstechnik" 11. Auflage 2014

# Returns the Diameter of the millhead for a given v_c and n_c
# Erzeugt also Kennlinien auf einem v_c / D Chart
nccalc <- function(v_c, n_c){
    if (n_c < 6000 || n_c > 24000) return(NaN)
    
    D <- (v_c * 10^3) / (n_c * pi)

    D <- ifelse(D>8,NA,D)

    return(D)

}


# Minimal D=0.5 n_c = 6000 --> 9m/min
# Maximal D=8, n_c = 24000 --> 600m/min
p <- ggplot(data.frame(x = c(0, 600)), aes(x))
p <- p + scale_y_continuous(limits = c(0, 8))

#png('schnittgeschwindigkeit.png', height = 800, width = 1000, units = 'px')

for (i in seq(6000, 24000, 6000)) {
    p <- p + stat_function(fun=nccalc, args=list(n_c = i)) + annotate("text", x=(i*0.025)-40, y=7.8, label=sprintf("%d rpm", i))
}

print(
      p + 
      xlab(bquote(v[c]~"[m/min]")) +
      ylab(bquote(D~"[mm]")) +
      labs(title="Schnittgeschwindigkeit in Abhängigkeit von Drehzahl und Fräserdurchmesser") 
) 
#dev.off()
