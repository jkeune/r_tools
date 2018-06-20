get_nearest <- function(b1,b2,l1,l2){
  # b1,b2: lat der Station,lat des Modells
  # l1,l2: lon der Station,lon des Modells
  # OUTPUT: s Abstand in [km]
    f = 1/298.257223563                # Abplattung Erde
    a = 6378137/1000                   # Aequatorradius Erde
    F = (b1+b2)/2
    G = (b1-b2)/2
    l = (l1-l2)/2
    F = pi/180*F
    G = pi/180*G
    l = pi/180*l
    S = (sin(G))^2 *cos(l)^2 + cos(F)^2 *sin(l)^2
    C = (cos(G))^2 * cos(l)^2 + sin(F)^2 *sin(l)^2
    w = atan(sqrt(S/C))
    D = 2*w*a
    R = sqrt(S*C)/w
    H1 = (3*R -1)/(2*C)
    H2 = (3*R+1)/(2*C)
    s = D*(1 + f*H1*(sin(F))^2*(cos(G))^2 - f*H2*(cos(F)^2*(sin(G))^2)) # Abstand
return(s)}

