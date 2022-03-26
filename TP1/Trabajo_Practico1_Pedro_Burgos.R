#Alumno:Pedro Burgos
#804/18
#Laboratorio de Datos-Verano 2022
###############################################################

datosTP1 = read.csv("nombres-2000-2004.csv",stringsAsFactors = FALSE)
View(datosTP1)
#1a)

#Establezco quienes son los primeros
primeros<-head(datosTP1)

#VIsualizo a los primeros
View(primeros)
#rta:Contiene nombre ,cantidad y anio ,siendo de los primeros 6 , la cabeza

#Convierto en minuscula todos los nombres
tolower(primeros$nombre)

##########################################################
#1b)

#coloco en una variables todos los nombres sin repetidos
#coloc los anios
nombres_unicos<-unique(datosTP1$nombre)
anios<-unique(datosTP1$anio)

#cantidad de nombres distintos
cantidad_de_nom_dist <- length(nombres_unicos) 
cantidad_de_nom_dist

#cantidad de anios
cantidad_de_anio <- length(anios)
cantidad_de_anio

#un vector que me diga la cantidad de nombres distintos por anio
cantidad_de_nom_dist_por_año<-rep()
for (nombre_por_año in 1:cantidad_de_anio) {
  x_0<-length(unique(datosTP1$nombre[datosTP1$anio==anios[nombre_por_año]]))
  cantidad_de_nom_dist_por_año[nombre_por_año]<-x_0
}
cantidad_de_nom_dist_por_año


#armo una matriz para ver que tanto se solapan los nombres,aunque por ahora
#es un cascaron vacio que sirve para distintos llamados
matriz_anios<-matrix(NA,nrow = cantidad_de_anio, ncol = cantidad_de_anio)
colnames(matriz_anios)<-anios
rownames(matriz_anios)<-anios


#vector donde coloco los nombres por anio a sacar
vector_de_nombres <- function(indice_de_anio_a_sacar){
  res_vn<-datosTP1$nombre[datosTP1$anio==anios[indice_de_anio_a_sacar]]
  return(res_vn)
}

#cuanto se solapan 
punto_b_solapan<-matriz_anios
for (i_0 in 1:cantidad_de_anio) {
  for(j_0 in 1:cantidad_de_anio){
    punto_b_solapan[i_0,j_0]<-length(intersect(vector_de_nombres(i_0), vector_de_nombres(j_0)))
  }
}
punto_b_solapan
#cuando vemos una matriz 1,2 que seria fila 2000 columna 2001 vemos cuanto se solapan
##########################################################

#1c)

#la funcion de coeficiente de jaccard
coe_jaccard <-function(vec1,vec2){
  interseccion_vec<-length(intersect(vec1,vec2))
  uniones_vec <- length(union(vec1,vec2))
  res_coe_jaccard<- (interseccion_vec)/(uniones_vec)
  return(res_coe_jaccard)
}

#la matriz donde vemos la solapacion de una manera mas asegurada 
punto_c<-matriz_anios
for (i_1 in 1:cantidad_de_anio) {
  for(j_1 in 1:cantidad_de_anio){
    punto_c[i_1,j_1]<-coe_jaccard(vector_de_nombres(i_1), vector_de_nombres(j_1))
  }
}
punto_c

#si el El índice de Jaccard ( IJ ) o coeficiente de Jaccard ( IJ ) 
#mide el grado de similitud entre dos conjuntos, sea cual sea el tipo de elementos
#como si fuese en probabilidad puedo ver entre 0 y 1 cuanta es la similitud

###########################################################

#1d)

#vector con los nombre mas repetido de un anio
vec_nom_mas_rep_anio<- function(anio_a_ev){
  vec_cant_dec<-sort(datosTP1$cantidad[datosTP1$anio==anio_a_ev],TRUE)
  vec_cant_dec<-vec_cant_dec[1:10]
  for (i_2 in 1:10) {
    x <-vec_cant_dec[i_2]
    nombre_encontrado<-datosTP1[datosTP1$cantidad==x,]$nombre
    vec_cant_dec[i_2]<-nombre_encontrado[1]    
  }
  return(vec_cant_dec)
}

#vector con los nombre menos repetido de un anio
vec_nom_men_rep_anio<- function(anio_a_ev){
  vec_cant_dec<-sort(datosTP1$cantidad[datosTP1$anio==anio_a_ev])
  vec_cant_dec<-vec_cant_dec[1:10]
  for (i_2 in 1:10) {
    x <-vec_cant_dec[i_2]
    nombre_encontrado<-datosTP1[datosTP1$cantidad==x,]$nombre
    vec_cant_dec[i_2]<-nombre_encontrado[1]    
  }
  return(vec_cant_dec)
}

#aca veo los distintos nombre mas repetido de cada anio
vec_nom_mas_rep_anio(2000)
vec_nom_mas_rep_anio(2001)
vec_nom_mas_rep_anio(2002)
vec_nom_mas_rep_anio(2003)
vec_nom_mas_rep_anio(2004)
#hay nombres repetidos

#aca veo los menos repetido de cada anio
vec_nom_men_rep_anio(2000)
vec_nom_men_rep_anio(2001)
vec_nom_men_rep_anio(2002)
vec_nom_men_rep_anio(2003)
vec_nom_men_rep_anio(2004)
#todos son el mismo nombre en este caso
##########################################################
#1e)

new_datos_tp1_1e <-aggregate(datosTP1$cantidad, by=list(datosTP1$nombre), FUN=sum)
View(new_datos_tp1_1e)
###########################################################

#2a)

da_el_paso<-function(x0){
  res <- x0 + runif(n = 10, min = -1, max = 1)
  #genera el desplazamiento
  return(res)
}

########################################################

#2b)
# Arme una función esta_entre(x0, T0, T1) que verifique si x0 está entre T0 y T1.
esta_entre<-function(x0, T0, T1){
  res <-(x0<T1 & T0<x0)
}
#########################################################
#2c)

camina<-function(x0, T0, T1){
  pasos <- 0
  while(esta_entre(x0,T0,T1)){
    x0 <- da_el_paso(x0)
    pasos <- pasos + 1
  }
  if(x0>=T1){
    res1<-c(pasos, T1)
    return(res1)
  }
  else{
    res2<-c(pasos, T0)
    return(res2)
  }
}
#vector con cuantas pasos dio y a donde llego
################################################################################

#2d)
v_menos1<-c()
v_diez10<-c()
i_4 <- 0
valor_caminataT0<-0
valor_caminataT1<-0
while (i_4 <= 1000) {
  #v = vector cuanto pasos dio y a donde llego
  v<-camina(x0=0, T0=-1, T1=10)
  if (v[2]==-1) {
    valor_caminataT0<-1+valor_caminataT0
    v_menos1 <- append(v_menos1,v[1])
    #agrego cada paso que dio si llego a -1
  }else{
    valor_caminataT1 <- 1 + valor_caminataT1
    v_diez10 <- append(v_diez10,v[1])
    #agrego cada paso que si llego a 10
  }
  i_4<-i_4+1
}
#cuanto duran a medio de que cuento cuantos pasos hice
sum(v_menos1)
sum(v_diez10)
#cuantas caminatas fueron a sus respondientes T
valor_caminataT0
valor_caminataT1

###############################################################
#2e)

calcular_recorrido<-function(x0, T0, T1){
  recorrido<-c()
  while(esta_entre(x0,T0,T1)){
    x0 <- da_el_paso(x0)
    recorrido<-append(recorrido,x0)
  }
  return(recorrido)
}
punt2e<-calcular_recorrido(0,-1,10)
max(punt2e)
min(punt2e)
length(punt2e)
#cantidad de pasos son el largo del recorrido
plot(ts(punt2e))
#puedo ver el minimo y maximo en el eje y(ose hasta donde hizo la caminata)
# ,cantidad en el eje x que es el largo de lo recorrido,osea pasos/tiempo 

