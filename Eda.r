 
str(orangeec)
?orangeec
?mtcars
summary(orangeec)
rename(orangeec,c("Creat.Ind...GDP"="AporteEcNja"))
glimpse(orangeec)

plot(mtcars$mpg ~ mtcars$cyl ,xlab = "millas por galo",
     main="realacion millas galon")

plot(mtcars$mpg ~ mtcars$hp ,
     xlab = "caballos de fuerzas",
     ylab="millas por galon",
     main="realacion millas galon")


plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab="iinvercion educacion (%pib)",
     ylab = "Desempleo",
     main="relacion invercion en educacion y desempleo"
     )

plot(orangeec$GDP.PC ~ orangeec$Creat.Ind...GDP,
     xlab="Aporte economia Naranja (%pib)",
     ylab = "Pir per capital",
     main="Relacion economia naranja y pib per Capital"
)
qplot(mtcars$hp,
      geom="histogram", 
      xlabel="caballos de fuerza",
      main="carros segun caballo de fuerza"
      )
ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="caballos de fuerza",y="cantidad de carros",title="caballos de fuerza en carros seleccionados
       ")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="caballos de fuerza",y="cantidad de carros",title="caballos de fuerza en carros seleccionados
       ")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=mtcars,aes(x=hp),fill="blue",color="red",binwidth = 20)+
  labs(x="caballos de fuerza",y="Cantidad de carro", title = "caballos de fuerza en carros seleccionados")+
  xlim(c(80,280))+
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#
ggplot(orangeec, aes(x=Internet.penetration...population))+geom_histogram(
  fill="red", color="yellow", binwidth = 5)+
  labs(x="Penetracion de internet en % poblacion", y="Cantidad de paises",
       title = "Penetracion de internet en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlim(30, 100)+ ylim(0, 4)+
  scale_x_continuous(breaks = seq(30, 100, by=5))

boxplot(mtcars$hp,
        ylab="caballos de fuerza",
        main="Caballos de fuerza en carreras mtcars"
        )

ggplot(mtcars, aes(x=as.factor(cyl) , y=hp,fill=cyl))+
  geom_boxplot(alpha=0.6)+
  labs(x="cilindros",y="caballos de fuerza",
       title="Caballos de fuerza segun cilindros")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#
mtcars$am <- factor(mtcars$am, levels = c(1,0),
                    labels = c("manual","automatico"))

ggplot(mtcars, aes(x=am, y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="tipo de caja", y="milla por galon",
       title="Millas por galon segun tipo de caja-mtcars")

economy<-mean(orangeec$GDP.PC)
economy
##
orangeec<-orangeec%>%
  mutate(Strong_economy=ifelse(GDP.PC<economy,
                               "Por debajo de promedio pib per cápica",
                               "sobre arriva promedio pib per capita"))
##
ggplot(orangeec, aes(x=Strong_economy, y=Creat.Ind...GDP,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais", y="Aporte economia naranjaal pib",
       title = "Aporte economia naranja en pib paises latam con alto y bajo pib per capita")+
  
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
##

ggplot(orangeec, aes(x=Strong_economy, y=Internet.penetration...population,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais", y="Penetracion de internet(%)",
       title = "Penetracion de internet en paises Latam con 
         alto y bajo pib per capita")+
  
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        anel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


##
ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="Caballos de fuerza", y="MILLAS POR galon",
       Title="Relacion caballs de fuerza y millas por galon")
+  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

##
ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs(x="Peso", y="Potencia",
       Title="Relacion Peso Potencia")
+  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

##
ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am, size=cyl))+
  labs(S="Caballos de Fuerza", y="1/4 de milla",
       Title="Relacion Peso Potencia")

##
ggplot(orangeec, aes(Internet.penetration...population,Creat.Ind...GDP))+
  geom_point(aes(color=factor(Strong_economy), size=GDP.Growth..))+
  labs(S="Penetracion", y="Aporte economia naranja",
       Title="Internet y aporte economia narang")
##
my_graph <- ggplot(orangeec, aes(Internet.penetration...population,
                                 Creat.Ind...GDP,
                                 label=row.names(orangeec)))+
geom_point()+
  labs(x="Penetracoion internet",y="Aporte economia  naranja",
       title="internet y aporte "
       )

my_graph


p <- ggplotly(my_graph)
p

#
pairs(mtcars[,2:6])
pairs(mtcars)

#
newdata<-subset(mtcars, select=c(2,7:8,11,12))
pairs(newdata)


#
pairs(mtcars[,-c(1,3,4,5,6,9,10)])


#
Eficientes <- filter(mtcars,mpg>=30)
Eficientes
pairs(Eficientes[,2:6])
 
#
merc <- mtcars %>%
  filter(str_detect(model,"Merc"))
merc

pairs(merc[,2:6])

#

cor(merc[,2:6])
cor(newdata)

#

pairs(orangeec[,2:6])
pairs(orangeec[,5:10])

newData <- subset(orangeec, select = c(5,6,10,11,12,13))
newData

pairs(newData)


#
cor(orangeec[,2:6])

#Quitar nA

cor(orangeec[,2:6],use="complete.obs")

#
cor(orangeec[,5:10],use="complete.obs")

cor(newData,use="complete.obs")


summary(mtcars)
#
standar <- sd(mtcars$mpg)
mean <- mean(mtcars$mpg)
#
conf <- standar / mean *100
conf


standarOrag <- sd(orangeec$Internet.penetration...population)
meanOrag <- mean(orangeec$Internet.penetration...population)
confOrag <- standarOrag/ meanOrag * 100 
confOrag

summary(orangeec)
#
m_ci <- mean(orangeec$Creat.Ind...GDP, na.rm=TRUE)
s_ci <- sd(orangeec$Creat.Ind...GDP, na.rm=TRUE)

s_ci/m_ci * 100



eficientes <- mean(mtcars$mpg)
mtcars <- mtcars %>% 
  mutate(Mas_eficientes = ifelse(mpg < eficientes,"bajo promedio","en o sobre el promedio"))
mtcars

mas_veloces <- mtcars[mtcars$qsec < 16, ]
mas_veloces

mtcars <- mtcars%>%
  mutate(velocidad_cuarto_de_milla = ifelse(qsec < 16,"menos 16 seg","mas 16 seg"))

#
mtcars <- mtcars%>%
  mutate(Peso_kg = (wt/2)*1000)

mtcars <- mtcars %>%
  mutate(Peso=ifelse(peso_kg <= 1500,"Livianos","Pesados"))

#


orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(GDP.Growth.. >= 2.5,"mas de 2.5%", "menos del 25%"))

orangeec <- orangeec %>%
  mutate(Anaranjados = ifelse(Creat.Ind...GDP >= 2.5, "mas anaranjados","menos anaranjados"))


orangeec <- orangeec %>%
  arrange(desc(Creat.Ind...GDP))

TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico", "Panama", "Argentina", "Colombia", "Brazil", "Paraguay", "Guatemala"))
TopNaranjas

TopNaranjas <- TopNaranjas %>%
  arrange(desc(Creat.Ind...GDP))

#
mtcars %>%
  arrange(desc(Peso_kg))

Mas_Pesados <- mtcars %>% 
  filter(Peso_kg > 2034)
Mas_Pesados

##

ggplot(Mas_Pesados, aes (x=hp, y=mpg))+
  geom_point()+
  facet_wrap(~model)

##

ggplot(mtcars, aes(x=cyl, y=mpg, size=Peso))+
  geom_point()+
  facet_wrap(~vs)

?mtcars  


#
ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=Services...GDP, size=GDP.PC))+
  geom_point()+
  facet_wrap(~Country)
#
ggplot(TopNaranjas, aes(x=Education.invest...GDP,
                        y=Creat.Ind...GDP, 
                        size=Unemployment))+
  geom_point()+
  facet_wrap(~Country)

myColors <- brewer.pal(9,"Reds")

brewer.pal.info

  ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=GDP.PC, fill=Creat.Ind...GDP
                        ))+
  geom_tile()+
  facet_wrap(~Country)+
    scale_fill_gradientn(colors = myColors)
  
  Imy_graph <- ggplot(topNaranja, aes(color=factor(Unemployment),x=Education.invest...GDP,y=Creat.Ind...GDP,z=Country,size=Unemployment))+geom_point()+facet_wrap(~ Country)
  ggplotly(my_graph)
  
  myColors <- brewer.pal(9,"Reds")
  
  ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                          y=GDP.PC, fill=Creat.Ind...GDP))+
    geom_tile()+
    facet_wrap(~Country)+
    scale_fill_gradientn(colors=myColors)
  
