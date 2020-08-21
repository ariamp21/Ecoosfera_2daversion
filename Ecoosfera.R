#Pagina Ecoosfera: "https://www.ecoosfera.com/"

#install.packages("rvest")
#install.packages("xml2")
#install.packages("ggplot2")

library('rvest')
library("xml2")
library('ggplot2')
library(tidyverse)

#Guardando la pagina
paginaEcoosfera <- "https://www.ecoosfera.com/"

#Leyendo el codigo de ecoosfera
paginaEcooRead <- read_html(paginaEcoosfera)
print(paginaEcooRead)

#Entrando a los links
paginaEcooNodes <- html_nodes(paginaEcooRead, "#menu-neo2")
paginaEcooNodes <- html_nodes(paginaEcooNodes, "a")
print(paginaEcooNodes)

#Obteniendo links
paginaEcooA <- html_attr(paginaEcooNodes, "href")
print(paginaEcooA)

##########################
#Extracción de información
##########################

#El orden de los print, es el siguiente:
#1. Link de la categoria
#2. Nombre de la categoria a estudiar
#3. Titulos de las noticias que presenta cada categoria
#4. Link de cada noticia
#5. Cantidad total de compartidos para cada noticia

ListaCategorias <- list()
ListaTitulos <- list()
ListaLinks <- list()
ListaCompartidos <- list()

for (i in paginaEcooA){
  print(i)
  
  lecturaEcoo <- read_html(i)
  CategoriaEcoo <- html_text(html_nodes(lecturaEcoo,"h1"))
  ListaCategorias <- c(ListaCategorias, CategoriaEcoo)
  print(CategoriaEcoo)
  
  TituloEcoo <- html_text(html_nodes(lecturaEcoo,".entry-header"))
  TituloEcoo <- gsub("\t","",TituloEcoo)
  TituloEcoo <- gsub("\n","",TituloEcoo)
  TituloEcoo <- gsub("                                                           ","",TituloEcoo)
  TituloEcoo <- gsub("       ","",TituloEcoo)
  ListaTitulos <- c(ListaTitulos, TituloEcoo)
  print(TituloEcoo)
    
  NodesNoticias <- html_nodes(lecturaEcoo, ".post-title")
  LinksNoticias <- html_nodes(NodesNoticias, "a")
  LinksNoticias <- html_attr(LinksNoticias, "href")
  ListaLinks <- c(ListaLinks, (LinksNoticias))
  print(LinksNoticias)

  for (x in LinksNoticias){
    lecturaLinks <- read_html(x)
    Compartidos <- html_text(html_nodes(lecturaLinks, ".share_count"))
    Compartidos <- as.numeric(Compartidos)
    ListaCompartidos <- c(ListaCompartidos, Compartidos)
    print(Compartidos)
  }
  
  }

#############
#Tabla final 
############

dfFinal <- data.frame(Categoria = unlist(ListaCategorias), Titulo = unlist(ListaTitulos), Link = unlist(ListaLinks), Compartidos = unlist(ListaCompartidos))%>%
  arrange(Categoria)

write.csv(dfFinal, file = "TablaFinalCompartidos.csv")

SumaTotal <- aggregate(dfFinal$Compartidos ~ dfFinal$Categoria, dfFinal[dfFinal$Categoria,], sum)

#########
#Graficos
#########

summary(dfFinal$Compartidos)

ggplot(dfFinal, 
       aes(x = Categoria, y = Compartidos, colour = Categoria)) + 
  geom_point() +
  theme_bw()

ggplot(dfFinal) +
  geom_bar(mapping = aes(x = Categoria, y = Compartidos, fill = Categoria), stat = "identity") +
  theme_minimal() +
  ggtitle("Compartidos por categoría")+
  guides(fill=FALSE)+
  coord_flip()

dfFinal %>%
  filter(Categoria == "Medio Ambiente")%>%
  select(Compartidos)%>%
boxplot(Compartidos,
        main = "Cantidad de compartidos por categoría",
        #sub = "Filtrado por peso relativo mayor a 0.2",
        xlab = "Medio Ambiente",
        ylab = "Compartidos",
        col = rainbow(6, alpha=0.2),
        border = rainbow(6, v=0.6))

dfFinal %>%
  filter(Categoria == "Medio Ambiente")%>%
  select(Titulo, Compartidos)%>%
  ggplot(mapping = aes( x = Titulo, y = Compartidos, fill= Titulo))+
  geom_bar(stat = "identity")+
  guides(fill=FALSE)+
  theme_bw()+
  coord_flip()


