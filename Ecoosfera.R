#Pagina Ecoosfera: "https://www.ecoosfera.com/"

library('rvest')
library("xml2")
library('ggplot2')
library('tidyverse')

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

###########################
#Extraccion de informacion
##########################

#El orden de los print, es el siguiente:
#1. Link de la categoria
#2. Nombre de la categoria a estudiar
#3. Titulos de las noticias 
#4. Link de cada noticia
#5. Cantidad total de compartidos para cada una de ellas

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

SumaTotal <- aggregate( dfFinal$Compartidos ~ dfFinal$Categoria, dfFinal[dfFinal$Categoria,], sum) 
  colnames(SumaTotal)[1] <- "Categoría"
  colnames(SumaTotal)[2] <- "Compartidos"

#########
#Graficos
#########

summary(dfFinal$Compartidos)

ggplot(dfFinal, 
       aes(x = Categoria, y = Compartidos, colour = Categoria)) + 
  geom_point() +
  theme_bw()

ggplot(dfFinal,aes(Categoria, Compartidos)) +
  geom_boxplot()+
  theme_bw()

ggplot(dfFinal) +
  geom_bar(mapping = aes(x = Categoria, y = Compartidos, fill = Categoria), stat = "identity") +
  theme_minimal() +
  ggtitle("Compartidos por categoría")+
  guides(fill=FALSE)+
  coord_flip()

dfFinal %>%
  filter(Categoria == "Arte")%>%
  select(Titulo, Compartidos)%>%
  ggplot(mapping = aes( x = Titulo, y = Compartidos, fill= Titulo))+
  ggtitle("Compartidos por noticia")+
  geom_bar(stat = "identity")+
  guides(fill=FALSE)+
  theme_bw()+
  coord_flip()

