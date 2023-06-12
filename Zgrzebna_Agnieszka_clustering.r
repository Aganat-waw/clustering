# AGNIESZKA ZGRZEBNA
############################################################################################

# CEL RAPORTU:
# Pogrupowanie zbioru Pokemon

############################################################################################
# biblioteki
library(dbscan)
library(fpc)
library(cluster)
library(mclust)
library(factoextra)

# zbiór danych:
download.file('http://staff.ii.pw.edu.pl/~rbembeni/dane/Pokemon.csv','Pokemon.csv')
pokemon.all <- read.csv("Pokemon.csv")

############################################################################################

# 1. Zappoznanie ze zbiorem, sprawdzenie jakiego typu s± atrybuty i czy s± warto¶ci brakuj±ce

view(pokemon.all)
summary(pokemon.all)
anyNA(pokemon.all)
str(pokemon.all)

# Atrybuty s± typu faktor lub liczby ca³kowite. Nie wystêpuj± braki danych. W zbiorze wystêpuj± dane nominalne,
# liczbowe i logiczne.

############################################################################################

#2. Grupowanie algorytmem partycjonuj±cym
library(dplyr)
set.seed(123)
pokemon <- pokemon.all %>% sample_frac(0.25)
rownames(pokemon)<- pokemon$Name
summary(pokemon)
view(pokemon)
summary(pokemon$Type.1)
summary(pokemon$Type.2)

pokemon2 <- pokemon[,(5:11)]
view(pokemon2)
summary(pokemon2)

pokemonScale <- scale(pokemon2, center = FALSE)
pokemonScale <- as.data.frame(pokemonScale)
summary(pokemonScale)

# metoda ³okcia k-¶rednich
wss<-0
for (i in 1:15) 
{
  km.out <- kmeans(pokemonScale, centers = i, nstart=20)
  wss[i] <- km.out$tot.withinss
}

plot(1:15, wss, type = "b",  xlab = "Liczba grup", ylab = "Suma blêdu kwadratowego wewn±trz grup")
#optymaln± liczbê grup okre¶lam jako 5

#a. Algorytm k-¶rodków

fviz_nbclust(pokemonScale, pam, method = "silhouette")+theme_classic()

pokemon3<- pokemonScale[,c(2,3,4, 7)]
fviz_nbclust(pokemon3, pam, method = "silhouette")+theme_classic()

#b. Ró¿ne paramtery dla miar odleg³o¶ci oraz liczby grup

pam.poke2 <- pam(pokemonScale, 2 , metric = "euclidean")
print(pam.poke2)

pam.pokm2 <- pam(pokemonScale, 2 , metric = "manhattan")
print(pam.pokm2)

pam.poke4 <- pam(pokemonScale, 4 , metric = "euclidean")
print(pam.poke4)

pam.pokm4 <- pam(pokemonScale, 4 , metric = "manhattan")
print(pam.pokm4)

pam.poke9 <- pam(pokemon3, 9 , metric = "euclidean")
print(pam.poke6)

pam.pokm9 <- pam(pokemon3, 9 , metric = "manhattan")
print(pam.pokm6)

#c. Ocena jako¶ci grupowania
fviz_silhouette(pam.poke2, palette="jco")

fviz_silhouette(pam.pokm2, palette="jco")

fviz_silhouette(pam.poke4, palette="jco")

fviz_silhouette(pam.pokm4, palette="jco")

fviz_silhouette(pam.poke9, palette="jco")

fviz_silhouette(pam.pokm9, palette="jco")


# Najlepsze grupowanie wykona³o siê dla 2 grup metod± manhattan. Dla 4 grup metoda euklidesowa by³a lepsza,
# tak jak dla zbioru z mniejsz± liczb± atrybutów.

#d.Przypisanie poszczególnych rekordów do grup

pokemon_clus1<-cbind(pokemon2, pam.pokm2$cluster)
head(pokemon_clus1)

pokemon_clus2<-cbind(pokemon2, pam.poke4$cluster)
head(pokemon_clus2)

pokemon_clus3<-cbind(pokemon2, pam.poke9$cluster)
head(pokemon_clus3)

print(pam.pokm2$medoids)
print(pam.pokm2$clusinfo)

print(pam.poke4$medoids)
print(pam.poke4$clusinfo)

print(pam.poke9$medoids)
print(pam.poke9$clusinfo)

fviz_cluster(pam.pokm2, ellipse.type = "t", repel = TRUE, show.clust.cent=TRUE, ggtheme = theme_light())

fviz_cluster(pam.poke4, ellipse.type = "t", repel = TRUE, show.clust.cent=TRUE, ggtheme = theme_minimal())

fviz_cluster(pam.poke9, ellipse.type = "t", repel = TRUE, show.clust.cent=TRUE, ggtheme = theme_void())

#e. Charakterystyczne elementy grup

# 2 grupy
table(pokemon$Type.1,pam.pokm2$cluster)
table(pokemon$Type.2,pam.pokm2$cluster)
table(pokemon$Generation,pam.pokm2$cluster)
table(pokemon$Legendary,pam.pokm2$cluster)
table(pokemon$Total,pam.pokm2$cluster)
table(pokemon$Attack,pam.pokm2$cluster)
table(pokemon$Defense,pam.pokm2$cluster)
table(pokemon$Sp..Atk,pam.pokm2$cluster)
table(pokemon$Sp..Def,pam.pokm2$cluster)
table(pokemon$Speed,pam.pokm2$cluster)

# Wizualizacja wybranych atrybutów
plot(pokemon2[1:2], col = pam.pokm2$cluster)
plot(pokemon2[3:4], col = pam.pokm2$cluster)
plot(pokemon2[c(2,7)], col = pam.pokm2$cluster)
plot(pokemon[c(3,5)], col = pam.pokm2$cluster)
plot(pokemon[c(3,7)], col = pam.pokm2$cluster)
plot(pokemon[c(4,7)], col = pam.pokm2$cluster)
plot(pokemon[c(5,13)], col = pam.pokm2$cluster)
plot(pokemon[c(5,12)], col = pam.pokm2$cluster)
plot(pokemon[c(5,11)], col = pam.pokm2$cluster)
plot(pokemon[c(5,8)], col = pam.pokm2$cluster)
plot(pokemon[c(5,7)], col = pam.pokm2$cluster)


# 4 grupy
table(pokemon$Type.1,pam.poke4$cluster)
table(pokemon$Type.2,pam.poke4$cluster)
table(pokemon$Generation,pam.poke4$cluster)
table(pokemon$Legendary,pam.poke4$cluster)
table(pokemon$Total,pam.poke4$cluster)
table(pokemon$Attack,pam.poke4$cluster)
table(pokemon$Defense,pam.poke4$cluster)
table(pokemon$Sp..Atk,pam.poke4$cluster)
table(pokemon$Sp..Def,pam.poke4$cluster)
table(pokemon$Speed,pam.poke4$cluster)

#wizualizacja wybranych atrybutów
plot(pokemon2[1:2], col = pam.poke4$cluster)
plot(pokemon2[3:4], col = pam.poke4$cluster)
plot(pokemon2[c(2,7)], col = pam.poke4$cluster)
plot(pokemon[c(3,5)], col = pam.poke4$cluster)
plot(pokemon[c(3,7)], col = pam.poke4$cluster)
plot(pokemon[c(4,7)], col = pam.poke4$cluster)
plot(pokemon[c(5,13)], col = pam.poke4$cluster)
plot(pokemon[c(5,12)], col = pam.poke4$cluster)
plot(pokemon[c(5,11)], col = pam.poke4$cluster)
plot(pokemon[c(5,8)], col = pam.poke4$cluster)
plot(pokemon[c(5,7)], col = pam.poke4$cluster)

# 9 grup
table(pokemon$Type.1,pam.poke9$cluster)
table(pokemon$Type.2,pam.poke9$cluster)
table(pokemon$Generation,pam.poke9$cluster)
table(pokemon$Legendary,pam.poke9$cluster)
table(pokemon$Total,pam.poke9$cluster)
table(pokemon$Attack,pam.poke9$cluster)
table(pokemon$Defense,pam.poke9$cluster)
table(pokemon$Sp..Atk,pam.poke9$cluster)
table(pokemon$Sp..Def,pam.poke9$cluster)
table(pokemon$Speed,pam.poke9$cluster)

#wizualizacja wybranych atrybutów
plot(pokemon2[1:2], col = pam.poke9$cluster)
plot(pokemon2[3:4], col = pam.poke9$cluster)
plot(pokemon2[c(2,7)], col = pam.poke9$cluster)
plot(pokemon[c(3,5)], col = pam.poke9$cluster)
plot(pokemon[c(3,7)], col = pam.poke9$cluster)
plot(pokemon[c(4,7)], col = pam.poke9$cluster)
plot(pokemon[c(5,13)], col = pam.poke9$cluster)
plot(pokemon[c(5,12)], col = pam.poke9$cluster)
plot(pokemon[c(5,11)], col = pam.poke9$cluster)
plot(pokemon[c(5,8)], col = pam.poke9$cluster)
plot(pokemon[c(5,7)], col = pam.poke9$cluster)


#Zbiór najlepiej dzieli siê przez ogólny wynik (Total) si³y pokemonów w szczególno¶ci dla 2 grup.
# Przy 4 grupach ³atwiej jest zauwa¿yæ podzia³y dodatkowo dla szybko¶ci pokemonów, ataku i obrony. 
# Dla 9 grup zdaje siê, ¿e oprócz ogólnej si³y, ³atwiej jest wychwyciæ obronê ni¿ atak czy szybko¶æ
# w¶ród cech charakterystycznych.

############################################################################################

#3.DBSCAN

#a. Wyznaczenie eps dla ró¿nych k

dbscan::kNNdistplot(pokemon2, k=2)
abline(h=40, lty="dashed")

dbscan::kNNdistplot(pokemon2, k=3)
abline(h=50, lty="dashed")

dbscan::kNNdistplot(pokemon2, k=5)
abline(h=60, lty="dashed")

dbscan::kNNdistplot(pokemon2, k=9)
abline(h=70, lty="dashed")

# Dla wiêkszego k ro¶nie szacowany eps od 40 do 70.

#b. Grupowanie dla ró¿nych paramterów

pokemonDB402 <- dbscan::dbscan(pokemon2, eps=40, minPts=2)
print(pokemonDB402)

pokemonDB503 <- dbscan::dbscan(pokemon2, eps=50, minPts=3)
print(pokemonDB503)

pokemonDB605 <- dbscan::dbscan(pokemon2, eps=60, minPts=5)
print(pokemonDB605)

pokemonDB709 <- dbscan::dbscan(pokemon2, eps=70, minPts=9)
print(pokemonDB709)

# Tylko dla eps=40 z minPts=2 przeprowadzi³o siê jakie¶ grupowanie, dla pozosta³ych przypadków otrzyma³am
# zbiory z jednym klastrem tylko. Postanowi³am przeprowadziæ dodatkowe pomiary z inna warto¶ci± eps dla minPts = 2 ni¿
# wstêpnie szacowa³am.

pokemonDB602 <- dbscan::dbscan(pokemon2, eps=60, minPts=2)
print(pokemonDB602)

pokemonDB502 <- dbscan::dbscan(pokemon2, eps=50, minPts=2)
print(pokemonDB502)

pokemonDB302 <- dbscan::dbscan(pokemon2, eps=30, minPts=2)
print(pokemonDB302)

# Dla takich warto¶ci tworz± siê ró¿ne grupy, prawie wszystkie podzia³y charkateryzuj± siê jednak dominacj± jednej grupy
# nad innymi. Wyj±tkiem jest eps=30 gdzie najliczniejsz± grup± jest szum, a z pozosta³ych danych stworzy³o siê wiele
# ma³olicznych grup.

#c. Ocena jako¶ci

# oszacowany

silhouette_values <- silhouette(pokemonDB402$cluster, dist(pokemon2))
mean_silhouette <- mean(silhouette_values[, "sil_width"])
print(paste("¦rednia warto¶æ silhouette:", mean_silhouette))
#otrzyma³am warto¶c ujemn± co oznacza z³y przydzia³

# dodatkowe testy

silhouette_values <- silhouette(pokemonDB602$cluster, dist(pokemon2))
mean_silhouette <- mean(silhouette_values[, "sil_width"])
print(paste("¦rednia warto¶æ silhouette:", mean_silhouette))
# warto¶æ dodatnia bardzo ma³a

silhouette_values <- silhouette(pokemonDB502$cluster, dist(pokemon2))
mean_silhouette <- mean(silhouette_values[, "sil_width"])
print(paste("¦rednia warto¶æ silhouette:", mean_silhouette))
# warto¶æ dodatnia bardzo ma³a

silhouette_values <- silhouette(pokemonDB302$cluster, dist(pokemon2))
mean_silhouette <- mean(silhouette_values[, "sil_width"])
print(paste("¦rednia warto¶æ silhouette:", mean_silhouette))
# warto¶æ ujemna czyli z³y przydzia³

#Najlepsze grupowanie czyli z najwy¿syzm wska¼nikiem silhouette wykona³o siê dla warto¶ci eps=60, ale
# warto¶æ silhouette wydaje siê mimo wszystko bardzo ma³a.

#d. Przypisanie rekordów

fviz_cluster(pokemonDB602, pokemon2, ellipse.type = "t", repel = TRUE, show.clust.cent=TRUE, ggtheme = theme_light())
# Grupowanie okaza³o siê s³abej jako¶ci. Powsta³a ogromna jedna grupa i kilka bardzo ma³ych.
#Postanowi³am zwizualizowaæ wstêpne oszacowane warto¶ci parametrów dla porównania.

fviz_cluster(pokemonDB402, pokemon2, ellipse.type = "t", repel = TRUE, show.clust.cent=TRUE, ggtheme = theme_light())
#Wizualnie mam wra¿enie, ¿e to przedstawienie jest trochê lepsze ni¿ pierwsze, chocia¿ warto¶æ silhoutte jest poni¿ej 0.

fviz_cluster(pokemonDB302, pokemon2, ellipse.type = "t", repel = TRUE, show.clust.cent=TRUE, ggtheme = theme_light())

#e.Charakterystyczne elementy grup
# najwy¿sza warto¶c silhouette
plot(pokemon2[1:2], col = pokemonDB602$cluster)
plot(pokemon2[3:4], col = pokemonDB602$cluster)
plot(pokemon2[c(2,7)], col = pokemonDB602$cluster)
plot(pokemon[c(3,5)], col = pokemonDB602$cluster)
plot(pokemon[c(3,7)], col = pokemonDB602$cluster)
plot(pokemon[c(4,7)], col = pokemonDB602$cluster)
plot(pokemon[c(5,13)], col = pokemonDB602$cluster)
plot(pokemon[c(5,12)], col = pokemonDB602$cluster)
plot(pokemon[c(5,11)], col = pokemonDB602$cluster)
plot(pokemon[c(5,8)], col = pokemonDB602$cluster)
plot(pokemon[c(5,7)], col = pokemonDB602$cluster)
# Zdaje siê, ¿e algorytm oddzieli³ typ "robak" od innych oraz stara³ siê oddzielaæ 
# poszczególne silniejsze pokemony od pozosta³ych.

# postanowi³am zwizualizowaæ równie¿ najliczniejsze pogrupowanie
plot(pokemon2[1:2], col = pokemonDB302$cluster)
plot(pokemon2[3:4], col = pokemonDB302$cluster)
plot(pokemon2[c(2,7)], col = pokemonDB302$cluster)
plot(pokemon[c(3,5)], col = pokemonDB302$cluster)
plot(pokemon[c(3,7)], col = pokemonDB302$cluster)
plot(pokemon[c(4,7)], col = pokemonDB302$cluster)
plot(pokemon[c(5,13)], col = pokemonDB302$cluster)
plot(pokemon[c(5,12)], col = pokemonDB302$cluster)
plot(pokemon[c(5,11)], col = pokemonDB302$cluster)
plot(pokemon[c(5,8)], col = pokemonDB302$cluster)
plot(pokemon[c(5,7)], col = pokemonDB302$cluster)
# Podobnie mo¿na zauwa¿yæ pewn± sugestiê podzia³u ze wzglêdu na si³ê, samo grupowanie jednak jest s³abej jako¶ci.


############################################################################################

# 4. Porównanie grupowañ

sapply(list(pam.pokm2$cluster, pokemonDB602$cluster ), 
       function(c) cluster.stats(dist(pokemon2),c)[c("within.cluster.ss","avg.silwidth")])

# W moim eksperymencie dla zbioru pokemon zdecydowanie lepiej poradzi³y sobie algorytmy grupowania partycjonuj±cego.

# Na podsatwie grupowania gêsto¶ciowego nie by³am w stanie wysnuæ wniosków o zbiorze oprócz tego, ¿e jego 

# spacyfika najwidoczniej nie do koñca wspó³gra z takim rodzajem metod. Na podstawie algorytmów partycjonuj±cych

# ustali³am, ¿e zbiór ca³kiem dobrze dzieli pokemony ze wzglêdu na ich ogóln± si³ê. 
