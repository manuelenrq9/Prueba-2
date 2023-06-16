getwd()
list.files()
file = read.csv("Amazon Sale Report.csv")
View(file)

columns =  c("Status", "Fulfilment", "Category")
df_limpio = file[, columns]


any(is.na(df_limpio$Status))
any(is.na(df_limpio$Fulfilment))
any(is.na(df_limpio$Category))

#metricas 
# 1)Tasa de envío fallidos: 
#Representa en porcentualmente los envíos cancelados sobre el total de envíos
# columnas necesarias: -Status 

total_cancelados = sum(df_limpio$Status == "Cancelled")
total_cancelados

total_envios =  length(df_limpio$Status)
total_envios 

tasa_envios_cancelados = (total_cancelados/total_envios)*100
tasa_envios_cancelados

tasa_envios_correctos = 100 - tasa_envios_cancelados       
tasa_envios_correctos

porcentajes1 = c(tasa_envios_cancelados,tasa_envios_correctos)
categorias1 =  c("Envios cancelados","Envios confirmados")
etiquetas <- paste0(categorias1, " (", round(porcentajes1, 2), "%)")

par(mar = c(2, 2, 2, 2))
grafica1 = pie(porcentajes1,labels = etiquetas,main="Envíos")
grafica1


# 2)Tasa de envío realizado por  Merchant:
# Representa porcentualmente los despachos de envíos realizados por Merchant
# sobre el total de despachos realizados.

total_Merchant = sum(df_limpio$Fulfilment == "Merchant")
total_Merchant

total_Amazon = sum(df_limpio$Fulfilment == "Amazon")
total_Amazon

total_envios =  length(df_limpio$Status)
total_envios 

tasa_envios_Merchant = (total_Merchant/total_envios)*100
tasa_envios_Merchant

tasa_envios_Amazon = (total_Amazon/total_envios)*100
tasa_envios_Amazon

porcentajes2 = c(tasa_envios_Merchant,tasa_envios_Amazon)
categorias2 =  c("Envios de Merchant","Envios de Amazon")
etiquetas2 <- paste0(categorias2, " (", round(porcentajes2, 2), "%)")

grafica2 = pie(porcentajes2,labels = etiquetas2,main="Fulfilment")
grafica2

# metrica 3
install.packages("ggplot2")
library(ggplot2)

df_amazon <- df_limpio[df_limpio$Fulfilment == "Amazon", ]
envios_por_amazon <- table(df_amazon$Category)

amazon <- ggplot(data =data.frame(Categoria = names(envios_por_amazon), Envios = envios_por_amazon) ,
                  mapping =  aes(x = factor(Categoria), y = factor(Envios.Freq))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                        xlab("Categoría") +
                         ylab("Envíos") +
                          ggtitle("Envíos por Categoría (Amazon)") +
                           theme_bw()


df_merchant <- df_limpio[df_limpio$Fulfilment == "Merchant", ]
envios_por_merchant <- table(df_merchant$Category)

merchant <- ggplot(data =data.frame(Categoria = names(envios_por_merchant), Envios = envios_por_merchant) ,
                   mapping =  aes(x = factor(Categoria), y = factor(Envios.Freq))) +
                    geom_bar(stat = "identity", fill = "green") +
                    xlab("Categoría") +
                    ylab("Envíos") +
                    ggtitle("Envíos por Categoría (Amazon)") +
                    theme_bw()
amazon
merchant


# install.packages("openxlsx")

#library(openxlsx)
#write.xlsx(df_limpio, "bd_limpia.xlsx")