#Parte I Exploración de datos:

library('ggplot2')
library('scales')
library('corrplot')
train<-read.csv('train.csv',sep=',',header=TRUE)
test<-read.csv('test.csv',sep=',',header=TRUE)
referencia<-read.csv('sample.csv',sep=',',header=TRUE)

#probando añadir columnas
SalePrice<-rep(NA,times=nrow(test))

e1<-data.frame(cbind(SalePrice,test)) #Prueba

d<-cbind(SalePrice,test) #Usemos este
#las dos formas son validas

#se combinan las datas
f<-rbind(train,d) #usemos este
f1<-rbind(train,e1)#Prueba
#Analicemos la data
dim(f)
str(f)
summary(f$SalePrice)
hist(train$SalePrice,col='darkblue',main='Antes',xlab='Precios',ylab='Frecuencia')

#Parte II Procesamiento de Datos
#Veamos los valores faltantes
s<-data.frame(sapply(f,is.na))
c<-colSums(s,na.rm=TRUE)
Data.faltante<-sort(c[c>0],decreasing = TRUE)

#Analicemos los valores faltantes

#Trabajemos primero con la calidad de la piscina

ggplot(f[!is.na(f$PoolQC),],aes(x=PoolQC))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Calidad de la piscina')+ylab('Numero de piscinas')

#Verfiquemos que es cierto que no hayan piscinas que tengan calidad
which(f$PoolArea==0 & !is.na(f$PoolQC))

#Veamos cuales de las viviendas tienen piscinas pero no tienen una calidad asociada
f[which(f$PoolArea>0 & is.na(f$PoolQC)),c('PoolArea','PoolQC')]

#Rellenemos estos valores sacandole la media al area de piscinas asociadas a las piscinas excelentes,buenas y regulares

m1<- f[which(f$PoolArea>0 & f$PoolQC=='Ex'),c('PoolArea','PoolQC')]
m2<- f[which(f$PoolArea>0 & f$PoolQC=='Gd'),c('PoolArea','PoolQC')]
m3<- f[which(f$PoolArea>0 & f$PoolQC=='Fa'),c('PoolArea','PoolQC')]

mean(m1$PoolArea)
mean(m2$PoolArea)
mean(m3$PoolArea)

#Asi de acuerdo a estos valores rellenemos los valores respectivos

f[2421,'PoolQC']='Ex'
f[2504,'PoolQC']='Ex'
f[2600,'PoolQC']='Fa'

#Veamos si las variedades de las viviendas tienen sus precios respectivos y si tienen precios sin variedad
table(f$MiscFeature)

which(f$MiscVal>0 & is.na(f$MiscFeature))
f[2550,c('MiscFeature','MiscVal')]

f[which(f$MiscVal==0 & !is.na(f$MiscFeature)),c('MiscFeature','MiscVal')]



M1<-f[which(f$MiscVal>0 & f$MiscFeature=='Othr'),c('MiscFeature','MiscVal')]
M2<-f[which(f$MiscVal>0 & f$MiscFeature=='Shed'),c('MiscFeature','MiscVal')]
M3<-f[which(f$MiscFeature=='Gar2'),c('MiscFeature','MiscVal')]

f[which(f$MiscFeature=='TenC'),c('MiscFeature','MiscVal')]
mean(M1$MiscVal)
mean(M2$MiscVal)
mean(M3$MiscVal)


f[874,'MiscVal']=433
f[1201,'MiscVal']=780
f[2432,'MiscVal']=780
f[2550,'MiscFeature']='Gar2'

ggplot(f,aes(x=MiscFeature,y=MiscVal))+geom_point(colour='darkblue')+xlab('Variedades')+ylab('Costo')+scale_y_continuous(label = dollar)

#Analicemos ahora los callejones y los caminos de cada vivienda
table(f$Alley)
table(f$Street)

ggplot(f[!is.na(f$Alley),],aes(x=Alley))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Callejon')+ylab('Cantidad')
ggplot(f,aes(x=Street))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Camino')+ylab('Cantidad')

#Analicemos las cercas

table(f$Fence)
ggplot(f[!is.na(f$Fence),],aes(x=Fence))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Cerca')+ylab('Cantidad')


#Analicemos las chimeneas

table(f$Fireplaces)
table(f$FireplaceQu)

#Verifiquemos si hay chimeneas que no tengan asociada una calidad
which(f$Fireplaces>0 & is.na(f$FireplaceQu))
which(f$Fireplaces==0 & !is.na(f$FireplaceQu))


ggplot(f[!is.na(f$FireplaceQu),],aes(x=FireplaceQu))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Calidad Chimeneas')+ylab('Cantidad')

#Analicemos la distancia entre la propiedad a la calle

h<-f[order(f$Neighborhood),c('Neighborhood','LotFrontage')]
h1<-h[!is.na(h$LotFrontage),]
h2<-aggregate(h1[,2],list(h1$Neighborhood),median)

H1<-f[,c('Neighborhood','LotFrontage')]

for(i in 1:nrow(f)){if(is.na(f[i,'LotFrontage']))f[i,'LotFrontage']<-h2$x[f[i,'Neighborhood']]}
hist(f$LotFrontage,main='Antes',col='darkblue',xlab='Distancia',ylab='Frecuencia')
hist(H1$LotFrontage,main='Despues',col='darkblue',xlab='Distancia',ylab='Frecuencia')

#Analicemos los garages de las viviendas

#Existen 159 NA para el año de construccion del garage, veamos cuantas viviendas coinciden con su año de construcción y su año de construcci'on del garage

length(which(f$YearBuilt==f$GarageYrBlt))

#Por lo que 544 viviendas no coinciden con el año de construccion del garage
#Rellenemos los valores faltantes con el mismo año de construccion de la vivienda

na1<-which(is.na(f$GarageYrBlt))
f[na1,'GarageYrBlt']<-f[na1,'YearBuilt']

which(f$GarageArea>0 & (f$GarageCars==0 | is.na(f$GarageCars)))
which(f$GarageArea>0 & is.na(f$GarageType))

which(f$GarageArea>0 & is.na(f$GarageQual))
which(f$GarageArea>0 & is.na(f$GarageFinish))
which(f$GarageArea>0 & is.na(f$GarageCond))

f[c(2127,which(is.na(f$GarageArea))),c('GarageArea','GarageCars','GarageQual','GarageFinish','GarageCond','GarageType')]

na2<-which(f$GarageArea==360 & f$GarageCars==1)
f[na2,c('GarageArea','GarageCars','GarageQual','GarageFinish','GarageCond','GarageType')]

f[2127,'GarageQual']<-'TA'
f[2127,'GarageFinish']<-'Unf'
f[2127,'GarageCond']<-'TA'

f[2577,'GarageType']<-NA
f[2577,c('GarageArea','GarageCars')]<-0

#Analicemos los sotanos

Bsmt<-c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')

A<-f[which(is.na(f$BsmtCond)),Bsmt]
A[which(A$TotalBsmtSF>0),]

ggplot(f[!is.na(f$BsmtCond),],aes(x=BsmtCond))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Condición Sótano')+ylab('Cantidad')

f[2041,'BsmtCond']<-'TA'
f[2186,'BsmtCond']<-'TA'
f[2525,'BsmtCond']<-'TA'

f[which(is.na(f$TotalBsmtSF)),Bsmt]

f[which(is.na(f$BsmtFullBath) & is.na(f$BsmtHalfBath)),Bsmt]
f[2121,c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]<-0
f[2189,c('BsmtFullBath','BsmtHalfBath')]<-0

A1<-f[which(is.na(f$BsmtExposure) & !is.na(f$BsmtCond)),Bsmt]
ggplot(f[!is.na(f$BsmtExposure),],aes(x=BsmtExposure))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Exposure')+ylab('Cantidad')

f[c(949,1488,2349),'BsmtExposure']<-'No'

A2<-f[which(is.na(f$BsmtQual)),Bsmt]
ggplot(f[!is.na(f$BsmtQual),],aes(x=BsmtQual))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Calidad Sotano')+ylab('Cantidad')

f[c(2218,2219),'BsmtQual']<-'TA'

A3<-f[which(is.na(f$BsmtFinType1)),Bsmt]
A4<-f[which(is.na(f$BsmtFinType2)),Bsmt]

ggplot(f[!is.na(f$BsmtFinType2),],aes(x=BsmtFinType2))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Tipo de Sotano')+ylab('Cantidad')

f[333,'BsmtFinType2']<-'Unf'

#Analicemos la fachada de la casa

V<-f[which(is.na(f$MasVnrType) | is.na(f$MasVnrArea)),c('MasVnrType','MasVnrArea')]

V1<-f[order(f$MasVnrType),c('MasVnrType','MasVnrArea')]
V2<-V1[!is.na(V1$MasVnrType),]
V3<-aggregate(V2[,2],list(V2$MasVnrType),median)

f[2611,'MasVnrType']<-'Stone'

#Analicemos la zona de donde es la vivienda

f[is.na(f$MSZoning),c('MSZoning','MSSubClass')]
table(f$MSZoning,f$MSSubClass)

f[c(2217,2905),'MSZoning']<-'RL'
f[c(1916,2251),'MSZoning']<-'RM'

#Analicemos los servicios de las viviendas

which(is.na(f$Utilities))

ggplot(f[!is.na(f$Utilities),],aes(x=Utilities))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Servicios')+ylab('Cantidad')
f[c(1916,1946),'Utilities']<-'AllPub'

#Analicemos la funcionalidad de las viviendas
which(is.na(f$Functional))

ggplot(f[!is.na(f$Functional),],aes(x=Functional))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Funcionalidad')+ylab('Cantidad')

f[c(2217,2474),'Functional']<-'Typ'

#Analicemos el exterior de las viviendas

f[which(is.na(f$Exterior1st) | is.na(f$Exterior2nd)),c('Exterior1st','Exterior2nd')]

ggplot(f[!is.na(f$Exterior1st),],aes(x=Exterior1st))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Exterior1')+ylab('Cantidad')
ggplot(f[!is.na(f$Exterior2nd),],aes(x=Exterior2nd))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Exterior2')+ylab('Cantidad')

f[2152,c('Exterior1st','Exterior2nd')]<-'VinylSd'

#Analicemos el sistema electrico

which(is.na(f$Electrical))
ggplot(f[!is.na(f$Electrical),],aes(x=Electrical))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Sistema electrico')+ylab('Cantidad')
f[1380,'Electrical']<-'SBrkr'

#Analicemos la calidad de la cocina

which(is.na(f$KitchenQual))
ggplot(f[!is.na(f$KitchenQual),],aes(x=KitchenQual))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Calidad cocina')+ylab('Cantidad')
f[1556,'KitchenQual']<-'TA'

#Analicemos el tipo de venta

which(is.na(f$SaleType))

ggplot(f[!is.na(f$SaleType),],aes(x=SaleType))+geom_bar(width = 0.5,fill='darkblue')+geom_text(stat='count',aes(label=..count..),vjust=-0.3)+xlab('Tipo de venta')+ylab('Cantidad')
#Veamos que condicion de venta tiene el valor faltante

f[which(is.na(f$SaleType)),'SaleCondition']

table(f$SaleCondition,f$SaleType)

f[2490,'SaleType']<-'WD'



#Asignemosle valores numericos a algunas variables categoricas


qc<-c('ExterQual1','ExterCond1','GarageQual1','GarageCond1','FireplaceQu1','KitchenQual1','HeatingQC1','BsmtQual1','BsmtCond1','PoolQC1')

ExterQual1<-rep(0,times=nrow(f))
ExterCond1<-rep(0,times=nrow(f))
GarageQual1<-rep(0,times=nrow(f))
GarageCond1<-rep(0,times=nrow(f))
FireplaceQu1<-rep(0,times=nrow(f))
KitchenQual1<-rep(0,times=nrow(f))
HeatingQC1<-rep(0,times=nrow(f))
BsmtQual1<-rep(0,times=nrow(f))
BsmtCond1<-rep(0,times=nrow(f))
PoolQC1<-rep(0,times=nrow(f))

c1<-c('ExterQual','ExterCond','GarageQual','GarageCond','FireplaceQu','KitchenQual','HeatingQC','BsmtQual','BsmtCond','PoolQC')

f1<-f[,c('ExterQual','ExterCond','GarageQual','GarageCond','FireplaceQu','KitchenQual','HeatingQC','BsmtQual','BsmtCond','PoolQC')]

z<-data.frame(ExterQual1,ExterCond1,GarageQual1,GarageCond1,FireplaceQu1,KitchenQual1,HeatingQC1,BsmtQual1,BsmtCond1,PoolQC1)

for(i in 1:ncol(z)){f<-cbind(z[i],f)}

for(j in 1:10){for(i in 1:nrow(f)){if(isTRUE(f[i,c1[j]]=='Ex'))f[i,qc[j]]<-5 
  if(isTRUE(f[i,c1[j]]=='Gd'))f[i,qc[j]]<-4 
    if(isTRUE(f[i,c1[j]]=='TA'))f[i,qc[j]]<-3 
      if(isTRUE(f[i,c1[j]]=='Fa'))f[i,qc[j]]<-2 
        if(isTRUE(f[i,c1[j]]=='Po'))f[i,qc[j]]<-1 
          if(is.na(f[i,c1[j]]))f[i,qc[j]]<-0}}


#Ahora BsmtExposure
BsmtExposure1<-rep(0,times=nrow(f))
f<-cbind(BsmtExposure1,f)

for(i in 1:nrow(f)){if(isTRUE(f[i,'BsmtExposure']=='Gd'))f[i,'BsmtExposure1']<-4 
if(isTRUE(f[i,'BsmtExposure']=='Av'))f[i,'BsmtExposure1']<-3 
if(isTRUE(f[i,'BsmtExposure']=='Mn'))f[i,'BsmtExposure1']<-2 
if(isTRUE(f[i,'BsmtExposure']=='No'))f[i,'BsmtExposure1']<-1 
if(is.na(f[i,'BsmtExposure']))f[i,'BsmtExposure1']<-0}

#Ahora BsmtFinType1 y BsmtFinType2
BsmtFinType1_1<-rep(0,times=nrow(f))
BsmtFinType2_1<-rep(0,times=nrow(f))

f<-cbind(BsmtFinType1_1,f)
f<-cbind(BsmtFinType2_1,f)

c2<-c('BsmtFinType1','BsmtFinType2')
qc2<-c('BsmtFinType1_1','BsmtFinType2_1')

f2<-f[,c('BsmtFinType1','BsmtFinType2')]

for(j in 1:2){for(i in 1:nrow(f)){if(isTRUE(f[i,c2[j]]=='GLQ'))f[i,qc2[j]]<-6 
if(isTRUE(f[i,c2[j]]=='ALQ'))f[i,qc2[j]]<-5 
if(isTRUE(f[i,c2[j]]=='BLQ'))f[i,qc2[j]]<-4 
if(isTRUE(f[i,c2[j]]=='Rec'))f[i,qc2[j]]<-3 
if(isTRUE(f[i,c2[j]]=='LwQ'))f[i,qc2[j]]<-2 
if(isTRUE(f[i,c2[j]]=='Unf'))f[i,qc2[j]]<-1
if(is.na(f[i,c2[j]]))f[i,qc2[j]]<-0}}

#Ahora la funcionalidad
Functional1<-rep(0,times=nrow(f))
f<-cbind(Functional1,f)

for(i in 1:nrow(f)){if(isTRUE(f[i,'Functional']=='Typ'))f[i,'Functional1']<-8 
if(isTRUE(f[i,'Functional']=='Min1'))f[i,'Functional1']<-7 
if(isTRUE(f[i,'Functional']=='Min2'))f[i,'Functional1']<-6 
if(isTRUE(f[i,'Functional']=='Mod'))f[i,'Functional1']<-5 
if(isTRUE(f[i,'Functional']=='Maj1'))f[i,'Functional1']<-4 
if(isTRUE(f[i,'Functional']=='Maj2'))f[i,'Functional1']<-3 
if(isTRUE(f[i,'Functional']=='Sev'))f[i,'Functional1']<-2
if(isTRUE(f[i,'Functional']=='Sal'))f[i,'Functional1']<-1
if(is.na(f[i,'Functional']))f[i,'']<-0}


#GarageFinish
GarageFinish1<-rep(0,times=nrow(f))
f<-cbind(GarageFinish1,f)

for(i in 1:nrow(f)){if(isTRUE(f[i,'GarageFinish']=='Fin'))f[i,'GarageFinish1']<-3 
if(isTRUE(f[i,'GarageFinish']=='RFn'))f[i,'GarageFinish1']<-2
if(isTRUE(f[i,'GarageFinish']=='Unf'))f[i,'GarageFinish1']<-1
if(is.na(f[i,'GarageFinish']))f[i,'GarageFinish1']<-0}

#Analicemos los vecindarios

N<-f[order(f$Neighborhood),c('Neighborhood','SalePrice')]
N1<-N[!is.na(N$SalePrice),]
N2<-aggregate(N1[,2],list(N1$Neighborhood),mean)

colnames(N2)<-c('Neighborhood','Media')

ggplot(N2,aes(x=reorder(Neighborhood,-Media),y=Media))+geom_point(size=4,colour='darkblue')+xlab('Vecindario')+ylab('Media del precio')+scale_y_continuous(label=dollar)

Neighborhood1<-rep(0,times=nrow(f))
f<-cbind(Neighborhood1,f)

f3<-f[,c('Id','Neighborhood')]

for(i in 1:nrow(f)){if(isTRUE(f[i,'Neighborhood']=='MeadowV')|isTRUE(f[i,'Neighborhood']=='IDOTRR')|isTRUE(f[i,'Neighborhood']=='BrDale'))f[i,'Neighborhood1']<-1 
if(isTRUE(f[i,'Neighborhood']=='BrkSide')|isTRUE(f[i,'Neighborhood']=='Edwards')| isTRUE(f[i,'Neighborhood']=='OldTown'))f[i,'Neighborhood1']<-2
if(isTRUE(f[i,'Neighborhood']=='Sawyer')|isTRUE(f[i,'Neighborhood']=='Blueste')| isTRUE(f[i,'Neighborhood']=='SWISU')|isTRUE(f[i,'Neighborhood']=='NPkVill')| isTRUE(f[i,'Neighborhood']=='NAmes')|isTRUE(f[i,'Neighborhood']=='Mitchel'))f[i,'Neighborhood1']<-3
if(isTRUE(f[i,'Neighborhood']=='SawyerW')|isTRUE(f[i,'Neighborhood']=='NWAmes')| isTRUE(f[i,'Neighborhood']=='Gilbert')|isTRUE(f[i,'Neighborhood']=='Blmngtn')|isTRUE(f[i,'Neighborhood']=='CollgCr'))f[i,'Neighborhood1']<-4
if(isTRUE(f[i,'Neighborhood']=='Crawfor')|isTRUE(f[i,'Neighborhood']=='ClearCr')| isTRUE(f[i,'Neighborhood']=='Somerst')|isTRUE(f[i,'Neighborhood']=='Veenker')|isTRUE(f[i,'Neighborhood']=='Timber'))f[i,'Neighborhood1']<-5
if(isTRUE(f[i,'Neighborhood']=='StoneBr')|isTRUE(f[i,'Neighborhood']=='NridgHt')| isTRUE(f[i,'Neighborhood']=='NoRidge'))f[i,'Neighborhood1']<-6
if(is.na(f[i,'Neighborhood']))f[i,'Neighborhood1']<-0}

#MasVnrArea
for(i in 1:nrow(f)){if(is.na(f[i,'MasVnrArea']))f[i,'MasVnrArea']<-0}



f_numeric<-f[names(which(sapply(f, is.numeric)))]



#Matriz de correlación

correlaciones<-cor(f_numeric[1:1460,])

cor.SalePrice<-as.matrix(sort(correlaciones[,'SalePrice'], decreasing = TRUE))
t<-names(which(apply(cor.SalePrice,1,function(x)(x>0.5))))

corrplot(correlaciones[t,t],type = 'upper',method = 'color',cl.cex = 0.7,tl.cex=0.7,addCoef.col = 'black')

#Modelo

fn1<-f_numeric[1:1460,]
E1<-sample(1:1460,1022)
entrenamiento1<-fn1[E1,]
prueba1<-fn1[-E1,]

modelo1<-lm(SalePrice~.,entrenamiento1)
p1<-predict(modelo1,prueba1)
p2<-as.matrix(p1)

error<-prueba1$SalePrice-p1
rmse<-function(error){sqrt(mean(error^2))}

count=0
for (i in 1:438) {
  if (abs(error[i])<=10000){count = count +1}
  else{count = count}
  
}


#Para la data de prueba


modelo<-lm(SalePrice~.,f_numeric[1:1460,])
p3<-predict(modelo,newdata=f_numeric[1461:2919,])
p4<-as.matrix(p3)

error1<-referencia$SalePrice-p3

rmse(error1)

count1=0
for (i in 1:1459) {
  if (abs(error1[i])<=10000){count1 = count1 +1}
  else{count1 = count1}
  
}

hist(p4,col='darkblue',main='Despues',xlab='Precios',ylab='Frecuencia')

plot(lm(SalePrice~.,f_numeric[1:1460,]))

