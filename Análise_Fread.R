  ###############################################
  ##################Preambulo####################
  ###############################################
  rm(list=ls())
  gc()
  ###############################################
  ############Preparação dos pacotes#############
  ###############################################
  # Selecioanar uma CRAN mirror
  local({r <- getOption("repos")
  r["CRAN"] <- "https://vps.fmvz.usp.br/CRAN/"
  options(repos=r)})
  #Função que carrega e requere os pacotes em silêncio 
  Install_And_Load <- function(Required_Packages)
  {
    Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
    
    if(length(Remaining_Packages)) 
    {
      install.packages(Remaining_Packages, dependencies = TRUE);
    }
    
    for(package_name in Required_Packages)
    {
      library(package_name,character.only=TRUE,quietly=TRUE);
      
    }
  }
  Required_Packages =c("RODBC","ggplot2","dplyr","stringr",
                       "AUC","anytime","plotROC","CHAID",
                       "RPostgreSQL","xlsx","MicrosoftML",
                       "ggmap","maps","mapdata",
                       "NbClust","factoextra","data.table");
  
  Install_And_Load(Required_Packages)
  
  options(scipen = 9999,digits = 10)
  ##############################################################################################
  ##############################################################################################
  ##############################################################################################
  
  ############################################################################
  ###Definindo diretrizes, conectando com o banco e lendo a primeira tabela###
  ############################################################################
  base<-fread(base.txt)
  
  ########################
  ###Primeiras análises###
  ########################
  
  summary(Base)
  Base%>%with(length(unique(medallion)))
  Base%>%with(length(unique(hack_license)))
  Base%>%group_by(store_and_fwd_flag)%>%summarise(Qnt=n())%>%mutate(`%`=Qnt/sum(Qnt))
  Base%>%group_by(payment_type)%>%summarise(Qnt=n())%>%mutate(`%`=Qnt/sum(Qnt))
  Base%>%group_by(Year_Month=format(pickup_datetime,'%Y-%m'))%>%summarise(Mean_trip_Time=mean(trip_time_in_secs),Qnt=n())%>%mutate(`%`=Qnt/sum(Qnt))
  ####################################
  ##Investigando os valores extremos##
  ####################################
  Base%>%filter(pickup_longitude==min(pickup_longitude))
  Base%>%filter(pickup_longitude==max(pickup_longitude))
  Base%>%filter(pickup_latitude==min(pickup_latitude))
  Base%>%filter(pickup_latitude==max(pickup_latitude))
  
  Base%>%filter(dropoff_latitude==min(dropoff_latitude))
  Base%>%filter(dropoff_longitude==max(dropoff_longitude))
  Base%>%filter(dropoff_latitude==min(dropoff_latitude))
  Base%>%filter(dropoff_latitude==max(dropoff_latitude))
  
  
  Base%>%filter(passenger_count==max(passenger_count))
  Base%>%filter(trip_distance==max(trip_distance))
  Base%>%filter(trip_time_in_secs==max(trip_time_in_secs))
  Base%>%filter(fare_amount==max(fare_amount))
  Base%>%filter(surcharge==max(surcharge))
  Base%>%filter(tip_amount==max(tip_amount))
  Base%>%filter(tolls_amount==max(tolls_amount))
  Base%>%filter(total_amount==max(total_amount))
  
  ################################################################
  ###Análisando gráficamente o ponto que a corrida foi iniciada###
  ################################################################
  
  get_map(c(-73.98190,40.75230),zoom = 13,maptype = 'hybrid')%>%
    ggmap()+
    geom_point(data=Base[1:1000,],aes(x=pickup_longitude,y=pickup_latitude),color='red',size=.5)
  
  Base%>%filter(pickup_longitude>0|dropoff_longitude>0)%>%nrow()
  Base%>%filter(pickup_latitude<0|dropoff_latitude<0)%>%nrow()
  
  Base<-Base%>%filter(!((pickup_latitude<0|dropoff_latitude<0)|(pickup_longitude>0|dropoff_longitude>0)))
  Base<-Base%>%filter(trip_time_in_secs>0)
  #Base<-Base%>%filter(trip_distance<2000)
  Base<-Base%>%mutate(trip_distance_01=(trip_distance-min(trip_distance))/(max(trip_distance)-min(trip_distance)),
                      trip_time_in_secs_01=(trip_time_in_secs-min(trip_time_in_secs))/(max(trip_time_in_secs)-min(trip_time_in_secs)))
  
  
  Base%>%select(pickup_latitude,dropoff_latitude,pickup_longitude,dropoff_longitude)%>%summary()
  
  Base%>%select(pickup_latitude,dropoff_latitude,pickup_longitude,dropoff_longitude)%>%
    filter(pickup_latitude==max(pickup_latitude))
  
  Base%>%select(pickup_latitude,dropoff_latitude,pickup_longitude,dropoff_longitude)%>%
    filter(dropoff_latitude==max(dropoff_latitude))
  
  ############################################
  #Coordenadas maximas da cidade de nova york#
  #Norte:40.917577############################
  #Sul:40.477399##############################
  #Leste: -73.700272##########################
  #Oeste:-74.259090###########################
  ############################################
  
  Base<-Base%>%filter((pickup_longitude>-74.259090 & pickup_longitude< -73.700272) &
                        (dropoff_longitude>-74.259090 & dropoff_longitude< -73.700272) &
                        (dropoff_latitude>40.477399 & dropoff_latitude< 40.917577) &
                        (pickup_latitude>40.477399 & pickup_latitude< 40.917577))
  
  
  NYC_Map<-get_map(c(-74.259090,40.477399,-73.700272,40.917577),zoom = 10,maptype = 'hybrid')
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Pickup_map.pdf')
  NYC_Map%>%
    ggmap()+
    geom_point(data=Base,aes(x=pickup_longitude,y=pickup_latitude),color='blue',size=.5)
  dev.off()
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Dropoff_map.pdf')
  NYC_Map%>%
    ggmap()+
    geom_point(data=Base,aes(x=dropoff_longitude,y=dropoff_latitude),color='green',size=.5)
  dev.off()
  
  ########################
  ###Análise de cluster###
  ########################
  ######################################
  #Determinando o K otimo para a Kmeans#
  ######################################
  #df<-Base%>%select(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude)
  #df<-df[1:12500,]
  
  #T1<-as.numeric(Sys.time())
  #T2<-as.numeric(Sys.time())
  #T3<-as.numeric(Sys.time())
  #nb <- NbClust(df, distance = "euclidean", min.nc = 2,
  #              max.nc = 6, method = "kmeans")$Best.partition
  #T4<-as.numeric(Sys.time())
  
  #df<-Base%>%select(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude)%>%mutate(idice=sample(1:136,size=1695553,replace=T))
  #statmod <- function(x) 
  #{
  #  z <- table(as.vector(x)) 
  #  return(as.numeric(names(z)[z == max(z)]))
  #}
  #i<-0
  #repeat(
  #  i<-i+1
  #  df_aus<-df%>%filter(indice==i)
  #)
  
  ####################################################
  #definindo a variavel resposta para a clusterização#
  ####################################################
  
  Base<-Base%>%mutate(Flag_Tip=0)
  Base$Flag_Tip[Base$tip_amount>0]<-1
  Base$Flag_Tip_Label<-'Not Tipped'
  Base$Flag_Tip_Label[Base$Flag_Tip==1]<-'Tipped'
  
  Base$store_and_fwd_flag_Label<-'Maybe'
  Base$store_and_fwd_flag_Label[Base$store_and_fwd_flag=='Y']<-'Yes'
  Base$store_and_fwd_flag_Label[Base$store_and_fwd_flag=='N']<-'No'
  Base$store_and_fwd_flag_Label<-factor(Base$store_and_fwd_flag_Label,levels = c('Yes','Maybe','No'))
  
  #SQC<-0
  #for(i in 1:100)
  #{
  #  SQC[i]<-sum(rxKmeans(~pickup_longitude+pickup_latitude+dropoff_longitude+dropoff_latitude,data =Base,numClusters=i)$withinss)
  #}
  
  #K_df<-data.frame(K=1:100,SQC)
  #pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Cluster.pdf')
  #K_df%>%ggplot(aes(x=K,y=SQC))+
  #  geom_point()+
  #  ggtitle('Sum of within cluster sum of squares by number of clusters')+
  #  xlab('Number of cluster')+
  #  ylab('Sum of within cluster sum of squares')
  #dev.off()
  #which(SQC==min(SQC))
  
  #KM<-rxKmeans(~pickup_longitude+pickup_latitude+dropoff_longitude+dropoff_latitude,data =Base,numClusters=98)
  
  #Base<-bind_cols(Base,Cluster=KM$cluster)
  #Base<-Base%>%mutate(Cluster=factor(Cluster,levels=1:98))
  
  #Chaid_Cluster<-chaid(as.factor(Flag_Tip)~Cluster,data = Base)
  #Chaid_Cluster%>%plot
  
  #NYC_Map%>%
  #  ggmap()+
  #  geom_point(data=Base,aes(x=pickup_longitude,y=pickup_latitude,color=Cluster,shape=Cluster),size=.3)+
  #  geom_point(data=Base%>%filter(trip_distance==max(trip_distance)),aes(x=dropoff_longitude,y=dropoff_latitude),color=('green'),size=1)
  ####################
  ###Sem Cluster!!!###
  ####################
  
  #######################################################   
  #Análissando a distribuição de recebimento de gorjetas#
  #######################################################
  
  Base%>%group_by(store_and_fwd_flag)%>%summarise(Qnt=n(),Tip_Ratio=mean(Flag_Tip))%>%mutate(Freq=Qnt/sum(Qnt))
  Base%>%group_by(payment_type)%>%summarise(Qnt=n(),Tip_Ratio=mean(Flag_Tip))%>%mutate(Freq=Qnt/sum(Qnt))
  Base%>%group_by(Flag_Tip)%>%summarise(Passenger_Ratio=mean(passenger_count))
  
  Base%>%select(Flag_Tip_Label,store_and_fwd_flag_Label)%>%
    with(table(Flag_Tip_Label,store_and_fwd_flag_Label))%>%
    prop.table(1)%>%addmargins(2)
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_store_and_fwd_flag.pdf')
  Base%>%ggplot(aes(x=store_and_fwd_flag_Label))+
    geom_bar(aes(y=100*(..count../sum(..count..))))+
    facet_grid(.~Flag_Tip_Label)+
    xlab('store and fwd')+
    ylab('%')
  dev.off()
  
  
  Base%>%select(Flag_Tip_Label,payment_type)%>%
    with(table(Flag_Tip_Label,payment_type))%>%
    prop.table(1)%>%addmargins(2)
  
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_Payment.pdf')
  Base%>%ggplot(aes(x=payment_type))+
    geom_bar(aes(y=100*(..count../sum(..count..))))+
    facet_grid(.~Flag_Tip_Label)+
    xlab('Payment type')+
    ylab('%')
  dev.off()
  
  Base%>%select(Flag_Tip_Label,passenger_count)%>%
    with(table(Flag_Tip_Label,passenger_count))%>%
    prop.table(1)%>%addmargins(2)
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_passenger_count.pdf')
  Base%>%ggplot(aes(x=passenger_count))+
    geom_bar(aes(y=100*(..count../sum(..count..))))+
    scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
    facet_grid(.~Flag_Tip_Label)+
    xlab('Passenger Count')+
    ylab('%')
  dev.off()
  
  
  Base%>%select(Flag_Tip_Label,mta_tax)%>%
    with(table(Flag_Tip_Label,mta_tax))%>%
    prop.table(1)%>%addmargins(2)
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_mta_tax.pdf')
  Base%>%ggplot(aes(x=mta_tax))+
    geom_bar(aes(y=100*(..count../sum(..count..))))+
    scale_x_continuous(breaks=c(-0.5,0,0.5))+
    facet_grid(.~Flag_Tip_Label)+
    xlab('Mta Tax')+
    ylab('%')
  dev.off()
  
  Base%>%group_by(Flag_Tip_Label)%>%
    summarise(MinDistance=min(trip_distance),
              Q1distance=quantile(trip_distance,.25),
              Q2distance=quantile(trip_distance,.5),
              meanDistance=mean(trip_distance),
              Q3distance=quantile(trip_distance,.75),
              Maxdistance=max(trip_distance),
              VarDistance=var(trip_distance),
              DPDistance=sd(trip_distance),
              CVDistance=sd(trip_distance)/mean(trip_distance))%>%View
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_trip_distance_01_Boxplot.pdf')
  Base%>%ggplot(aes(x=Flag_Tip_Label,y=trip_distance_01))+
    geom_boxplot()+
    xlab('Tipped')+
    ylab('Trip distance transformed to [0,1]')+
    scale_y_continuous(limits = c(0,.0000025))
  dev.off()
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_trip_distance_01_Density.pdf')
  Base%>%ggplot(aes(x=trip_distance_01, fill=Flag_Tip_Label))+
    geom_density(alpha=.2)+
    scale_x_continuous(limits = c(0,.0000025))+
    xlab('Trip distance transformed to [0,1]')+
    ylab('Density')
  dev.off()
  
  
  Base%>%group_by(Flag_Tip_Label)%>%
    summarise(MinTime=min(trip_time_in_secs),
              Q1Time=quantile(trip_time_in_secs,.25),
              Q2Time=quantile(trip_time_in_secs,.5),
              meanTime=mean(trip_time_in_secs),
              Q3Time=quantile(trip_time_in_secs,.75),
              MaxTime=max(trip_time_in_secs),
              VarTime=var(trip_time_in_secs),
              DPTime=sd(trip_time_in_secs),
              CVTime=sd(trip_time_in_secs)/mean(trip_time_in_secs))%>%View
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_trip_time_in_secs_01_Boxplot.pdf')
  Base%>%ggplot(aes(x=Flag_Tip_Label,y=trip_time_in_secs_01))+
    geom_boxplot()+
    xlab('Tipped')+
    ylab('Trip time in secs transformed to [0,1]')+
    scale_y_continuous(limits = c(0,.00045))
  dev.off()
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_trip_time_in_secs_01_Density.pdf')
  Base%>%ggplot(aes(x=trip_time_in_secs_01,fill=Flag_Tip_Label))+
    geom_density(alpha=.2)+
    ylab('Density')+
    xlab('Trip time in secs transformed to [0,1]')+
    scale_x_continuous(limits = c(0,.00045))
  dev.off()
  
  
  Base%>%group_by(Flag_Tip_Label)%>%
    summarise(MinFare=min(fare_amount),
              Q1Fare=quantile(fare_amount,.25),
              Q2Fare=quantile(fare_amount,.5),
              meanFare=mean(fare_amount),
              Q3Fare=quantile(fare_amount,.75),
              MaxFare=max(fare_amount),
              VarFare=var(fare_amount),
              DPFare=sd(fare_amount),
              CVFare=sd(fare_amount)/mean(fare_amount))%>%View
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_fare_amount_Boxplot.pdf')
  Base%>%ggplot(aes(x=Flag_Tip_Label,y=fare_amount))+
    geom_boxplot()+
    xlab('Tipped')+
    ylab('Fare Amount')+
    scale_y_continuous(limits = c(0,100))
  dev.off()
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_fare_amount_Density.pdf')
  Base%>%ggplot(aes(x=fare_amount,fill=Flag_Tip_Label))+
    geom_density(alpha=.2)+
    ylab('Density')+
    xlab('Fare Amount')+
    scale_x_continuous(limits = c(0,100))
  dev.off()
  
  Base%>%group_by(Flag_Tip_Label)%>%
    summarise(MinSurcharge=min(surcharge),
              Q1Surcharge=quantile(surcharge,.25),
              Q2Surcharge=quantile(surcharge,.5),
              meanSurcharge=mean(surcharge),
              Q3Surcharge=quantile(surcharge,.75),
              MaxSurcharge=max(surcharge),
              VarSurcharge=var(surcharge),
              DPSurcharge=sd(surcharge),
              CVSurcharge=sd(surcharge)/mean(surcharge))%>%View
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_surcharge_Boxplot.pdf')
  Base%>%ggplot(aes(x=Flag_Tip_Label,y=surcharge))+
    geom_boxplot()+
    xlab('Tipped')+
    ylab('Surcharge')+
    scale_y_continuous(limits = c(0,1.5))
  dev.off()
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_surcharge_Boxplot.pdf')
  Base%>%ggplot(aes(x=surcharge,fill=Flag_Tip_Label))+
    geom_density(alpha=.2)+
    ylab('Density')+
    xlab('Surcharge')+
    scale_x_continuous(limits = c(0,1.5))
  dev.off()
  
  Base%>%group_by(Flag_Tip_Label)%>%
    summarise(MinToll=min(tolls_amount),
              Q1Toll=quantile(tolls_amount,.25),
              Q2Toll=quantile(tolls_amount,.5),
              meanToll=mean(tolls_amount),
              Q3Toll=quantile(tolls_amount,.75),
              MaxToll=max(tolls_amount),
              VarToll=var(tolls_amount),
              DPToll=sd(tolls_amount),
              CVToll=sd(tolls_amount)/mean(tolls_amount))%>%View
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_tolls_amount.pdf')
  Base%>%ggplot(aes(y=tolls_amount,x=Flag_Tip_Label))+
    geom_point()+
    ylab('Tolls Amount')+
    xlab('Tipped')
  dev.off()
  
  Base%>%group_by(Flag_Tip_Label)%>%
    summarise(MinTotal=min(total_amount),
              Q1Total=quantile(total_amount,.25),
              Q2Total=quantile(total_amount,.5),
              meanTotal=mean(total_amount),
              Q3Total=quantile(total_amount,.75),
              MaxTotal=max(total_amount),
              VarTotal=var(total_amount),
              DPTotal=sd(total_amount),
              CVTotal=sd(total_amount)/mean(total_amount))%>%View
  
  pdf('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/tipp_tolls_amount.pdf')
  Base%>%ggplot(aes(y=total_amount,x=Flag_Tip_Label))+
    geom_point()+
    ylab('Total Amount')+
    xlab('Tipped')
  dev.off()
  
  #######################
  ###Gráficos em lotes###
  #######################
  
  n<-nrow(Base)
  NLinhas<-5000
  NLotes<-round(n/NLinhas,0)
  
  set.seed(123)
  Base<-Base%>%mutate(Lote=sample(1:340,n,replace=T))
  #Lotes<-1:340
  #i<-0
  #repeat
  #{
  #  i<-i+1
  #  Base_Lote<-Base%>%filter(Lote==i)
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','Pickup_map_Chunk_',i,'.pdf',sep=''))
  #  p<-NYC_Map%>%ggmap()+
  #    geom_point(data=Base_Lote,aes(x=pickup_longitude,y=pickup_latitude),color='blue',size=.5)
  #  print(p)
  #  dev.off()
  #  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','dropoff_map_Chunk_',i,'.pdf',sep=''))
  #  p<-NYC_Map%>%
  #    ggmap()+
  #    geom_point(data=Base_Lote,aes(x=dropoff_longitude,y=dropoff_latitude),color='green',size=.5)
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_store_and_fwd_flag_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=store_and_fwd_flag_Label))+
  #        geom_bar(aes(y=100*(..count../sum(..count..))))+
  #        facet_grid(.~Flag_Tip_Label)+
  #        xlab('store and fwd')+
  #        ylab('%')
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_Payment_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=payment_type))+
  #        geom_bar(aes(y=100*(..count../sum(..count..))))+
  #        facet_grid(.~Flag_Tip_Label)+
  #        xlab('Payment type')+
  #        ylab('%')
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_passenger_count_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=passenger_count))+
  #        geom_bar(aes(y=100*(..count../sum(..count..))))+
  #        scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  #        facet_grid(.~Flag_Tip_Label)+
  #        xlab('Passenger Count')+
  #        ylab('%')
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_mta_tax_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=mta_tax))+
  #        geom_bar(aes(y=100*(..count../sum(..count..))))+
  #        scale_x_continuous(breaks=c(-0.5,0,0.5))+
  #        facet_grid(.~Flag_Tip_Label)+
  #        xlab('Mta Tax')+
  #        ylab('%')
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_trip_distance_01_Boxplot_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=Flag_Tip_Label,y=trip_distance_01))+
  #        geom_boxplot()+
  #        xlab('Tipped')+
  #        ylab('Trip distance transformed to [0,1]')+
  #        scale_y_continuous(limits = c(0,.0000025))
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_trip_distance_01_Density_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=trip_distance_01, fill=Flag_Tip_Label))+
  #        geom_density(alpha=.2)+
  #        scale_x_continuous(limits = c(0,.0000025))+
  #        xlab('Trip distance transformed to [0,1]')+
  #        ylab('Density')
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_trip_time_in_secs_01_Boxplot_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=Flag_Tip_Label,y=trip_time_in_secs_01))+
  #        geom_boxplot()+
  #        xlab('Tipped')+
  #        ylab('Trip time in secs transformed to [0,1]')+
  #        scale_y_continuous(limits = c(0,.00045))
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_trip_time_in_secs_01_Density_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=trip_time_in_secs_01,fill=Flag_Tip_Label))+
  #        geom_density(alpha=.2)+
  #        ylab('Density')+
  #        xlab('Trip time in secs transformed to [0,1]')+
  #        scale_x_continuous(limits = c(0,.00045))
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_fare_amount_Boxplot_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=Flag_Tip_Label,y=fare_amount))+
  #        geom_boxplot()+
  #        xlab('Tipped')+
  #        ylab('Fare Amount')+
  #        scale_y_continuous(limits = c(0,100))
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_fare_amount_Density_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=fare_amount,fill=Flag_Tip_Label))+
  #        geom_density(alpha=.2)+
  #        ylab('Density')+
  #        xlab('Fare Amount')+
  #        scale_x_continuous(limits = c(0,100))
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_surcharge_Boxplot_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=Flag_Tip_Label,y=surcharge))+
  #        geom_boxplot()+
  #        xlab('Tipped')+
  #        ylab('Surcharge')+
  #        scale_y_continuous(limits = c(0,1.5))
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_surcharge_Boxplot_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(x=surcharge,fill=Flag_Tip_Label))+
  #        geom_density(alpha=.2)+
  #        ylab('Density')+
  #        xlab('Surcharge')+
  #        scale_x_continuous(limits = c(0,1.5))
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_tolls_amount_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(y=tolls_amount,x=Flag_Tip_Label))+
  #        geom_point()+
  #        ylab('Tolls Amount')+
  #        xlab('Tipped')
  #  print(p)
  #  dev.off()
  
  #  pdf(paste('D:/Disco\ C/Documentos/r/Aprendendo\ MicrosoftML/Gráficos/Split/','tipp_total_amount_Chunk_',i,'.pdf',sep=''))
  #  p<-Base_Lote%>%ggplot(aes(y=total_amount,x=Flag_Tip_Label))+
  #        geom_point()+
  #        ylab('Total Amount')+
  #        xlab('Tipped')
  #  print(p)
  #  dev.off()
  #if(i==340) break()
  #}
  ################################################
  ###Calculando a correlação enter as variaveis###
  ################################################
  
  Base%>%with(cor(Flag_Tip,total_amount))
  Base%>%with(cor.test(Flag_Tip,total_amount)$p.value)
  Base%>%with(cor.test(Flag_Tip,rate_code)$p.value)
  Base%>%with(cor.test(Flag_Tip,store_and_fwd_flag,method = "kendall")$p.value)
  ########################################
  ###Modelagem seguindo o post original###
  ########################################
  
  set.seed(2345, "L'Ecuyer-CMRG")
  # Randomly split the data 75-25 between train and test sets.
  dataProb <- c(Train = 0.75, Test = 0.25)
  dataSplit <-
    rxSplit(Base,
            splitByFactor = "splitVar",
            transforms = list(splitVar =
                                sample(dataFactor,
                                       size = .rxNumRows,
                                       replace = TRUE,
                                       prob = dataProb)),
            transformObjects =
              list(dataProb = dataProb,
                   dataFactor = factor(names(dataProb),
                                       levels = names(dataProb))),
            outFilesBase = tempfile())
  
  # Name the train and test datasets.
  dataTrain <- dataSplit[[1]]
  dataTest <- dataSplit[[2]]
  rxSummary(~ Flag_Tip, dataTrain)$sDataFrame
  rxSummary(~ Flag_Tip, dataTest)$sDataFrame
model <- formula(paste("Flag_Tip ~ passenger_count + trip_time_in_secs + trip_distance + total_amount"))
rxLogisticRegressionFit <- rxLogisticRegression(model, data = dataTrain)
rxFastLinearFit <- rxFastLinear(model, data = dataTrain)
rxFastTreesFit <- rxFastTrees(model, data = dataTrain)
rxFastForestFit <- rxFastForest(model, data = dataTrain)
rxNeuralNetFit <- rxNeuralNet(model, data = dataTrain)                
fitScores <- rxPredict(rxLogisticRegressionFit, dataTest, suffix = ".rxLogisticRegression",
                       extraVarsToWrite = names(dataTest),
                       outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxFastLinearFit, fitScores, suffix = ".rxFastLinear",
                       extraVarsToWrite = names(fitScores),
                       outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxFastTreesFit, fitScores, suffix = ".rxFastTrees",
                       extraVarsToWrite = names(fitScores),
                       outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxFastForestFit, fitScores, suffix = ".rxFastForest",
                       extraVarsToWrite = names(fitScores),
                       outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxNeuralNetFit, fitScores, suffix = ".rxNeuralNet",
                       extraVarsToWrite = names(fitScores),
                       outData = tempfile(fileext = ".xdf"))

# Compute the fit models's ROC curves.
fitRoc <- rxRoc("Flag_Tip", grep("Probability.", names(fitScores), value = T), fitScores)
# Plot the ROC curves and report their AUCs.
plot(fitRoc)

# Create a named list of the fit models.
fitList <-
  list(rxLogisticRegression = rxLogisticRegressionFit,
       rxFastLinear = rxFastLinearFit,
       rxFastTrees = rxFastTreesFit,
       rxFastForest = rxFastForestFit,
       rxNeuralNet = rxNeuralNetFit)

# Compute the fit models's AUCs.
fitAuc <- rxAuc(fitRoc)
names(fitAuc) <- substring(names(fitAuc), nchar("Probability.") + 1)

# Find the name of the fit with the largest AUC.
bestFitName <- names(which.max(fitAuc))

# Select the fit model with the largest AUC.
bestFit <- fitList[[bestFitName]]

# Report the fit AUCs.
cat("Fit model AUCs:\n")
print(fitAuc, digits = 2)

# Report the best fit.
cat(paste0("Best fit model with ", bestFitName,
           ", AUC = ", signif(fitAuc[[bestFitName]], digits = 2),
           ".\n"))
