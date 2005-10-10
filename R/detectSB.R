## Spatial bias detection on arrays

## Copyright (C) 2003 Institut Curie
## Author(s): Philippe Hupé (Institut Curie) 2003
## Contact: manor@curie.fr


detectSB <- function(...){
  UseMethod("detectSB")
}

detectSB.arrayCGH <- function(arrayCGH, variable, proportionup=0.25, proportiondown, type="up", thresholdup=0.2, thresholddown=0.2, ...)
{
    if(length(which(names(arrayCGH$arrayValues)==variable))<1)stop(paste("variable",variable,"not found",sep=" "))
    if(variable=="ZoneNem")stop("ZoneNem is not allowed : choose another variable")
    if(length(which(names(arrayCGH$arrayValues)=="ZoneNem"))<1)stop(paste("run nem function to compute the spatial classification",sep=" "))
	
  zone <- arrayCGH$arrayValues$ZoneNem
  values <- arrayCGH$arrayValues[[variable]]

  ZoneLabel <- unique(zone)
  nbzone <- length(unique(zone)) #number of zones on the array
  mu <- rep(NULL,nbzone) #mean of values by zone
  effectif <- rep(NULL,nbzone) #effectif by  zone
  zone.number <- rep(NULL,nbzone) #zone number
  zone.flagged <- rep(0,length(zone)) #flagged zone indicated by 1 (i.e. biased zone)

  ###################################
  #initialization
  i <- 1
  
  #mean by zone computation
  for (i in 1:nbzone)
    {
      index <- which(zone==ZoneLabel[i])
      values.zone <- values[index]
      mu[i] <- mean(na.omit(values.zone))
      effectif[i] <- length(na.omit(values.zone))
      zone.number[i] <- ZoneLabel[i]
    }

    tab <- data.frame(zone.number, mu, effectif)

    ### Il se peut qu'il y ait des effectifs à 0
    ### par exemple dans le cas d'une classe à 1 élément
    ### et dont le log-ratio a une valeur manquante

    ind <- which(tab$effectif==0)
    if (length(ind)>0)
      {
        tab <- tab[-ind,]
        nbzone <- nbzone-length(ind)
      }

    
  #tab contains caracteristics of each zone
  #tab <- data.frame(zone.number, mu, effectif)
  #tab <- tab[sort.list(tab$mu, decreasing=T),]
  #effectif.cumul <- cumsum(tab$effectif)
  #frequency.cumul <- effectif.cumul/effectif.cumul[nbzone]
  #tab <- data.frame(tab, effectif.cumul, frequency.cumul)


  #unbiased zone
  #unbiased.zone <- tab[which(tab$frequency.cumul>proportion),]
  #unbiased.zone.mean <- sum(unbiased.zone$effectif*unbiased.zone$mu)/sum(unbiased.zone$effectif)
  #unbiased.zone <- unbiased.zone[sort.list(unbiased.zone$mu, decreasing=T),]
  #print(paste("mean of unbiased zone : ", unbiased.zone.mean))

  #putative biased zone
  #biased.zone <- tab[which(tab$frequency.cumul<=proportion),]
  #biased.zone <- biased.zone[sort.list(biased.zone$mu, decreasing=T),]

 if (type=="up")
 { 

  #tab contains caracteristics of each zone
#  tab <- data.frame(zone.number, mu, effectif)
  tab <- tab[sort.list(tab$mu, decreasing=TRUE),]
  effectif.cumul <- cumsum(tab$effectif)
  frequency.cumul <- effectif.cumul/effectif.cumul[nbzone]
  tab <- data.frame(tab, effectif.cumul, frequency.cumul)

  #unbiased zone
  unbiased.zone <- tab[which(tab$frequency.cumul>proportionup),]
  unbiased.zone.mean <- sum(unbiased.zone$effectif*unbiased.zone$mu)/sum(unbiased.zone$effectif)
  unbiased.zone <- unbiased.zone[sort.list(unbiased.zone$mu, decreasing=TRUE),]
  print(paste("mean of unbiased zone : ", unbiased.zone.mean))

  #putative biased zone
  biased.zone <- tab[which(tab$frequency.cumul<=proportionup),]
  biased.zone <- biased.zone[sort.list(biased.zone$mu, decreasing=TRUE),]



  if(length(which(biased.zone$mu>=(unbiased.zone.mean+thresholdup)))!=0)
    {
      print("Spatial bias has been detected")
      if(nbzone-length(unbiased.zone$zone.number)==1)
        {
          zone.flagged[which(zone==tab$zone.number[1])] <- 1 #biased zone are coded  by 1
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- 1 #only the first zone is biased
        }

      else
        {
          
          #number of putative bias zones
          number.bias.zone <- length(biased.zone$zone.number)
          
          #boolean variable which indicate if a zone is biased (1) or not (0)
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- 1 #highest zone is biased

          #initialization
          i <- 1
          
          for (i in 2:number.bias.zone)
            {
              if(tab$mu[i]>=(unbiased.zone.mean+thresholdup))
                {
                  biased.zone[i] <- 1
                }

              else
                {
                  if((tab$mu[i]>(tab$mu[i-1]+unbiased.zone.mean)/2)&biased.zone[i-1]!=0)
                    {
                      biased.zone[i] <- 1
                    }
                  
                }
            }

                 

          #initialization
          i <- 1
  
          for (i in 1:nbzone)
            {
              zone.flagged[which(zone==tab$zone.number[i])] <- biased.zone[i]
            }
        }
    }

  else
    {
      print("There is no spatial bias")
      biased.zone <- rep(0,nbzone)
    }

  tab <- data.frame(tab, biased.zone)

  
  print(tab)

  arrayCGH$arrayValues$SB <- zone.flagged
  
  return(arrayCGH)
 }
  

 if (type=="down")
 { 
   

  #tab contains caracteristics of each zone
#  tab <- data.frame(zone.number, mu, effectif)
  tab <- tab[sort.list(tab$mu, decreasing=F),]
  effectif.cumul <- cumsum(tab$effectif)
  frequency.cumul <- effectif.cumul/effectif.cumul[nbzone]
  tab <- data.frame(tab, effectif.cumul, frequency.cumul)

  #unbiased zone
  unbiased.zone <- tab[which(tab$frequency.cumul>proportiondown),]
  unbiased.zone.mean <- -sum(unbiased.zone$effectif*unbiased.zone$mu)/sum(unbiased.zone$effectif)
  unbiased.zone <- unbiased.zone[sort.list(unbiased.zone$mu, decreasing=TRUE),]
  print(paste("mean of unbiased zone : ", unbiased.zone.mean))

  #putative biased zone
  biased.zone <- tab[which(tab$frequency.cumul<=proportiondown),]
  biased.zone <- biased.zone[sort.list(biased.zone$mu, decreasing=F),]



   biased.zone$mu <- -biased.zone$mu
   tab$mu <- -tab$mu
   if(length(which(biased.zone$mu>=(unbiased.zone.mean+thresholddown)))!=0)
    {
      print("Spatial bias has been detected")
      if(nbzone-length(unbiased.zone$zone.number)==1)
        {
          zone.flagged[which(zone==tab$zone.number[1])] <- -1 #biased zone are coded  by -1
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- -1 #only the first zone is biased
        }

      else
        {
          
          #number of putative bias zones
          number.bias.zone <- length(biased.zone$zone.number)
          
          #boolean variable which indicate if a zone is biased (1) or not (0)
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- -1 #highest zone is biased

          #initialization
          i <- 1
          
          for (i in 2:number.bias.zone)
            {
              if(tab$mu[i]>=(unbiased.zone.mean+thresholddown))
                {
                  biased.zone[i] <- -1
                }

              else
                {
                  if((tab$mu[i]>(tab$mu[i-1]+unbiased.zone.mean)/2)&biased.zone[i-1]!=0)
                    {
                      biased.zone[i] <- -1
                    }
                  
                }
            }

                 

          #initialization
          i <- 1
  
          for (i in 1:nbzone)
            {
              zone.flagged[which(zone==tab$zone.number[i])] <- biased.zone[i]
            }
        }
    }

  else
    {
      print("There is no spatial bias")
      biased.zone <- rep(0,nbzone)
    }

  tab <- data.frame(tab, biased.zone)

  
  print(tab)

  arrayCGH$arrayValues$SB <- zone.flagged
  
  return(arrayCGH)
 }



 if (type=="upanddown")
 { 

  #tab contains caracteristics of each zone
  #tab <- data.frame(zone.number, mu, effectif)
  tab <- tab[sort.list(tab$mu, decreasing=TRUE),]
  effectif.cumul <- cumsum(tab$effectif)
  frequency.cumul <- effectif.cumul/effectif.cumul[nbzone]
  tab <- data.frame(tab, effectif.cumul, frequency.cumul)

  #unbiased zone
  unbiased.zone <- tab[which(tab$frequency.cumul>proportionup&tab$frequency.cumul<(1-proportiondown)),]
  unbiased.zone.mean <- sum(unbiased.zone$effectif*unbiased.zone$mu)/sum(unbiased.zone$effectif)
  unbiased.zone <- unbiased.zone[sort.list(unbiased.zone$mu, decreasing=TRUE),]
  print(paste("mean of unbiased zone : ", unbiased.zone.mean))

  #putative biased zone
  biased.zone <- tab[which(tab$frequency.cumul<=proportionup),]
  biased.zone <- biased.zone[sort.list(biased.zone$mu, decreasing=TRUE),]


  # detection of "up" biased zone
  if(length(which(biased.zone$mu>=(unbiased.zone.mean+thresholdup)))!=0)
    {
      print("Spatial bias has been detected")
      if(nbzone-length(unbiased.zone$zone.number)==1)
        {
          zone.flagged[which(zone==tab$zone.number[1])] <- 1 #biased zone are coded  by 1
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- 1 #only the first zone is biased
        }

      else
        {
          
          #number of putative bias zones
          number.bias.zone <- length(biased.zone$zone.number)
          
          #boolean variable which indicate if a zone is biased (1) or not (0)
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- 1 #highest zone is biased

          #initialization
          i <- 1
          
          for (i in 2:number.bias.zone)
            {
              if(tab$mu[i]>=(unbiased.zone.mean+thresholdup))
                {
                  biased.zone[i] <- 1
                }

              else
                {
                  if((tab$mu[i]>(tab$mu[i-1]+unbiased.zone.mean)/2)&biased.zone[i-1]!=0)
                    {
                      biased.zone[i] <- 1
                    }
                  
                }
            }

                 

          #initialization
          i <- 1
  
          for (i in 1:nbzone)
            {
              zone.flagged[which(zone==tab$zone.number[i])] <- biased.zone[i]
            }
        }
    }

  else
    {
      print("There is no spatial bias")
      biased.zone <- rep(0,nbzone)
    }

  tab <- data.frame(tab, biased.zone)

  
  print(tab)

  arrayCGH$arrayValues$SB <- zone.flagged

  # detection of "down" biased zone
  
  #tab contains caracteristics of each zone
  tab <- data.frame(zone.number, mu, effectif)
  tab <- tab[sort.list(tab$mu, decreasing=F),]
  effectif.cumul <- cumsum(tab$effectif)
  frequency.cumul <- effectif.cumul/effectif.cumul[nbzone]
  tab <- data.frame(tab, effectif.cumul, frequency.cumul)

  #unbiased zone
  #unbiased.zone <- tab[which(tab$frequency.cumul>proportiondown),]
  unbiased.zone.mean <- -unbiased.zone.mean
  unbiased.zone <- unbiased.zone[sort.list(unbiased.zone$mu, decreasing=TRUE),]
  print(paste("mean of unbiased zone : ", unbiased.zone.mean))

  #putative biased zone
  biased.zone <- tab[which(tab$frequency.cumul<=proportiondown),]
  biased.zone <- biased.zone[sort.list(biased.zone$mu, decreasing=F),]



   biased.zone$mu <- -biased.zone$mu
   tab$mu <- -tab$mu
   if(length(which(biased.zone$mu>=(unbiased.zone.mean+thresholddown)))!=0)
    {
      print("Spatial bias has been detected")
      if(nbzone-length(unbiased.zone$zone.number)==1)
        {
          zone.flagged[which(zone==tab$zone.number[1])] <- -1 #biased zone are coded  by -1
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- -1 #only the first zone is biased
        }

      else
        {
          
          #number of putative bias zones
          number.bias.zone <- length(biased.zone$zone.number)
          
          #boolean variable which indicate if a zone is biased (-1) or not (0)
          biased.zone <- rep(0,nbzone)
          biased.zone[1] <- -1 #highest zone is biased

          #initialization
          i <- 1
          
          for (i in 2:number.bias.zone)
            {
              if(tab$mu[i]>=(unbiased.zone.mean+thresholddown))
                {
                  biased.zone[i] <- -1
                }

              else
                {
                  if((tab$mu[i]>(tab$mu[i-1]+unbiased.zone.mean)/2)&biased.zone[i-1]!=0)
                    {
                      biased.zone[i] <- -1
                    }
                  
                }
            }

                 

          #initialization
          i <- 1
  
          for (i in 1:nbzone)
            {
              if (biased.zone[i]!=0)
              {
                zone.flagged[which(zone==tab$zone.number[i])] <- biased.zone[i]
              }
            }
        }
    }

  else
    {
      print("There is no spatial bias")
      biased.zone <- rep(0,nbzone)
    }

  tab <- data.frame(tab, biased.zone)

  
  print(tab)

  arrayCGH$arrayValues$SB <- zone.flagged

  return(arrayCGH)

 
 }
  



}
