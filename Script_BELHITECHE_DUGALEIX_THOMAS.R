
#******************************************************************************#
#****************************** Initialisation ********************************#
#* Chargement des librairies :
library(randomForest)
library(class)
library(ggplot2)

#* Changement du dossier de travail :
setwd('/TP_BELHITECHE_DUGALEIX_THOMAS_Projet4/')


#* Chargement des donnees :
load(file = "./data/bacteria-dataset.Rdata")

#* Visualisation des matrices :
View(X)
View(Y)
#******************************************************************************#




#******************************************************************************#
#****************** Permet d'afficher la figure de l'enonce *******************#
# en rouge les individus de type 1 en vert ceux de type 0 

plot(X[1,], col = "green", type = "l", ylim = c(0.02,0.1),
     main="Classification de \ndeux types de bacteries",
     xlab="Canal spectral",
     ylab="Intensité du signal",xlim=c(0,240))

legend(160, 0.04, legend=c("Categorie 0", "Categorie 1"),
       col=c("green", "red"),
       lty=c(1,1), cex=0.6)

for (i in 2:nrow(X)) {
  if (Y[i] == "0") { points(X[i,], col = "green", type = "l",) } 
  else { points(X[i,], col = "red", type = "l") }
}
#******************************************************************************#


#******************************************************************************#
#*********Creation de 10 folds pour la prodedure de validation croise**********#

id <- sample(1:10, nrow(X), replace =TRUE) 
#creer un vecteur de longueur 2403 (nombre de ligne de X) 
#contenant les des nombre entre 1 et 10

x <- cbind(X, id) 
#ajoute sur les id de chaque fold sur la derniere colonne de x

x[,ncol(x)] #contiens les id de chaque fold (1 id par ligne place aleatoirement)
#******************************************************************************#




#******************************************************************************#
#******************************************************************************#
#************ Fonction d'apprentissage du model RandomForest  *****************#

# initialisation des variables utile a l'algo randomForest :
testErrRF <- array() 
testSensiRF <- array()
testSpeciRF <- array()
errorRF <- array()
sensiRF <- array()
speciRF <- array()

# valeures prises par le parametre ntree :
trees = c(25,50,75,100,250,375,500,750,1000) 
#indice du tableau de stockage des valeures 1:length(trees) :
m <- 1

#************** Algorithme principal d'apprentissage du model ****************#

for (n in trees){ # pour chaque valeure du parametre ntree (du model RF)
  print("nb arbre :") 
  print(n) #debug

    for (j in 1:10) { # pour chaque fold
      print("nb fold") 
      print( j) #debug
      
      #*************** Reinitialisation pour chaque fold ***************#
      train <- array() 
      trainY <- array()
      # on reinitialise les tableaux contenant les valeures d'entrainement 
      test <- array()
      testY <- array()
      # on reinitialise les tableaux contenant les valeures de test
      

      # permet d'assigner chaque ligne de X et Y a la matrice correspondante :
      for (i in 1:nrow(x)) { #pour chaque ligne de notre matrice
        if(x[i,ncol(x)] == j) { #si la colonne de l'id de la fold est egal a j
          test <- rbind(test, x[i,-ncol(x)]) 
          #on place la ligne de x dans la matrice test
          testY <- rbind(testY, Y[i])
          #on place la ligne de Y dans la matrice testY
        }
        else { #sinon
          train <- rbind(train, x[i,-ncol(x)])
          #on place la ligne de x dans la matrice train
          trainY <- rbind(trainY, Y[i])
          #on place la ligne de Y dans la matrice trainY
        }
      }

      # enleve la colonne contenant l'id :
      train <- train[-1,]
      trainY <- trainY[-1,]
      test <- test[-1,]
      testY <- testY[-1,]
      #*************************************************************#
      
      #*********************************************************#
      #****************** Model random Forest : ****************#
      model = randomForest(x = train, y=as.factor(trainY), ntree=n) 
      #entrainement du model sur les matrice d'entrainement en fonctiond de n
      pred = predict(model, newdata = test) 
      #prediction des valeures correspondante a la matrice de test
      
      
      #************ Matrice de confusion ****************#
      #* Initialisation :
      confusion = matrix(c(0,0,0,0),nrow = 2) 
      
      #* Remplissage :
      for(k in 1:length(pred)){ #pour chaques valeures predites
        if(pred[k]==testY[k]){ #si la valeure predite est egale la valeure reel
          if(pred[k]==0){ # et que cette valeure est egal a 0
            confusion[1,1] <- confusion[1,1] + 1 
          }else{ #sinon 
            confusion[2,2] <- confusion[2,2] + 1
          }
        }else{ #si la valeure predite est differente de la valeure reel
          if(pred[k]==0){ # et que cette valeure est egal a 0
            confusion[1,2] <- confusion[1,2] + 1
          }else{ #sinon
            confusion[2,1] <- confusion[2,1] + 1
          }
        }
      }
      
      
      #ainsi, pour chaque repetition du  model sur la fold j:
      testErrRF[j] <- 1 - ((confusion[1,1] + confusion[2,2]) / nrow(test)) 
      #on calcule le taux de mauvaise classification du model
      testSensiRF[j] <- confusion[1,1] / (confusion[1,1] + confusion[1,2]) 
      #on calcule la sensibilite du model
      testSpeciRF[j] <- confusion[2,2] / (confusion[2,2] + confusion[2,1]) 
      #on calcule la spécificite du model
      
      #***********************************************#
      
    }
  
  errorRF[m] <- mean(testErrRF) #on recupere la moyenne des taux de mauvaise 
  # classification de nos 10 modeles pour un ntree donne
  sensiRF[m] <- mean(testSensiRF) 
  #idem pour la sensibilite
  speciRF[m] <- mean(testSpeciRF) 
  #et pour la specificite
  
  m <- m+1
}

errorRF # affiche les moyennes des erreures pour chaque valeure de ntree
sensiRF # affiche les moyennes des sensibilites pour chaque valeure de ntree
speciRF # affiche les moyennes des specificites pour chaque valeure de ntree








#******************************************************************************#
#******************************************************************************#
#************** Fonction d'apprentissage du model Knn  ************************#

# initialisation des variables utile a l'algo Kppv :
testErrKnn <- array() 
testSensiKnn <- array()
testSpeciKnn <- array()
errorKnn <- array()
sensiKnn <- array()
speciKnn <- array()
nnn = 1:9

for (w in nnn) { #pour chaque valeure du parametre k (du model kppv){
  print("valeur de k :") 
  print( w) #debug
  for (j in 1:10) { # pour chaque fold
    print("nb fold") 
    print( j) #debug
    
    #*************** Reinitialisation pour chaque fold ***************#
    trainKnn <- array() 
    trainYKnn <- array()
    # on reinitialise les tableaux contenant les valeures d'entrainement 
    testKnn <- array()
    testYKnn <- array()
    # on reinitialise les tableaux contenant les valeures de test
    
    
    # permet d'assigner chaque ligne de X et Y a la matrice correspondante :
    for (i in 1:nrow(x)) { #pour chaque ligne de notre matrice
      if(x[i,ncol(x)] == j) { #si la colonne de l'id de la fold est egal a j
        testKnn <- rbind(testKnn, x[i,-ncol(x)]) 
        #on place la ligne de x dans la matrice test
        testYKnn <- rbind(testYKnn, Y[i])
        #on place la ligne de Y dans la matrice testY
      }
      else { #sinon
        trainKnn <- rbind(trainKnn, x[i,-ncol(x)])
        #on place la ligne de x dans la matrice train
        trainYKnn <- rbind(trainYKnn, Y[i])
        #on place la ligne de Y dans la matrice trainY
      }
    }
    
    # nettoie les donnees :
    # enleve la colonne contenant l'id *#
    trainKnn <- trainKnn[-1,]
    trainYKnn <- trainYKnn[-1,]
    testKnn <- testKnn[-1,]
    testYKnn <- testYKnn[-1,]
    #*************************************************************#
    
    
    #*************************************************************#
    #****************** Model K-nearest neighbor: ****************#
    modelKnn = knn(train = trainKnn, k = w, cl = trainYKnn, test = testKnn)
    
    
    #************ Matrice de confusion ****************#
    #* Initialisation :
    confusionKnn = matrix(c(0,0,0,0),nrow = 2) 
    
    #* Remplissage :
    for(k in 1:length(modelKnn)){ #pour chaques valeures predites
      if(modelKnn[k]==testYKnn[k]){ #si valeure predite egale valeure reel
        if(modelKnn[k]==0){ # et que cette valeure est egal a 0
          confusionKnn[1,1] <- confusionKnn[1,1] + 1 
        }else{ #sinon 
          confusionKnn[2,2] <- confusionKnn[2,2] + 1
        }
      }else{ #si la valeure predite est differente de la valeure reel
        if(modelKnn[k]==0){ # et que cette valeure est egal a 0
          confusionKnn[1,2] <- confusionKnn[1,2] + 1
        }else{ #sinon
          confusionKnn[2,1] <- confusionKnn[2,1] + 1
        }
      }
    }
    
    #ainsi, pour chaque repetition du  model sur la fold j:
    testErrKnn[j]<-1-((confusionKnn[1,1]+confusionKnn[2,2])/nrow(testKnn))
    #on calcule le taux de mauvaise classification du model
    testSensiKnn[j]<-confusionKnn[1,1]/(confusionKnn[1,1]+confusionKnn[1,2])
    #on calcule la sensibilite du model
    testSpeciKnn[j]<-confusionKnn[2,2]/(confusionKnn[2,2]+confusionKnn[2,1])
    #on calcule la spécificite du model
    #***********************************************#
    
  }
  
  
  errorKnn[w] <- mean(testErrKnn) #on recupere la moyenne des taux de mauvaise 
  # classification de nos 10 modeles pour une valeure de k
  sensiKnn[w] <- mean(testSensiKnn) 
  #idem pour la sensibilité
  speciKnn[w] <- mean(testSpeciKnn) 
  #et pour la spécificité
  
}

errorKnn # affiche les moyennes des erreures pour chaque valeure de K
sensiKnn # affiche les moyennes des sensibilites pour chaque valeure de K
speciKnn # affiche les moyennes des specificites pour chaque valeure de K


#******************************************************************************#
#********************** AFFICHAGE ET COMPARAISON ******************************#
#******************************************************************************#



#********************* Avec ggplot2 : RANDOM FOREST ***************************#

dfRF = cbind (errorRF,sensiRF,speciRF,trees)
dfRF = data.frame(dfRF)

#******************************************************************************#
#Taux d'erreur
ploterrorRF = ggplot(data = dfRF, aes(x = trees))
ploterrorRF + 
  geom_line(aes(x = trees, y = errorRF),color = "darkblue") + 
  ggtitle("Taux moyen d'erreur de classification\nen fonction du nombre d'arbres - Random Forest") +
  xlab("Nombre d'arbres") + ylab("Taux moyen d'erreur\n(via validation croisée sur 10 folds")

ploterrorRF + geom_line(aes(x = trees, y = errorRF),color = "darkblue") + 
  ggtitle("Taux moyen d'erreur de classification\nen fonction du nombre d'arbres - Random Forest") +
  xlab("Nombre d'arbres") + ylab("Taux moyen d'erreur\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept = c(25, 50, 100, 250, 500, 1000), linetype="dotted", color = "black", size=0.5)

#******************************************************************************#
#Sensibilit�
plotsensiRF  = ggplot(data = dfRF, aes(x = trees))
plotsensiRF + geom_line(aes(x=trees, y = sensiRF),color = "darkgreen") + 
  ggtitle("Sensibilité moyenne\nen fonction du nombre d'arbres - Random Forest") +
  xlab("Nombre d'arbres") + ylab("Sensibilité moyenne\n(via validation croisée sur 10 folds")

plotsensiRF + 
  geom_line(aes(x=trees, y = sensiRF),color = "darkgreen") + 
  ggtitle("Sensibilité moyenne\nen fonction du nombre d'arbres - Random Forest") +
  xlab("Nombre d'arbres") + ylab("Sensibilité moyenne\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept = c(25, 50, 100, 250, 500, 1000), linetype="dotted", color = "black", size=0.5)

#******************************************************************************#
#Sp�cificit�
plotspeciRF  = ggplot(data = dfRF, aes(x = trees))
plotspeciRF + 
  geom_line(aes(x = trees,y = speciRF),color = "darkred") + 
  ggtitle("Specificité moyenne\nen fonction du nombre d'arbres - Random Forest") +
  xlab("Nombre d'arbres") + ylab("Spécificité moyenne\n(via validation croisée sur 10 folds")

plotspeciRF + 
  geom_line(aes(x = trees,y = speciRF),color = "darkred") + 
  ggtitle("Specificité moyenne\nen fonction du nombre d'arbres - Random Forest") +
  xlab("Nombre d'arbres") + ylab("Spécificité moyenne\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept = c(25, 50, 100, 250, 500, 1000), linetype="dotted", color = "black", size=0.5)




#******************************************************************************#
#**************************** Avec ggplot2 : KPPV *****************************#
nnn = 1:9

dfKnn = cbind (errorKnn,sensiKnn,speciKnn,nnn)
dfKnn = data.frame(dfKnn)

#******************************************************************************#
#Taux d'erreur
ploterrorKnn = ggplot(data = dfKnn, aes(x = nnn))
ploterrorKnn + 
  geom_line(aes(x = nnn, y = errorKnn),color = "darkblue") + 
  ggtitle("Taux moyen d'erreur de classification\nen fonction du nombre de voisins - K-ppv") +
  xlab("Nombre de plus proches voisins") + 
  ylab("Taux moyen d'erreur\n(via validation croisée sur 10 folds")
ploterrorKnn + 
  geom_line(aes(x = nnn, y = errorKnn),color = "darkblue") + 
  ggtitle("Taux moyen d'erreur de classification\nen fonction du nombre de voisins - K-ppv") +
  xlab("Nombre de plus proches voisins") + 
  ylab("Taux moyen d'erreur\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept = nnn, linetype="dotted", color = "black", size=0.5)


#******************************************************************************#
#Sensibilit�
plotsensiKnn  = ggplot(data = dfKnn, aes(x = nnn))
plotsensiKnn + 
  geom_line(aes(x=nnn, y = sensiKnn),color = "darkgreen") + 
  ggtitle("Sensibilité moyenne\nen fonction du nombre de voisins - K-ppv") +
  xlab("Nombre de plus proches voisins") + 
  ylab("Sensibilité moyenne\n(via validation croisée sur 10 folds")

plotsensiKnn + 
  geom_line(aes(x=nnn, y = sensiKnn),color = "darkgreen") + 
  ggtitle("Sensibilité moyenne\nen fonction du nombre de voisins - K-ppv") +
  xlab("Nombre de plus proches voisins") + 
  ylab("Sensibilité moyenne\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept = nnn, linetype="dotted", color = "black", size=0.5)


#******************************************************************************#
#Sp�cificit�
plotspeciKnn  = ggplot(data = dfKnn, aes(x = nnn))
plotspeciKnn + 
  geom_line(aes(x = nnn,y = speciKnn),color = "darkred") + 
  ggtitle("Specificité moyenne\nen fonction du nombre de voisins - K-ppv") +
  xlab("Nombre de plus proches voisins") + 
  ylab("Spécificité moyenne\n(via validation croisée sur 10 folds")

plotspeciKnn + 
  geom_line(aes(x = nnn,y = speciKnn),color = "darkred") + 
  ggtitle("Specificité moyenne\nen fonction du nombre de voisins - K-ppv") +
  xlab("Nombre de plus proches voisins") + 
  ylab("Spécificité moyenne\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept =nnn, linetype="dotted", color = "black", size=0.5)
#******************************************************************************#
#******************************************************************************#





#******************************************************************************#
#**************** Variation de Mtry avec ntree fixe (500) *********************#

# initialisation des variables utile a l'algo randomForest :
testErrRF2 <- array() 
testSensiRF2 <- array()
testSpeciRF2 <- array()
errorRF2 <- array()
sensiRF2 <- array()
speciRF2 <- array()
# valeures prises par le parametre mtry :
mtrys = c(5,10,15,20,25,30)
#indice du tableau de stockage des valeures 1:length(mtrys) :
m <- 1

#************** Algorithme principale d'apprentissage du model ****************#

for (n in mtrys){ # pour chaque valeure du parametre mtrys (du model RF)
  print("valeure mtry :") 
  print(n) #debug
    for (j in 1:10) { # pour chaque fold
      print("nb fold") 
      print( j) #debug
      
      #*************** Reinitialisation pour chaque fold ***************#
      train2 <- array() 
      trainY2 <- array()
      # on reinitialise les tableaux contenant les valeures d'entrainement 
      test2 <- array()
      testY2 <- array()
      # on reinitialise les tableaux contenant les valeures de test
      
      
      # permet d'assigner chaque ligne de X et Y a la matrice correspondante :
      for (i in 1:nrow(X)) { #pour chaque ligne de notre matrice
        if(x[i,ncol(x)] == j) { #si la colonne de l'id de la fold est egal a j
          test2 <- rbind(test2, x[i,-ncol(x)]) 
          #on place la ligne de x dans la matrice test
          testY2 <- rbind(testY2, Y[i])
          #on place la ligne de Y dans la matrice testY
        }
        else { #sinon
          train2 <- rbind(train2, x[i,-ncol(x)])
          #on place la ligne de x dans la matrice train
          trainY2 <- rbind(trainY2, Y[i])
          #on place la ligne de Y dans la matrice trainY
        }
      }
      
      # enleve la colonne contenant l'id :
      train2 <- train2[-1,]
      trainY2 <- trainY2[-1,]
      test2 <- test2[-1,]
      testY2 <- testY2[-1,]
      #*************************************************************#

      #****************** Model random Forest : ****************#
      model2 = randomForest(x = train2,y=as.factor(trainY2),ntree=500,mtry = n) 
      #entrainement du model sur les matrice d'entrainement en fonctiond de n
      pred2 = predict(model2, newdata = test2) 
      #prediction des valeures correspondante a la matrice de test
      
      
      #************ Matrice de confusion ****************#
      #* Initialisation :
      confusion2 = matrix(c(0,0,0,0),nrow = 2) 
      
    #* Remplissage :
    for(k in 1:length(pred2)){ #pour chaques valeures predites
      if(pred2[k]==testY2[k]){ #si la valeure predite est egale la valeure reel
        if(pred2[k]==0){ # et que cette valeure est egal a 0
          confusion2[1,1] <- confusion2[1,1] + 1 
        }else{ #sinon 
          confusion2[2,2] <- confusion2[2,2] + 1
        }
      }else{ #si la valeure predite est differente de la valeure reel
        if(pred2[k]==0){ # et que cette valeure est egal a 0
          confusion2[1,2] <- confusion2[1,2] + 1
        }else{ #sinon
          confusion2[2,1] <- confusion2[2,1] + 1
        }
      }
    }
      
      
      #ainsi, pour chaque repetition du  model sur la fold j:
      testErrRF2[j] <- 1 - ((confusion2[1,1] + confusion2[2,2]) / nrow(test2)) 
      #on calcule le taux de mauvaise classification du model
      testSensiRF2[j] <- confusion2[1,1] / (confusion2[1,1] + confusion2[1,2]) 
      #on calcule la sensibilite du model
      testSpeciRF2[j] <- confusion2[2,2] / (confusion2[2,2] + confusion2[2,1]) 
      #on calcule la spécificite du model
    }
  
  errorRF2[m] <- mean(testErrRF2) #on recupere la moyenne des taux de mauvaise 
  # classification de nos 10 modeles pour un ntree donne
  sensiRF2[m] <- mean(testSensiRF2) 
  #idem pour la sensibilite
  speciRF2[m] <- mean(testSpeciRF2) 
  #et pour la specificite
  m <- m+1
}

errorRF2 # affiche les moyennes des erreures pour chaque valeure de ntree
sensiRF2 # affiche les moyennes des sensibilites pour chaque valeure de ntree
speciRF2 # affiche les moyennes des specificites pour chaque valeure de ntree




#******************************************************************************#
#******************************* Affichage ************************************#

#on affiche nos resultats
plot(x = mtrys, y = errorRF2, type = 'l') 
#taux de bonne classification en fonction de ntree
plot(x = mtrys, y = sensiRF2, type = 'l') 
#sensibilité en fonction de ntree
plot(x = mtrys, y = speciRF2, type = 'l') 
#specificité en fonction de ntree

#******************************************************************************#
#******************* Avec ggplot2 : variation de mtrys ************************#

dfRF2 = cbind (errorRF2,sensiRF2,speciRF2,mtrys)
dfRF2 = data.frame(dfRF2)

#******************************************************************************#
#Error
ploterrorRF2 = ggplot(data = dfRF2, aes(x = mtrys))
ploterrorRF2 + 
  geom_line(aes(x = mtrys, y = errorRF2),color = "darkblue") + 
  ggtitle("Taux moyen d'erreur de classification\nen fonction du parametre mtry - Random Forest (500 arbres)") +
  xlab("Valeur de mtry") + 
  ylab("Taux moyen d'erreur\n(via validation croisee sur 10 folds")
ploterrorRF2 + 
  geom_line(aes(x = mtrys, y = errorRF2),color = "darkblue") + 
  ggtitle("Taux moyen d'erreur de classification\nen fonction du parametre mtry - Random Forest (500 arbres)") +
  xlab("Valeur de mtry") + 
  ylab("Taux moyen d'erreur\n(via validation croisee sur 10 folds") + 
  geom_vline(xintercept = mtrys, linetype="dotted", color = "black", size=0.5)

#******************************************************************************#
#Sensibilit�
plotsensiRF2  = ggplot(data = dfRF2, aes(x = mtrys))
plotsensiRF2 + 
  geom_line(aes(x=mtrys, y = sensiRF2),color = "darkgreen") + 
  ggtitle("Sensibilite moyenne\nen fonction du paramètre mtry - Random Forest (500 arbres)") +
  xlab("Valeur de mtry") + 
  ylab("Sensibilite moyenne\n(via validation croisee sur 10 folds")
plotsensiRF2 + geom_line(aes(x=mtrys, y = sensiRF2),color = "darkgreen") + 
  ggtitle("Sensibilite moyenne\nen fonction du paramètre mtry - Random Forest (500 arbres)") +
  xlab("Valeur de mtry") + 
  ylab("Sensibilite moyenne\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept = mtrys, linetype="dotted",color = "black", size=0.5)

#******************************************************************************#
#Sp�cificit�
plotspeciRF2  = ggplot(data = dfRF2, aes(x = mtrys))
plotspeciRF2 + 
  geom_line(aes(x = mtrys,y = speciRF2),color = "darkred") + 
  ggtitle("Specificité moyenne\nen fonction du paramètre mtry - Random Forest (500 arbres)") +
  xlab("Valeur de mtry") + 
  ylab("Spécificité moyenne\n(via validation croisée sur 10 folds")

plotspeciRF2 + 
  geom_line(aes(x = mtrys,y = speciRF2),color = "darkred") + 
  ggtitle("Specificité moyenne\nen fonction du paramètre mtry - Random Forest (500 arbres)") +
  xlab("Valeur de mtry") + 
  ylab("Spécificité moyenne\n(via validation croisée sur 10 folds") + 
  geom_vline(xintercept = mtrys, linetype="dotted", color = "black", size=0.5)








#******************************************************************************#
#******************************************************************************#
#********************* Itialisation de nouvelles folds ************************#

trainbest <- array() 
trainYbest <- array()
# on reinitialise les tableaux contenant les valeures d'entrainement 
testbest <- array()
testYbest <- array()
# on reinitialise les tableaux contenant les valeures de test


# permet d'assigner chaque ligne de X et Y a la matrice correspondante :
for (i in 1:nrow(X)) { #pour chaque ligne de notre matrice
  if(x[i,ncol(x)] == 10) { #si la colonne de l'id de la fold est egal a j
    testbest <- rbind(testbest, x[i,-ncol(x)]) 
    #on place la ligne de x dans la matrice test
    testYbest <- rbind(testYbest, Y[i])
    #on place la ligne de Y dans la matrice testY
  }
  else { #sinon
    trainbest <- rbind(trainbest, x[i,-ncol(x)])
    #on place la ligne de x dans la matrice train
    trainYbest <- rbind(trainYbest, Y[i])
    #on place la ligne de Y dans la matrice trainY
  }
}

# enleve la colonne contenant l'id :
trainbest <- trainbest[-1,]
trainYbest <- trainYbest[-1,]
testbest <- testbest[-1,]
testYbest <- testYbest[-1,]
#******************************************************************************#

#***************** Test du "meilleur" model de random forest ******************#
#************** Avec les parametres ntree = 500 et mtry = 15 : ****************#


bestmodel = randomForest(x = trainbest, y = as.factor(trainYbest), #entrainement
                         xtest = testbest, ytest = as.factor(testYbest), #test
                         ntree = 500, mtry = 15) #parametre

bestmodel # affichage des resultats du model


# Affichage des variables en fonction de leur prediction par random forest :
plot(testbest[1,], col = "green", type = "l", ylim = c(0.02,0.1),
     main="Classification de deux types \nde bact�ries - Random Forest \n(500 arbres, mtry = 15 par d�faut)",
     xlab="Canal spectral",
     ylab="Intensit� du signal",xlim=c(0,240))
legend(160, 0.04, legend=c("Cat�gorie 0", "Cat�gorie 1"), 
       col=c("green", "red"), lty=c(1,1), cex=0.6)

for (i in 2:length(bestmodel$test$predicted)) {
  if (bestmodel$test$predicted[i] == "0"){ points(testbest[i,], col = "green", type = "l") } 
  else { points(testbest[i,], col = "red", type = "l") }
}

# Affichage des variables en fonction de la cat�gorie de base :
plot(x[1,], col = "green", type = "l", ylim = c(0.02,0.1),
     main="Classification du jeu de donn�e",
     xlab="Canal spectral",
     ylab="Intensit� du signal",xlim=c(0,240))
legend(160, 0.04, legend=c("Cat�gorie 0", "Cat�gorie 1"),
       col=c("green", "red"), lty=c(1,1), cex=0.6)

j <- 0
for (i in 2:nrow(x)) {
  if (x[i,ncol(x)] == 10){
    j= j+1
    if (Y[i] == "0"){ points(x[i,], col = "green", type = "l") } 
    else { points(x[i,], col = "red", type = "l") }
  }
}
#******************************************************************************#



#******************************************************************************#
#************* Test du "meilleur" model de k plus proche voisin ***************#
#*********************** Avec le parametre k = 3 ******************************#

bestmodelknn = knn(train = trainbest, k = 3, cl = trainYbest, test = testbest)

#**************************** Matrice de confusion ****************************#
#* Initialisation :
confusionknnbest = matrix(c(0,0,0,0),nrow = 2) 

#* Remplissage :
for(k in 1:length(bestmodelknn)){ #pour chaques valeures predites
  if(bestmodelknn[k]==testYbest[k]){ #si valeure predite egale valeure reel
    if(bestmodelknn[k]==0){ # et que cette valeure est egal a 0
      confusionknnbest[1,1] <- confusionknnbest[1,1] + 1 
    }else{ #sinon 
      confusionknnbest[2,2] <- confusionknnbest[2,2] + 1
    }
  }else{ #si la valeure predite est differente de la valeure reel
    if(bestmodelknn[k]==0){ # et que cette valeure est egal a 0
      confusionknnbest[1,2] <- confusionknnbest[1,2] + 1
    }else{ #sinon
      confusionknnbest[2,1] <- confusionknnbest[2,1] + 1
    }
  }
}

#ainsi, pour chaque repetition du  model sur la fold j:
besttestErrKnn<-1-((confusionknnbest[1,1]+confusionknnbest[2,2])/nrow(testbest))
#on calcule le taux de mauvaise classification du model
besttestSensiKnn<-confusionknnbest[1,1]/(confusionknnbest[1,1]+confusionknnbest[1,2])
#on calcule la sensibilite du model
besttestSpeciKnn<-confusionknnbest[2,2]/(confusionknnbest[2,2]+confusionknnbest[2,1])
#on calcule la spécificite du model
#******************************************************************************#



# Affichage des variables en fonction de leur prediction par kppv :
plot(testbest[1,], col = "green", type = "l", ylim = c(0.02,0.1),
     main="Classification de deux types \nde bact�ries - K plus proche voisins \n(k = 3)",
     xlab="Canal spectral",
     ylab="Intensit� du signal",xlim=c(0,240))
legend(160, 0.04, legend=c("Cat�gorie 0", "Cat�gorie 1"),
       col=c("green", "red"), lty=c(1,1), cex=0.6)

for (i in 2:length(bestmodelknn)) {
  if (bestmodelknn[i] == "0"){ points(testbest[i,], col = "green", type = "l") } 
  else { points(testbest[i,], col = "red", type = "l") }
}

#******************************************************************************#
#******************************************************************************#
#******************************** FIN *****************************************#
#******************************************************************************#
#******************************************************************************#