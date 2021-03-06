# Isa�as V. Prestes <isaias.prestes at gmail.com >
# November 23, 2006 03:33:53 PM 
#
# Vers�o V (rev. 6)
#
# Este algoritmo implementa para o R o procedimento de Scott-Knott
# para compara��es m�ltiplas entre m�dias de tratamentos ap�s ANOVA.
#

"print.cordena" <- function (x, ...) {
   if (!inherits(x, "cordena")) stop("Object must be of class 'cordena'")
   cat(x[[1]],"\n",x[[2]] ," \n")
}

ordena <- function(g,x) {
	n <- length(x)
	ansX <- array(,0)
	ansG <- array(,0)
	baldeX <- x
	baldeG <- g
	for (i in 1:n) {
		meiaX <- max(baldeX)
		pos <- which( x == meiaX)
		ansX <- c(ansX,x[pos])
		ansG <- c(ansG,g[pos])
		baldeX <- baldeX[baldeX != meiaX]
	}
	ans <- list(medias = ansX,grupos = ansG)
	class(ans) <- c("cordena","list")
	return(ans)
}

matrixSQ <- function(x,group) {
	group <- as.factor(group)
	groupNam <- levels(group)
	ng <- length(group)
	mdata <- matrix(0, ng, ng, dimnames = list(group,group) )
	for (i in 1:ng) {
		for (j in i:ng) {
			delta <- abs( x[i] - x[j] )
			mdata[i,j] <- delta	
		}	
	}
	return(mdata^2)
}

particaoB <- function(EucMatr, s2ybar, gle) {
	Grupos <- as.character(row.names(EucMatr))
	Ng <- length(Grupos)
	MatrGrup <- matrix(Grupos,Ng,Ng,byrow = TRUE)
	vetSQD <- vetSQE <- vetPOSini <- vetPOSfin <- array(,0)
	vetcluster1 <- vetcluster2 <- array(,0)
	vetlambda <- vetpvalue <- vetsig2 <- vetgl <- array(,0)
	SQE <- sum(EucMatr)/Ng
	for ( i in 1:(Ng-1)) {
		# Computa o SQD para cada combina��o com k grupos
		SQD1 <- sum(EucMatr[1:i,1:i]) / length(1:i)
		SQD2 <- sum(EucMatr[(i+1):Ng,(i+1):Ng]) / length( ((i+1):Ng) )
		SQD <- SQD1 + SQD2
		vetSQD <- c(vetSQD,SQD)
		vetSQE <- c(vetSQE,SQE - SQD)
		BSS <- SQE - SQD
		BSSgeral <- SQE
		glChisq <- Ng / (pi-2)
		Sig2 <- (BSSgeral + gle * s2ybar) / (Ng + gle)
		lambda <- pi/(2*(pi-2))*(BSS/Sig2)
		pvalue <- 1 - pchisq(lambda,glChisq)
		vetpvalue <- c(vetpvalue, pvalue)
		vetlambda <- c(vetlambda, lambda)
		vetsig2 <- c(vetsig2, Sig2)
		vetgl <- c(vetgl, glChisq)
		vetPOSini <- c(vetPOSini, length(Grupos[1:i]) )
		vetPOSfin <- c(vetPOSfin, length(Grupos[(i+1):Ng]) )
		# Computa Grupos para exibi��o
		vetcluster1 <- c(vetcluster1,paste(Grupos[1:i],collapse="",sep=""))
		vetcluster2 <- c(vetcluster2,paste(Grupos[(i+1):Ng],collapse="",sep=""))
	}
	ans <- data.frame(Cluster1 = vetcluster1, Cluster2 = vetcluster2, SQD = vetSQD, SQE = vetSQE, P1 = vetPOSini, 
	P2 = vetPOSfin, Lambda = vetlambda, Pvalue = vetpvalue, Sig2 = vetsig2, GL = vetgl)
	pos <- which(ans$SQD ==  min(ans$SQD))
	ans <- ans[pos,]
	return(ans)
}

localiza <- function(wer,asd) {
	ans <- array(,0)
	compri <- length(wer)
	for (i in 1:compri) {
		pos <- which(wer == asd[i])
		ans <- c(ans, wer[pos])
	}
	return(ans)
}

getPOS <- function(wer,zxc,asd) {
	ans <- array(,0)
	compri <- length(wer)
	for (i in 1:compri) {
		pos <- which(wer == asd[i])
		ans <- c(ans, zxc[pos])
	}
	return(ans)
}

# getmeans		Esta fun��o retorna a m�dia de cada fator (tratamento) se
#				fornecidos os vetores com as medidas e fatores (tratamentos)
#
# Uso:
# 
# medidas		: vetor num�rico que cont�m os dados mensurados.
# tratamentos	: vetor de fatores com os nomes dos fatores (tratamentos).
#

getmeans <- function(medidas,tratamentos) {
	tratamentos <- levels(tratamentos)
	Tabela <- data.frame(medidas,tratamentos)
	mediasAMO <- array(,0)
	for (i in tratamentos) {
		dados <- Tabela[Tabela$tratamentos == i, 1]
		mediasAMO <- c(mediasAMO,mean(dados))
	}
	return(mediasAMO)
}

# preSK			Esta fun��o retorna a m�dia de cada fator (tratamento) se
#				fornecidos os vetores com as medidas e fatores (tratamentos)
#
# Uso:
# 
# medidas		: vetor num�rico que cont�m os dados mensurados.
# tratamentos	: vetor de fatores com os nomes dos fatores (tratamentos).
#

preSK <- function(medidas,tratamentos) {
	tratamentos <- levels(tratamentos)
	Tabela <- data.frame(medidas,tratamentos)
	mediasAMO <- array(,0)
	for (i in tratamentos) {
		dados <- Tabela[Tabela$tratamentos == i, 1]
		mediasAMO <- c(mediasAMO,mean(dados))
	}
	ans <- list(medias = mediasAMO, grupos = tratamentos)
	class(ans) <- c("cordena","list")
	return(ans)
}

# SundK			Esta fun��o retorna o resultado do procedimento de Skott-Knott
#				para compara��es de m�dias ap�s ANOVA.
#
# Uso:
# 
# x				: vetor com as m�dias dos fatores (grupos, tratamentos).
# group			: vetor com o nome dos grupos a que est�o associadas as
#				  m�dias do vetor 'x'.
# s2ybar		: � a raz�o entre o quadrado m�dio do erro experimental
#				  e o n�mero de repeti��es nos tratamentos (QME/r). Ou
#				  seja a vari�ncia comum �s m�dias de 'tratamentos'.
#				  ************************************************************
#				  * ATEN��O: Este procedimentos n�o suporta n�mero diferente *
#				  * de repeti��es entre fatores (grupos,tratamentos).        *
#				  ************************************************************
# gle			: n�mero de graus de liberdade do erro experimental.
#

"print.skmc" <- function (x, ...) {
   if (!inherits(x, "skmc")) stop("Object must be of class 'skmc'")
   cat("        M�todo de Scott-Knott para Agrupamentos de M�dias \n\n")
   print(x[[2]])
   cat(" \n")
   print(x[[1]])
   cat(" \n")
}

SundK <- function(x,group, s2ybar, gle) {
	# Faz ordenamento para que tudo funcione bem
	potinho <- ordena(group,x)
	group <- potinho$grupos
	g <- potinho$grupos
	x <- potinho$medias
	#--------------------------------------------
	Stable <- data.frame(group,x)
	c <- 0
	nivel <- 1
	MatrSS <- matrixSQ(x,g)
	dimMSS <- dim(MatrSS)[2]	# Tamanho da matriz SS
	k <- length(group)
	Tabfinal <- data.frame(nivel=1,Parte = t(g))
	balde <- particaoB(MatrSS, s2ybar, gle)
	VaiComDeus <- balde
	parte1 <- c(group[1:balde$P1],rep(" ",(k-balde$P1)))
	parte2 <- c(group[(balde$P1+1):(balde$P1+balde$P2)],rep(" ",balde$P1))
	nivel <- 2
 	Tabfinal <- rbind(Tabfinal, data.frame(nivel=nivel,Parte = t(parte1)) )
 	Tabfinal <- rbind(Tabfinal, data.frame(nivel=nivel,Parte = t(parte2)) )
	while (c == 0) {
		Tabparc <- Tabfinal[Tabfinal$nivel == nivel,]
		linTabParc <- dim(Tabparc)[1]
		nivel <- nivel + 1
		for (j in 1:linTabParc) {
			G <- Tabparc[j,2:(k+1)]
 			groupParc <- t(t(Tabparc[j,2:(k+1)]))
			groupParc <- localiza(group,groupParc)
			z <- length(groupParc)
			if (z > 1) {
				xParc <- getPOS(group,x,groupParc)
				matrixSQParc <- matrixSQ(xParc,groupParc)
				balde <- particaoB(matrixSQParc, s2ybar, gle)
				VaiComDeus <- rbind(VaiComDeus,balde)
				parte1 <- c(groupParc[1:balde$P1],rep(" ",k))
				parte1 <- parte1[1:k]
				parte2 <- c(groupParc[(balde$P1+1):(balde$P1+balde$P2)],rep(" ",k) ) 
				parte2 <- parte2[1:k]
 				Tabfinal <- rbind(Tabfinal, data.frame(nivel=nivel,Parte = t(parte1)) )
 				Tabfinal <- rbind(Tabfinal, data.frame(nivel=nivel,Parte = t(parte2)) )
			}
		}
		Tabparc <- Tabfinal[Tabfinal$nivel == nivel,]
		if (sum(Tabparc[,2] == ' ') == dim(Tabparc )[1]) c <- 1

	}
	ans <- list(tabela = VaiComDeus, niveis = Tabfinal)
	class(ans) <- c("skmc","list")
	return(ans)
}

###################################################################################################################


#############
# EXEMPLO I #
#############

medias <- c(85,76,72,62) # m�dias dos tratamentos com correspond�ncia no vetor 'g'
NomeTratamentos <- c('b','c','a','d')	# r�tulos (nomes) dos tratamentos a serem comparados
s2ybar <- 16.82 		 # s2ybar � QME/r
gle <- 20 				 # gle � o n�mero de graus de liberdade do erro

SundK(medias,NomeTratamentos, s2ybar, gle)

##############
# EXEMPLO II #
##############

x <- c(85,84,23,22)			# m�dias dos tratamentos com correspond�ncia no vetor 'g'
g <- c('tB','tC','tA','tD') # r�tulos (nomes) dos tratamentos a serem comparados
s2ybar <- 12.992 			# s2ybar � QME/r
gle <- 20 					# gle � o n�mero de graus de liberdade do erro

SundK(x,g, s2ybar, gle)

###############
# EXEMPLO III #
###############

# N�mero de repeti��es
r <- 6

# Gera n�meros pseudo-aleat�rios para tratamento
x1 <- rnorm(r, 008, 25)
x2 <- rnorm(r, 100, 25)
x3 <- rnorm(r, 105, 25)
x4 <- rnorm(r, 299, 25)

# Cria a coluna com o nome do tratamento associado
nomesTratamentos <- c(rep('tA',r),rep('tB',r),rep('tC',r),rep('tD',r))

# Cria a coluna com as medidas (concatena os dados previamente gerados)
medidas <- c(x1,x2,x3,x4)

# Constitui um data.frame
Dados <- data.frame( Tratamento = nomesTratamentos, Medida = medidas )
Dados

# Executa a ANOVA - DCC
minhaANOVA <- aov( Medida ~ Tratamento, data = Dados)
summary(minhaANOVA)

# Calcula dados necess�rios para o Scott-Knott
gle <- minhaANOVA$df.residual			# Os graus de liberdade do erro.
QME <- sum(minhaANOVA$residuals^2)/gle	# O QME.
s2ybar <- QME/r							# vari�ncia comum �s m�dias de 'tratamentos'.

# Guarda em um vetor o nome dos tratamentos
vetorTratamentos <- preSK(Dados$Medida, Dados$Tratamento)$grupos
vetorTratamentos

# Guarda em um vetor as m�dias
vetorMediasAmostrais <- preSK(Dados$Medida, Dados$Tratamento)$medias
vetorMediasAmostrais

# Observa os vetores criados:
vetorTratamentos ; vetorMediasAmostrais

# Ent�o o procedimentos de Scott-Knott
SundK(vetorMediasAmostrais, vetorTratamentos, s2ybar, gle)
