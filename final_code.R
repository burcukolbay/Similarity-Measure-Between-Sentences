# install.packages("igraph");

library(rJava);
library(igraph);
.jinit();
.jaddClassPath("D:\\emdmkm-2nd\\case study\\Mif20\\Mif20\\dist\\Mif20.jar");
sim <- .jnew(class="mif20.Parser");

.jaddClassPath("D:\\emdmkm-2nd\\case study\\exempleWordnet\\Wordnet\\edu.mit.jwi_2.3.3.jar");
.jaddClassPath("D:/emdmkm-2nd/case study/exempleWordnet/Wordnet/jws-0.12.00.jar");
.jaddClassPath("D:\\emdmkm-2nd\\case study\\exempleWordnet\\Wordnet\\Distance.class");
.jaddClassPath("D:\\emdmkm-2nd\\case study\\exempleWordnet\\Wordnet\\");

dist <- .jnew(class="Distance", "D:\\emdmkm-2nd\\case study\\exempleWordnet\\", "3.0");

.jaddClassPath("D:\\emdmkm-2nd\\case study\\Mif20\\Mif20\\dist\\Mif20.jar");

sim2 <- .jnew(class="mif20.Similarity", "D:\\emdmkm-2nd\\case study\\exempleWordnet","3.0");

file_0<-read.delim("C:\\Users\\ASUS\\Desktop\\lastv\\STS.input.train_MSRvid.txt", sep="\t", header=FALSE)
names(file_0) <- c("sentence 1", "sentence 2")
file_1<-read.delim("C:\\Users\\ASUS\\Desktop\\lastv\\STS.gs.train_MSRvid.txt", sep="\t", header=FALSE)
names(file_1) <-c("Gold Score")

file_0[,3]<-file_1[,1]

adj<-c("JJ","JJR","JJS");
noun<-c("NN","NNS","NNP","NNPS");
adv<-c("RB","RBR","RBS");
verb<-c("VB","VBD","VBG","VBN","VBP","VBZ");

getDist = function(x,y,z) {
  tryCatch(dist$getMaxScoreHSO(x,y,z),
           error = function(e) {print(paste("Error processing text")); 
                                -1}) 
}

file_2<-read.delim("C:\\Users\\ASUS\\Desktop\\lastv\\STS.input.test_MSRvid.txt", sep="\t", header=FALSE)

file_3<-read.delim("C:\\Users\\ASUS\\Desktop\\lastv\\STS.gs.test_MSRvid.txt", sep="\t", header=FALSE)
names(file_3) <-c("Gold Score")

file_2[,3]<-file_3[,1]

fileorg<-file_0;
# file_0<-file_2;

for (l in 1:nrow(file_0)){
# for (l in 1:5){
  sentence_p<-sim2$getPair(as.character(file_0[l,1]),as.character(file_0[l,2]));

  s1pair1 <- data.frame(word=character(),
                        class=character(), 
                        indexnum=numeric(),
                        stringsAsFactors=FALSE);
  s2pair1 <- data.frame(word=character(),
                        class=character(), 
                        indexnum=numeric(),
                        stringsAsFactors=FALSE);
  for(i in 1:(length(sentence_p)/2)){
    s1pair1[nrow(s1pair1) + 1,] <- unlist(strsplit(sentence_p[2*i-1],split="/",fixed=TRUE));
    s2pair1[nrow(s2pair1) + 1,] <- unlist(strsplit(sentence_p[2*i],split="/",fixed=TRUE));
  }
  
  bu_1<-sim$getListDependencies(as.character(file_0[l,1]));
  bu1<-capture.output(bu_1)
  bu_2<-sim$getListDependencies(as.character(file_0[l,2]));
  bu2<-capture.output(bu_2)
  
  bu1<-substr(bu1,nchar("[1] qJava-Object{[")+1,nchar(bu1)-4);
  bu1<-strsplit(bu1,split="), ",fixed=TRUE)
  bu1<-unlist(bu1);
  
  
  bu2<-substr(bu2,nchar("[1] qJava-Object{[")+1,nchar(bu2)-4);
  bu2<-strsplit(bu2,split="), ",fixed=TRUE)
  bu2<-unlist(bu2);
  
  listdep1 <- data.frame(index1=numeric(), 
                         index2=numeric(),
                         stringsAsFactors=FALSE);

  for (dep in bu1){
   
    ind2<-unlist(strsplit(dep,split="-",fixed=TRUE))[length(unlist(strsplit(dep,split="-",fixed=TRUE)))]
    ind1_0<-unlist(strsplit(dep,split=", ",fixed=TRUE))[1]
    ind1<-unlist(strsplit(ind1_0,split="-",fixed=TRUE))[length(unlist(strsplit(ind1_0,split="-",fixed=TRUE)))]
    listdep1[nrow(listdep1) + 1,] <- c(ind1,ind2);
  
  listdep2 <- data.frame(index1=numeric(), 
                         index2=numeric(),
                         stringsAsFactors=FALSE);
  for (dep in bu2){
    ind2<-unlist(strsplit(dep,split="-",fixed=TRUE))[length(unlist(strsplit(dep,split="-",fixed=TRUE)))]
    ind1_0<-unlist(strsplit(dep,split=", ",fixed=TRUE))[1]
    ind1<-unlist(strsplit(ind1_0,split="-",fixed=TRUE))[length(unlist(strsplit(ind1_0,split="-",fixed=TRUE)))]
    
    listdep2[nrow(listdep2) + 1,] <- c(ind1,ind2);
  }
  
  c<-data.frame(listdep1[,1],listdep1[,2]);
  c1 <- data.frame(listdep2[,1],listdep2[,2]);
  g<-graph.edgelist(as.matrix(c));
  g1<-graph.edgelist(as.matrix(c1));
  a0 <-get.adjacency(g);
  a1 <-get.adjacency(g1);
  a0_2 <- sqrt(length(a0));
  a1_2 <- sqrt(length(a1));
  a0_3 <- as.matrix(a0);
  a1_3 <- as.matrix(a1);
  f<-matrix(1,a1_2,a0_2);
  f_old <- matrix(0,a1_2,a0_2);
  f_old2<- matrix(0,a1_2,a0_2);
  counter<-0;
  # iterate until there is convergence
  while(norm((f_old2-f),type="F")>0.001){
    # even number of iterations
    for(i in c(1,2)){
      f_old2<-f_old;
      f_old <- f;
      f <- (a1_3 %*% f %*% t(a0_3)+t(a1_3) %*% f %*% a0_3)/norm((a1_3 %*% f %*% t(a0_3)+t(a1_3) %*% f %*% a0_3),type="F");
      counter<-counter+1;
    }
    if (counter == 1000){
      print(l);
      break;
    }
  }
  f<-round(f,4);
  f_scale <- (f-min(f))/(max(f)-min(f));


  d <- f - f;
  
  for (r in 1:nrow(s1pair1)){
    if (s1pair1[r,2] %in% noun){
      form <- "n"   
    } else if (s1pair1[r,2] %in% verb){
      form <- "v"   
    } else if (s1pair1[r,2] %in% adj){
      form <- "a"   
    } else if (s1pair1[r,2] %in% adv){
      form <- "r"   
    }
    else form<-"e";
    worddist<-getDist(s1pair1[r,1],s2pair1[r,1], form);
    print("getting word distance");
    if(worddist!=-1){
      d[s2pair1[r,3],s1pair1[r,3]]<-worddist;
    }
    if (form=="e")
    {
      lev_dist<-adist(s2pair1[r,1],s1pair1[r,1])
      lev_std<-1-lev_dist/max(nchar(s2pair1[r,1]),nchar(s1pair1[r,1]))
      d[s2pair1[r,3],s1pair1[r,3]]<-16*lev_std;
    }
   
  }
   
  wd <- d*f;
  wd <- (1/16)*wd;
  jaccard_a <- sum(wd);
  jaccard_b <- 0;
  for(i in 1:nrow(wd)){
    jaccard_b <- jaccard_b + (1-max(wd[i,]));
  }
  jaccard_c <- 0;
  for(i in 1:ncol(wd)){
    jaccard_c <- jaccard_c + (1-max(wd[,i]));
  }
  jaccard <- jaccard_a / (jaccard_a+jaccard_b+jaccard_c)
  file_0[l,4] <- jaccard;

  wd <- d*f_scale;
  wd <- (1/16)*wd;
  jaccard_a <- sum(wd);
  jaccard_b <- 0;
  for(i in 1:nrow(wd)){
    jaccard_b <- jaccard_b + (1-max(wd[i,]));
  }
  jaccard_c <- 0;
  for(i in 1:ncol(wd)){
    jaccard_c <- jaccard_c + (1-max(wd[,i]));
  }
  jaccard2 <- jaccard_a / (jaccard_a+jaccard_b+jaccard_c)
  file_0[l,5] <- jaccard2;
  print(l);
}

file_0[,4] <- round(file_0[,4],4);
file_0[,5] <- round(file_0[,5],5);

library(e1071);
file_org <- file_0;
fit1 <- lm(V3~0+V4+V5, data=file_org);
fit2 <- lm(V3~0+V4, data=file_org);
fit3 <- lm(V3~0+V5, data=file_org);
model1 <- svm(V3~0+V4+V5, data=file_org);
model2 <- svm(V3~0+V4, data=file_org);
model3 <- svm(V3~0+V5, data=file_org);


file_0[,6]<-predict(fit1, file_0);
file_0[,7]<-predict(fit2, file_0);
file_0[,8]<-predict(fit3, file_0);
file_0[,9]<-predict(model1, file_0);
file_0[,10]<-predict(model2, file_0);
file_0[,11]<-predict(model3, file_0);

file_x <- file_0;
file_x$V6[file_x$V6>5]<-5;
file_x$V7[file_x$V7>5]<-5;
file_x$V8[file_x$V8>5]<-5;
file_x$V9[file_x$V9>5]<-5;
file_x$V10[file_x$V10>5]<-5;
file_x$V11[file_x$V11>5]<-5;

correlation<-cor(file_x[,4:12])
write.csv(correlation,"corr.csv");

