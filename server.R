shinyServer(function(input, output, session) {
  
 palavra2 = ""
 palavra3 = ""
 palavra4 = ""
 palavra5 = ""
 aux = ""
 p2original = ""
 palavras<-read.table("palavras1.txt", sep =";", stringsAsFactors = FALSE);
 duplas<-read.table("duplas2.txt", sep =";", stringsAsFactors = FALSE);
 triplas<-read.table("triplas3.txt", sep =";", stringsAsFactors = FALSE);
 quadru<-read.table("quadru4.txt", sep =";", stringsAsFactors = FALSE);
 usuario<-read.table("usuario.txt", sep =";", stringsAsFactors = FALSE);
 
  observe({
    
    

    usuario<-read.table("usuario.txt", sep =";", stringsAsFactors = FALSE);
     updateTextInput(session,"contador")
    updateSelectInput(session,"selector")  
    
 v<- input$botao

        todo<-input$text
      maiuscula2 = FALSE;
      maiuscula3 = FALSE;
      
      tamanhotodo = nchar(todo)
      nc<-as.integer(input$contador)
  if (is.na(nc)) {b<-todo; bres="";} else {b<-substr(todo,1,nc);bres<-substr(todo,nc+1,tamanhotodo)}

  tamanhotexto = nchar(b)
   ultima = substr(b,tamanhotexto,tamanhotexto);
   
   texto=b;
   tam = tamanhotexto; tamorig = tam;
   s = c("","","",""); 
   conta = 1;
   caraca = substr(b,tam,tam);
   while ((tam>0)&&(conta<=4)){
     if (caraca==" ") {
       while ((caraca==" ")&&(tam>0)) {tam = tam-1; caraca = substr(b,tam,tam);}
     } 
     if (caraca!=" ") { s[conta]=paste(caraca,s[conta],sep="");
     while ((caraca!=" ")&&(tam>0)) {tam = tam-1; caraca = substr(b,tam,tam);
     if (caraca!=" ") {s[conta]=paste(caraca,s[conta],sep="");}}
     };
     conta = conta+1;
     if (conta<=4){s[conta]="";}
   }
   
   
        palavra2=s[1]; p2original = palavra2;
        tamp2 = nchar(palavra2); ultp2 = substr(palavra2,tamp2,tamp2);if ((ultp2==".")||(ultp2=="!")||(ultp2=="?")) {maiuscula2=TRUE;}
        palavra2 = gsub("[[:punct:]]", "", as.character(palavra2)); palavra2 = tolower(palavra2);

        palavra3=s[2]; p3original = palavra3;
        tamp3 = nchar(palavra3); ultp3 = substr(palavra3,tamp3,tamp3);if ((ultp3==".")||(ultp3=="!")||(ultp3=="?")) {maiuscula3=TRUE;}
        palavra3 = gsub("[[:punct:]]", "", as.character(palavra3)); palavra3 = tolower(palavra3);
        palavra4=s[3]; palavra4 = gsub("[[:punct:]]", "", as.character(palavra4)); palavra4 = tolower(palavra4);
        palavra5=s[4]; palavra5 = gsub("[[:punct:]]", "", as.character(palavra5)); palavra5 = tolower(palavra5);
    
   
   
   if ((palavra2!="")&&(palavra3=="")&&(palavra4=="")&&(palavra5=="")) {maiuscula3=TRUE}
  
   geral= "";
    
    if ((ultima!=" ")&&(palavra2!="")) {
     tam = nchar(palavra2);
     aux = usuario[(tolower(substring(usuario$V1,1,tam))==palavra2),]
     verifica =  length(aux)
     if (verifica>=1){
     minimo = min(verifica,5)
     aux = aux[1:minimo]
     geral = c(geral,aux);
     }
     aux = palavras[(substring(palavras$V1,1,tam)==palavra2),]
     verifica =  length(aux)
     if (verifica>=1){
       minimo = min(verifica,5)
       aux = aux[1:minimo]
       geral = c(geral,aux);
     }
       
     if (palavra3!=""){
       paux=paste(palavra3,palavra2,sep=" ");
       tam = nchar(paux);
       aux = duplas[(substring(duplas$V1,1,tam)==paux),]
       verifica =  length(aux)
       if (verifica>=1){
         minimo = min(verifica,5)
         aux = aux[1:minimo]
         geral = c(geral,aux);
       }  
       
       if (palavra4!=""){
         paux=paste(palavra4,palavra3,palavra2,sep=" ");
         tam = nchar(paux);
         aux = triplas[(substring(triplas$V1,1,tam)==paux),]
         verifica =  length(aux)
         if (verifica>=1){
           minimo = min(verifica,5)
           aux = aux[1:minimo]
           geral = c(geral,aux);
         }  
       
         if (palavra5!=""){
           paux=paste(palavra5,palavra4,palavra3,palavra2,sep=" ");
           tam = nchar(paux);
           aux = quadru[(substring(quadru$V1,1,tam)==paux),]
           verifica =  length(aux)
           if (verifica>=1){
             minimo = min(verifica,5)
             aux = aux[1:minimo]
             geral = c(geral,aux);
           }  
         }
         
         }
       
       }
      
   }
   
   
   if ((ultima==" ")&&(palavra2!="")) {
     
     
     tam = nchar(palavra2);
     aux = palavras[(tolower(palavras$V1)==palavra2),]
     v1 =  length(aux)
     aux = usuario[(tolower(usuario$V1)==palavra2),]
     v2 =  length(aux)
       if ((v1+v2)<1){
         
         usuario = rbind(palavra2, usuario);
         names(usuario)=c("V1")
         minimo = min(length(usuario$V1),1000);
         usuario = usuario[1:minimo,]
         usuario = as.data.frame(usuario)
         names(usuario)=c("V1")
       write.table(usuario, file = "usuario.txt", append = FALSE, quote = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
         }
     
     
     paux =paste(palavra2," ",sep=""); 
     tam = nchar(paux);
     aux = duplas[(substring(duplas$V1,1,tam)==paux),]
     verifica =  length(aux)
     if (verifica>=1){
       minimo = min(verifica,5)
       aux = aux[1:minimo]
       geral = c(geral,aux);
         }
     
     if (palavra3!=""){
       paux=paste(palavra3,palavra2,sep=" ");
       tam = nchar(paux);
       aux = triplas[(substring(triplas$V1,1,tam)==paux),]
       verifica =  length(aux)
       if (verifica>=1){
         minimo = min(verifica,5)
         aux = aux[1:minimo]
         geral = c(geral,aux);
       }
    
       if (palavra4!=""){
         paux=paste(palavra4,palavra3,palavra2,sep=" ");
         tam = nchar(paux);
         aux = quadru[(substring(quadru$V1,1,tam)==paux),]
         verifica =  length(aux)
         if (verifica>=1){
           minimo = min(verifica,5)
           aux = aux[1:minimo]
           geral = c(geral,aux);
         }
       }
       
       }
      } 
   
   
  
 updateSelectInput(session,"selector", choices=c(geral),selected="")    
      
   sel = input$selector;
   if (is.null(sel)){sel=""}
  
 if (sel!=""){
         tamsel = nchar(sel);
         jaux = tamsel;
         vespaco = substr(sel,jaux,jaux);
           novosel =vespaco;
         while ((vespaco!=" ")&&(jaux>1)){
                jaux=jaux-1;
                vespaco = substr(sel,jaux,jaux);
                if (vespaco!=" "){novosel = paste(vespaco,novosel,sep="");}
                                         }
          sel = novosel;
         if ((maiuscula3==TRUE)&&(ultima!=" ")){tamsel=nchar(sel); sel1=substr(sel,1,1); sel1=toupper(sel1);
         if (tamsel>=2){sel2=substr(sel,2,tamsel);sel=paste(sel1,sel2,sep="") }
         if (tamsel<=1){sel = sel1;}
         }
          
          if ((maiuscula2==TRUE)&&(ultima==" ")){tamsel=nchar(sel); sel1=substr(sel,1,1); sel1=toupper(sel1);
          if (tamsel>=2){sel2=substr(sel,2,tamsel);sel=paste(sel1,sel2,sep="") }
          if (tamsel<=1){sel = sel1;}
          }  
          
        if (ultima!=" "){
          tam = nchar(p2original)
          tamx = nchar(palavra2)
          if (tam>tamx){selx = substr(p2original,tamx+1,tam); }
          tamb = nchar(b)
          b = substr(b,1,tamb-tam)
          b = paste(b,sel, sep="")
          if (tam>tamx) {b =paste(b,selx,sep="")}
          }
        
        if (ultima==" "){
          
          b = paste(b,sel, sep="")
        }
        peddir = "";
        tamres = nchar(bres);
        if (tamres>0) {
          conta = 1;
          peddir="";
          pontofinal = FALSE;
          charaux = substr(bres,conta,conta);
          if (charaux == ".") {pontofinal=TRUE}
          while ((charaux!=" ")&&(conta<=tamres)){
            peddir = paste(peddir,charaux,sep="");
            conta = conta+1;
            charaux = substr(bres,conta,conta);
            if (charaux == ".") {pontofinal=TRUE}
          }
          tamres = nchar(bres);
          tampeddir = nchar(peddir);
          bres = substr(bres,tampeddir+1,tamres);
          if (pontofinal==TRUE) {b = paste(b,".",sep="");}
          
        }  
        
        updateTextInput(session,"contador", value=nchar(b))
        todo =paste(b,bres,sep="")
        updateTextInput(session,"text", value=todo)

 }


 })  
  
  }
)