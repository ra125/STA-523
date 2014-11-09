is_valid=function(input){
  
  # 1: Testing if the input has a list object
  t_obj1_1<-as.character(typeof(input)=="list") 
  n=length(input)
  t_obj1_2=list()
  t_obj1_3=as.vector(NULL)
  for(i in 1:n){
    t_obj1_2[[i]]<-as.character(typeof(input[[i]])=="list")  
    t_obj1_3 =c(t_obj1_3, t_obj1_2[[i]])
  }
  
  t_obj1_4<-data.frame(table(t_obj1_3))
  t_obj1_5<-t_obj1_4[which(t_obj1_4[,1]==FALSE),2]
  
  if(length(t_obj1_5)>0){
    return(F)
  }  else {
    t1<-TRUE
  }
  
  
  # 2: Testing if the names of the lists in object are all unique
  if(  length(names(input))==0   )   {  #begin first ifelse
    t2<-TRUE
  } else { 
    if(as.character( length(unique(names(input)))==length(input)) =="TRUE"){  #begin second ifelse
      t2<-TRUE
    } else{
      return(F)
    }                  #end second ifelse 
  } #end first ifelse
  
  
  # 3: Testing if the object has both weights and edges as lists
  if(as.character(sum(grep("weights",names(input[[1]])))>0 & sum(grep("edges",names(input[[1]])))>0 )=="TRUE"){
    t3<-TRUE
  } else{
    return(F)
  }
  
  
  # 4: Testing if the types of edges are integer and weights are double
  if(length(input[[1]]$edges)==0) {
    t4<-TRUE
  } else{ 
    n=length(input)
    t_obj4_1 =data.frame(table(is.na(input[[n]]$edges) ) )
    t_obj4_2 =t_obj4_1[which(t_obj4_1[,1]==TRUE),2]
    
    if(length(t_obj4_2) >0){
      t4<-FALSE
    } else{
      
      if(as.character(typeof(input[[1]]$edges)  == "integer" & typeof(input[[1]]$weights)=="double")=="TRUE"){
        t4<-TRUE
      } else{
        return(F)
      } 
    }
  }
  
  
  # 5: Testing if there are not any edges to non-existent vertices.   
  n=length(input)
  temp=NULL
  t_obj5_1=list()
  t_obj5_2=(1:n)
      
  for(i in 1:n){
        t_obj5_1[[i]]<-input[[i]]$edges
        temp=c(temp, t_obj5_1[[i]])    #temp; directed edges. the list number of these numbers should exist
                    } 

     if(length(temp)==0){   #First ifelse: If there is no edge numbers, the object passes this test.
       t5<-TRUE
     } else {  
       if(is.na(temp)[1]==TRUE){ # Second ifelse; no else condition
         return(F)
       }
       
      
      temp=unique(temp)
      TF=NULL
      dt=matrix(nrow=max(unique(c(temp,n))))
       for(i in 1:n){
        dt[i,1]=is.integer(input[[i]]$edges)    #binary vector: if 1, such vertex exists
              }
           TF=matrix(nrow=max(unique(c(temp,n))))
           for(i in temp){
            TF[i,]<- dt[i,1]=="TRUE"
                          }
    tf5<- which(data.frame(is.na(data.frame(TF)[temp,]) )[,1]=="TRUE")
    if(length(tf5)>0 ){  # Third ifelse
      return(F)
    }   else{
      test=data.frame(table(TF))[  which(data.frame(table(TF))[,1]==FALSE) ,2]
    }   #End third ifelse
      
      
      if(length(test)>0){        #begin 4th if else
        return(F)
      }     else {
        t5=TRUE
       }  ## end 4th ifelse 
    }    # end First ifelse
    

  # 6: Testing if the weights are greater than 0.
  if(typeof( length(input[[1]]$weights))=="integer"  ){  
    
    if( length(input[[1]]$weights)==0 ){
      t6<-TRUE
    }    else{
      t_obj6<-as.character(input[[1]]$weights>0)
      t_obj6_1 <-data.frame(table(t_obj6))
      int<-t_obj6_1[which(t_obj6_1[,1]==TRUE),2]
      t_obj6_2<-length(rep (int, int ))
      
      t_obj6_3<-length(input[[1]]$weights)
      if(t_obj6_2==t_obj6_3) {
        t6<-TRUE
      }  else{
       return(F)              
      }
    }
  } else{
    return(F)
  }
  
  
  # 7: Testing if edges and weights have the same length
  n=length(input)
  t_obj7<-rep(NA,n)
  for(i in 1:n){
    t_obj7[i]<-length(input[[i]]$weights) == length(input[[i]]$edges)
    t_obj7_1<-data.frame(table(t_obj7))
    t_obj7_2<-t_obj7_1[which(t_obj7_1[,1]=="FALSE"),2]
  }  
  
  if (length(t_obj7_2) >0){
    return(F)
  } else{
    t7<-TRUE
  }
  
  
  #8: Testing if there are duplicate edges
  n=length(input)
  t_obj8<-rep(NA, n)
  for(i in 1:n){
    t_obj8[i]<- length(unique(input[[i]]$edges)) == length(input[[i]]$edges)
    t_obj8_1 <-data.frame(table(t_obj8))
    t_obj8_2 <-t_obj8_1[which(t_obj8_1[,1]=="FALSE"),2]
  }
  
  if (length(t_obj8_2)>0)
  {
    return(F)
  } else{
    t8<-TRUE
  }
  
  
  # Testing if the object passes all tests above
  each<-cbind(t1,t2,t3,t4,t5,t6, t7, t8)
  if(sum(which(each==TRUE))==36) {
    return(T)
  } else{
    return(F)
  }

}
