cos_similarity <- function(i){
    similarity = rep(0,compound_num);
    print(i);
    for(j in i:compound_num){
      if(i==j){
        similarity[j]=1;
      }
      else{
        a1 = chemical_attribute[i,];
        #print(a1);
        a2 = chemical_attribute[j,];
        cos_sim = abs(sum(as.numeric(a1*a2)))/(sqrt(sum(as.numeric(a1*a1)))*sqrt(sum(as.numeric(a2*a2))));
        similarity[j] = cos_sim;
        
        
#         f1 = chemical_fingerprint[i,];
#         f2 = chemical_fingerprint[j,];
#         intersectfinger_num = length(intersect(which(f1==1),which(f2==1)));
#         unionfinger_num = length(union(which(f1==1),which(f2==1)));
#         chemfingprint_sim[i,j] = intersectfinger_num/unionfinger_num;
#         chemfingprint_sim[j,i] = intersectfinger_num/unionfinger_num;
      }
    }
  return(similarity);
}


