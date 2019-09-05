Jaccard_similarity <- function(i){
  similarity = rep(0,compound_num);
  print(i);
  for(j in i:compound_num){
    if(i==j){
      similarity[j] = 1;
    }
    else{
      f1 = chemical_fingerprint[i,];
      f2 = chemical_fingerprint[j,];
      intersectfinger_num = length(intersect(which(f1==1),which(f2==1)));
      unionfinger_num = length(union(which(f1==1),which(f2==1)));
      similarity[j] = intersectfinger_num/unionfinger_num;
    }
  }
  return(similarity);
}


