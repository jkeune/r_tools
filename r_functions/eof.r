# MM : input data matrix  NT = dimension sample, NPP = data vector length
# M : data matrix without missing values
# calculates ;
# C : matrix of eigenvectors
# Lambda : diagonal matrix with eigenvalues
# A : matrix of pricipal components
#
# MM = C * t(A)
#

 eof <- function(MM)

# eliminate dimensions with missing values (NA)
#

 NT <- dim(MM)[1]
 NPP <- dim(MM)[2]

 if all( !is.na(MM) )
 {
  M = MM 
  print('No missing values!')
 }
 else
 {
  for ( nt in 1:NT )
  {
   nfehl <- which( is.na( MM[nt,:] ) ) ;
   MM[1:NT,nfehl] = NA;
  }

  nonfehl <- find( !is.na( MM[1,:] ) ) ;
  NNF <- dim(nonfehl)[2] ;
  M(1:NT,1:NNF) = 0. ;

  for ( nt in 1:NT )
  {
    M[nt,1:NNF] = MM[nt,nonfehl] ;
  }
 }

# calculate EOF              

 NP <- dim(M)[2]

# BIS HIER !!!!!!!!!!!!

 F = detrend(M,0);
 F = M ;
 
 if (NT >= NP)  % calcul EOF "normale"

  R = F' * F;
  [C,Lambda] = eig(R); 
  A = F * C;

 else

  L = F * F';
  [B,Lambda] = eig(L);
  D = F' * B;
  sq = [sqrt(diag(Lambda))+eps]'; 
  sq = sq(ones(1,NP),:);
  C = D./sq;
  A = F * C;

 end

# PC auf standard deviation 1 normieren

#  Lambda(find(Lambda<0)) = Lambda(find(Lambda<0))*-1 ;
#  N = min(NP,NT) ; N=1 ;
#  W = sqrt( Lambda(N,N) ) / std(A(:,N)) ;

#  C = C * sqrt(Lambda) / ( W ) ;
#  A = A / sq * ( W ) ;
#  Lambda = diag( Lambda / ( W*W ) ) ;

  N = min(NP,NT) ;
  W = sqrt( Lambda(N,N) ) / std(A(:,N)) ;
  for n=1:N
   if ( Lambda(n,n) == 0 ) Lambda(n,n) = Lambda(N,N)/1000. ;end ;end

  C = C * sqrt(Lambda) / ( W ) ;
  A = A / sqrt(Lambda) * ( W ) ;
  Lambda = diag( Lambda / ( W*W ) ) ;

# Fehlwerte wieder einfuegen

if ( NPP == NP )
  
  CC = C ;

else

  CC(1:NPP,1:min(NT,NP)) = NaN ;
  CC(nonfehl,1:min(NT,NP)) = C ;

end
