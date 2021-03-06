# 070527 (ver. 081126), Atte Tenkanen
# s, time shift
trent<-function(Y,X,s=1){
# for binary data only?

    #---------------------------------#
    # Transition probability vectors: #
    #---------------------------------#

    L4=L1=length(X)-s # Lengths of vector Xn+1.
    L3=L2=length(X) # Lengths of vector Xn (and Yn).

    #-------------------#
    # 1. p(Xn+s,Xn,Yn): #
    #-------------------#

    TPvector1=rep(0,L1) # Init.

    for(i in 1:L1)
    {
            TPvector1[i]=paste(c(X[i+s],"i",X[i],"i",Y[i]),collapse="") # "addresses"
    }

    TPvector1T=table(TPvector1)/length(TPvector1) # Table of probabilities.

    #-----------#
    # 2. p(Xn): #
    #-----------#

    TPvector2=X
    TPvector2T=table(X)/sum(table(X))

    #--------------#
    # 3. p(Xn,Yn): #
    #--------------#

    TPvector3=rep(0,L3)

    for(i in 1:L3)
    {
            TPvector3[i]=paste(c(X[i],"i",Y[i]),collapse="") # addresses
    }

    TPvector3T=table(TPvector3)/length(TPvector2)

    #----------------#
    # 4. p(Xn+s,Xn): #
    #----------------#

    TPvector4=rep(0,L4)

    for(i in 1:L4)
    {
            TPvector4[i]=paste(c(X[i+s],"i",X[i]),collapse="") # addresses
    }

    TPvector4T=table(TPvector4)/length(TPvector4)

    #--------------------------#
    # Transfer entropy T(Y->X) #
    #--------------------------#

    SUMvector=rep(0,length(TPvector1T))
    for(n in 1:length(TPvector1T))
    {
        SUMvector[n]=TPvector1T[n]*log10((TPvector1T[n]*TPvector2T[(unlist(strsplit(names(TPvector1T)[n],"i")))[2]])/(TPvector3T[paste((unlist(strsplit(names(TPvector1T)[n],"i")))[2],"i",(unlist(strsplit(names(TPvector1T)[n],"i")))[3],sep="",collapse="")]*TPvector4T[paste((unlist(strsplit(names(TPvector1T)[n],"i")))[1],"i",(unlist(strsplit(names(TPvector1T)[n],"i")))[2],sep="",collapse="")]))
    }
    return(sum(SUMvector))
} 


# ## Testcases
# X=(c(1,1,1,0,1,1,0,1,0,0,1,1,0,0,1,1,1,0,1,1,0,1))
# Y=(c(1,0,1,0,1,0,1,0,1,1,0,1,1,0,1,1,0,1,1,0,0,1))
# X=(c(0,1,2,3,4,5,6,6,7,7.5,6,5,3,2,2,1,1,0,1,1,0,1))
# 
# X=rnorm(20)
# Y=rnorm(20,mean=10)
# trent(X,Y)



