skew<-function(x,meth="unbiased"){

    m.i<-function(x,i){
        n<-length(as.vector(x))
        m<-(1/n)*sum((x-mean(x))^i)
        m}

    n<-length(as.vector(x))
    if(meth=="unbiased"){
        skew<-(n/((n-1)*(n-2)))*sum(((x-mean(x))/sd(x))^3)}

    if(meth=="moments"){
        skew<-m.i(x,3)/(m.i(x,2)^(3/2))}
    skew
}

kurt<-function(x,meth="unbiased"){
    m.i<-function(x,i){
        n<-length(as.vector(x))
        m<-(1/n)*sum((x-mean(x))^i)
        m}

    n<-length(as.vector(x))
    if(meth=="unbiased"){
        kurtosis<-((n*(n+1))/((n-1)*(n-2)*(n-3)))*sum(((x-mean(x))/sd(x))^4)-
            ((3*((n-1)^2))/((n-2)*(n-3)))}

    if(meth=="moments"){
        kurtosis<-m.i(x,4)/(m.i(x,2)^2)}

    if(meth=="excess"){
        kurtosis<-(m.i(x,4)/(m.i(x,2)^2))-3}
    kurtosis
}

doornik2008 <- function(Y,Y.names=NULL){
    ## Doornik-Hansen's (2008) test of multivariate normality
    ## https://github.com/selcukorkmaz/MVN/blob/master/R/mvn.R

    n<-nrow(Y)
    p<-ncol(Y)
    S<-var(Y)
    S.d<-diag(S)
    V<-diag(S.d^(-0.5))
    C<-V%*%S%*%V
    L<-diag((eigen(C)$values)^-0.5)
    H<-eigen(C)$vectors
    y.i<-H%*%L%*%t(H)%*%V%*%t(Y-sapply(Y,mean))
    B.1<-apply(y.i,1,function(x){skew(x,"moments")})
    B.2<-apply(y.i,1,function(x){kurt(x,"moments")})

    ##i<-rep(1,p) ##Only useful for extremely large samples
    ##E<-((n%*%t(B.1)%*%B.1)/6)+((n*t(B.2-3*i)%*%(B.2-3*i))/24)

    ##z2
    del<-(n-3)*(n+1)*(n^2+(15*n)-4)
    a<-((n-2)*(n+5)*(n+7)*(n^2+(27*n)-70))/(6*del)
    c<-((n-7)*(n+5)*(n+7)*(n^2+(2*n)-5))/(6*del)
    k<-((n+5)*(n+7)*(n^3+37*n^2+(11*n)-313))/(12*del)
    alpha<-a+B.1^2*c
    chi<-(B.2-1-B.1^2)*2*k
    Z.2<-(((chi/(2*alpha))^(1/3))-1+(1/(9*alpha)))*((9*alpha)^(0.5))

    ##z1
    del<-(n-3)*(n+1)*(n^2+(15*n)-4)
    a<-((n-2)*(n+5)*(n+7)*(n^2+(27*n)-70))/(6*del)
    c<-((n-7)*(n+5)*(n+7)*(n^2+(2*n)-5))/(6*del)
    k<-((n+5)*(n+7)*(n^3+37*n^2+(11*n)-313))/(12*del)
    alpha<-a+B.1^2*c
    chi<-(B.2-1-B.1^2)*2*k
    Z.2<-(((chi/(2*alpha))^(1/3))-1+(1/(9*alpha)))*((9*alpha)^(0.5))

    ##z1=0k
    beta<-(3*(n^2+(27*n)-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
    w2<--1+((2*(beta-1))^0.5)
    del<-1/((log(sqrt(w2)))^0.5)
    y<-B.1*((((w2-1)/2)*(((n+1)*(n+3))/(6*(n-2))))^0.5)
    Z.1<-del*(log(y+(y^2+1)^0.5))

    E<-t(Z.1)%*%Z.1+t(Z.2)%*%Z.2
    ##multivariate
    multi<-data.frame(TS=E,df=2*p,pval=pchisq(E,2*p,lower.tail=FALSE))
    ##names(multi)<-c("E","df","P(Chi > E)")
    ##univariate
    if(is.null(Y.names)){Y.names=paste(rep("Y",p),seq(1,p),sep="")}
    Eu<-Z.1^2+Z.2^2
    pu<-pchisq(Eu,2,lower.tail=FALSE)
    univ<-data.frame(TS=Eu,df=rep(2,p),pval=pu)
    names(univ)<-c("E","df","P(Chi > E)")
    rownames(univ)<-Y.names

    result <- list(method = "Doornik-Hansen (2008)",
                   doi = "https://doi.org/10.1111/j.1468-0084.2008.00537.x",
                   statistic = multi$TS,
                   df = multi$df,
                   p.value = multi$pval)

    return(result)
}
