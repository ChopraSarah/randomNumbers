givePrimeNumbers <- function(n)
{
  n <- as.integer(n)
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}

PrimeNumbers_0_to_1000000 = givePrimeNumbers(1000000)
write.csv(a,'/Users/sarah/Desktop/TRU/DBMS/prime_numbers.csv')
hist(PrimeNumbers_0_to_1000000,main=NULL)

b<-c(2,3,5,7)
hist(b,main=NULL)
