
library(lsr)
library(R.matlab)

SignificantTest <- function(vector1, vector2, alpha = 0.05)
{
  temp <- t.test(vector1, vector2, paired = TRUE, conf.level = alpha);
  
  t <- temp$statistic;
  df <- temp$parameter;
  p <- temp$p.value;
  interval <- temp$conf.int;
  
  cohens_d <- cohensD(vector1 - vector2)
  r2 <- (t^2) / (t^2 + df);
  
  result <- list();
  if (p >= alpha)
  {
    flag = 0;
  }
  else
  {
    if (mean(vector1) < mean(vector2))
    {
      flag = -1;
    }
    else
    {
      flag = 1;
    }
  }
  result$flag <- flag;
  result$cohens.d <- cohens_d;
  result$r2 <- r2;
  
  return(result);
  
}

main <- function()
{
  filenames <- dir('stat');
  
  result1_table <- matrix(nrow=45, ncol=3);
  colnames(result1_table) <- c('flag', 'cohens.d', 'r2');
  
  result2_table <- matrix(nrow=45, ncol=3);
  colnames(result2_table) <- c('flag', 'cohens.d', 'r2');
  
  for (i in 1:length(filenames))
  {
    data <- readMat(paste('stat/', filenames[i], sep = ''));
    PA <- data$mistakes.list.PA0[,10];
    HT1 <- data$mistakes.list.HT10[,10];
    HT2 <- data$mistakes.list.HT20[,10];
    
    result1 <- SignificantTest(PA, HT1);
    result2 <- SignificantTest(PA, HT2);
    
    result1_table[i,] <- c(result1$flag, result1$cohens.d, result1$r2)    
    result2_table[i,] <- c(result2$flag, result2$cohens.d, result2$r2)    
    
  }
  
  write.table(result1_table, 'significant_result1');
  write.table(result2_table, 'significant_result2');

}

main()



