FPKMToTPM <- function(counts) {
  
  # Process one column at a time.
  tpm <-  {
    rate = sum(counts)
    (counts/rate) * 1e6
  }
  
  log2TPM = log2(tpm+1)
  # Copy the row and column names from the original matrix.
  colnames(log2TPM) <- colnames(counts)
  rownames(log2TPM) <- rownames(counts)
  
  return(log2TPM)
}