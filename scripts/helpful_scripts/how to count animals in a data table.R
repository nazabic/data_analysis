#to count how many animals I have:
unique(dt)[,
           .N
           ,by=c("treatment")]
