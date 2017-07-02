### Problem with dcast when duplicates
# df <- structure(list(id = c("A", "B", "C", "A", "B", "C"), cat = c("SS","SS", "SS", "SV", "SV", "SV"), val = c(220L, 222L, 223L, 224L,225L, 2206L), class = "data.frame"))

id <- c("A", "B", "C", "A", "B", "C")
cat <- c("SS","SS","SS","SV","SV","SV")
val <- c(220L, 222L, 223L, 224L,225L, 2206L)    
df <- data.frame(id = id, cat=cat, val = val)
dcast(df, id~cat, value.var="val")

id = c("A", "B", "C", "A", "B", "C", "C")
cat = c("SS","SS", "SS", "SV", "SV", "SV", "SV")
val = c(220L, 222L, 223L,224L, 225L, 220L, 1L)
df2 <- data.frame(id = id, cat=cat, val = val)
dcast(df2, id~cat, value.var="val",function(x){ mean(x,na.rm=TRUE) })
