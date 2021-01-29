## This is a try to formulate own function for mapping square matrix
## Works only for matrices with even number of rows and columns.

## Created: FrK 2021-01-01
## Edited:  FrK 2021-01-07

library(dplyr)
library(ggplot2)


# Visualizing function
map.viz = function(m, br = 4, labs = FALSE, path = FALSE, cp = FALSE){
  # Preparing matrix parameters
  r = nrow(m)
  c = ncol(m)

  # Fixing of missing row and column names
  if (is.null(colnames(m))) colnames(m) = 1:ncol(m)
  if (is.null(row.names(m))) row.names(m) = 1:nrow(m)

  # Preparing data
  values = m %>% as.vector()
  x = rep(colnames(m), r) %>% matrix(data = ., nrow = r, byrow = TRUE) %>% as.vector() %>% as.numeric()
  y = rep(row.names(m), c) %>% matrix(data = ., nrow = r, byrow = FALSE) %>% as.vector() %>% as.numeric()
  dat = data.frame(x, y, values) %>% arrange(values) %>% mutate(Svalues = as.character(values))
  dfp = dat[dat$values!=-1,]
  if (cp) {
    dfp = rbind(dfp, dfp[1,])
    mxv = max(dfp$values) + 1
    dfp[dfp$Svalues == "0",]$Svalues = paste0("0=",mxv)

  }

  # Plotting prepared data
  mv = ggplot(dat, aes(x, y, fill = values, label = Svalues)) +
    geom_tile() +
    geom_text(data = dfp) +
    geom_path(data = dfp) +
    scale_x_continuous(breaks = seq(2, max(x), br)) +
    scale_y_continuous(breaks = seq(min(y), max(y), by = br)) +
    theme_minimal() +
    scale_color_gradient2(low ="red", mid = "yellow", high = "blue",
                          midpoint = ((min(values) + max(values)) / 2),
                          breaks = seq(min(values), max(values), 2),
                          aesthetics = "fill")

  # Erasing unwanted layers
  if (!path) mv[["layers"]][[3]] = NULL
  if (!labs) mv[["layers"]][[2]] = NULL

  # Printing graph
  mv
}

# Mapping function
map = function(m, viz = TRUE) {
  # Checking matrix parameters
  r = nrow(m)
  c = ncol(m)
  if ((r %% 2) != 0) stop("Odd number of rows -- we have to stop.")
  if ((c %% 2) != 0) stop("Odd number of cols -- we have to stop.")

  # Preparing parameters for mapping
  mp = m

  # Mapping in two loops
  for (i in 1:c){
    for (j in 1:r){
      if ((i %% 2) == 1 & j <= r/2) mp[1+(2*(j-1)), 1+((i-1)/2)] = m[j,i]
      if ((i %% 2) == 0 & j <= r/2) mp[2*j, i/2] = m[j,i]
      if ((i %% 2) == 1 & j > r/2) mp[1+(2*(j-(1+(r/2)))), (c/2)+1+((i-1)/2)] = m[j,i]
      if ((i %% 2) == 0 & j > r/2) mp[2*(j-(r/2)), (c/2)+(i/2)] = m[j,i]
    }
  }
  if (viz) map.viz(mp) %>% print()
  mp
}

# Reverse mapping function
map.rev = function(m, viz = TRUE) {
  # Checking matrix parameters
  r = nrow(m)
  c = ncol(m)
  if ((r %% 2) != 0) stop("Odd number of rows -- we have to stop.")
  if ((c %% 2) != 0) stop("Odd number of cols -- we have to stop.")

  # Preparing parameters for mapping
  mp = m

  # Mapping in two loops
  for (i in 1:c){
    for (j in 1:r){
      if ((i %% 2) == 1 & j <= r/2) mp[j,i] = m[1+(2*(j-1)), 1+((i-1)/2)]
      if ((i %% 2) == 0 & j <= r/2) mp[j,i] = m[2*j, i/2]
      if ((i %% 2) == 1 & j > r/2) mp[j,i] = m[1+(2*(j-(1+(r/2)))), (c/2)+1+((i-1)/2)]
      if ((i %% 2) == 0 & j > r/2) mp[j,i] = m[2*(j-(r/2)), (c/2)+(i/2)]
    }
  }
  if (viz) map.viz(mp) %>% print()
  mp
}

# Finding matrix orbit --
# Answering question: After how many steps I will get same matrix?
map.orb = function(m, viz = 0, echo = 0,  maxTime = 1000) {
  broken = FALSE
  old = m
  new = map(m, FALSE)
  tick = 1
  while(!((old == new) %>% unique() %>% all())){
    new = map(new, FALSE)
    tick = tick + 1
    if (viz > 0 & (tick%%viz) == 0) {
      map.viz(new) %>% print()
      cat(paste0('Viz at tick no. ', tick, '.\n'))
    }
    if (echo > 0 & (tick%%echo) == 0)print(tick)
    if (maxTime == tick) {
      cat("Time is over! We break mapping now.\n")
      broken = TRUE
      break
    }
  }
  if (!broken) print(paste0('Converged after ', tick, ' ticks.'))
  # map.viz(new)
  tick
}

# Measuring similarity during seeking orbit --
# Answering question: How much is new matrix similar to original one?
map.sim = function(m, viz = 0, echo = 0, dev = 0, maxTime = 1000) {
  broken = FALSE
  old = m
  new = map(m, FALSE)
  tick = 1
  similarity = rep(0, maxTime)
  similarity[tick] = (old == new) %>% sum()
  similarity[tick] =  (similarity[tick] / (ncol(m) * nrow(m))) %>% round(digits = 5)
  while(!((old == new) %>% unique() %>% all())){
    new = map(new, FALSE)
    tick = tick + 1
    if (viz > 0 & (tick%%viz) == 0) {
      map.viz(new) %>% print()
      print(paste0('Viz at tick no. ', tick, '.'))
    }
    similarity[tick] = (old == new) %>% sum()
    similarity[tick] =  (similarity[tick] / (ncol(m) * nrow(m))) %>% round(digits = 5)
    if (echo > 0 & (tick%%echo) == 0) {
      print(paste0('Similarity ', similarity[tick], ' at tick no. ', tick, '.'))
    }
    if (dev > 0 & (tick%%dev) == 0){
      (tibble(Ticks = ((1 + tick - dev):tick), Similarity = similarity[(1 + tick - dev):tick]) %>%
        ggplot(aes(x = Ticks, y = Similarity)) + geom_line() + theme_minimal()) %>%
        print()
    }
    if (maxTime == tick) {
      print("Time is over! We break mapping now.")
      broken = TRUE
      break
    }
  }
  if (!broken) {
    print(paste0('Converged after ', tick, ' ticks.'))
    (tibble(Ticks = (1:tick), Similarity = similarity[1:tick]) %>%
      ggplot(aes(x = Ticks, y = Similarity)) + geom_line() + theme_minimal()) %>%
      print()
  }
  tibble(Similarity = similarity[1:tick])
}

# Exploring combinations of 'x' and 'y' of matrices --
# We use functions to explore space of combinaions of values to find how quickly we find orbit.
map.exp = function(strt = 2, fnsh = 20, maxTime = 1000000){
  # Matrix and list for storing results
  matSize = 1 + ((fnsh - strt) / 2)
  xNames = seq(strt, fnsh, 2)
  yNames = seq(strt, fnsh, 2)
  orb = matrix(data = rep(-1, matSize^2), ncol = matSize, dimnames = list(yNames, xNames))

  # Loops for exploration
  for (x in xNames){
    for (y in yNames){
      # For not double computing we compare X and Y, for X > Y we copy stored results
      if (y >= x & (orb[y/2, x/2] == -1)) {
        # Construction of matrix for input
        mtr = matrix(data = 1:(x*y), nrow = y, ncol = x)
        print(paste0("Computing X=", x,", Y=", y, ":"))

        # Seeking orbit and storing results
        orb[y/2, x/2] = map.orb(mtr, viz = 0, echo = 1000, maxTime = 1000000)
      } else {
        orb[y/2, x/2] = orb[x/2, y/2]
      }
      save(orb, file = "ORB.R")
    }
  }
  orb
}

# Exploring just squares, i.e. 'x' == 'y' matrices --
# We use functions to explore space of square values to find how quickly we find orbit.
# Square seems good aproximation -- hard sizes becomes harder and easy easier
sqr.exp = function(strt = 2, fnsh = 20, maxTimeSE = 1000000){
  # Matrix and list for storing results
  vecSize = 1 + ((fnsh - strt) / 2)
  xNames = seq(strt, fnsh, 2)
  orb = matrix(data = rep(-1, vecSize), ncol = vecSize, nrow = 1)

  # Loops for exploration
  for (x in xNames){
      # For not double computing we compare X and Y, for X > Y we copy stored results
        # Construction of matrix for input
        mtr = matrix(data = 1:(x^2), nrow = x, ncol = x)
        print(paste0("Computing X=", x,", Y=", x, ":"))

        # Seeking orbit and storing results
        orb[1, (x - strt + 2)/2] = map.orb(mtr, viz = 0, echo = 50000, maxTime = maxTimeSE)
        save(orb, file = "ORB.RData")
  }
  orb
}

# Exploring combinations of 'x' and 'y' of matrices --
# We use functions to explore space of combinaions of values to find how quickly we find orbit.
map.exp2 = function(xVals = c(2, 4), yVals = c(2, 4), echoME2 = 5000, maxTimeME2 = 100000){
  # Matrix and list for storing results
  xSize = length(xVals)
  ySize = length(yVals)
  orb = matrix(data = rep(1, (xSize * ySize)),
               ncol = xSize, dimnames = list(yVals, xVals))

  # Loops for exploration
  for (xop in 1:xSize){  # 'xop' stands for 'X of Orb mtrix Position', similarily, 'yop' stands for...
    for (yop in 1:ySize){
      # Preparing explored 'x' and 'y'
      x = xVals[xop]
      y = yVals[yop]

      # Construction of matrix for 'shuffeling'/manipulation, exploration...
      mtr = matrix(data = 1:(x*y), nrow = y, ncol = x)
      print(paste0("Computing X=", x,", Y=", y, ":"))

      # Seeking orbit and storing results
      orb[yop, xop] = map.orb(mtr, viz = 0, echo = echoME2, maxTime = maxTimeME2)
      save(orb, file = "orbME2.RData")
    }
  }
  orb
}

# Function for printing/plotting trajectory of matrix's cell given by 'cv' (cell value)
map.atr = function(m, cv = 2, maxTimeMA = 100){
  # Tests
  if (length(m[m==cv]) > 1 ) stop("'cv' is not unique value in the matrix 'm'")
  if (length(m[m==cv]) < 1 ) stop("'cv' is not present in the matrix 'm'")

  # Preparation of matrices and other stuff
  atr = matrix(data = rep(-1, length(m)), ncol = ncol(m))
  o = map.orb(m, maxTime = maxTimeMA)

  # For cycle for tracing attractor
  for (step in 0:o) {
    if (atr[which(m == cv)] == 0) {
      cat("Attractor ", cv, " closed after ", step, " steps.\n")
      break
    }
    atr[which(m == cv)] = step
    m = map(m, viz = FALSE)
    if (step == o){
      if (atr[which(m == cv)] == 0) cat("Attractor ", cv, " closed exactly after ", step, " steps.\n") else
        cat("Attractor ", cv, " still doesn't reach its start after ", step, " steps.\n")
    }
  }
  atr
}

# Function for comparing attractors
cmp.atr = function(mca, atrs = c(2, 3,), overlap = FALSE, path = FALSE, maxTimeCA = 100){
  # Preparing matrix for storing attractors
  cam = matrix(data = rep(0, length(mca)), ncol = ncol(mca))

  # For cycle for finding/storing all attractors
  for (a in atrs) {
    if (cam[a]==0 | overlap | path){
      trj = map.atr(m = mca, cv = a, maxTimeMA = maxTimeCA)
      if (path) cam = cam + trj + 1 else cam[trj>-1] = cam[trj>-1] + a
    }
  }
  cam[cam == 0] = -1
  cam
}

# Function for getting vector with lengths of attractors
len.atr = function(mla, atrs = c(2,3), maxTimeLA = 100, immediateStop = TRUE){
  # Preparation
  al = length(atrs)
  vl = rep(-1, al)

  # For cycle for getting lengths
  for (p in 1:al){
    atr = atrs[p]
    trj = map.atr(m = mla, cv = atr, maxTimeMA = maxTimeLA)
    atl = max(trj)
    if ((atl == maxTimeLA) & immediateStop)
      stop(paste0("Attractor probably doesn't reached it's start even after ",
                  maxTimeLA, " steps. We stop here."))
    vl[p] = atl + 1
  }
  vl
}



### Using functions
# Finding attractors
mat = matrix(data = 1:(128*128), ncol = 128)
cmp.atr(m = mat, atrs = c(2, 4, 6, 8, 10, 12, 14, 16, 26, 27, 28, 30, 31, 32, 44), maxTimeCA = 100, overlap = T) %>%
  map.viz(labs = T, path = F, cp = F, br = 2)

atx = cmp.atr(m = mat, atrs = c(seq(122, 128, 6)), overlap = T, path = T, maxTimeCA = 200)
atx %>% map.viz(labs = T, path = F, cp = F, br = 5)
(atx <= 130) %>% all()

map.atr(mat, 1418, maxTimeMA = 1000) %>% map.viz(labs = T, path = T, cp = (max(.) < 1000), br = 10)

# Getting attractors length
mat = matrix(data = 1:(18*18), ncol = 18)
atl = len.atr(mat, atrs = seq(2, (18*9), 1), maxTimeLA = 100) %>% unique() %>% sort()
atl %>% sort()
((19 * 5 * 3 * 13 * 16 * 11 * 17 * 7) %% atl) %>% sum()

mat = matrix(data = 1:(54*54), ncol = 54)
atl2 = len.atr(mat, atrs = seq(2, (54*27), 1), maxTimeLA = 1000) %>% unique() %>% sort()
atl2 %>% sort()
((16 * 9 * 5 * 7 * 11 * 13 * 17 * 19 * 31 * 47 * 53 * 61) %% atl) %>% sum()

atx2 = cmp.atr(m = mat, atrs = c(seq(1418, 1420, 1)), overlap = T, path = F, maxTimeCA = 200)
atx2 %>% map.viz(labs = T, path = F, cp = F, br = 5)

# For matrices 2^n x 2^m the majority of attractors have length n+m, only few are shorter.
# So we could compute how many orbits/attractors are in the matrix by computing:
# floor((2^n * 2^m) / (n + m))
# the number of resting cells we get by computing:
# (2^n * 2^m) %% (n + m)
# For n = m = c(6, 7) the resting cells are just 4, for n = m = 10 it is 16,
# it is obvious it should be at least 2 because cells [1, 1] and [n, m] don't move,
# so they have length 1, so the product of (2^n * 2^m) %% (n + m) i.e. 'number of resting cells'
# should be number in interval:
# <2, (m + n - 2)> for (m + n) > 4

# So let's inspect the development of 'number of resting cells':
size = 20
resting = matrix(data = rep(0, size^2), ncol = size)
attract = matrix(data = rep(0, size^2), ncol = size)
for (m in 1:size) {
  for (n in m:size) {
    resting[m, n] = (2^n * 2^m) %% (n + m)
    if (resting[m, n] == 0) resting[m, n] = (m + n)
    resting[n, m] = resting[m, n]
    attract[m, n] = floor((2^n * 2^m) / (n + m))
    attract[n, m] = attract[m, n]
  }
}
resting %>% map.viz(br = 2)
attract %>% log2() %>% map.viz(br=2)

# Exploration itself
yVals = seq(2, 128, 2)
xVals = c(seq(2, 16, 2), 32, 64, 128, 256, 512, 1024, 2048, 4096)
res2 = map.exp2(xVals = xVals, yVals = yVals, echoME2 = 50000, maxTimeME2 = 262145)
res2 %>% log2() %>% map.viz()
res2

res = map.exp(fnsh = 16)
res %>% log2() %>% map.viz()
res

# Visualization of first exploration
load("RES.RData")
res %>% log2() %>% map.viz()
res

# Visualization of non-finished exploration
load("orbME2.RData")
m = orb[,c(1:5)]
m[m==262145 | m==-1] = 1
m[,c(1,2,4)] %>% log2() %>% map.viz()
m[,c(1,2,4)]
m %>% log2() %>% map.viz()
m


# Viz of combined finished and unfinished explorations
load("orbME2.RData")
load("RES.RData")
mx = rbind(res[, c(1:8)], orb[,c(1:8)])
mx[mx==1000000 | mx==262145 | mx==-1] = 1
mx[, 1:8] %>% log2() %>% map.viz()
mx


# Strangely long odd attractors
modd = orb[orb<262145 & orb > -1 & (orb%%2 == 1)] %>% max()  # Longest odd matrix conversion
which(orb==modd, arr.ind = TRUE)
mat = matrix(data = 1:(256*368), ncol = 368)
ma3 = map.atr(mat, 3, maxTimeMA = 100)
ma4 = map.atr(mat, 4, maxTimeMA = 200)
ma3 %>% map.viz(labs = T, path = T, cp = (max(.) < 100), br = 10)
ma4 %>% map.viz(labs = T, path = T, cp = (max(.) < 200), br = 10)



# --- Rubish code ---

# Testing
mat = map(mat)
mat = map.rev(mat)
map.viz(mat)
map.orb(mat, viz = 10000, echo = 0, maxTime = 100000)
map.sim(mat, dev = 1000, echo = 1000, maxTime = 100000) %>% ggplot(aes(x = Similarity)) + geom_histogram(bins = 16) + scale_y_log10()
map.exp2(yVals = seq(34, 320, 2), xVals = c(seq(2, 16, 2), 32, 64, 128, 256))

# Generating matrix
mat = matrix(data = 1:(1024*1024), nrow = 1024, ncol = 1024)
mat = matrix(data = rep(1:4, 2), ncol = 2)

# --- End of rubish code ---
