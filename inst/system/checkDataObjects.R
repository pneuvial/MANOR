library(R.utils);

rootPath <- "data";
rootPath <- Arguments$getReadablePath(rootPath);

pattern <- ".RData";
lf <- list.files(rootPath, pattern=pattern);

filename <- lf[1];
datPathname <- file.path(rootPath, filename);
lts <- load(datPathname);

## write each element to a distinct file
fn <- gsub(pattern, "", filename)
path <- file.path(rootPath, fn)
path <- Arguments$getWritablePath(path);

for (elt in elts) {
  dat <- eval(as.name(elt));
  eltFilename <- sprintf("%s.xdr", elt);
  pathname <- file.path(path, eltFilename);
  saveObject(dat, file=pathname);
}

## read elements from file
objPatt <- ".xdr";
filenames <- list.files(path, pattern=objPatt);
pathnames <- file.path(path, filenames);
objList <- lapply(pathnames, loadObject);

objNames <- gsub(objPatt, "", filenames);
names(objList) <- objNames;

## add all elements to a temporary environment
myEnv <- new.env();
for (oo in seq(along=objList)) {
  assign(objNames[oo], objList[[oo]], envir=myEnv);
}

ls(myEnv);
save(list=objNames, file=datPathname, envir=myEnv);
