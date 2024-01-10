##  This code is for creating xlsx file

## -----------------------------------------------------------------------------
##   Date                     Programmer
## ----------   --------------------------------------------------------------
##   Dec-28-2023    Md Yousuf Ali (md.ali@fda.hhs.gov)

# this function get attribute of dataframe and return attribute as dataframe
create_xlsx <- function(df) {

  df_label <- sapply(df, attr, 'label')
  df_format <- sapply(df,attr, 'format.sas')


  # variable names
  vars <- c()
  # format like $2
  form_sas <- c()
  fm <- df_format
  for (i in 1:length(fm)){
    vars <- c(vars, names(fm[i]))
    if( is.null(fm[[i]])) {
      format_sas <- NA
    } else {
      format_sas <- fm[[i]]
    }
    form_sas <- c(form_sas, format_sas)

  }

# get digit from $2
  leng <- c()
  # get $ or NA
  form <- c()
  for(i in 1:length(form_sas)) {

  if(!is.na(form_sas[i])) {

    dg <- unlist(strsplit(form_sas[i], '\\$'))[2]
    dol <- unlist(strsplit(form_sas[i], ''))[1]
    leng <- c(leng,dg)
    form <- c(form, dol)
  } else {
    leng <- c(leng, '8')
    form <- c(form, NA)

  }
  }
  leng <- as.integer(leng)

  type_var <- unlist(lapply(df,class))

  df_fn <- data.frame(Variable = names(df_label),
                      Label = df_label,
                      Type = type_var,
                      Length = leng,
                      Format = form,
                      row.names = NULL)

}

# example
## df <- haven::read_xpt('cl.xpt')
## kd <- create_xlsx(df)
## kd

## this function read edited/downloaded xlsx file,
## it get metadata from variable metadate sheet
## and add the meta data to domain dataframe

get_xlsx <- function(file) {

  v_meta <- readxl::read_xlsx(file, sheet = 'Variable Metadata')
  vm <- gsub('character', 'text', v_meta$Type)
  df_xl <- readxl::read_xlsx(file, sheet = 3, col_types = vm)
  df_xl[] <- lapply(df_xl, function(x) ifelse(is.character(x) & is.na(x), "", x))
for (i in 1:length(v_meta$Variable)) {
  attr(df_xl[[i]], 'label') <- v_meta$Label[[i]]
  if(!is.na(v_meta$Format[i])) {
    formSAS <- paste0(v_meta$Format[i], v_meta$Length[i])
    attr(df_xl[[i]], 'format.sas') <- formSAS
  ## print(formSAS)
  } else {
    attr(df_xl[[i]], 'format.sas') <- NULL
  }
}
  return(df_xl)
}
 #example
## d <- get_xlsx('')
## df_xl <- get_xlsx('bw_func.xlsx')
## str(df_xl)
