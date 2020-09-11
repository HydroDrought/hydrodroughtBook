export_table <- function(x, name,
                         # outdir = file.path(tempdir(),
                         #                    "tables_hydrologicalDrought"),
                         outdir = ".",
                         Rdata = file.path(outdir, "tables.Rdata"),
                         xls = file.path(outdir, "tables.xlsx"))
{
  # import Rdata
  if(file.exists(Rdata)) load(Rdata) else tables <- list()

  # write to Rdata
  tables[[name]] <- x
  save(tables, file = Rdata)

  # write Rdata content to xls
  library(writexl)
  d <- dirname(xls)
  dir.create(d, recursive = T, showWarnings = F)
  writexl::write_xlsx(tables, path = xls)

  message("Tables written to  ", d)
  return(invisible(x))
}
