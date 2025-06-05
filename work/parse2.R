library(datapackage)
pkgload::load_all()

dp <- open_datapackage("inst/employ")

dp

dta <- dp |> dp_get_data("employment")
dta

dp_categorieslist(dta$employ)

dp_to_code(dta$employ)

dp_to_factor(dta$employ)


dp |> dp_get_data("codelist-gender", standardise = FALSE)

