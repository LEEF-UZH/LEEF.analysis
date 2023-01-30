library(LEEF.analysis)
timestamps <- c(
  20221107, 20221109, 20221111, 20221114, 20221116, 20221118, 20221121, 20221123, 20221125, 20221128, 20221130,
  20221202, 20221205, 20221207, 20221209, 20221212, 20221214, 20221216, 20221219, 20221221, 20221223, 20221226, 20221228, 20221230,
  20230102, 20230104, 20230106, 20230109, 20230111, 20230113, 20230116, 20230118
)
report_flowcytometer(
    timestamp = timestamps,
    extracted_base_dir = "~/Duck/LEEFSwift3/LEEF_2.archived.data/LEEF/3.archived.data/extracted/",
    leef = "LEEF-2",
    output_dir = ".",
    format = "html",
    browse = FALSE
)
