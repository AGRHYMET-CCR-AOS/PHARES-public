# ============================================================
# Daily hydrological bulletin rendering
# ============================================================
project_dir <- "D:/CCR_AOS/ACTIVITES/AGRHYMET/2026/DCEM/Dev/bulletin/hydro_bulletin"
time_ref <- format(Sys.Date(), "%Y%m%d")

# ============================================================
# Output directory
# ============================================================

daily_word_dir <- file.path(
  "outputs",
  "bulletins_word",
  time_ref
)

dir.create(
  daily_word_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# Output file name
# ============================================================

output_file <- paste0(
  "Bulletin_hydrologique_",
  time_ref,
  ".docx"
)

pdf_output_file <- paste0(
  "Bulletin_hydrologique_",
  time_ref,
  ".pdf"
)
# ============================================================
# Copy required rendering files
# ============================================================

# QMD
file.copy(
  from = file.path(
    "templates",
    "bulletin_hydrologique.qmd"
  ),
  to = file.path(
    daily_word_dir,
    "bulletin_hydrologique.qmd"
  ),
  overwrite = TRUE
)

# Word reference document
file.copy(
  from = file.path(
    "templates",
    "reference_doc.docx"
  ),
  to = file.path(
    daily_word_dir,
    "reference_doc.docx"
  ),
  overwrite = TRUE
)

# ============================================================
# Render bulletin
# ============================================================
initial_dir <- getwd()
setwd(daily_word_dir)
quarto::quarto_render(
  input = "bulletin_hydrologique.qmd",
  output_format = "docx",
  output_file = output_file,
  execute_dir = ".",
  execute_params = list(
    bulletin_number = paste0("BH-", time_ref),
    issue_date = format(Sys.Date(), "%d/%m/%Y"),
    validity_date = format(Sys.Date() + 10, "%d/%m/%Y")
  )
)
doconv::docx2pdf(
  input = output_file,
  output = pdf_output_file
)

setwd(initial_dir)

# ============================================================
# Clean temporary rendering directory
# ============================================================
file.remove(c(file.path(daily_word_dir,"bulletin_hydrologique.qmd"),
              file.path(daily_word_dir,"reference_doc.docx")))

message(
  "Bulletin generated successfully: ",
  file.path(getwd(),daily_word_dir,output_file)
)