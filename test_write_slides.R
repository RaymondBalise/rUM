# REMOVE after testing & before merge to dev branch

devtools::load_all()

# no example, none template
write_slides('noExNone', example = FALSE, template = "none")

# no example, rmed template
write_slides('noExRmed', example = FALSE, template = "rmed2025")

# yes example, none template
write_slides('yExNone', example = TRUE, template = "none")

# yes example, rmed template
write_slides('yExRmed', example = TRUE, template = "rmed2025")

# yes example, other template
write_slides('yExOther', example = TRUE, template = "does_not_exist")
