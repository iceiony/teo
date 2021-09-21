.onLoad <- function(libname, pkgname) {
    options(
        teo_base_path = system.file('teo_pictures', package = 'teo')
    )
}
