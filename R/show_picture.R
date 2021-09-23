#'  Creates a token symbol that would display as a picture in the viewer
#' 
#'  The token names are used to filter through picture names before randomly
#'  selecting one for display.
#'  
#'  @param names (character) The token names.
#'  
#'  @return A toe_picture class object that can render in the RStudio viewer.
#' 
#' @export 
#' 
token <- function(names){
  structure(names,  class = 'teo_picture')
}

#' Arithmetic operator + for picture tokens
#' 
#' @param e1 (teo_picture) The first picture.
#' @param e2 (teo_picture) The second picture.
#' 
#' @return A new token that will filter images based on both pictures.
#' 
#' @export
#' 
`+.teo_picture` <- function(e1, e2){
  token(c(e1, e2))
}

#' Saves to the image database a given image
#' @param path (character) A local path or URL where the image can be found
#' @export
save_content <- function(
  path,
  type = 'teo'
) {
  file_number <- getOption('teo_base_path') %>% 
    fs::dir_ls(regexp = type) %>%
    length() + 1 
  
  local_path <- getOption('teo_base_path') %>% 
    fs::path(
      str_c(type, '_' , file_number), 
      ext = fs::path_ext(path))
  
  if(grepl("^(https|http)://", path)){
    curl::curl_download(url = path, dest = local_path)
  } else {
    fs::file_copy(path, local_path)
  }
  
  if(fs::path_ext(local_path) == 'mp4'){
    convert_to_webm(local_path)
  }
}

#' Converts a given video file to webm format
#' 
#' @param path (character) Local file path where the video file is located.
#' 
#' @export
#' 
convert_to_webm <- function(path){
  cmd <- str_glue("
    ffmpeg  -i {path}  -b:v 0  -crf 30  -pass 1  -an -f webm -y /dev/null;
    ffmpeg  -i {path}  -b:v 0  -crf 30  -pass 2  {fs::path_ext_remove(path)}.webm;
    rm {path};
  ")
  system(cmd);
}

#' Transforms a `teo` package function return value into a picture in the viewer
#' 
#' @param x (teo_picture class) An object returned by the other teo package functions.
#' 
#' @export
print.teo_picture <- function(x, ...){
  all_content <- getOption('teo_base_path') %>%  fs::dir_ls()
  all_names <- all_content %>% basename()
  
  content_path <- x %>% purrr::map(
      ~ all_content[str_detect(all_names, .x)]
    ) %>%
    purrr::reduce(intersect) %>%
    sample(size = 1)
  
  x <- str_flatten(x , ' + ')
  
  if(!fs::file_exists(fs::path_temp(basename(content_path)))){
    fs::file_copy(content_path, fs::path_temp())
  }
  
  content_type <- fs::path_ext(content_path)
  style <- "max-height:100vh; max-width:100vh;display:block; margin-left:auto; margin-right:auto;"
  
  content <- switch(
    content_type,
    'jpeg'=,
    'jpg' =,
    'png' = tags$image(
      x, 
      class = "centred", 
      src = fs::path('..', basename(content_path)),
      style = style),
    'webm' =  tags$video(
      class = 'centred', controls = TRUE, autoplay = TRUE,
      style = style
    ) %>% 
      tagAppendChild(
        tags$source(
          src = fs::path('..', basename(content_path)),
          type = fs::path('video', content_type)
        )),
    str_glue("content could not be rendered : {content_path}") 
  )
  
  htmlwidgets::createWidget(
    'example', list(),
    width = 1, height = 1
  ) %>%
    htmlwidgets::prependContent(content) %>%
    htmlwidgets:::print.htmlwidget()
}