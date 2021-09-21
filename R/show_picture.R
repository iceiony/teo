#' Shows a random picture of Teo
#' @export 
teo <- function(){
  structure('teo',  class = 'teo_picture')
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
    content_path <- getOption('teo_base_path') %>% 
        fs::dir_ls(regexp = x) %>%
        sample(size = 1)
    
    if(!fs::file_exists(fs::path_temp(basename(content_path)))){
      fs::file_copy(content_path, fs::path_temp())
    }
    
    content_type <- fs::path_ext(content_path)
    
    content <- switch(content_type,
      'jpeg'=,
      'jpg' =,
      'png' = tags$image(
        x, 
        class = "centred", 
        src = fs::path('..', basename(content_path)),
        style = "height:100vh; display:block; margin-left:auto; margin-right:auto;"),
      'webm' =  tags$video(
          class = 'centred', controls = TRUE, autoplay = TRUE,
          style = "height:100vh; display:block; margin-left:auto; margin-right:auto;"
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