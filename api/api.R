library(plumber)
library(pak)

get_latest_archived_version <- function(package_name, repos = "https://cran.r-project.org") {
  # Construct the URL for the package's archive
  archive_url <- paste0(repos, "/src/contrib/Archive/", package_name, "/")
  
  # Get the list of files in the archive
  archive_content <- tryCatch({
    readLines(archive_url)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(archive_content)) {
    return(NULL)  # Package not found in archive
  }
  
  # Extract version numbers from file names
  pattern <- paste0(package_name, "_(\\d+\\.\\d+(\\.\\d+)*)\\.tar\\.gz")
  versions <- regmatches(archive_content, regexpr(pattern, archive_content))
  versions <- sub(pattern, "\\1", versions)
  
  if (length(versions) == 0) {
    return(NULL)  # No versions found
  }
  
  # Sort versions and return the latest
  latest_version <- sort(package_version(versions), decreasing = TRUE)[1]
  return(as.character(latest_version))
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
}

packageDepsList <- function(deps, av_pack) {
  paste0(av_pack[av_pack$Package %in% deps, ]$Package, "@", av_pack[av_pack$Package %in% deps, ]$Version)
}


read_token <- function(filename, variable_name) {
  tryCatch({
    token <- readChar(filename, file.info(filename)$size - 1)
    assign(variable_name, token, envir = parent.frame())
    return(NULL)  # Success, no error message
  }, error = function(e) {
    return(paste("Error reading token file for", variable_name, ":", e$message))
  })
}


download_rspm_binary <- function(ppm_url = "http://packagemanager:4242", token) {
  if (!file.exists("rspm")) {
    tryCatch({
      system_result <- system(
        paste0(
          'rm -f rspm && curl -fLOJH "Authorization: Bearer ',
          token,
          '" "',
          ppm_url,
          '/__api__/download" && chmod +x ./rspm'
        ),
        intern = TRUE
      )
      print(paste(
        "Debug: rspm download result:",
        paste(system_result, collapse = "\n")
      ))
    }, error = function(e) {
      return(list(error = paste(
        "Error downloading rspm:", e$message
      )))
    })
  }
}

#* Get full list of packages including dependencies
#* @param input_list:list List of main R packages
#* @param pm URL of packagemanager
#* @param repo Repository to use (cran, bioconductor,...)
#* @param snapshot Date of snapshot for CRAN (YYYY-MM-DD)
#* @param deps:list Dependency Type (cf. https://pak.r-lib.org/reference/pkg_deps.html)
#* @param debug:logical If TRUE, print debug messages
#* @return List of packages, their versions and their dependencies
#* @post /package_list

function(input_list = c("nlmixr2", "mschubert/clustermq"),
         ppm_url = "http://packagemanager:4242",
         repo = "cran",
         snapshot = "2025-01-08",
         deps = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
         debug = FALSE) {
  # Set the repos option
  options(repos = c(CRAN = paste(ppm_url, repo, snapshot, sep = "/")))
  
  if (debug) {
    print(paste("Debug: using dependencies", deps))
    print("Debug: repo options")
    print(paste(options()$repos))
    print("Debug: input_list")
    print(input_list)
  }

  saveRDS(input_list,"/tmp/pkgs.rds")
  
  # get dependencies
  pkg_deps <- pak::pkg_deps(c(input_list,"*=?ignore-unavailable"), dependencies = deps)
  
  file.remove("/tmp/pkgs.rds")

  pkg_deps$repotype
  
  # Construct the result based on the repotype

  result <- data.frame(
    name = pkg_deps$package,
    version = pkg_deps$version,
    URN = ifelse(is.na(pkg_deps$repotype),
                pkg_deps$ref,
                paste0(pkg_deps$package, "@", pkg_deps$version)),
    stringsAsFactors = FALSE
  )

return(result)
}

#* Upload a package
#* @param pkg_file:file the file to upload
#* @param pkg_type "binary" or "source"
#* @param ppm_url URL of the package manager
#* @param ppm_source name of source on PPM
#* @post /upload_package
function(pkg_file, pkg_type, ppm_url, ppm_source) {
  # Check if a file was uploaded
  if (is.null(pkg_file)) {
    return(list(error = "No file uploaded (file is NULL)"))
  }
  
  # If file is a list with a single element that is raw data, process it
  if (is.list(pkg_file) &&
      length(pkg_file) == 1 && is.raw(pkg_file[[1]])) {
    raw_data <- pkg_file[[1]]
    temp_file <- tempfile()
    writeBin(raw_data, temp_file)
    file <- list(name = basename(temp_file), datapath = temp_file)
  } else if (!is.list(pkg_file) ||
             !all(c("name", "datapath") %in% names(pkg_file))) {
    return(list(error = "Unable to process the file object", file_structure = capture.output(str(file))))
  }
  
  # Define the path where the file will be saved
  save_path <- file.path("/api", file$name)
  
  # Debug: Print save path
  print(paste("Debug: Save path:", save_path))
  
  # Debug: Print save path
  print(paste("Debug: ppm_url:", ppm_url))
  
  # Check if the source file exists
  if (!file.exists(file$datapath)) {
    return(list(error = paste(
      "Source file does not exist:", file$datapath
    )))
  }
  
  
  # Save the uploaded file
  tryCatch({
    copy_result <- file.copy(file$datapath, save_path)
    print(paste("Debug: File copy result:", copy_result))
    
    if (!copy_result) {
      return(list(
        error = "Failed to copy file",
        from = file$datapath,
        to = save_path
      ))
    }
  }, error = function(e) {
    return(list(error = paste("Error copying file:", e$message)))
  })
  
  #
  
  # Check if the file was actually saved
  if (!file.exists(save_path)) {
    return(list(error = paste("File was not saved at:", save_path)))
  }
  
  #return(system(paste("ls -l",save_path)))
  
  
  # Read PPM source token
  read_token("/var/lib/token/testing-source-r.token",
             "ppm_source_token")
  
  Sys.setenv("PACKAGEMANAGER_ADDRESS" = ppm_url)
  Sys.setenv("PACKAGEMANAGER_TOKEN" = ppm_source_token)
  
  # Download rspm if needed
  download_rspm_binary(ppm_url = ppm_url, token = ppm_source_token)
  
  
  tryCatch({
    if (pkg_type == "binary")
    {
      system(
        paste0(
          "./rspm add binary ",
          "--distribution=internal --source=testing-source-r ",
          "--path=",
          save_path
        )
      )
    }
    else if (pkg_type == "source")
    {
      system(paste0(
        "./rspm add --source=testing-source-r ",
        "--path=",
        save_path
      ))
    }
  }, error = function(e) {
    return(list(error = paste(
      "Error uploading", type, "package :", e$message
    )))
  })
  
  # Delete saved file
  unlink(save_path)
  
  # Return a success message with the file path
  return(
    list(
      message = "File upload attempt completed",
      path = save_path,
      copy_success = TRUE,
      file_size = file.size(save_path)
    )
  )
  
}


find_content_guid <- function(client, content_title) {
  # Retrieve all content
  all_content <- connectapi::get_content(client)
  
  # Filter content by title
  content <- all_content[all_content$title == content_title, ]
  
  # Check if content is found and return the GUID
  if (nrow(content) > 0) {
    return(content$guid)
  } else {
    stop("Content not found")
  }
}

upload_metadata <- function(repo="", type="", name="", version="", key="", value="") {
  if (type == "validation_strategy" || type == "validation_plan" || type == "validation_report") {
    cmd <- paste0(
      "./rspm create metadata --repo='",
      repo,
      "' --key='",
      key,
      "' --value='",
      value,
       "' --succeed-on-existing ",
      "--update-existing"
    )
    print("validation_*")
    print(cmd)
    system(cmd)
  } else {
    cmd <- paste0(
      "./rspm create metadata --package-name='",
      name,
      "' --version='",
      version,
      "' --key='",
      key,
      "' --value='",
      value,
      "' --succeed-on-existing ",
      "--update-existing"
    )
    print("package")
    print(cmd)
    system(cmd)
  }
}


create_temp_dir <- function(prefix = "temp") {
  # Create a unique directory name within the system temp directory
  temp_path <- file.path(tempdir(), paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1)))
  
  # Create the directory
  dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)
  
  # Return the path to the created directory
  return(temp_path)
}

#* Produce Outputs and add metadata to PPM
#* @param type of content
#* @param data used to render type
#* @param desc package description
#* @param ppm_url URL of the package manager
#* @param connect_url URL of the connect server
#* @post /render_content
function(type="validation_strategy",
         data,
         desc="",
         main_packages="",
         extra_packages="",
         ppm_url = "http://packagemanager:4242",
         connect_url = "http://connect:3939")
{
  print(ppm_url)
  
  library(connectapi)
  
  # Read PPM source token
  print("Debug: Reading PPM source token")
  read_token("/var/lib/token/testing-source-r.token",
             "ppm_source_token")
  
  Sys.setenv("PACKAGEMANAGER_ADDRESS" = ppm_url)
  
  print("Debug: Download rspm binary if needed")
  download_rspm_binary(ppm_url = ppm_url, token = ppm_source_token)
  
  # Read PPM metadata API key
  read_token("/var/lib/token/metadata.token", "ppm_metadata_token")
  Sys.setenv("PACKAGEMANAGER_TOKEN" = ppm_metadata_token)
  
  Sys.setenv(PACKAGEMANAGER_ADDRESS = ppm_url)
  
  # Read Connect API key and connect to Posit Connect
  print("Debug: Reading connect api key")
  read_token("/var/lib/token/connect-bootstrap.key", "connect_apikey")
  
  Sys.setenv(CONNECT_SERVER = connect_url)
  
  print("Debug: Rendering doc")

  temp_dir<-create_temp_dir("render")
  print(paste("Debug: temp dir:",temp_dir))

  if(type == "validation_strategy" || type == "validation_plan" || type == "validation_report") {
    if(type == "validation_strategy") {
      quarto::quarto_render(
        input = paste0("/api/",type,".qmd"),
        execute_params = list(),
        output_file = paste0(type, ".html"),
        quarto_args = c("--output-dir", temp_dir)
      )
    }
    
    if(type == "validation_plan") {
      quarto::quarto_render(
        input = paste0("/api/",type,".qmd"),
        execute_params = list(r_version=data$r_version, 
          snapshot_date=data$snapshot_date, 
          main_packages=data$main_packages,
          extra_packages=data$extra_packages,
          validation_strategy_url=data$validation_strategy_url
        ),
        quarto_args = c("--output-dir", temp_dir),
        output_file = paste0(type, ".html")
      )
    }

    if(type == "validation_report") {

      quarto::quarto_render(
        input = paste0("/api/",type,".qmd"),
        execute_params = list(r_version=data$r_version, 
          snapshot_date=data$snapshot_date, 
          main_packages=data$main_packages,
          validation_strategy_url=data$validation_strategy_url,
          validation_plan_url=data$validation_plan_url
        ),
        quarto_args = c("--output-dir", temp_dir),
        output_file = paste0(type, ".html")
      )
    }

    # Connect to the server
    client <- connectapi::connect(
      server = "http://connect:3939",
      api_key = connect_apikey
    )

    # create manifest.json
    rsconnect::writeManifest(appDir=temp_dir)

    # Create a new bundle 
    bundle<-connectapi::bundle_dir(temp_dir)

    # Deploy the bundle 
    content <- client %>%
      connectapi::deploy(bundle, name = type) %>%
      connectapi::poll_task()

    # Allow access for all
    content$update(access_type = "all")

    # set vanity url

    content %>%
      connectapi::set_vanity_url(paste0("/",type), force=TRUE)

    dep_url <- paste0("http://localhost:3939",connectapi::get_vanity_url(content))

    Sys.setenv(PACKAGEMANAGER_TOKEN = ppm_metadata_token)
    
    # Upload Metadata
    upload_metadata(repo="tested-r",
                    type=type,
                    key=type,
                    value=dep_url)
    
    return(list(message = "Rendering completed", results = list(url=dep_url)))
  }

  if(type == "package") {

    print("Debug: Convert to tibble")

    # Load furrr package for parallel processing
    library(furrr)
    library(future)

    # Set up parallel processing
    plan(multisession, workers = 2)  # Adjust number of workers as needed
    # Convert data to tibble if it's not already
    data_tibble <- tibble::as_tibble(data)

    print("Debug: Start furrr to walk through all packages in parallel")
    # Loop through each row of the tibble in parallel
    results <- furrr::future_pmap(data_tibble, function(...) {
      row <- tibble::tibble(...)

      # Access individual variables
      name <- row$name
      version <- row$version
      nameversion <- row$nameversion
      print(paste("Debug 1: ", row$iq_stdout))
      print(paste("Debug 2: ", row$iq_stderr))
      row <- tibble::tibble(...)
      
      # Access individual variables
      name <- row$name
      version <- row$version
      nameversion <- row$nameversion
      print(paste("Debug 1: ", row$iq_stdout))
      print(paste("Debug 2: ", row$iq_stderr))
      
      iq_stdout <- if (is.list(row$iq_stdout))
        row$iq_stdout[[1]]
      else
        row$iq_stdout
      iq_stderr <- if (is.list(row$iq_stderr))
        row$iq_stderr[[1]]
      else
        row$iq_stderr
      oq_stdout <- if (is.list(row$oq_stdout))
        row$oq_stdout[[1]]
      else
        row$oq_stdout
      oq_stderr <- if (is.list(row$oq_stderr))
        row$oq_stderr[[1]]
      else
        row$oq_stderr
      pq_stdout <- if (is.list(row$pq_stdout))
        row$pq_stdout[[1]]
      else
        row$pq_stdout
      pq_stderr <- if (is.list(row$pq_stderr))
        row$pq_stderr[[1]]
      else
        row$pq_stderr
      risk_number <- row$risk_number
      risk_category <- ifelse(risk_number < 1/3, "Low", ifelse(risk_number < 2/3, "Medium" , "High")) 
      # Process each package
      print(paste("Debug: Processing package:", name, "version:", version))
      print(paste("Debug: ", desc[[name]]))
      
      filename <- paste0(name, "-", version)
      
      quarto::quarto_render(
        input = "/api/testing.qmd",
        execute_params = list(
          name = name,
          version = version,
          description = desc[[name]],
          iq = list(stdout = iq_stdout, stderr = iq_stderr),
          oq = list(stdout = oq_stdout, stderr = oq_stderr),
          pq = list(stdout = pq_stdout, stderr = pq_stderr)
          #readLines(con = filename)
        ),
        quarto_args = c("--output-dir", temp_dir),
        output_file = paste0(filename, ".html")
      )

      vanity_url <- gsub("\\.", "_", paste0(name,".",version))

      # Connect to the server
      client <- connectapi::connect(
        server = "http://connect:3939",
        api_key = connect_apikey
      )

      # create manifest.json
      rsconnect::writeManifest(appDir=temp_dir)

      # Create a new bundle 
      bundle<-connectapi::bundle_dir(temp_dir)

      # Deploy the bundle 
      content <- client %>%
        connectapi::deploy(bundle, name = vanity_url) %>%
        connectapi::poll_task()

      # Allow access for all
      content$update(access_type = "all")

      # set vanity url

      content %>%
        connectapi::set_vanity_url(vanity_url, force=TRUE)

      dep_url <- paste0("http://localhost:3939",connectapi::get_vanity_url(content))
      
      Sys.setenv(PACKAGEMANAGER_TOKEN = ppm_metadata_token)
      
      # Upload Metadata
      upload_metadata("tested-r",
                      "package",
                      name,
                      version,
                      "test_report",
                      dep_url)
      
      upload_metadata("tested-r", "package", name, version, "risk_score", risk_number)
      
      upload_metadata("tested-r", "package", name, version, "risk_category", risk_category) 
      
      # Return the result for each row
      list(name = name,
          version = version,
          url = dep_url,
          result = "Processed"
      )
    })
      
    # Return a success message with the results
    return(list(message = "Rendering completed", results = results))
  }
    
  }
