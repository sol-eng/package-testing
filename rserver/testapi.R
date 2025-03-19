library(httr)
library(pak)
library(rcmdcheck)
library(dplyr)
library(tidyr)
library(purrr)
library(tinytex)
library(riskmetric)

infra_setup <- function(work_dir = "/work") {
  # Install pak into a separate folder so that the R package installations only stricly load pak and do not interfere with renv
  pak_install_dir <- paste0(work_dir , "/pak")
  
  if (!dir.exists(pak_install_dir)) {
    dir.create(pak_install_dir, recursive = TRUE)
    pak_install_data <-
      callr::r_vanilla(
        function(install_dir) {
          .libPaths(install_dir)
          install.packages(
            "pak",
            install_dir,
            repos = sprintf(
              "https://r-lib.github.io/p/pak/devel/%s/%s/%s",
              .Platform$pkgType,
              R.Version()$os,
              R.Version()$arch
            )
          )
        },
        args = list(install_dir = pak_install_dir),
        env = c("PKG_SYSREQS_PLATFORM" = "rockylinux-9"),
        show = TRUE
      )
  }
  
}

render_content <- function(url = "http://api:8000/render_content", data = list()) {
  # Make the POST request
  response <- POST(url, body = data, encode = "json")
  
  # Check the response status code
  if (response$status_code == 200) {
    # Parse the response body as JSON
    result <- content(response, "parsed")
  } else {
    # Handle the error
    stop(paste("Error:", response$status_code, content(response)$error))
  }
  
  #return(unlist(result))
  return(result)
}


package_list <- function(input_list = c("nlmixr2", "mschubert/clustermq"),
         ppm_url = "http://packagemanager:4242",
         repository = "cran",
         snapshot = "2025-01-08",
         deps = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
         libdir = "",
         debug = FALSE) {
  # Set the repos option
  my_repo <- paste(ppm_url, repository, snapshot, sep = "/")

  if (debug) {
    print(paste("Debug: using dependencies", deps))
    print("Debug: repo options")
    print(my_repo)
    print("Debug: input_list")
    print(input_list)
  }
  
  # get dependencies
  pkg_deps<-callr::r_vanilla(
    function(input_list,deps) {
      #options(repos="http://packagemanager:4242/cran/2025-01-08")
      print("XXXXX")
      print(.libPaths())
      pak::pkg_deps(c(input_list, "*=?ignore-unavailable"), dependencies = deps)
    },
    args = list(
      input_list = input_list,
      deps = deps
    ),
    libpath = libdir,
    repos = my_repo,
    env = c("PKG_SYSREQS_PLATFORM" = "rockylinux-9"),
    show = TRUE
  )

  print("Debug: Done with packages")

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

upload_package <- function(url = "http://api:8000/upload_package",
                           pkg_file = NULL,
                           data = list(),
                           debug = FALSE) {
  # Prepare the file for upload
  print(pkg_file)
  
  file_to_upload <- upload_file(pkg_file)
  
  if (debug) {
    print(paste("Debug: file to upload = ", pkg_file))
  }
  
  # Create and send the POST request
  response <- POST(
    url = url,
    add_headers(accept = "*/*"),
    body = list(pkg_file = file_to_upload),
    query = data,
    # Add the 'test' parameter as a query parameter
    encode = "multipart"
  )
  
  # Print the response
  return(content(response, "text"))
  
}

pm_make_url <- function(pm_url, repo, snapshot = "latest") {
  paste(pm_url, repo, ifelse(nchar(snapshot) > 0, snapshot, ""),sep="/")
}

# List of packages to be installed and tested
# Note: You need to replace "SuppDists" and "clustermq" with the actual package names you want to test
test_list <- c("tidyverse","nlmixr2","clustermq","SuppDists")
test_list <- "SuppDists"
#test_list <- c("tidyverse")
#test_list <- c("clustermq")
#test_list <- c("nlmixr2", "SuppDists","clustermq")
#test_list <- c("codetools")

# Turn on debug statements
debug <- TRUE

# work directory to temporarily install "things"
work_dir <- "/tmp/work"

# PPM URL
ppm_url <- "http://packagemanager:4242"

if(debug) {
  print("Debug: Deploying validation Strategy")
}

# Render and upload validation strategy document
validation_strategy <- render_content(
  data = list(
    type="validation_strategy",
    data = "",
    desc = "",
    ppm_url = ppm_url
  )
)

# PPM CRAN Snapshot to use
snapshot_date <- "2024-11-02"
snapshot_date <- "2025-01-08"

# Add bioconductor support
options(BioC_mirror =
          pm_make_url(ppm_url, "bioconductor", snapshot = snapshot_date))
options(BIOCONDUCTOR_CONFIG_FILE = paste0(
  pm_make_url(ppm_url, "bioconductor", snapshot = snapshot_date),
  "/config.yaml"
))

default_repo <- "cran"
default_url <- pm_make_url(ppm_url, default_repo, snapshot = snapshot_date)

options(repos = c(CRAN = default_url, STAN = 'https://stan-dev.r-universe.dev'))

options(repos = BiocManager::repositories())

# Little hack to make system requirement detection work on rl9 w/pak
Sys.setenv("PKG_SYSREQS_PLATFORM" = "rockylinux-9")

# Let's get pak and TinyTEX installed if they are not yet available
infra_setup(work_dir = work_dir)

pak_install_dir = paste0(work_dir, "/pak")

# Get a list of all to be installed and tested packages including dependencies
main_data <- list(
  input_list = test_list,
  ppm_url = ppm_url,
  repo = "cran",
  snapshot = snapshot_date,
  deps = c("Imports", "LinkingTo"),
  debug = TRUE
)

# We will install all to be tested packages into site-library
main_install_dir <- paste0(R.home(), "/site-library")
#main_install_dir <- paste0(work_dir, "/main")

dir.create(main_install_dir, recursive = TRUE)

if (debug) {
  print("Debug: Getting list of all dependencies")
}

main_package_list <- package_list(
  input_list = test_list,
  ppm_url = ppm_url,
  repo = "cran",
  snapshot = snapshot_date,
  deps = c("Imports", "LinkingTo"),
  libdir = pak_install_dir,
  debug = TRUE
)

if (debug) {
  print("Debug: package list for testing/validation")
  print(main_package_list$URN)
}

if (debug) {
  print("Debug: Installing all packages for testing/validation")
}

# Install all packages to be tested
pak_main_data <-
  callr::r_vanilla(
    function(deps, dest_dir) {
      #options(repos = repos)
      #.libPaths(dest_dir)
      print("XXXXX")
      print(.libPaths())
      print(dest_dir)
      print(deps)
      #inst_packages<-as.data.frame(installed.packages())$Package

      # Filter out already installed packages from deps
      #deps_to_install <- deps# [!sapply(strsplit(deps, "@"), function(x) x[1]) %in% inst_packages]
      
      pak::pkg_install(c(deps, "*=?ignore-unavailable"), lib = dest_dir)
    },
    args = list(
      deps = c(main_package_list$URN, "*=?ignore-unavailable"),
      dest_dir = main_install_dir
    ),
    libpath = pak_install_dir,
    repos = default_url,
    env = c("PKG_SYSREQS_PLATFORM" = "rockylinux-9"),
    show = TRUE
  )

if (debug) {
  print("Debug: packages installed")
}

# tale a copy of current pak cache state
pak_cache <- pak::cache_list() %>%
  distinct(package, version, platform, .keep_all = TRUE)

if (debug) {
  print("Debug: pak_cache copy")
}

# Get a list of all additional dependencies needed for rcmdcheck
main_data[["deps"]] <-  c("Imports", "LinkingTo", "Suggests", "Enhances")
main_data[["input_list"]] <- main_package_list$URN

extra_install_dir <- paste0(work_dir, "/extra")
dir.create(extra_install_dir, recursive = TRUE)

if (debug) {
  print("Debug: extra packages identification")
}

if (debug) {
  print("Debug: identifying extra packages needed for R CMD check")
}

extra_package_list <- package_list(
  input_list = main_package_list$URN,
  ppm_url = ppm_url,
  repository = "cran",
  snapshot = snapshot_date,
  deps =  c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
  libdir = pak_install_dir,
  debug = TRUE
)

if (debug) {
  print("Debug: extra packages identified")
  print(extra_package_list$URN)
}

if (debug) {
  print("Debug: creating validation plan")
}

validation_plan <- render_content(
  data = list(
    type="validation_plan",
    data = list(
      main_packages = main_package_list,
      extra_packages = extra_package_list,
      snapshot_date = snapshot_date,
      validation_strategy_url = validation_strategy$results$url[[1]],
      r_version = paste(R.version$major,R.version$minor,sep=".")
    ),
    ppm_url = ppm_url
  )
)

if (debug) {
  print("Debug: validation Plan deployed !")
}

if (debug) {
  print("Debug: deploying extra packages")
  print(default_url)
}

extra_pak_data <-
  callr::r_vanilla(
    function(deps, dest_dir) {
      #options(repos = repos)
      #.libPaths(lib_dir)
      #inst_packages<-as.data.frame(installed.packages())$Package

      # Filter out already installed packages from deps
      #deps_to_install <- deps# [!sapply(strsplit(deps, "@"), function(x) x[1]) %in% inst_packages]
      
      pak::pkg_install(c(deps, "*=?ignore-unavailable"), lib = dest_dir)
    },
    args = list(
      deps = c(extra_package_list$URN, "*=?ignore-unavailable"),
      dest_dir = extra_install_dir
    ),
    libpath = pak_install_dir,
    repos = default_url,
    env = c("PKG_SYSREQS_PLATFORM" = "rockylinux-9"),
    show = TRUE
  )

  if (debug) {
    print("Debug: extra packages deployed")
  }


# Convert main_package_list to a tibble with name and version columns
main_package_data <- tibble(nameversion = main_package_list$URN) %>%
  separate(
    nameversion,
    into = c("name", "version"),
    sep = "@",
    remove = FALSE
  ) %>%
  select(name, version, nameversion)

# loop through the tibble to add additional information and upload packages to PPM
# Define a function to process each row
process_package <- function(name, version, nameversion) {
  if (debug) {
    print(paste("Debug: Processing package:", name, "version:", version))
  }
  
  source_pkg <- (
    pak_cache %>% filter(package == name) %>%
      filter(version == version) %>%
      filter(platform == "source")
  )$fullpath

  if(length(source_pkg)>1) {
    source_pkg <- source_pkg[length(source_pkg)]
  }  
  if (debug) {
    print(paste("Debug: uploading source package", source_pkg))
  }
  
  source_upload_result <- tryCatch({
    upload_package(pkg_file = source_pkg,
                   data = list(pkg_type = "source", ppm_url = ppm_url))
  }, error = function(e) {
    if (debug) {
      print(paste(
        "Debug: Error uploading source package:",
        name,
        "version:",
        version
      ))
    }
    return(paste("Error uploading source package:", e$message))
  })

  binary_pkg <- (
    pak_cache %>% filter(package == name) %>%
      filter(version == version) %>%
      filter(platform == "x86_64-pc-linux-gnu-rocky-9.3")
  )$fullpath

  if(length(binary_pkg)>1) {
    binary_pkg <- binary_pkg[length(binary_pkg)]
  }
  if (debug) {
    print(paste("Debug: uploading binary package", binary_pkg))
  }
  
  binary_upload_result <- tryCatch({
    upload_package(pkg_file = binary_pkg,
                   data = list(pkg_type = "binary", ppm_url = ppm_url))
  }, error = function(e) {
    if (debug) {
      print(paste(
        "Debug: Error uploading binary package:",
        name,
        "version:",
        version
      ))
    }
    return(paste("Error uploading binary package:", e$message))
  })
  
  pak_main_package_metadata <- pak_main_data %>%
    filter(package == name) %>%
    filter(version == version) %>%
    filter (platform == "source")
  
  if (debug) {
    print(paste0("Debug: Running OQ as library(", name, ")"))
  }
  
  oq <-
    callr::r_vanilla(
      function(name, lib_dir) {
        .libPaths(lib_dir)
        output <- character()
        stderr <- character()
        
        captureOutput <- function(expr) {
          msgs <- character()
          
          withCallingHandlers({
            stdout <- capture.output({
              result <- withVisible(expr)
            })
            list(result = result, output = c(stdout, msgs))
          }, message = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
          }, warning = function(w) {
            stderr <<- c(stderr, paste("Warning:", conditionMessage(w)))
            invokeRestart("muffleWarning")
          })
        }
        
        tryCatch({
          captured <- captureOutput({
            do.call("library", list(name))
          })
          output <- captured$output
        }, error = function(e) {
          stderr <<- c(stderr, paste("Error:", conditionMessage(e)))
        })
        
        list(stdout = output, stderr = stderr)
      },
      args = list(name = name, lib_dir = main_install_dir),
      show = TRUE
    )
  
  if (debug) {
    print("Debug: Running PQ as R CMD check")
    print(paste("       using", source_pkg))
  }
  
  # R CMD check
  if (length(source_pkg) == 0) {
    pk <- pak::pkg_download(
      nameversion,
      dest_dir = "/work/pkgs",
      platform = "source",
      dependencies = FALSE
    )
    source_pkg <- pk[2, ]$fulltarget
  }
  
  rcmdcheck <- rcmdcheck::rcmdcheck(
    path = source_pkg,
    libpath = extra_install_dir,
    quiet = FALSE,
    check_dir = "~/tmp",
    env = c("_R_CHECK_FORCE_SUGGESTS_" = 1)
  )
  
  if (debug) {
    print("Debug: Calculating risk score")
  }
  
  risk_number <- pkg_ref(name, lib.loc = c(main_install_dir,paste0(R.home(),"/library"))) %>%
    as_tibble() %>%
    pkg_assess() %>%
    pkg_score() %>%
    summarize_scores()
  
  # Ensure all columns have at least one element
  iq_stdout <- list(
    if (is.null(pak_main_package_metadata$build_stdout))
      NA_character_
    else
      paste(pak_main_package_metadata$build_stdout, collapse = "\n")
  )
  iq_stderr <- list(
    if (is.null(pak_main_package_metadata$build_stderr))
      NA_character_
    else
      paste(pak_main_package_metadata$build_stderr, collapse = "\n")
  )
  oq_stdout <- if (length(oq$stdout) > 0)
    list(paste(oq$stdout, collapse = "\n"))
  else
    list(NA_character_)
  oq_stderr <- if (length(oq$stderr) > 0)
    list(paste(oq$stderr, collapse = "\n"))
  else
    list(NA_character_)
  pq_stdout <- if (length(rcmdcheck$stdout) > 0)
    list(paste(rcmdcheck$stdout, collapse = "\n"))
  else
    list(NA_character_)
  pq_stderr <- if (length(rcmdcheck$stderr) > 0)
    list(paste(rcmdcheck$stderr, collapse = "\n"))
  else
    list(NA_character_)
  
  
  
  # Package Description
  
  pkg_desc <- packageDescription(name, lib.loc = c(main_install_dir, paste0(R.home(), "/library")))
  
  pkg_desc_tibble <- pkg_desc %>%
    unclass() %>%
    as.list() %>%
    tibble::enframe(name = "field", value = "value") %>%
    tidyr::unnest(value)
  
  # Return a modified version of the row
  result_tibble <- tibble(
    name = name,
    version = version,
    nameversion = nameversion,
    # iq_stdout = as.list(iq_stdout),
    # iq_stderr = as.list(iq_stderr),
    # oq_stdout = list(oq_stdout),
    # oq_stderr = list(oq_stderr),
    # pq_stdout = list(pq_stdout),
    # pq_stderr = list(pq_stderr),
    iq_stdout = iq_stdout,
    iq_stderr = iq_stderr,
    oq_stdout = oq_stdout,
    oq_stderr = oq_stderr,
    pq_stdout = pq_stdout,
    pq_stderr = pq_stderr,
    risk_number = risk_number
  )
  
  # Return both the result tibble and the package description
  list(result = result_tibble, description = pkg_desc_tibble)
  }

# Apply the function to each row of the tibble
# main_package_data_modified <- main_package_data %>%
#   pmap_dfr(~process_package(..1, ..2, ..3))

# Apply the function to each row of the tibble
main_package_results <- main_package_data %>%
  pmap( ~ process_package(..1, ..2, ..3))

# Separate the results and descriptions
main_package_data_modified <- map_dfr(main_package_results, "result")
package_descriptions <- map(main_package_results, "description")

# If you want to combine the descriptions into a named list
package_descriptions <- set_names(package_descriptions, main_package_data$name)

saveRDS(main_package_data_modified, file = "data.rds")
saveRDS(package_descriptions, file = "pkgs.rds")
package_render <- render_content(
  data = list(
    type="package",
    data = main_package_data_modified,
    desc = package_descriptions,
    ppm_url = ppm_url
  )
)

saveRDS(main_package_list,"/tmp/main_package_list.rds")
saveRDS(package_render$results,"/tmp/package_render.rds")

val_report_package_data <- lapply(package_render$results, function(item) {
  data.frame(
    name = item$name[[1]],
    version = item$version[[1]],
    url = item$url[[1]],
    stringsAsFactors = FALSE
  )
})


render_content(
  data = list(
    type="validation_report",
    data = list(
      main_packages = val_report_package_data,
      snapshot_date = snapshot_date,
      validation_strategy_url = validation_strategy$results$url[[1]],
      validation_plan_url = validation_plan$results$url[[1]],
      r_version = paste(R.version$major,R.version$minor,sep=".")
    ),
    ppm_url = ppm_url
  )
)
