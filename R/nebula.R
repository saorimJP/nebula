library(tidyverse)
library(glue)
library(tidyllm)
library(jsonlite)


#' Setup Nebula API Environment
#'
#' Loads Nebula API configurations (API key, default model, and base URL)
#' from an environment file into R global options.
#' !You need to have set a NEBULA_API_KEY environment file.
#'
#' @param env_file Character. Path to the environment file containing `NEBULA_API_KEY`. Default is `".nebula_env"`.
#' @param model_name Character. Default LLM model name to use for requests. Default is `"FAST.gemma3:12b"`.
#' @param api_url Character. Base URL of the Nebula API. Default is `"https://nebula.cs.vu.nl/api/"`.
#' @param api_key_env_var Character. Name of the environment variable storing your API key. Default is `"NEBULA_API_KEY"`.
#' @param api_path Character. Endpoint path for chat completions. Default is `"api/chat/completions"`.
#'
#' @return Invisibly returns `TRUE` if setup succeeds. Stops with an error if file or key is missing.
#' @export
#'
#' @examples
#' \dontrun{
#'   setup_nebula(env_file = ".nebula_env", model_name = "FAST.gemma4:31b")
#' }

setup_nebula <- function(
    env_file = ".nebula_env",
    model_name,
    api_url = "https://nebula.cs.vu.nl/api/",
    api_key_env_var = "NEBULA_API_KEY",
    api_path = "chat/completions"
) {

  if (!is.character(model_name) || length(model_name) != 1L ||
      is.na(model_name) || !nzchar(trimws(model_name))) {
    stop("model_name must be one non-empty model ID.")
  }

  # Load API key
  if (!file.exists(env_file)) {
    stop("Environment file not found: ", env_file)
  }

  readRenviron(env_file)

  if (!nzchar(Sys.getenv(api_key_env_var))) {
    stop(
      "Nebula API key not found in environment variable: ",
      api_key_env_var
    )
  }

  # Store configuration
  options(
    nebula.model = model_name,
    nebula.api_url = api_url,
    nebula.api_key_env_var = api_key_env_var,
    nebula.api_path = api_path
  )

  message(
    "--> Nebula environment loaded. Model: ",
    model_name
  )

  invisible(TRUE)
}

#' List accessible Nebula models
#' @param host Nebula server URL.
#' @return A tibble of available models.
#' @export
nebula_models <- function(host = "https://nebula.cs.vu.nl") {
  key_var <- getOption("nebula.api_key_env_var", "NEBULA_API_KEY")
  key <- Sys.getenv(key_var, unset = "")
  if (!nzchar(key)) stop("Set up your Nebula API key first.")
  httr2::request(host) |>
    httr2::req_url_path_append("api", "models") |>
    httr2::req_auth_bearer_token(key) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("data", "info") |>
    tibble::as_tibble()
}


#' Create a fresh ellmer Chat object for Nebula
#' @param model_name Optional model override.
#' @return An ellmer Chat object with no prior conversation history.
#' @export
create_nebula_chat <- function(model_name = NULL) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Install package 'ellmer' to use Nebula chat functions.")
  }
  model <- if (is.null(model_name)) getOption("nebula.model") else model_name
  url <- getOption("nebula.api_url")
  key_var <- getOption("nebula.api_key_env_var")
  if (is.null(model) || is.null(url) || is.null(key_var)) {
    stop("Call setup_nebula(model_name = ...) first.")
  }
  ellmer::chat_openai_compatible(
    base_url = url,
    model = model,
    credentials = function() {
      key <- Sys.getenv(key_var, unset = "")
      if (!nzchar(key)) stop("Nebula API key unavailable: ", key_var)
      key
    }
  )
}

#' Generate unconstrained text with Nebula
#' @param prompt A single prompt.
#' @param model_name Optional model override.
#' @return A character response.
#' @export
nebula_chat <- function(prompt, model_name = NULL) {
  create_nebula_chat(model_name)$chat(prompt)
}


#' Generate data conforming to an ellmer schema
#' @param prompt A single prompt.
#' @param schema An ellmer type specification, such as type_object().
#' @param model_name Optional model override.
#' @return An R list conforming to the structured output specification.
#' @export
nebula_chat_structured <- function(prompt, schema, model_name = NULL) {
  create_nebula_chat(model_name)$chat_structured(prompt, type = schema)
}

#' Translate diary entries with resumable checkpoints
#' @param data A data frame.
#' @param text_col Column containing the source text (character name).
#' @param lang_col Column containing source language (character name).
#' @param target_language Target language, default English.
#' @param model_name Optional model override.
#' @param cache_file Checkpoint path; NULL disables checkpointing.
#' @return Data with an update_en column.
#' @export
translate_diary <- function(data, text_col, lang_col,
                            target_language = "English", model_name = NULL,
                            cache_file = "processed_data/d_all_translation_checkpoint.rds") {
  if (!all(c(text_col, lang_col) %in% names(data))) stop("Unknown input column.")
  if (!is.null(cache_file) && file.exists(cache_file)) {
    current <- readRDS(cache_file)
    if (nrow(current) != nrow(data)) {
      stop("Checkpoint row count differs from input; use a new cache_file.")
    }
    message("--> Resuming translation from ", cache_file)
  } else {
    current <- data
  }
  if (!"update_en" %in% names(current)) current$update_en <- NA_character_

  for (i in seq_len(nrow(current))) {
    if (!is.na(current$update_en[i]) && nzchar(trimws(current$update_en[i]))) next
    txt <- as.character(current[[text_col]][i])
    lang <- as.character(current[[lang_col]][i])
    if (is.na(txt) || !nzchar(trimws(txt))) next
    if (!is.na(lang) && lang == target_language) {
      current$update_en[i] <- txt
    } else {
      prompt <- paste0(
        "Translate the following text into ", target_language,
        ". Preserve its meaning. Return only the translation.\n\n", txt
      )
      current$update_en[i] <- nebula_chat(prompt, model_name = model_name)
    }
    if (!is.null(cache_file)) {
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(current, cache_file)
    }
  }
  current
}

#' Build preceding diary context by participant and wave
#' @param data Long diary table.
#' @param text_col,id_col,wave_col Column names.
#' @param context_n Number of previous nonmissing entries to include.
#' @return Data with past_context column, ordered by ID and wave.
#' @export
build_past_context <- function(data, text_col, id_col, wave_col, context_n = Inf) {
  if (!all(c(text_col, id_col, wave_col) %in% names(data))) stop("Unknown input column.")
  if (length(context_n) != 1L || is.na(context_n) || context_n < 0) {
    stop("context_n must be nonnegative or Inf.")
  }
  data |>
    dplyr::arrange(.data[[id_col]], .data[[wave_col]]) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::group_modify(function(.x, .y) {
      values <- as.character(.x[[text_col]])
      waves <- .x[[wave_col]]
      .x$past_context <- purrr::map_chr(seq_along(values), function(i) {
        previous <- if (i <= 1L) integer(0) else seq_len(i - 1L)
        previous <- previous[!is.na(values[previous]) & nzchar(trimws(values[previous]))]
        if (is.finite(context_n)) previous <- utils::tail(previous, as.integer(context_n))
        if (length(previous) == 0L) return("No prior entries.")
        paste0("Wave ", waves[previous], ": ", values[previous], collapse = "\n")
      })
      .x
    }) |>
    dplyr::ungroup()
}

#' Code diary entries with ellmer structured output
#' @param data A data frame of diary entries.
#' @param text_col Column name holding the text to code.
#' @param prompt_fn Function accepting text and past_context, returning a prompt.
#' @param schema An ellmer structured output schema.
#' @param id_col Optional unique-entry ID column; required for checkpointing.
#' @param use_context Include the supplied past_context column.
#' @param context_col Name of the context column.
#' @param checkpoint_dir Optional directory for per-entry RDS checkpoints.
#' @param model_name Optional model override.
#' @param validate_fn Optional function taking a one-row tibble and returning TRUE/FALSE.
#' @return Input rows plus llm_ prefixed structured output fields and llm_status.
#' @export
code_diary_structured <- function(data, text_col, prompt_fn, schema,
                                  id_col = NULL, use_context = FALSE,
                                  context_col = "past_context", checkpoint_dir = NULL,
                                  model_name = NULL, validate_fn = NULL) {
  if (!text_col %in% names(data)) stop("Unknown text_col: ", text_col)
  if (use_context && !context_col %in% names(data)) stop("Missing context column: ", context_col)
  if (!is.null(checkpoint_dir)) {
    if (is.null(id_col) || !id_col %in% names(data)) {
      stop("A valid id_col is required for checkpointing.")
    }
    ids <- as.character(data[[id_col]])
    if (anyNA(ids) || anyDuplicated(ids)) stop("Checkpoint IDs must be unique and nonmissing.")
    safe_ids <- gsub("[^A-Za-z0-9._-]", "_", ids)
    if (anyDuplicated(safe_ids)) stop("Different IDs map to the same checkpoint filename.")
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }

  coded <- purrr::map(seq_len(nrow(data)), function(i) {
    txt <- as.character(data[[text_col]][i])
    if (is.na(txt) || !nzchar(trimws(txt))) return(tibble::tibble(llm_status = "SKIPPED_EMPTY"))
    context <- if (use_context) as.character(data[[context_col]][i]) else NULL
    path <- NULL
    if (!is.null(checkpoint_dir)) {
      path <- file.path(checkpoint_dir, paste0(safe_ids[i], ".rds"))
      if (file.exists(path)) return(readRDS(path))
    }
    message("Coding ", i, "/", nrow(data))
    result <- tryCatch({
      prompt <- prompt_fn(txt, context)
      raw <- nebula_chat_structured(prompt, schema, model_name = model_name)
      output <- tibble::as_tibble(raw) |>
        dplyr::rename_with(~ paste0("llm_", .x))
      if (!is.null(validate_fn) && !isTRUE(validate_fn(output))) {
        stop("Structured output failed validation")
      }
      dplyr::mutate(output, llm_status = "SUCCESS")
    }, error = function(e) {
      warning("Coding failed for entry ", i, ": ", conditionMessage(e), call. = FALSE)
      tibble::tibble(llm_status = "ERROR")
    })
    if (!is.null(path) && identical(result$llm_status[[1]], "SUCCESS")) {
      saveRDS(result, path)
    }
    result
  })
  dplyr::bind_cols(data, dplyr::bind_rows(coded))
}
