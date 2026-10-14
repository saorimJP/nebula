


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
#'   setup_nebula(env_file = ".nebula_env", model_name = "FAST.gemma3:12b")
#' }
setup_nebula <- function(
    env_file = ".nebula_env",
    model_name = "FAST.gemma3:12b",
    api_url = "https://nebula.cs.vu.nl/api/",
    api_key_env_var = "NEBULA_API_KEY",
    api_path = "api/chat/completions"
) {

  if (!file.exists(env_file)) {
    stop("Environment file not found: ", env_file)
  }

  readRenviron(env_file)

  if (Sys.getenv(api_key_env_var) == "") {
    stop(
      "Nebula API key not found in environment variable: ",
      api_key_env_var
    )
  }

  options(
    nebula.model = model_name,
    nebula.api_url = api_url,
    nebula.api_key_env_var = api_key_env_var,
    nebula.api_path = api_path
  )

  message(
    "--> Nebula environment loaded. Model: ", model_name
  )

  invisible(TRUE)
}


#' Read NEBULA models
#'
#' Download a table of available Nebula models.
#' !You need to have set a NEBULA_API_KEY environment file.
#'
#' @param host URL of NEBULA server. default is https://nebula.cs.vu.nl
#'
#' @returns tibble with available models and their specs.
#' @export
#'
#' @examples
#' nebula_models("https://nebula.cs.vu.nl")
nebula_models <- function(host = "https://nebula.cs.vu.nl") {
  httr2::request(host) |>
    httr2::req_url_path_append("api","models") |>
    httr2::req_auth_bearer_token(Sys.getenv('NEBULA_API_KEY')) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("data", "info") |>
    tibble::as_tibble()
}

#' Send a single prompt to the Nebula API.
#'
#' Sends a prompt to Nebula and returns the text response as text.
#' @param prompt Text prompt to send to the LLM.
#' @param model_name Model name to use. If NULL, uses default model from options.
#'
#' @return Character string containing the response, or NA if the API call fails.
#' @export

nebula_chat <- function(
    prompt,
    model_name = NULL
) {

  if (is.null(model_name)) {
    model_name <- getOption(
      "nebula.model",
      "FAST.gemma3:12b"
    )
  }

  tryCatch({

    resp <- suppressWarnings(
      llm_message(
        .prompt = as.character(prompt),
        .role = "user"
      ) |>
        chat_completions_chat(
          .api_url = getOption(
            "nebula.api_url",
            "https://nebula.cs.vu.nl/api/"
          ),
          .api_key_env_var = getOption(
            "nebula.api_key_env_var",
            "NEBULA_API_KEY"
          ),
          .api_path = getOption(
            "nebula.api_path",
            "api/chat/completions"
          ),
          .model = model_name
        )
    )

    get_reply(resp) |>
      str_trim()

  }, error = function(e) {

    warning(
      "Nebula API call failed: ",
      conditionMessage(e)
    )

    NA_character_
  })
}


#' Parse JSON output returned by Nebula
#'
#' Removes Markdown code blocks, then parses JSON text into an R list.
#'
#' @param json_str Character string containing JSON output from LLM.
#'
#' @return A list with parsed JSON data, or an empty list if parsing fails.
#' @export
parse_llm_json <- function(
    json_str
) {

  if (
    is.na(json_str) ||
    str_trim(json_str) == ""
  ) {
    return(list())
  }

  tryCatch({

    cleaned <- json_str |>
      str_remove_all("```json|```") |>
      str_trim()

    fromJSON(
      cleaned,
      simplifyVector = TRUE
    )

  }, error = function(e) {

    list()
  })
}


#' Translate diary entries using Nebula API.
#'
#' Translate non-English diary into English using Nebula and adds column "update_en."
#' Entries already written in English are kept unchanged.
#' @param data Data frame containing diary entries.
#' @param text_col Column name of text to translate.
#' @param lang_col Column name of original language.
#' @param target_language Language to translate into (default: "English").
#' @param model_name Model name to use. If NULL, uses default model.
#'
#' @return A data frame with an added `update_en` column.
#' @export
#
translate_diary <- function(
    data,
    text_col,
    lang_col,
    target_language = "English",
    model_name = NULL
) {

  translate_one <- function(
    text,
    language
  ) {

    if (
      is.na(text) ||
      str_trim(text) == ""
    ) {
      return(NA_character_)
    }

    if (
      !is.na(language) &&
      language == target_language
    ) {
      return(text)
    }

    prompt <- glue("
Translate the following text into {target_language}.

Preserve the meaning as precisely as possible.
Output only the translation, without markdown or explanation.

Text:
{text}
")

    nebula_chat(
      prompt,
      model_name
    )
  }

  message(
    "--> Starting translation via Nebula..."
  )

  data |>
    mutate(
      update_en = map2_chr(
        .data[[text_col]],
        .data[[lang_col]],
        translate_one
      )
    )
}


#' Build Longitudinal Context from Previous Diary Entries.
#'
#' Creates formatted past history strings for each participant ordered by time wave.
#'
#' @details
#' Expects input data in long format (1 row = 1 diary entry per wave).
#' The data frame must contain columns corresponding to participant ID,
#' survey wave number, and diary text.
#'
#' Data will automatically be sorted by ID and wave within the function.
#'
#' @param data Longitudinal diary data frame.
#' @param text_col Column name containing diary text.
#' @param id_col Column name for participant ID.
#' @param wave_col Column name for survey wave number.
#' @param context_n Number of past entries to include (default: Inf for all past entries).
#'
#' @return A data frame with an added `past_context` column.
#' @export
build_past_context <- function(
    data,
    text_col,
    id_col,
    wave_col,
    context_n = Inf
) {

  data |>
    arrange(
      .data[[id_col]],
      .data[[wave_col]]
    ) |>
    group_by(
      .data[[id_col]]
    ) |>
    group_modify(function(.x, .y) {

      texts <- .x[[text_col]]
      waves <- .x[[wave_col]]

      .x$past_context <- map_chr(
        seq_along(texts),
        function(i) {

          if (i == 1) {
            return(
              "No prior entries."
            )
          }

          first_idx <- if (
            is.infinite(context_n)
          ) {
            1
          } else {
            max(
              1,
              i - context_n
            )
          }

          idx <- first_idx:(i - 1)

          paste(
            glue(
              "Wave {waves[idx]}: {texts[idx]}"
            ),
            collapse = "\n"
          )
        }
      )

      .x
    }) |>
    ungroup()
}


#' Code diary entries using an Nebula
#'
#' Runs LLM coding on diary entries based on a custom prompt function.
#'
#' @param data Data frame containing diary entries.
#' @param text_col Column name of text to analyze.
#' @param id_col Column name of participant ID.
#' @param wave_col Column name of wave number.
#' @param prompt_fn Function accepting `(text, past_context)` that returns a prompt string.
#' @param use_context Logical. Whether to include past context (default: TRUE).
#' @param context_n Number of past entries to include in context (default: Inf).
#' @param model_name Model name to use. If NULL, uses default model.
#'
#' @return A data frame with added columns: `past_context`, `llm_raw_response`, and `parsed_result`.
#' @export
code_diary_llm <- function(
    data,
    text_col,
    id_col,
    wave_col,
    prompt_fn,
    use_context = TRUE,
    context_n = Inf,
    model_name = NULL
) {

  if (use_context) {

    analysis_data <- build_past_context(
      data = data,
      text_col = text_col,
      id_col = id_col,
      wave_col = wave_col,
      context_n = context_n
    )

  } else {

    analysis_data <- data |>
      mutate(
        past_context =
          "No prior context provided."
      )
  }

  call_one <- function(
    text,
    past_context
  ) {

    if (
      is.na(text) ||
      str_trim(text) == ""
    ) {
      return(
        NA_character_
      )
    }

    prompt <- prompt_fn(
      text = text,
      past_context = past_context
    )

    nebula_chat(
      prompt,
      model_name
    )
  }

  message(
    "--> Running LLM diary coding via Nebula..."
  )

  analysis_data |>
    mutate(
      llm_raw_response = map2_chr(
        .data[[text_col]],
        past_context,
        call_one
      ),

      parsed_result = map(
        llm_raw_response,
        parse_llm_json
      )
    )
}

