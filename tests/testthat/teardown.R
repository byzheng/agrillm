agrillm_options$reset()
llm_cache_clear()
unlink(file.path(getwd(), ".cache_agrillm"), recursive = TRUE, force = TRUE)