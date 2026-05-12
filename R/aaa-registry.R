.recipe_registry_env <- if (exists(".recipe_registry_env", inherits = FALSE)) {
    .recipe_registry_env
} else {
    new.env(parent = emptyenv())
}