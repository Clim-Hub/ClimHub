---
applyTo: '**'
---


## R function documentation guidelines
Please ensure that all R functions are documented according to the [Roxygen2](https://roxygen2.r-lib.org/) format. This includes providing clear descriptions, parameter explanations, return values, and examples. Specifically, follow these guidelines:
- Use `#'` to denote Roxygen2 comments.
- Include a title and description for each function.
- Document all parameters using `@param`.
- Specify the return value with `@return`.
- Provide examples of how to use the function with `@examples`.
- If applicable, include `@seealso` for related functions.
- Use `@export` to indicate that the function should be exported.
- Ensure that the documentation is clear, concise, and free of grammatical errors.
- Ensure that function names do not contain dots but use underscores instead (e.g., `my_function` instead of `my.function`).
- Ensure that function arguments use camelCase (e.g., `myArgument` instead of `my_argument`).
- Maintain consistency in terminology and style throughout the documentation.
- Ensure that function arguments targeting the same or highly similar inputs have the same name across different functions.
- Ensure that all arguments are documented, even if they seem self-explanatory. The documnentation of an argument should follow this format: `@param argumentName Optional, Class. Description of the argument.` Where "Optional" is included if the argument is not required, and "Class" is the expected data type (e.g., character, numeric, data.frame, etc.; if in doubt, ask).
- Add an author tag `@author Your Name` to each function, replacing "Your Name" with the actual author's name (if in doubt, ask).
- Ensure that "Helper_" functions are documented as well, following the same guidelines but are not exported (i.e., do not include the `@export` tag).
- Ensure that functions come with a prefix name that falls into the following categories: "Discovery_", "Meta_", "Read_", "Helper_", "Metrics_", "WriteRead_", "Interpolate_", "Temporal_", "Spatial_", "Process_". If no category fits, ask which should be applied.
- Ensure that file names match the function name exactly (e.g., `MyFunction.r` for `MyFunction`). Where a file contains multiple functions, ensure that the file name reflects the primary function or purpose of the contained functions (this will usually be the first function in the file).
- Do NOT make changes to the files in the man/ folder directly. These files are auto-generated from the R scripts when building the package.