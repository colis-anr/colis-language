
# CHANGES
## Colis-language
   - src/symbolic/utilities/cp.ml
     + fix application of cp2 multiple_times
     + fix case of source path ending by / for file and directory   
   - src/symbolic/utilities/mkdir.ml
     + fix case of paths ending by multiple slashes by ignoring them
     + fix case of paths ending by /. or /.. by error cases 
   - src/symbolic/utilities/mv.ml
     + fix case of source paths ending by slashes 
   - src/symbolic/utilities/rm.ml
     + fix case of paths ending by multiple slashes by ignoring them
     + fix case of paths ending by /. or /.. by error cases 
   - src/symbolic/utilities/test.ml
     + fix case of paths ending by multiple slashes by ignoring them
     + fix case of paths ending by /. or /.. by error case if not option -d 
   - src/symbolic/utilities/touch.ml
     + fix case of paths ending by multiple slashes by ignoring them

## Colis-constraints
   - src/common/path.ml
     + nomralize : reverse the path generated
     + string_trailing_slashes : reverse string generated
 
