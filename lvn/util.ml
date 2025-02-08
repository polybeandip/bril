let fresh_int = 
    let counter = ref 0 in
    fun () -> 
        counter := !counter + 1; 
        !counter

let fresh_name = 
    let counter = ref 0 in
    fun name -> 
        counter := !counter + 1; 
        Printf.sprintf "öö_%d_%s" !counter name
