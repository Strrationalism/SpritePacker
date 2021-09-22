(*open Bin_pack;;*)


let help () =
    print_endline "Sprite Packer";
    print_endline "by Strrationalism Studio 2021";
    print_endline "";
    print_endline "Usage:";
    print_endline "    sppak <inputDir> <output> [--margin <margin>]";
    print_endline ""
;;


let rec get_files prefix dir =
    if Sys.is_directory dir then
        Sys.readdir dir
        |> Array.to_list
        |> List.concat_map (fun name ->
            get_files 
                (prefix ^ name ^ "/") 
                (dir ^ "/" ^ name))
    else [String.sub prefix 0 (String.length prefix - 1)]
;;


let get_image_files dir =
    get_files "" dir
    |> List.filter (fun file ->
        let ext = 
            Filename.extension file 
            |> String.lowercase_ascii in
        ext = ".png" || ext = ".bmp")
;;


let () =
    Sys.argv
    |> Array.to_list
    |> function
        | [ _; input_dir; _ ] ->
            if Sys.is_directory input_dir |> not then
                failwith "<inputDir> must be a dirctory.";

            get_image_files input_dir
            |> List.iter (Printf.printf "%s\n");
            
        | _ -> help ()
;;


