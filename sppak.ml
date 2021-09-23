open Bin_pack;;

let help () =
    print_endline "Sprite Packer";
    print_endline "by Strrationalism Studio 2021";
    print_endline "";
    print_endline "Usage:";
    print_endline "    sppak <inputDir> <output> [--margin <margin-in-pixels>]";
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
    |> List.map (fun name ->
        let img_result = Stb_image.load ~channels:4 (dir ^ "/" ^ name) in
        match img_result with
        | Ok x -> name, x
        | Error (`Msg x) -> failwith x)
;;


type options = 
    { margin: int }
;;


let default_options =
    { margin = 0 }
;;


let rec parse_options prev_option =
    function
    | [] -> Ok prev_option
    | "--margin" :: margin_value :: other -> 
        parse_options 
            { margin = int_of_string margin_value } 
            other
    | _ -> Error ()
;;


let process input_dir _ options =
    if Sys.is_directory input_dir |> not then
        failwith "<inputDir> must be a dirctory.";

    match options with
    | Error () -> help ()
    | Ok options ->
        get_image_files input_dir
        |> List.map (fun (name, image) ->
            { w = Stb_image.width image + 2 * options.margin;
              h = Stb_image.height image + 2 * options.margin;
              tag = name, image })
        |> bin_pack
        |> ignore
;;


let () =
    Sys.argv
    |> Array.to_list
    |> function
        | _ :: input_dir :: out_dir :: options ->
            let options = parse_options default_options options in
            process input_dir out_dir options
            
        | _ -> help ()
;;


