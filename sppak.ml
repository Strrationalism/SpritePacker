open Bin_pack;;
(*open Bigarray;;*)


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
        let img_result = Stb_image.loadf ~channels:4 (dir ^ "/" ^ name) in
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


let write_bin_pack_result options _ result =
    (*let _ = 
        Array1.create 
            float32 
            c_layout 
            (result.width * result.height) in*)

    let csv = ref "name, x, y, w, h\n" in

    let sprites =
        result.rects
        |> List.map (fun (rect, tag) ->
            { x = rect.x + options.margin;
            y = rect.y + options.margin;
            w = rect.w - 2 * options.margin;
            h = rect.h - 2 * options.margin; },
            tag) in
    
    sprites
    |> List.iter (fun (rect, (name, _)) ->
        csv := 
            Printf.sprintf "%s\"%s\", %d, %d, %d, %d\n" 
                (!csv) 
                (Filename.chop_extension name)
                rect.x 
                rect.y 
                rect.w 
                rect.h);

    print_endline (!csv)
;;


let () =
    Sys.argv
    |> Array.to_list
    |> function
        | _ :: input_dir :: out :: options ->
            if Sys.is_directory input_dir |> not then
                failwith "<inputDir> must be a dirctory.";
            begin
                match parse_options default_options options with
                | Error () -> help ()
                | Ok options ->
                    get_image_files input_dir
                    |> List.map (fun (name, image) ->
                        { w = Stb_image.width image + 2 * options.margin;
                          h = Stb_image.height image + 2 * options.margin;
                          tag = name, image })
                    |> bin_pack
                    |> write_bin_pack_result options out
            end
            
        | _ -> help ()
;;


