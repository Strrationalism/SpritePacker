open Bin_pack;;
open Printf;;
open Stb_image;;
open Bigarray;;

let help () =
    print_endline "Sprite Packer";
    print_endline "by Strrationalism Studio 2021";
    print_endline "";
    print_endline "Usage:";
    print_endline "    sppak <inputDir> <output> [OPTIONS]";
    print_endline "";
    print_endline "Options:";
    print_endline "    --align-to-4       Align packed image to 4*N pixels.";
    print_endline "    --margin <margin>  Set margin in pixels for every sprite.";
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
        | Error (`Msg x) -> failwith x
        | Ok img -> 
            if 
                Array1.dim (data img) 
                <> width img * height img * 4 
            then
                failwith "You must pass png in 4 channels.";

            name, img)
;;


type align_mode =
    | No_align
    | Align_to_4
;;


type options = 
    { margin: int;
      align_mode: align_mode }
;;


let default_options =
    { margin = 0;
      align_mode = No_align }
;;


let rec parse_options prev_option =
    function
    | [] -> Ok prev_option
    | "--margin" :: margin_value :: other -> 
        parse_options 
            { prev_option with margin = int_of_string margin_value } 
            other
    | "--align-to-4" :: other ->
        parse_options
            { prev_option with align_mode = Align_to_4 }
            other
    | _ -> Error ()
;;


let write_bin_pack_result options out (result: _ Bin_pack.result) =
    let pack = 
        Array1.create 
            Int8_unsigned
            c_layout 
            (result.width * result.height * 4) in

    Array1.fill pack 0;

    let output_csv = open_out (out ^ ".csv") in
    fprintf output_csv "name, x, y, w, h\n";

    let sprites =
        result.rects
        |> List.map (fun (rect, tag) ->
            { x = rect.x + options.margin;
              y = rect.y + options.margin;
              w = rect.w - 2 * options.margin;
              h = rect.h - 2 * options.margin; },
              tag) in
    
    sprites
    |> List.iter (fun (rect, (name, image)) ->
        if width image <> rect.w then
            failwith "Width of image is not equals to rect.";

        if height image <> rect.h then
            failwith "Height of image is not equals to rect.";

        let image_pixels = data image in

        for src_y = 0 to rect.h - 1 do
            for src_x = 0 to rect.w - 1 do
                let dst_y = rect.y + src_y in
                let dst_x = rect.x + src_x in
                let dst_offset = 
                    (dst_y * result.width + dst_x) * 4 in
                
                let src_offset = 
                    (src_y * rect.w + src_x) * 4 in

                for channel = 0 to 3 do
                    Array1.get image_pixels (src_offset + channel)
                    |> Array1.set pack (dst_offset + channel)
                done
            done
        done;

        fprintf 
            output_csv
            "\"%s\", %d, %d, %d, %d\n" 
            (Filename.chop_extension name)
            rect.x 
            rect.y 
            rect.w 
            rect.h);

    close_out output_csv;
    
    let save_function =
        match Filename.extension out with
        | ".png" -> Stb_image_write.png
        | ".bmp" -> Stb_image_write.bmp
        | ".tga" -> Stb_image_write.tga
        | _ -> failwith "Unknown output type." in

    save_function out ~w:result.width ~h:result.height ~c:4 pack
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
                        { w = width image + 2 * options.margin;
                          h = height image + 2 * options.margin;
                          tag = name, image })
                    |> bin_pack
                    |> write_bin_pack_result options out
            end
            
        | _ -> help ()
;;


