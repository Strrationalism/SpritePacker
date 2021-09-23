open Bin_pack;;
open Printf;;


let test test_name test_case = 
    let rects =
        test_case
        |> List.concat_map (fun (w, h, count) ->
            List.init count (fun _ ->
                { w = w; h = h; tag = () })) in

    let start_time = Sys.time () in
    let result = bin_pack rects in
    let time_span = Sys.time () -. start_time in
    let all_area = result.width * result.height in
    let filled_area = sum_by (fun (r, _) -> r.w * r.h) result.rects in
    let fill_rate = float_of_int filled_area /. float_of_int all_area in
    
    printf 
        "rects:%3d\tfill-rate:%f%%\ttime:%.3fs\t in %s\n" 
        (List.length rects)
        (fill_rate *. 100.0) 
        time_span
        test_name 
;;


let () =
    test "simple" 
        [ 500, 200, 1;
          250, 200, 1;
          50, 50, 20 ];

    test "tall" 
        [ 50, 400, 2;
          50, 300, 5;
          50, 200, 10;
          50, 100, 20;
          50, 50, 40 ];

    test "wide" 
        [ 400, 50, 2;
          300, 50, 5;
          200, 50, 10;
          100, 50, 20;
          50, 50, 40 ];

    test "tallAndWide" 
        [ 100, 400, 3;
          400, 100, 3 ];

    test "powersOf2" 
        [ 2, 2, 256;
          4, 4, 128;
          8, 8, 64;
          16, 16, 32;
          32, 32, 16;
          64, 64, 8;
          128, 128, 4;
          256, 256, 2 ];

    test "oddAndEven" 
        [ 50, 50, 20;
          47, 31, 20;
          23, 17, 20;
          109, 42, 20;
          42, 109, 20;
          17, 33, 20 ];

    test "complex" 
        [ 100, 100, 3;
          60, 60, 3;
          50, 20, 20;
          20, 50, 20;
          250, 250, 1;
          250, 100, 1;
          100, 250, 1;
          400, 80, 1;
          80, 400, 1;
          10, 10, 100;
          5, 5, 500 ];

    test "superComplex" 
        [ 100, 100, 30;
          60, 60, 30;
          50, 20, 200;
          20, 50, 200;
          250, 250, 10;
          250, 100, 10;
          100, 250, 10;
          400, 80, 10;
          80, 400, 10;
          10, 10, 1000;
          5, 5, 5000 ];

    test "superSuperComplex" 
        [ 100, 100, 300;
          60, 60, 300;
          50, 20, 2000;
          20, 50, 2000;
          250, 250, 100;
          250, 100, 100;
          100, 250, 100;
          400, 80, 100;
          80, 400, 100;
          10, 10, 10000;
          5, 5, 50000 ]
          
